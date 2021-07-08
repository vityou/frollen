#lang racket/base

(require racket/contract
         (for-syntax racket/base
                     syntax/parse)
         racket/path
         racket/file)

; if first value errors, use second
(define-syntax define-with-fallback
  (syntax-parser
    [(_ name:id try-value:expr fallback-value:expr)
     #'(define name (with-handlers ([exn:fail?
                                     (lambda (exn) fallback-value)])
                      try-value))]))

(define-syntax in-dir
  (syntax-parser
    [(_ dir expr) #'(parameterize ([current-directory dir])
                      expr)]))

(define/contract (build config-file)
  (-> path-string? any/c)

  ; me when I see a string:
  ; "we don't do that here"
  ; Probably could use a coersion contract, but I dont know
  ; how they work
  (when (string? config-file)
    (set! config-file (string->path config-file)))


  ; where the project is.
  ; things like the config file and the source dir are in here
  (define enclosing-dir (simplify-path (build-path config-file "..")))


  (define-with-fallback source
    (in-dir enclosing-dir
            (path->complete-path (dynamic-require config-file 'source)))
    (build-path enclosing-dir "src"))
  
  (define-with-fallback output-dir
    (in-dir enclosing-dir
            (path->complete-path (dynamic-require config-file 'output-dir)))
    (build-path enclosing-dir "site"))

  (define-with-fallback template-dir
    (in-dir enclosing-dir
            (path->complete-path (dynamic-require config-file 'template-dir)))
    (build-path enclosing-dir "templates"))

  ; which files in the posts dir are made into actual posts
  ; takes a path, and determines if it is a post file
  (define-with-fallback post-file-filter*
    (dynamic-require config-file 'post-file-filter)
    (λ (path) (equal? (path-get-extension path) #".rkt")))

  ; add a check to make sure path is a file and not a directory
  (define post-file-filter (λ (path) (and (file-name-from-path path)
                                          (post-file-filter* path))))

  (define-with-fallback index-file
    (in-dir enclosing-dir
            (path->complete-path (dynamic-require config-file 'index-file)))
    (build-path enclosing-dir "index.rkt"))
  
  ; check if things exist
  (when (not (file-exists? index-file))
    (error (format "index file: \"~a\" doesn't exist" index-file)))
  (when (not (directory-exists? source))
    (error (format "source dir: \"~a\" doesn't exist" source)))
  (when (not (directory-exists? (build-path enclosing-dir "posts")))
    (error (format "no \"posts\" dir within: \"~a\"" enclosing-dir)))
  ; user will have to manually specify each template
  (when (not (directory-exists? template-dir))
    (set! template-dir #f))

  (define post-file-list (filter post-file-filter
                                 (directory-list (build-path enclosing-dir "posts")
                                                 #:build? #t)))


  ; build all of the files as posts
  (map (λ (file) (build-file file
                             (build-path template-dir "post.rkt")
                             config-file
                             output-dir))
       post-file-list)



  ; a list that contains a list for each post
  ; each sublist contains the output name of the file,
  ; the `doc` export from the file, and the `metas` from the file
  ; this is given to `index-proc` from the index file
  (define posts-index-info (map (λ (file)
                                  (list (get-output-name file
                                                         (hash-ref (dynamic-require file 'metas)
                                                                   'template
                                                                   #f))
                                        (dynamic-require file 'doc)
                                        (dynamic-require file 'metas)))
                                post-file-list))

  (define index-proc (dynamic-require index-file 'index-proc))


  (define index-template (dynamic-require index-file 'template-proc))
  
  (display-to-file (index-template (index-proc posts-index-info config-file) (dynamic-require index-file 'metas) config-file)
                   (build-path output-dir
                               "index.html")
                   #:exists 'replace)
  
  
  posts-index-info)

; builds a finished file to output-dir
(define (build-file file default-template config-file output-dir)

  (when (string? file)
    (set! file (string->path file)))
  

  (define doc (dynamic-require file 'doc))
  (define metas (dynamic-require file 'metas))
  (define template-file (in-dir (build-path file "..")
                                (path->complete-path
                                 (hash-ref metas
                                           'template
                                           default-template))))

  
  (define output-file-name (get-output-name file template-file))

  (define template-proc (dynamic-require template-file 'template-proc))

  ; apply template-proc to the file contents and write to the output-file
  (display-to-file (template-proc doc metas config-file)
                   (build-path output-dir
                               output-file-name)
                   #:exists 'replace))

(define (get-output-name file-path [template-file-path #f])

  (define output-extension
    (if template-file-path
        (with-handlers ([exn:fail? (λ (exn) ".html")])
          (dynamic-require template-file-path 'file-extension))
        ".html"))
  
  (define file-name
    (hash-ref (dynamic-require file-path 'metas)
              'output-name
              (path->string
               (file-name-from-path
                (path-replace-extension
                 file-path
                 output-extension)))))
  file-name)


(define ns (build "/mnt/c/Users/zlee3/frollen-proj/test-config.rkt"))