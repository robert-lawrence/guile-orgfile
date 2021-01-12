

(define-module (orgfile sxml)
  #:use-module (orgfile parser)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:export (orgfile->sxml))

(define* (orgfile->sxml #:optional (string-or-port (current-input-port)))
  "Parses an org document from the provided port or the current input port to
sxml."
  (let ((port (if (string? string-or-port)
                  (open-input-string string-or-port)
                  string-or-port)))
    ;;TODO
    (document->sxml (parse-blocks port))))

(define (initialBlock->sxml initialBlock)
  `(p ,@(node->sxml (node-children initialBlock))))

(define (block->sxml block)
  `(div ,(header->sxml (assq-ref block 'header))
    (p ,@(node->sxml (node-children block)))))

(define (header->sxml header)
  (let ((num (min (string-length (car (assoc-ref header 'level))) 4)))
    `(,(string->symbol (string-append "h" (number->string num))) ,(car (assoc-ref header 'title)))))

(define (node-children node)
  ;;TODO: this is just a dummy
  (assoc 'content (cdr node)))

(define (node->sxml n)
  (if (list? n)
      (let ((nname (car n)))
        (cond ((eq? nname 'initialBlock) (initialBlock->sxml n))
              ((eq? nname 'block) (block->sxml n))
              ((eq? nname 'content) `(,(cadr n)))))
      #f))

(define (document->sxml doc)
  (if (eq? doc #f)
      #f
      (cons
       'div
       (map node->sxml doc))))
