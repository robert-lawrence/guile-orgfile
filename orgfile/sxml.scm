

(define-module (orgfile sxml)
  #:use-module (orgfile parser)
  #:use-module (orgfile node)
  #:use-module (orgfile blocks)
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
    (document->sxml (parse-blocks port))))

(define (document->sxml d)
  (if (document-node? d)
      (fold+convert (node-children d))
      (error "not a document node")))

(define (node->sxml n)
  (cond ((section-node? n) (section-node->sxml n))
        ((list-node? n) (list-node->sxml n))
        ((item-node? n) (item-node->sxml n))
        ((paragraph-node? n) (paragraph-node->sxml n))
        ((text-node? n) (text-node->sxml n))
        (else (error "unrecognized node"))))

(define (section-node->sxml n)
  ;;TODO tags
  (let* ((level (node-get-data n 'level))
         (headline (node-get-data n 'headline))
         (tags (node-get-data n 'tags))
         (htag (string->symbol (string-append "h" (number->string (min 6 level))))))
    `(div (@ (class ,(string-join tags " "))) (,htag ,headline) ,@(fold+convert (node-children n)))))

(define (list-node->sxml n)
  (let* ((ordered? (node-get-data n 'ordered))
         (list-type (if ordered? 'ol 'ul)))
    `(,list-type ,(fold+convert (node-children n)))))

(define (item-node->sxml n)
  `(li ,(fold+convert (node-children n))))

(define (paragraph-node->sxml n)
  (let ((folded (fold+convert (node-children n))))
    `(p ,@folded)))

(define (text-node->sxml n)
  (string-join (reverse (node-children n))))

(define (fold+convert lst)
  (define (convert+cons elem prev)
    (cons (node->sxml elem) prev))
  (fold convert+cons '() lst))
