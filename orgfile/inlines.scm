;; Copyright (C) 2021 Robert Smith <robertsmith@posteo.net>

(define-module (orgfile inlines)
  #:use-module (orgfile parser)
  #:use-module (orgfile node)

  #:use-module (ice-9 peg)
  #:use-module (srfi srfi-1)
  #:export (parse-inlines))

(define (parse-inlines node)
  (transform-links (simplify-text-nodes node)))

(define (simplify-text-nodes n)
  (postorder-node-transform
   (lambda (type data children)
     (if (not (eq? type 'text))
         (make-node type data children)
         (make-node type data (list (string-join
                                     (reverse children)
                                     "\n")))))
   n))

(define (transform-links n)
  (postorder-node-transform
   (lambda (type data children)
     (make-node type
                data
                (append-map (lambda (child)
                              (if (text-node? child)
                                  (parse-links child)
                                  (list child)))
                            children)))
   n))

;; node -> (node ...)
(define (parse-links n)
  (define (match->link pm)
    (let* ((tree (peg:tree pm))
           (url (assoc-ref tree 'link-uri))
           (desc (assoc-ref tree 'link-description)))
      (make-link-node (car url) (if desc (car desc) #f))))
  (define (parse-links-str str)
    (let ((link-match (uri-link str)))
      (if link-match
          ;; reversed order
          (append (if (eqv? (peg:end link-match) (string-length str))
                      '()
                      (parse-links-str (substring str (peg:end link-match))))
                  (list (match->link link-match))
                  (if (eqv? 0 (peg:start link-match))
                      '()
                      (list (make-text-node (substring str 0 (peg:start link-match))))))
          (list (make-text-node str)))))
  (if (eqv? (length (node-children n)) 1)
      (parse-links-str (car (node-children n)))
      (error "text node must be normalized")))
