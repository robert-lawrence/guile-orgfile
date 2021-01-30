;; Copyright (C) 2016-2018, 2020  Erik Edrosa <erik.edrosa@gmail.com>
;; Copyright (C) 2021 Robert Smith <robertsmith@posteo.net>

(define-module (orgfile blocks)
  #:use-module (orgfile parser)
  #:use-module (orgfile node)
  #:use-module (orgfile utils)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)

  #:export (parse-blocks))


;; Port -> Document
(define (parse-blocks port)
  "Parses CommonMark blocks from PORT returning a CommonMark Document tree"
  (let loop ((root (make-document-node))
             (line (read-line-without-nul port)))
    (if (eof-object? line)
        ;;TODO clean up list items with only one child (paragraph)
        root
        (loop (parse-open-block root (make-parser line))
              (read-line-without-nul port)))))

;; Node Parser -> Node
(define (parse-open-block node parser)
  (cond ((node-closed? node) node)
        ((document-node? node) (parse-document-block node parser))
        ((section-node? node) (parse-section-block node parser))
        ;((drawer-node? node) (parse-drawer node parser))
        ;((table-node? node) (parse-table node parser))
        ;((code-block-node? node) (parse-code-block node parser))
        ((list-node? node) (parse-list node parser))
        ; items always under list, list does not call this fn
        ;((item-node? node) (parse-item node parser))
        ((paragraph-node? node) (parse-paragraph node parser))))

;; read metadata from beginning of file
;; Node Parser -> Node
(define (parse-document-block node parser)
  (define (add-metadata node match)
    (let* ((str (match:string match))
           (key (string-downcase
                 (substring str
                            (+ 2 (match:start match))
                            (- (match:end match) 1))))
           (value (parser-rest-str
                   (parser-advance-next-nonspace
                    (make-parser (substring str (match:end match)))))))
      (node-add-data node (string->symbol key) value)))
  (if (node-get-data node '__init)
      (cond ((metadata parser) => (cut add-metadata node <>))
            (else (node-add-data (parse-container-block node parser)
                                 '__init
                                 #f)))
      (parse-container-block node parser)))

;; Node Parser -> Node
(define (parse-container-block node parser)
  (cond ((and (no-children? node) (empty-line parser)) ;; empty line
         node)
        ((no-children? node)                            ;; first line
         (add-child-node node (parse-line parser)))
        ((and (node-closed? (last-child node)) (not (empty-line parser))) ;; new block
         (add-child-node node (parse-line parser)))
        (else (let ((new-child (parse-open-block (last-child node) parser)))
                (cond ((and (not (empty-line parser))
                            (node-closed? new-child)
                            ; these nodes close themselves without reading the next line
                            ;(not (fenced-code-node? new-child))
                            )
                       (add-child-node (replace-last-child node new-child)
                                       (parse-line parser)))
                      (else (replace-last-child node new-child)))))))

(define (parse-section-block node parser)
  (cond ((and (no-children? node) (empty-line parser))
         node)
        ((let ((n (parse-line parser)))
           (and (section-node? n)
              (<= (node-get-data n 'level) (node-get-data node 'level))))
         (close-node node))
        (else (parse-container-block node parser))))

(define (parse-list node parser)
  (let* ((nonspace-parser (parser-advance-next-nonspace parser))
         (line-indent (parser-indent parser nonspace-parser))
         (item-match (list-item nonspace-parser))
         (list-indent (node-get-data node 'indent))
         (node (if (and (not (empty-line parser)) (node-get-data node 'last-line-empty))
                   (node-add-data node 'last-line-empty #f)
                   node))
         (child (last-child node)))
    (cond ((and (empty-line parser) (eq? (node-get-data node 'last-line-empty) #t))
           (close-node node))
          ((empty-line parser)
           (node-add-data node 'last-line-empty #t))
          ((< line-indent list-indent)
           (close-node node))
          ((eqv? line-indent list-indent)
           (if item-match
               (add-child-node node (make-item nonspace-parser list-indent item-match))
               (close-node node)))
          (else (replace-last-child node (parse-item (last-child node) parser nonspace-parser))))))

(define (parse-item node parser nonspace-parser)
;; item node is always created with a child
  (let ((child (last-child node)))
    (if (node-closed? child)
        (add-child-node node (parse-line parser))
        (let ((new-child (parse-open-block child parser)))
          (if (node-closed? new-child)
              (add-child-node node (parse-line parser))
              (replace-last-child node new-child))))))

(define (parse-paragraph node parser)
  (let ((parsed-line (parse-line parser)))
    (cond ((section-node? parsed-line)
           (close-node node))
          ((paragraph-node? parsed-line)
           (replace-last-child node (join-text-nodes (last-child node) (last-child parsed-line))))
          (else (close-node node)))))

;; Parser -> Node
(define (parse-line parser)
  (let ((nonspace-parser (parser-advance-next-nonspace parser)))
    (cond ((empty-line nonspace-parser)              (make-blank-node))
          ((section-headline parser)                => (cut make-section parser <>))
          ((and
            (parser-indented? parser nonspace-parser)
            (list-item nonspace-parser))            => (cut make-new-list parser nonspace-parser <>))
          ;((parser-indented? parser nonspace-parser) (make-code-block parser))
          ;((block-quote nonspace-parser)          => make-block-quote)
          ;((atx-heading nonspace-parser)          => make-atx-heading)
          ;((fenced-code nonspace-parser)          => make-fenced-code)
          ;((bullet-list-marker nonspace-parser)   => (cut make-bullet-list-marker parser <>))
          ;((ordered-list-marker nonspace-parser)  => (cut make-ordered-list-marker parser <>))
          (else                                      (make-paragraph nonspace-parser)))))


(define (make-section parser match)
  (let* ((level (- (match:end match) 1))
         (str (parser-rest-str parser))
         (tag-match (section-tags parser))
         (headline (if tag-match
                       (substring str (+ level 1) (match:start tag-match))
                       (substring str (+ level 1))))
         (tags (if tag-match
                   (get-section-tags (substring str (match:start tag-match)))
                   '())))
    (make-section-node level headline tags)))

(define (make-new-list parser nonspace-parser match)
  (let* ((indent (parser-indent parser nonspace-parser))
         (ordered (string->number (substring (parser-rest-str nonspace-parser) 0 1)))
         (data `((ordered . ,ordered) (indent . ,indent)))
         (item (make-item nonspace-parser indent match)))
    (make-list-node item data)))

(define (make-item nonspace-parser indent match)
  (let* ((rest (substring (match:string match) (match:end match)))
         (text (parse-line (make-parser rest))))
    (if (blank-node? text)
        (make-item-node indent (make-paragraph (make-parser rest)))
        (make-item-node indent text))))

(define (get-section-tags str)
  ;;remove first and last element
  (reverse (cdr (reverse (cdr (string-split str #\:))))))

(define (make-paragraph parser)
  (make-paragraph-node (parser-rest-str parser)))
