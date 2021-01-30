;; Copyright (C) 2016-2018, 2020  Erik Edrosa <erik.edrosa@gmail.com>
;; Copyright (C) 2021 Robert Smith <robertsmith@posteo.net>

(define-module (orgfile parser)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 peg)

  #:export (make-parser
            parser-char=?
            parser-end?
            parser-advance
            parser-advance-optional
            parser-advance-next-nonspace
            parser-advance-min-spaces
            parser-advance-next-line
            parser-indented?
            parser-indent
            parser-rest-str
            empty-line
            section-headline
            section-tags
            list-item
            metadata
            uri-link
            ))


(define-record-type <parser>
  (%make-parser str pos col)
  parser?
  (str parser-str)
  (pos parser-pos)
  (col parser-col))

(define (make-parser str)
  (%make-parser str 0 0))

(define (parser-char=? parser ch)
  (char=? (string-ref (parser-str parser) (parser-pos parser))
          ch))

(define (parser-end? parser)
  (>= (parser-pos parser) (string-length (parser-str parser))))

(define (parser-advance parser offset)
  (let ((str (parser-str parser)))
    (let loop ((pos (parser-pos parser))
               (col (parser-col parser))
               (count offset))
      (cond ((>= pos (string-length str))
                (%make-parser str pos col))
            ((<= count 0)
             (%make-parser str pos col))
            ((char=? (string-ref str pos) #\tab)
             (let ((col-change (- 4 (modulo col 4))))
               (if (>= count col-change)
                   (loop (+ pos 1) (+ col col-change) (- count col-change))
                   (%make-parser str pos (+ col count)))))
            (else (loop (+ pos 1) (+ col 1) (- count 1)))))))

(define (parser-advance-optional parser ch)
  (define new-parser (cut %make-parser
                          (parser-str parser)
                          (+ (parser-pos parser) 1)
                          <>))
  (if (and (not (parser-end? parser)) (parser-char=? parser ch))
      (new-parser (+ (parser-col parser)
                     (case ch
                       ((#\tab) (- 4 (modulo (parser-col parser) 4)))
                       (else 1))))
      parser))

(define (parser-advance-next-nonspace parser)
  (let ((str (parser-str parser)))
    (let loop ((pos (parser-pos parser))
               (col (parser-col parser)))
      (if (>= pos (string-length str))
          (%make-parser str pos col)
          (case (string-ref str pos)
            ((#\space) (loop (+ pos 1) (+ col 1)))
            ((#\tab)   (loop (+ pos 1) (+ col (- 4 (modulo col 4)))))
            (else (%make-parser str pos col)))))))

(define (parser-advance-min-spaces parser n)
  (let ((str (parser-str parser)))
    (let loop ((pos (parser-pos parser))
               (col (parser-col parser))
               (count n))
      (cond ((>= pos (string-length str))
             (%make-parser str pos col))
            ((<= count 0)
             (%make-parser str pos col))
            ((char=? (string-ref str pos) #\space)
             (loop (+ pos 1) (+ col 1) (- count 1)))
            ((char=? (string-ref str pos) #\tab)
             (let ((col-change (- 4 (modulo col 4))))
               (if (>= count col-change)
                   (loop (+ pos 1) (+ col col-change) (- count col-change))
                   (%make-parser str pos (+ col count)))))
            (else (%make-parser str pos col))))))

(define (parser-advance-next-line parser)
  (let ((str (parser-str parser)))
    (let loop ((pos (parser-pos parser))
               (col (parser-col parser)))
      (cond ((>= pos (string-length str))
             (%make-parser str pos col))
            ((char=? (string-ref str pos) "\n")
             (%make-parser str (+ 1 pos) 0))
            ((char=? (string-ref str pos) "\t")
             (let ((col-change (- 4 (modulo col 4))))
               (loop (+ pos 1) (+ col col-change))))
            (else (loop (+ pos 1) (+ col 1)))))))

(define (parser-indented? start end)
  (>= (- (parser-col end) (parser-col start)) 1))

(define (parser-indent start end)
  (- (parser-col end) (parser-col start)))

(define (parser-rest-str parser)
  (let ((str (parser-str parser))
        (pos (parser-pos parser)))
    (if (or (>= pos (string-length str))
            (not (char=? (string-ref str pos) #\tab)))
        (substring str pos)
        (let* ((col (parser-col parser))
               (expand (- 4 (modulo col 4))))
          (if (= expand 0)
              (substring str pos)
              (string-append (make-string expand #\space) (substring str (+ pos 1))))))))


(define re-empty-line (make-regexp "^[ \t]*$"))
(define re-section-headline (make-regexp "^\\*+ "))
(define re-section-tags (make-regexp "[ \t](:[^ \t]+)+:[ \t]*$"))
(define re-list-item (make-regexp "^[1-9][:digit:]*\\) |^[1-9][:digit:]*\\. |^- |^\\+ |^\\* "))
(define re-metadata (make-regexp "^#\\+[a-zA-Z]+:"))
;; Peg for link
(define-peg-pattern brace body (or "[" "]"))
(define-peg-pattern doubleback body (and (ignore "\\") "\\"))
(define-peg-pattern escaped-brace body (and (* doubleback)
                                            (ignore "\\")
                                            brace))
(define-peg-pattern bslashes body (and (+ "\\") (not-followed-by (or brace "\\"))))
;TODO: uri validation
(define-peg-pattern link-uri all
  (and (ignore "[")
       (+ (or escaped-brace
              bslashes
              (and (not-followed-by (or "\\" "[" "]"))
                   peg-any)))
       (* doubleback)
       (ignore "]")))
;; just a copy of link-uri
(define-peg-pattern link-description all
  (and (ignore "[")
       (+ (or escaped-brace
              bslashes
              (and (not-followed-by (or "\\" "[" "]"))
                   peg-any)))
       (* doubleback)
       (ignore "]")))
(define-peg-pattern full-link all
  (and (ignore "[")
       link-uri
       (? link-description)
       (ignore "]")))

(define (empty-line parser)
  (regexp-exec re-empty-line (parser-str parser) (parser-pos parser)))

(define (section-headline parser)
  (regexp-exec re-section-headline (parser-str parser) (parser-pos parser)))

(define (section-tags parser)
  (regexp-exec re-section-tags (parser-str parser) (parser-pos parser)))

(define (list-item parser)
  (regexp-exec re-list-item (parser-str parser) (parser-pos parser)))

(define (metadata parser)
  (regexp-exec re-metadata (parser-str parser) (parser-pos parser)))

(define (uri-link str)
  (search-for-pattern full-link str))
