
(define-module (orgfile parser)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)

  #:export (parse-blocks))

(define simple-org-file
  "* TITLE
text
and more text

** subtitle
etc etc etc
")

;; This PEG will just organize the file into sections
;; the text blocks will then be processed for links, lists, etc
(define-peg-string-patterns
  "orgfile <-- initialBlock? block* !.
initialBlock <-- !header content
block <-- header content
content <-- (! NL .)* (blockCont (! NL .)*)* NL?
header <-- level title NLhidden?
specialLine <- header
level <-- STAR+ SPACE
title <-- (! NL .)*
blockCont <- NL !specialLine
SPACE < ' '
STAR <- '*'
NLhidden < '\n'
NL <- '\n'")


(define (parse-blocks input-port)
  (let ((pattern (match-pattern orgfile (get-string-all input-port))))
    (if (eq? pattern #f)
        #f
        (peg:tree pattern))))
