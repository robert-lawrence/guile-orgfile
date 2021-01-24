;; Copyright (C) 2016  Erik Edrosa <erik.edrosa@gmail.com>
;;
;; This file is part of guile-orgfile
;;
;; guile-orgfile is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-orgfile is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with guile-orgfile.  If not, see <http://www.gnu.org/licenses/>.

(define-module (orgfile common)
  #:use-module (ice-9 regex)
  #:use-module (orgfile entities)
  #:export (ascii-punctuation-characters
            escaped-characters
            control-characters
            regular-characters
            in-parens-no-space
            link-destination
            link-title
            link-label
            re-entity-or-numeric
            remove-quotes
            entity->string
            unescape-string))

;; ']' needs to be the first character after an openning '[' to be able
;; to match ']'
(define ascii-punctuation-characters "]!\"#$%&'()*+,-./:;<=>?@[\\^_`{|}~")
(define escaped-characters (string-append "\\\\[" ascii-punctuation-characters "]"))
;; can't use range as it is not portable
(define control-characters "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19")
(define regular-characters (string-append "[^" control-characters " ()\\\\]"))
(define in-parens-no-space (string-append "\\((" regular-characters "|" escaped-characters "|\\\\)*\\)"))
(define link-destination (string-append "((" regular-characters "+|"
                                        escaped-characters "|"
                                        "[\\][^() \t\v\f\r\n]|"
                                        in-parens-no-space ")+)"))
(define link-title (string-append "((\"(" escaped-characters "|[^\"])*\"|"
                                  "'(" escaped-characters "|[^'])*'|"
                                  "\\((" escaped-characters "|[^)])*\\)))"))
(define link-label (string-append "\\[(([^][]|"
                                     escaped-characters
                                     "){1,1000})\\]"))
(define decimal-numeric "#[0-9]{1,8}")
(define hexadecimal-numeric "#x[0-9a-f]{1,8}")
(define entity "[a-z][a-z0-9]{1,31}")
(define entity-or-numeric (string-append "&(" decimal-numeric
                                         "|" hexadecimal-numeric
                                         "|" entity ");"))
(define re-entity-or-numeric (make-regexp (string-append "^" entity-or-numeric) regexp/icase))

(define re-escaped-or-entity (make-regexp (string-append escaped-characters
                                                         "|" entity-or-numeric) regexp/icase))

(define (remove-quotes str)
  (substring str 1 (- (string-length str) 1)))

(define (entity->string str)
  (define (decimal? str)
    (and (char=? (string-ref str 0) #\#) (not (char-ci=? (string-ref str 1) #\x))))

  (define (hexadecimal? str)
    (and (char=? (string-ref str 0) #\#) (char-ci=? (string-ref str 1) #\x)))

  (define (numeric->string str base)
    (let ((ch (false-if-exception (integer->char (string->number str base)))))
      (string (if (and ch (not (char=? ch #\nul)))
                  ch
                  #\xfffd))))

  (cond ((decimal? str) (numeric->string (substring str 1) 10))
        ((hexadecimal? str) (numeric->string (substring str 2) 16))
        (else (let ((codepoints (entity->codepoints str)))
                (and codepoints (list->string (map integer->char codepoints)))))))

(define (unescape-string str)
  (define (replace m)
    (let ((str (match:substring m 0)))
      (if (char=? (string-ref str 0) #\\)
          (string (string-ref str 1))
          (or (and (> (string-length str) 1)
                   (entity->string (substring str 1 (- (string-length str) 1))))
              str))))
  (regexp-substitute/global #f re-escaped-or-entity str
                            'pre replace 'post))
