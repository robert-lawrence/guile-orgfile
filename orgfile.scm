;; Copyright (C) 2021  Robert Smith <robertsmith@posteo.net>
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

(define-module (orgfile)
  #:use-module (orgfile blocks)
  #:use-module (orgfile inlines)
  #:use-module (orgfile node)
  #:use-module (orgfile sxml)
  #:export (parse-orgfile
            orgfile-get-metadata
            orgfile->sxml))

(define* (parse-orgfile #:optional (string-or-port (current-input-port)))
  "Parses an org document from the provided port or the current input port."
  (let ((port (if (string? string-or-port)
                  (open-input-string string-or-port)
                  string-or-port)))
    (parse-inlines (parse-blocks port))))

(define (orgfile-get-metadata orgfile)
  "Returns metadata read from the beginning of an orgfile as an association
list of key value pairs. Returns false if argument is not a valid orgfile."
  (if (document-node? orgfile)
      (filter (lambda (a)
                (not (eq? (car a) '__init)))
              (node-data orgfile))
      #f))

(define (orgfile->sxml orgfile)
  "Converts a parsed orgfile to sxml."
  (if (document-node? orgfile)
      (document->sxml orgfile)
      #f))
