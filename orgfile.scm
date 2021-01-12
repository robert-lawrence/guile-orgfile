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
  #:use-module (orgfile sxml)
  #:re-export (orgfile->sxml))


