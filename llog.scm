
;; This file is part of mowedline.
;; Copyright (C) 2011-2013  John J. Foerch
;;
;; mowedline is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; mowedline is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with mowedline.  If not, see <http://www.gnu.org/licenses/>.

(module llog
    (llog-watch  
     llog-line
     llog)

(import chicken scheme)

(use extras)

(define llog-watch (make-parameter '()))

(define (llog-line type format . args)
  (when (memq type (llog-watch))
    (apply printf (string-append "~A " format "~%") type args)))

(define-syntax llog
  (syntax-rules ()
    ((llog type format (args ...))
     (llog-line type format args ...))))

)
