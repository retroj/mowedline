
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
     llog-unwatch
     llog-watch-only
     llog-unwatch-all
     llog-indent-string
     llog-line
     llog
     llog-unwind)

(import chicken scheme)

(use srfi-1
     extras)

(define llog-watches (make-parameter '()))

(define (llog-watch . syms)
  (llog-watches (lset-union eq? (llog-watches) syms)))

(define (llog-unwatch . syms)
  (llog-watches (lset-difference eq? (llog-watches) syms)))

(define (llog-watch-only . syms)
  (llog-watches syms))

(define (llog-unwatch-all)
  (llog-watches '()))

(define llog-depth (make-parameter 0))

(define llog-indent-string (make-parameter "  "))

(define (llog-line type format . args)
  (when (memq type (llog-watches))
    (llog-depth (+ 1 (llog-depth)))
    (apply printf (string-append "~A " format "~%") type args)))

(define (llog-unwind type)
  (when (memq type (llog-watches))
    (llog-depth (- (llog-depth) 1))))

(define-syntax llog
  (syntax-rules ()
    ((llog type format (args ...))
     (llog-line type format args ...))
    ((llog type format (args ...) form . forms)
     (dynamic-wind
         (lambda () #f)
         (lambda ()
           (llog type format (args ...))
           form . forms)
         (lambda () (llog-unwind type))))))

)
