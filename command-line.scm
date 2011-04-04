
;; This file is part of mowedline.
;; Copyright (C) 2011  John J. Foerch
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

(module command-line
        (make-command
         command-name
         command-args
         command-doc
         command-body
         command-name-string
         make-command-group
         call-info-name
         call-info-args
         call-info-thunk
         parse-command-line)

(import chicken scheme extras)

(use srfi-1
     srfi-13)

(define rest cdr)

(define-record command
  name args doc body)

(define (command-name-string command-def)
  (symbol->string (command-name command-def)))

(define-syntax mk-command
  (syntax-rules (#:doc)
    ((mk-command (name . args) #:doc doc . body)
     (make-command 'name 'args doc (lambda args . body)))
    ((mk-command (name . args) . body)
     (make-command 'name 'args #f (lambda args . body)))))

(define-syntax make-command-group
  (syntax-rules ()
    ((make-command-group command ...)
     (list (mk-command . command) ...))))

(define (find-command-def name command-group)
  (find (lambda (x) (equal? name (command-name-string x)))
        command-group))


(define-record call-info
  name args thunk)

(define (mkcmd def args)
  (let ((name (command-name-string def))
        (body (command-body def)))
    (make-call-info name args
                    (lambda () (apply body args)))))

(define (parse-command-line input . command-groups)
  (let ((out (map (lambda (x) (list)) command-groups)))
    (define (loop input count)
      (if (null? input)
          (apply values out)
          (let* ((opsym (first input))
                 (input (rest input))
                 (count (- count 1))
                 (op (string-trim opsym #\-))
                 (def #f)
                 (group-index (list-index
                               (lambda (group)
                                 (set! def (find-command-def op group))
                                 def)
                               command-groups)))
            (unless def
              (error (sprintf "unexpected symbol ~S~%" opsym)))
            (let ((narg (length (command-args def))))
              (when (< count narg)
                (error (sprintf "~A requires ~A arguments, but only ~A were given"
                                op narg count)))
              (let ((d (list-tail out group-index)))
                (set-car! d (append! (car d) (list (mkcmd def (take input narg))))))
              (loop (list-tail input narg) (- count narg))))))
    (loop input (length input))))

)
