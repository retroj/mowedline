
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

(import chicken scheme extras)

(use srfi-1
     coops
     data-structures
     (prefix dbus dbus:)
     filepath
     list-utils
     miscmacros
     posix)

(include "command-line")
(import command-line)

(include "version")

(define rest cdr)

(define (start-client commands)
  (for-each (lambda (cmd) ((callinfo-thunk cmd)))
            commands))


(define client-options
  (make-command-group
   ((quit)
    doc: "quit the program"
    (let ((dbus-context
           (dbus:make-context service: 'mowedline.server
                              interface: 'mowedline.interface)))
      (dbus:call dbus-context "quit")))

   ((read widget)
    doc: "updates widget by reading lines from stdin"
    (let ((dbus-context
           (dbus:make-context service: 'mowedline.server
                              interface: 'mowedline.interface)))
      (let loop ()
        (let ((line (read-line (current-input-port))))
          (unless (eof-object? line)
            (when (equal? '(#f) (dbus:call dbus-context "update" widget line))
              (printf "widget not found, ~S~%" widget))
            (loop))))))

   ((update widget value)
    doc: "updates widget with value"
    (let ((dbus-context
           (dbus:make-context service: 'mowedline.server
                              interface: 'mowedline.interface)))
      (when (equal? '(#f) (dbus:call dbus-context "update" widget value))
        (printf "widget not found, ~S~%" widget))))))


(define special-options
  (make-command-group
   ((help)
    doc: "displays this help"
    (let ((longest
           (fold max 0
                 (map
                  (lambda (def)
                    (apply + 2 (string-length (command-name-string def))
                           (* 3 (length (command-args def)))
                           (map (compose string-length symbol->string)
                                (command-args def))))
                  (append client-options special-options))))
          (docspc 3))
      (define (help-section option-group)
        (for-each
         (lambda (def)
           (let ((col1 (apply string-append " -" (command-name-string def)
                              (map (lambda (a)
                                     (string-append " <" (symbol->string a) ">"))
                                   (command-args def)))))
             (display col1)
             (when (command-doc def)
               (dotimes (_ (+ docspc (- longest (string-length col1)))) (display " "))
               (display (command-doc def)))
             (newline)))
         option-group))
      (printf "mowedline version ~A, by John J. Foerch~%" version)
      (printf "~%SPECIAL OPTIONS  (evaluate first one and exit)~%~%")
      (help-section special-options)
      (printf "~%CLIENT OPTIONS~%~%")
      (help-section client-options)
      (newline)))

   ((version)
    doc: "prints the version"
    (printf "mowedline version ~A, by John J. Foerch~%" version))))


(let-values (((client-commands special-commands)
              (parse-command-line (command-line-arguments)
                                  client-options
                                  special-options)))
  (cond
   ((not (null? special-commands))
    (let ((cmd (first special-commands)))
      ((callinfo-thunk cmd)))
    (unless (and (null? (rest special-commands))
                 (null? client-commands))
      (printf "~%Warning: the following commands were ignored:~%")
      (for-each
       (lambda (x) (printf "  ~S~%" (cons (callinfo-name x) (callinfo-args x))))
       (append! (rest special-commands) client-commands))))
   (else
    (start-client client-commands))))
