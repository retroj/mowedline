
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

(import chicken scheme)

(use srfi-1
     extras
     (prefix dbus dbus:)
     (prefix imperative-command-line-a icla:))

(include "version")

(define rest cdr)

(define (start-client commands)
  (for-each (lambda (cmd) ((icla:callinfo-thunk cmd)))
            commands))

(icla:help-heading
 (sprintf "mowedline-client version ~A, by John J. Foerch" version))

(icla:add-command-group
 "CLIENT OPTIONS"
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
      (printf "widget not found, ~S~%" widget))))

 ((log symlist)
  doc: "enable or disable logging; [+-]SYM1,[+-]SYM2,..."
  (let ((dbus-context
         (dbus:make-context service: 'mowedline.server
                            interface: 'mowedline.interface)))
    (dbus:call dbus-context "log" symlist))))

(let-values (((client-commands special-commands)
              (icla:parse (command-line-arguments)
                          (cdr (second (icla:groups)))
                          (cdr (first (icla:groups))))))
  (cond
   ((not (null? special-commands))
    (let ((cmd (first special-commands)))
      ((icla:callinfo-thunk cmd)))
    (unless (and (null? (rest special-commands))
                 (null? client-commands))
      (printf "~%Warning: the following commands were ignored:~%")
      (for-each
       (lambda (x) (printf "  ~S~%" (cons (icla:callinfo-name x) (icla:callinfo-args x))))
       (append! (rest special-commands) client-commands))))
   (else
    (start-client client-commands))))
