
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

(define dbus-context
  (dbus:make-context service: 'mowedline.server
                     interface: 'mowedline.interface))

(icla:help-heading
 (sprintf "mowedline-client version ~A, by John J. Foerch" version))

(icla:define-command-group client-options
 ((quit)
  doc: "quit the program"
  (dbus:call dbus-context "quit"))

 ((read widget)
  doc: "updates widget by reading lines from stdin"
  (let loop ()
    (let ((line (read-line (current-input-port))))
      (unless (eof-object? line)
        (when (equal? '(#f) (dbus:call dbus-context "update" widget line))
          (printf "widget not found, ~S~%" widget))
        (loop)))))

 ((update widget value)
  doc: "updates widget with value"
  (when (equal? '(#f) (dbus:call dbus-context "update" widget value))
    (printf "widget not found, ~S~%" widget)))

 ((log symlist)
  doc: "enable or disable logging; [+-]SYM1,[+-]SYM2,..."
  (dbus:call dbus-context "log" symlist)))

(icla:parse (command-line-arguments))
