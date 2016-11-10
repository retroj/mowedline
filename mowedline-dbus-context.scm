;; This file is part of mowedline.
;; Copyright (C) 2011-2015  John J. Foerch
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

(use (prefix dbus2 dbus:))

(define mowedline-dbus-path "/net/retroj/mowedline")
(define mowedline-dbus-service 'net.retroj.mowedline)
(define mowedline-dbus-interface 'net.retroj.mowedline)

(define mowedline-dbus-context
  (dbus:make-context service: mowedline-dbus-service
                     interface: mowedline-dbus-interface
                     path: mowedline-dbus-path))
