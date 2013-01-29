
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

(import chicken scheme extras foreign)

(use srfi-1
     srfi-4 ;; homogeneous numeric vectors
     srfi-18 ;; threads
     srfi-69 ;; hash tables
     coops
     data-structures
     (prefix dbus dbus:)
     filepath
     list-utils
     lolevel
     miscmacros
     posix
     regex ;; only needed for my .mowedline!
     xft
     (except xlib make-xrectangle
                  xrectangle-x xrectangle-y
                  xrectangle-width xrectangle-height)
     xtypes)

(include "command-line")
(import command-line)

(include "version")


;;;
;;; Language
;;;

(define L list)
(define rest cdr)


;;;
;;; Globals
;;;

(define *display* #f)

(define *windows* (list))

(define *widgets* (make-hash-table test: equal?))

(define *default-widgets* (list))

(define *internal-events* (make-queue))


;;;
;;; Window Property Utils
;;;

(define-record window-property
  type format data count)

(define (make-atom-property atom-name)
  (let ((data (xinternatom *display* atom-name 0)))
    (let-location ((data unsigned-long data))
      (make-window-property "ATOM" 32
                            (location data)
                            1))))

(define (make-number-property number)
  (let-location ((data unsigned-long number))
    (make-window-property "CARDINAL" 32 (location data) 1)))

(define (make-numbers-property numbers)
  (let* ((vec (list->u32vector numbers))
         (len (u32vector-length vec))
         (lvec ((foreign-lambda* c-pointer ((u32vector s) (int length))
                  "unsigned long * lvec = malloc(sizeof(unsigned long) * length);"
                  "int i;"
                  "for (i = 0; i < length; i++) {"
                  "    lvec[i] = s[i];"
                  "}"
                  "C_return(lvec);")
                vec len)))
    (set-finalizer! lvec free)
    (make-window-property "CARDINAL" 32 lvec len)))

(define (make-text-property textp)
  (let ((tp (make-xtextproperty)))
    (set-xtextproperty-value! tp (make-locative textp))
    (set-xtextproperty-encoding! tp XA_STRING)
    (set-xtextproperty-format! tp 32)
    (set-xtextproperty-nitems! tp 1)
    tp))

(define (window-property-set win key value)
  (xchangeproperty *display* win
                   (xinternatom *display* key 0)
                   (xinternatom *display* (window-property-type value) 0)
                   (window-property-format value)
                   PROPMODEREPLACE
                   (window-property-data value)
                   (window-property-count value)))

(define (window-property-append win key value)
  (xchangeproperty *display* win
                   (xinternatom *display* key 0)
                   (xinternatom *display* (window-property-type value) 0)
                   (window-property-format value)
                   PROPMODEAPPEND
                   (window-property-data value)
                   (window-property-count value)))


(define (switch-to-desktop/number desktop)
  (let ((root (xrootwindow *display* (xdefaultscreen *display*)))
        (event-mask (bitwise-ior SUBSTRUCTURENOTIFYMASK
                                 SUBSTRUCTUREREDIRECTMASK))
        (event (make-xclientmessageevent)))
    (set-xclientmessageevent-type! event CLIENTMESSAGE)
    (set-xclientmessageevent-serial! event 0)
    (set-xclientmessageevent-send_event! event 1)
    (set-xclientmessageevent-display! event *display*)
    (set-xclientmessageevent-window! event root)
    (set-xclientmessageevent-message_type!
     event (xinternatom *display* "_NET_CURRENT_DESKTOP" 0))
    (set-xclientmessageevent-format! event 32)
    (define make-event-data-l
      (foreign-lambda* void (((c-pointer long) data_l)
                             (long l0) (long l1) (long l2)
                             (long l3) (long l4))
        "data_l[0] = l0; data_l[1] = l1;"
        "data_l[2] = l2; data_l[3] = l3;"
        "data_l[4] = l4;"))
    (make-event-data-l (xclientmessageevent-data-l event)
                       desktop (current-seconds) 0 0 0)
    (xsendevent *display* root 0 event-mask event)))


(define (switch-to-desktop desktop)
  (cond
   ((number? desktop) (switch-to-desktop/number desktop))
   ((string? desktop)
    (let ((root (xrootwindow *display* (xdefaultscreen *display*)))
          (MAX_PROPERTY_VALUE_LEN 4096)
          (property (xinternatom *display* "_NET_DESKTOP_NAMES" 0))
          (req_type (xinternatom *display* "UTF8_STRING" 0)))
      (let-location ((xa_ret_type unsigned-long)
                     (ret_format int)
                     (ret_nitems unsigned-long)
                     (ret_bytes_after unsigned-long)
                     (ret_prop unsigned-c-string*))
        (xgetwindowproperty *display* root property
                            0 (/ MAX_PROPERTY_VALUE_LEN 4) 0
                            req_type
                            (location xa_ret_type)
                            (location ret_format)
                            (location ret_nitems)
                            (location ret_bytes_after)
                            (location ret_prop))
        (assert (= req_type xa_ret_type))
        (define find-desktop
          (foreign-lambda* int ((unsigned-c-string target)
                                ((c-pointer unsigned-c-string) names)
                                (unsigned-long nitems))
            "int i, d = 0, atstart = 1;"
            "for (i = 0; i < nitems; i++) {"
            "    if (atstart) {"
            "        if (0 == strcmp(target, &names[0][i]))"
            "            C_return(d);"
            "        atstart = 0;"
            "    }"
            "    if (names[0][i] == 0) {"
            "        atstart = 1;"
            "        d++;"
            "    }"
            "}"
            "C_return(-1);"))
        (let ((desktop-number (find-desktop desktop (location ret_prop) ret_nitems)))
          (when (> desktop-number -1)
            (switch-to-desktop/number desktop-number))))))))



;;;
;;; Other Utils
;;;

(define (print-rectangle rect)
  (let ((x (xrectangle-x rect))
        (y (xrectangle-y rect))
        (width (xrectangle-width rect))
        (height (xrectangle-height rect)))
    (printf "#<xrectangle ~A ~A ~A ~A>~%"
            x y width height)))


;;;
;;; Button
;;;

(define-record button
  xrectangle thunk)


;;;
;;; Window
;;;
(define window-position (make-parameter 'top))
(define window-lower (make-parameter #t))

(define-class <window> ()
  ((screen initform: (xdefaultscreen *display*))
   (position initform: (window-position))
   (height initform: #f)
   (width initform: #f)
   (baseline initform: #f)
   (widgets initform: (list))
   (xwindow)))

(define-method (initialize-instance (window <window>))
  (call-next-method)
  (for-each (lambda (widget) (widget-set-window! widget window))
            (slot-value window 'widgets))
  (let* ((screen (slot-value window 'screen))
         (shei (xdisplayheight *display* screen))
         (position (slot-value window 'position))
         (width (or (slot-value window 'width) (xdisplaywidth *display* screen)))
         (height (or (slot-value window 'height)
                     (fold max 1 (map widget-preferred-height
                                      (slot-value window 'widgets)))))
         (window-top (case position
                       ((bottom) (- shei height))
                       (else 0)))
         (xwindow (xcreatesimplewindow
                   *display*
                   (xrootwindow *display* screen)
                   0 window-top width height 0
                   (xblackpixel *display* screen)
                   (xwhitepixel *display* screen))))
    (assert xwindow)
    (set! (slot-value window 'width) width)
    (set! (slot-value window 'height) height)
    (set! (slot-value window 'baseline)
          (fold max 1 (map widget-preferred-baseline
                           (slot-value window 'widgets))))
    (set! (slot-value window 'xwindow) xwindow)
    (for-each widget-init (slot-value window 'widgets))
    (window-update-widget-dimensions! window)

    (let ((attr (make-xsetwindowattributes)))
      (set-xsetwindowattributes-background_pixel! attr (xblackpixel *display* screen))
      (set-xsetwindowattributes-border_pixel! attr (xblackpixel *display* screen))
      (set-xsetwindowattributes-override_redirect! attr 1)
      (xchangewindowattributes *display* xwindow
                               (bitwise-ior CWBACKPIXEL CWBORDERPIXEL CWOVERRIDEREDIRECT)
                               attr))

    ;; Window Properties
    ;;
    (xstorename *display* xwindow "mowedline")

    (let ((p (make-xtextproperty))
          (str (make-text-property (get-host-name))))
      (xstringlisttotextproperty str 1 p)
      (xsetwmclientmachine *display* xwindow p))

    (window-property-set xwindow "_NET_WM_PID"
                         (make-number-property (current-process-id)))
    (window-property-set xwindow "_NET_WM_WINDOW_TYPE"
                         (make-atom-property "_NET_WM_TYPE_DOCK"))
    (window-property-set xwindow "_NET_WM_DESKTOP"
                         (make-number-property #xffffffff))
    (window-property-set xwindow "_NET_WM_STATE"
                         (make-atom-property "_NET_WM_STATE_BELOW"))
    (window-property-append xwindow "_NET_WM_STATE"
                            (make-atom-property "_NET_WM_STATE_STICKY"))
    (window-property-append xwindow "_NET_WM_STATE"
                            (make-atom-property "_NET_WM_STATE_SKIP_TASKBAR"))
    (window-property-append xwindow "_NET_WM_STATE"
                            (make-atom-property "_NET_WM_STATE_SKIP_PAGER"))

    ;; Struts: left, right, top, bottom,
    ;;         left_start_y, left_end_y, right_start_y, right_end_y,
    ;;         top_start_x, top_end_x, bottom_start_x, bottom_end_x
    ;;
    ;; so for a top panel, we set top, top_start_x, and top_end_x.
    (window-property-set xwindow "_NET_WM_STRUT_PARTIAL"
                         (make-numbers-property
                          (if (eq? position 'bottom)
                              (list 0 0 0 height 0 0 0 0 0 0 0 0)
                              (list 0 0 height 0 0 0 0 0 0 0 0 0))))

    (let ((d-atom (xinternatom *display* "WM_DELETE_WINDOW" 1)))
      (let-location ((atm unsigned-long d-atom))
        (xsetwmprotocols *display* xwindow (location atm) 1)))

    (when (window-lower)
      (xlowerwindow *display* xwindow))

    (push! window *windows*)))

(define window-expose
  (case-lambda
   ((window xrectangle)
    ;; exposing a given rectangle means drawing all widgets which
    ;; intersect that rectangle, passing the rectangle in to them so they
    ;; can use it as a mask (via a region).
    (let ((widgets (slot-value window 'widgets))
          (r (xcreateregion)))
      (xunionrectwithregion xrectangle (xcreateregion) r)
      (for-each
       (lambda (widget)
         ;; does this widget intersect xrectangle?
         (let* ((wrect (slot-value widget 'xrectangle))
                (x (xrectangle-x wrect))
                (y 0)
                (width (xrectangle-width wrect))
                (height (slot-value window 'height)))
           (when (member (xrectinregion r x y width height)
                         (L RECTANGLEPART RECTANGLEIN))
             ;;intersect r with wrect and pass result to widget-draw
             (let ((wreg (xcreateregion))
                   (out (xcreateregion)))
               (xunionrectwithregion wrect out wreg)
               (xintersectregion r wreg out)
               (widget-draw widget out)))))
       widgets)
      ;; if the entire window is not filled with widgets, we may need to
      ;; clear the area to the right of the last widget.
      (let* ((lastwidget (last widgets))
             (wrect (slot-value lastwidget 'xrectangle))
             (p (+ (xrectangle-x wrect)
                   (xrectangle-width wrect)))
             (m (+ (xrectangle-x xrectangle)
                   (xrectangle-width xrectangle))))
        (when (> m p)
          (xcleararea *display* (slot-value window 'xwindow)
                      p 0 (- m p) (slot-value window 'height)
                      0)))))
    ((window)
     (window-expose window
                    (make-xrectangle
                     0 0 (slot-value window 'width)
                     (slot-value window 'height))))))

;; window-update-widget-dimensions! sets x coordinates and widths of all
;; widgets in window.  returns #f if nothing changed, otherwise an
;; xrectangle of the changed area.
;;
(define (window-update-widget-dimensions! window)
  (let* ((widgets (slot-value window 'widgets))
         (widsum 0)
         (flexsum 0)
         (wids (map       ;; sum widths & flexes, and accumulate widths
                (lambda (widget)
                  (and-let* ((flex (slot-value widget 'flex)))
                    (inc! flexsum flex))
                  (and-let* ((wid (widget-preferred-width widget)))
                    (inc! widsum wid)
                    wid))
                widgets))
         (remainder (- (slot-value window 'width) widsum))
         (flexunit (if (> flexsum 0) (/ remainder flexsum) 0))
         (x 0)
         (rmin #f)  ;; redraw range
         (rmax #f))
    (for-each
     (lambda (widget wid)
       (let* ((rect (slot-value widget 'xrectangle))
              (wid (or wid (* flexunit (slot-value widget 'flex))))
              (oldx (xrectangle-x rect))
              (oldwid (xrectangle-width rect)))
         (unless (and (= oldx x)
                      (= oldwid wid))
           (set! rmin (min (or rmin x) oldx x))
           (let ((rt (+ x wid))
                 (oldrt (+ oldx oldwid)))
             (set! rmax (max (or rmax rt) oldrt rt))))
         (set-xrectangle-x! rect x)
         (set-xrectangle-width! rect wid)
         (inc! x wid)))
     widgets
     wids)
    (if rmin
        (make-xrectangle rmin 0 (- rmax rmin) (slot-value window 'height))
        #f)))

(define (window-widget-at-position window x)
  (find
   (lambda (widget)
     (let ((wrect (slot-value widget 'xrectangle)))
       (and (>= x (xrectangle-x wrect))
            (< x (+ (xrectangle-x wrect)
                    (xrectangle-width wrect))))))
   (slot-value window 'widgets)))


;;;
;;; Widgets
;;;

(define-generic (widget-draw widget region))
(define-generic (widget-preferred-height widget))
(define-generic (widget-preferred-width widget))
(define-generic (widget-preferred-baseline widget))
(define-generic (widget-set-window! widget window))
(define-generic (widget-init widget))
(define-generic (widget-update widget params))

(define widget-background-color (make-parameter (list 0 0 0 1)))
(define widget-flex (make-parameter #f))

(define-class <widget> ()
  ((name initform: #f)
   (flex initform: (widget-flex))
   (window)
   (xrectangle initform: (make-xrectangle 0 0 0 0))
   (background-color initform: (widget-background-color))
   (buttons initform: (list))))

(define-method (initialize-instance (widget <widget>))
  (call-next-method)
  (and-let* ((name (slot-value widget 'name)))
    (when (hash-table-exists? *widgets* name)
      (error "duplicate widget name"))
    (hash-table-set! *widgets* name widget)))

(define-method (widget-set-window! (widget <widget>) (window <window>))
  (set! (slot-value widget 'window) window))

(define-method (widget-init (widget <widget>))
  (let ((window (slot-value widget 'window)))
    (set-xrectangle-height! (slot-value widget 'xrectangle)
                            (slot-value window 'height))))

(define-method (widget-preferred-baseline (widget <widget>)) 0)
(define-method (widget-preferred-height (widget <widget>)) 1)
(define-method (widget-preferred-width (widget <widget>))
  (if (slot-value widget 'flex)
      #f
      1))

(define (widget-button-at-position widget x)
  (find
   (lambda (button)
     (let ((rect (button-xrectangle button)))
       (and (>= x (xrectangle-x rect))
            (< x (+ (xrectangle-x rect)
                    (xrectangle-width rect))))))
   (slot-value widget 'buttons)))


;; Text Widget
;;
(define text-widget-font (make-parameter "mono-10:bold"))
(define text-widget-color (make-parameter (list 1 1 1 1)))
(define text-widget-format (make-parameter identity))

(define-class <text-widget> (<widget>)
  ((text initform: "")
   (font initform: (text-widget-font))
   (color initform: (text-widget-color))
   (format initform: (text-widget-format))))

(define-method (widget-set-window! (widget <text-widget>) (window <window>))
  (call-next-method)
  ;; initialize the font here so that the preferred height of the widget
  ;; can be queried as early as possible.
  (let ((font (slot-value widget 'font)))
    (when (string? font)
      (set! (slot-value widget 'font)
            (xft-font-open/name *display*
                                (slot-value window 'screen)
                                font)))))

(define-method (widget-draw (widget <text-widget>) region)
  (let* ((window (slot-value widget 'window))
         (xwindow (slot-value window 'xwindow))
         (wrect (slot-value widget 'xrectangle))
         (font (slot-value widget 'font))
         (x (xrectangle-x wrect))
         (baseline (slot-value window 'baseline))
         (visual (xdefaultvisual *display* (xdefaultscreen *display*)))
         (colormap (xdefaultcolormap *display* (xdefaultscreen *display*)))
         (draw (xftdraw-create *display* xwindow visual colormap)))
    (define (make-color c)
      (apply make-xftcolor *display* visual colormap
             (ensure-list c)))
    (set! (slot-value widget 'buttons) (list))
    (let ((color (make-color (slot-value widget 'color)))
          (background-color (make-color (slot-value widget 'background-color))))
      (xftdraw-set-clip! draw region)
      (xft-draw-rect draw background-color x 0
                     (xrectangle-width wrect)
                     (xrectangle-height wrect))
      (let walk ((term (slot-value widget 'text))
                 (colors (list color)))
        (cond
         ((string? term)
          (xft-draw-string draw font (first colors) x baseline term)
          (inc! x (xglyphinfo-xoff (xft-text-extents *display* font term))))
         ((pair? term)
          (cond
           ((eq? 'button (first term))
            (let ((thunk (second term))
                  (buttonx1 x))
              (walk (cddr term) colors)
              (push! (make-button (make-xrectangle buttonx1 0
                                                   (- x buttonx1)
                                                   (xrectangle-height wrect))
                                  thunk)
                     (slot-value widget 'buttons))))
           ((eq? 'color (first term))
            (walk (cddr term) (cons (make-color (second term)) colors)))
           (else
            (walk (first term) colors)
            (walk (rest term) colors)))))))))

(define-method (widget-preferred-baseline (widget <text-widget>))
  (xftfont-ascent (slot-value widget 'font)))

(define-method (widget-preferred-height (widget <text-widget>))
  ;; i find even common fonts extend a pixel lower than their
  ;; declared descent.  tsk tsk.
  (let ((font (slot-value widget 'font)))
    (+ (xftfont-ascent font) (xftfont-descent font) 2)))

(define-method (widget-preferred-width (widget <text-widget>))
  (if (slot-value widget 'flex)
      #f
      (xglyphinfo-xoff
       (xft-text-extents *display* 
                         (slot-value widget 'font)
                         (text-widget-raw-text widget)))))

(define-method (widget-update (widget <text-widget>) params)
  (set! (slot-value widget 'text)
        ((slot-value widget 'format) (first params))))

(define (text-widget-raw-text widget)
  (let walk ((term (slot-value widget 'text)))
    (cond
     ((null? term) "")
     ((string? term) term)
     ((and (pair? term)
           (memq (first term) '(color button)))
      (walk (cddr term)))
     ((pair? term)
      (string-append
       (walk (first term))
       (walk (rest term))))
     (else ""))))


;; Clock
;;
(define (clock-thread widget)
  (lambda ()
    (let loop ()
      (let* ((time (seconds->local-time (current-seconds)))
             (s (vector-ref time 0)))
        (queue-add!
         *internal-events*
         (lambda ()
           (update widget
                   (time->string time (slot-value widget 'time-format)))))
        (thread-sleep! (- 60 s)))
      (loop))))

(define-class <clock> (<text-widget>)
  ((time-format initform: "%a %b %e %H:%M %Z %Y")))

(define-method (widget-init (widget <clock>))
  (call-next-method)
  (thread-start! (make-thread (clock-thread widget))))


;; Flags
;;
(define-class <flags> (<text-widget>)
  ((flags initform: '())
   (curflags initform: '())
   (text initform: '())))

(define-method (widget-update (widget <flags>) params)
  (let* ((changes (first params))
         (op (cond ((string-prefix? "+" changes) 'add)
                   ((string-prefix? "-" changes) 'remove)
                   (else 'replace)))
         (tokenize-start (if (eq? 'replace op) 0 1))
         (tokens (string-tokenize changes char-set:graphic tokenize-start)))
    (case op
      ((add)
       (set! (slot-value widget 'curflags)
             (lset-union equal? (slot-value widget 'curflags) tokens)))
      ((remove)
       (set! (slot-value widget 'curflags)
             (lset-difference equal? (slot-value widget 'curflags) tokens)))
      ((replace)
       (set! (slot-value widget 'curflags) tokens)))
    (let ((curflags (slot-value widget 'curflags)))
      (set!
       (slot-value widget 'text)
       ((slot-value widget 'format)
        (fold (lambda (flagdef result)
                (let ((flag (first flagdef))
                      (r (rest flagdef)))
                  (if (member flag curflags)
                      (cons r result)
                      result)))
              '()
              (reverse (slot-value widget 'flags))))))))


;;;
;;; Server
;;;

(define (update widget-or-name . params)
  (and-let*
      ((widget (if (string? widget-or-name)
                   (hash-table-ref/default *widgets* widget-or-name #f)
                   widget-or-name)))
    (widget-update widget params)
    (let ((window (slot-value widget 'window)))
      (window-expose window (or (window-update-widget-dimensions! window)
                                (slot-value widget 'xrectangle))))
    #t))


(define bypass-startup-script (make-parameter #f))

(define (start-server commands input output)
  (file-close input)
  (set! *display* (xopendisplay #f))
  (assert *display*)

  (let ((event (make-xevent))
        (done #f))

    (define (quit . params)
      (set! done #t))

    (define (eventloop)
      (when (> (xpending *display*) 0)
        (xnextevent *display* event)
        (case (xevent-type event)
          ((CLIENTMESSAGE)
           (set! done #t))
          ((EXPOSE)
           (let* ((xwindow (xexposeevent-window event))
                  (window (find (lambda (win)
                                  (equal? (slot-value win 'xwindow) xwindow))
                                *windows*))
                  (x (xexposeevent-x event))
                  (y (xexposeevent-y event))
                  (width (xexposeevent-width event))
                  (height (xexposeevent-height event)))
             (window-expose window (make-xrectangle x y width height))))
          ((BUTTONPRESS)
           (and-let* ((xwindow (xexposeevent-window event))
                      (window (find (lambda (win)
                                      (equal? (slot-value win 'xwindow) xwindow))
                                    *windows*))
                      (widget (window-widget-at-position
                               window (xbuttonpressedevent-x event)))
                      (button (widget-button-at-position
                               widget (xbuttonpressedevent-x event))))
             ((eval (button-thunk button)))))))
      (dbus:poll-for-message)
      (while (not (queue-empty? *internal-events*))
        ((queue-remove! *internal-events*)))
      (unless done
        (thread-sleep! 0.01)
        (eventloop)))

    ;; process server commands
    (for-each (lambda (cmd) ((callinfo-thunk cmd)))
              commands)

    (unless (null? *default-widgets*)
      (make <window> 'widgets (reverse! *default-widgets*))
      (set! *default-widgets* (list)))

    (if* (and (not (bypass-startup-script))
              (find file-read-access?
                    (L (filepath:join-path (L "~" ".mowedline"))
                       (filepath:join-path (L "~" ".config" "mowedline" "init.scm")))))
         (load it))

    (when (null? *windows*)
      (make <window>
        'widgets
        (L (make <text-widget>
             'name "default"
             'flex 1))))

    (let ((dbus-context
           (dbus:make-context service: 'mowedline.server
                              interface: 'mowedline.interface)))
      (dbus:enable-polling-thread! enable: #f)
      (dbus:register-method dbus-context "update" update)
      (dbus:register-method dbus-context "quit" quit))

    (file-write output "ready\n")
    (file-close output)

    (for-each
     (lambda (w)
       (xselectinput *display*
                     (slot-value w 'xwindow)
                     (bitwise-ior EXPOSUREMASK
                                  BUTTONPRESSMASK
                                  STRUCTURENOTIFYMASK))
       (xmapwindow *display* (slot-value w 'xwindow))
       (xnextevent *display* event)
       (window-expose w))
     *windows*)
    (xflush *display*)
    (eventloop))
  (xclosedisplay *display*))


;;;
;;; Client / Command Line
;;;

(define (start-client commands)
  (for-each (lambda (cmd) ((callinfo-thunk cmd)))
            commands))


(define (daemon-running?)
  ;; this is our workaround for not being able to use
  ;; dbus:discover-services prior to forking.  (it seems
  ;; to be impossible to fork a dbus program once it's
  ;; performed any communication.)
  (find
   (lambda (line) (string-contains line "mowedline.server"))
   (call-with-input-pipe
    (string-join
     (L "dbus-send" "--type=method_call" "--print-reply"
        "--dest=org.freedesktop.DBus" "/org/freedesktop/DBus"
        "org.freedesktop.DBus.ListNames"))
    read-lines)))


(define server-options
  (make-command-group
   ((q)
    doc: "bypass .mowedline"
    (bypass-startup-script #t))
   ((text-widget name)
    (push! (make <text-widget>
             'name name)
           *default-widgets*))
   ((clock)
    (push! (make <clock>)
           *default-widgets*))
   ((bg color)
    doc: "set default background-color"
    (widget-background-color color))
   ((fg color)
    doc: "set the default text color"
    (text-widget-color color))
   ((flex value)
    doc: "set the default flex value"
    (widget-flex value))
   ;;(make-command (screen screen) 1)
   ((position value)
    doc: "set the default window position (top or bottom)"
    (window-position (string->symbol value)))
   ((window)
    doc: "make a window containing the foregoing widgets"
    (make <window> 'widgets (reverse! *default-widgets*))
    (set! *default-widgets* (list)))))


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
                  (append server-options client-options special-options))))
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
      (printf "~%SERVER OPTIONS  (only valid when starting the server)~%~%")
      (help-section server-options)
      (printf "~%CLIENT OPTIONS~%~%")
      (help-section client-options)
      (newline)))

   ((version)
    doc: "prints the version"
    (printf "mowedline version ~A, by John J. Foerch~%" version))))


(let-values (((server-commands client-commands special-commands)
              (parse-command-line (command-line-arguments)
                                  server-options
                                  client-options
                                  special-options)))
  (cond
   ((not (null? special-commands))
    (let ((cmd (first special-commands)))
      ((callinfo-thunk cmd)))
    (unless (and (null? (rest special-commands))
                 (null? server-commands)
                 (null? client-commands))
      (printf "~%Warning: the following commands were ignored:~%")
      (for-each
       (lambda (x) (printf "  ~S~%" (cons (callinfo-name x) (callinfo-args x))))
       (append! (rest special-commands) server-commands client-commands))))
   ((daemon-running?)
    (when (not (null? server-commands))
      (printf "Warning: the following commands were ignored because the daemon is already running:~%")
      (for-each
       (lambda (x) (printf "  ~S~%" (cons (callinfo-name x) (callinfo-args x))))
       server-commands))
    (start-client client-commands))
   (else
    (call-with-values create-pipe
      (lambda (input output)
        (process-fork
         (lambda ()
           (create-session)
           (start-server server-commands input output)))
        (file-close output)
        ;; wait for something to come through on input so that we don't
        ;; send commands until the daemon is ready to receive.
        (file-read input 10)
        (file-close input)
        (start-client client-commands))))))
