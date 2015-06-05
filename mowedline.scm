
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

(include "llog")

(module mowedline
    *

(import chicken scheme foreign)

(use srfi-1
     srfi-4 ;; homogeneous numeric vectors
     srfi-13 ;; string
     srfi-14 ;; character sets
     srfi-18 ;; threads
     srfi-69 ;; hash tables
     coops
     data-structures
     (prefix dbus dbus:)
     extras
     filepath
     (prefix imperative-command-line-a icla:)
     list-utils
     lolevel
     mailbox
     miscmacros
     ports
     posix
     xft
     (except xlib make-xrectangle
                  xrectangle-x xrectangle-y
                  xrectangle-width xrectangle-height)
     xtypes)

(import llog)

(include "version")

(include "utils")


;;;
;;; Globals
;;;

(define *display* #f)

(define *windows* (list))

(define *widgets* (make-hash-table test: equal?))

(define *default-widgets* (list))

(define *command-line-windows* (list))

(define *internal-events* (make-mailbox))


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
;;; Button
;;;

(define-record button
  xrectangle thunk)


;;;
;;; Window
;;;
(define window-position (make-parameter 'top))
(define window-lower (make-parameter #t))

(define window-get-next-id
  (let ((last -1))
    (lambda ()
      (inc! last)
      last)))

(define-class <window> ()
  ((id initform: (window-get-next-id))
   (screen initform: (xdefaultscreen *display*))
   (position initform: (window-position))
   (height initform: #f)
   (width initform: #f)
   (baseline initform: #f)
   (widgets initform: (list))
   (xwindow)
   (fonts initform: (list))))

(define (window . args)
  (receive (props widgets)
      (split-properties args)
    (apply make <window> 'widgets widgets props)))

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

(define (window-get-create-font window font)
  (let ((fonts (slot-value window 'fonts)))
    (or (alist-ref font fonts)
        (let ((fontref (xft-font-open/name *display*
                                           (slot-value window 'screen)
                                           font)))
          (set! (slot-value window 'fonts)
                (cons (cons font fontref)
                      fonts))
          fontref))))

(define window-expose
  (case-lambda
   ((window xrectangle)
    ;; exposing a given rectangle means drawing all widgets which
    ;; intersect that rectangle, passing the rectangle in to them so they
    ;; can use it as a mask (via a region).
    (let ((widgets (slot-value window 'widgets))
          (r (xcreateregion)))
      (xunionrectwithregion xrectangle (xcreateregion) r)
      (llog expose "window ~A, ~A"
            (slot-value window 'id)
            (xrectangle->string xrectangle))
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
      (unless (null? widgets)
        (let* ((lastwidget (last widgets))
               (wrect (slot-value lastwidget 'xrectangle))
               (p (+ (xrectangle-x wrect)
                     (xrectangle-width wrect)))
               (m (+ (xrectangle-x xrectangle)
                     (xrectangle-width xrectangle))))
          (when (> m p)
            (xcleararea *display* (slot-value window 'xwindow)
                        p 0 (- m p) (slot-value window 'height)
                        0))))
      (xflush *display*)))
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

(define (widget:text . args)
  (apply make <text-widget> args))

(define-method (widget-draw (widget <text-widget>) region)
  (let* ((window (slot-value widget 'window))
         (xwindow (slot-value window 'xwindow))
         (wrect (slot-value widget 'xrectangle))
         (font (window-get-create-font window (slot-value widget 'font)))
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
                 (fonts (list font))
                 (colors (list color)))
        (cond
         ((string? term)
          (xft-draw-string draw (first fonts) (first colors) x baseline term)
          (inc! x (xglyphinfo-xoff (xft-text-extents *display* (first fonts) term))))
         ((pair? term)
          (cond
           ((eq? 'button (first term))
            (let ((thunk (second term))
                  (buttonx1 x))
              (walk (cddr term) fonts colors)
              (push! (make-button (make-xrectangle buttonx1 0
                                                   (- x buttonx1)
                                                   (xrectangle-height wrect))
                                  thunk)
                     (slot-value widget 'buttons))))
           ((eq? 'color (first term))
            (walk (cddr term) fonts (cons (make-color (second term)) colors)))
           ((eq? 'font (first term))
            (walk (cddr term) (cons (window-get-create-font window (second term)) fonts) colors))
           (else
            (walk (first term) fonts colors)
            (walk (rest term) fonts colors)))))))))

(define-method (widget-preferred-baseline (widget <text-widget>))
  (xftfont-ascent (window-get-create-font
                   (slot-value widget 'window)
                   (slot-value widget 'font))))

(define-method (widget-preferred-height (widget <text-widget>))
  ;; i find even common fonts extend a pixel lower than their
  ;; declared descent.  tsk tsk.
  (let ((font (window-get-create-font
               (slot-value widget 'window)
               (slot-value widget 'font))))
    (+ (xftfont-ascent font) (xftfont-descent font) 2)))

(define-method (widget-preferred-width (widget <text-widget>))
  (let ((window (slot-value widget 'window)))
    (define (x-extent str font)
      (xglyphinfo-xoff
       (xft-text-extents *display* 
                         (window-get-create-font window font)
                         str)))
    (if (slot-value widget 'flex)
        #f
        (let walk ((term (slot-value widget 'text))
                   (font (slot-value widget 'font)))
          (cond
           ((null? term) 0)
           ((string? term) (x-extent term font))
           ((pair? term)
            (cond
             ((eq? 'font (first term))
              (walk (cddr term) (second term)))
             ((memq (first term) '(color button))
              (walk (cddr term) font))
             (else
              (+ (walk (first term) font)
                 (walk (rest term) font)))))
           (else 0))))))

(define-method (widget-update (widget <text-widget>) params)
  (set! (slot-value widget 'text)
        ((slot-value widget 'format) (first params))))


;; Clock
;;
(define (clock-thread widget)
  (lambda ()
    (let loop ()
      (let* ((time (seconds->local-time (current-seconds)))
             (s (vector-ref time 0)))
        (mailbox-send!
         *internal-events*
         (lambda ()
           (update widget
                   (time->string time (slot-value widget 'time-format)))))
        (thread-sleep! (- 60 s)))
      (loop))))

(define-class <clock> (<text-widget>)
  ((time-format initform: "%a %b %e %H:%M %Z %Y")))

(define (widget:clock . args)
  (apply make <clock> args))

(define-method (widget-init (widget <clock>))
  (call-next-method)
  (thread-start! (make-thread (clock-thread widget))))


;; Flags
;;
(define-class <flags> (<text-widget>)
  ((flags initform: '())
   (curflags initform: '())
   (text initform: '())))

(define (widget:flags . args)
  (apply make <flags> args))

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


;; Map
;;
(define (map-format-pair k v)
  (string-append (->string k) "=" (->string v)))

(define-class <map> (<text-widget>)
  ((data initform: (make-hash-table))
   (format-pair initform: map-format-pair)
   (separator initform: ",")))

(define (widget:map . args)
  (apply make <map> args))

(define-method (widget-update (widget <map>) params)
  (let ((pair (with-input-from-string
                  (first params)
                (lambda ()
                  (let* ((fst (read))
                         (snd (read)))
                    (list fst snd))))))
    (hash-table-set! (slot-value widget 'data)
                     (first pair) (second pair))
    (set!
     (slot-value widget 'text)
     ((slot-value widget 'format)
      (hash-table-fold
       (slot-value widget 'data)
       (lambda (k v a)
         (let ((part ((slot-value widget 'format-pair) k v)))
           (if part
               (string-append part
                              (if (string-null? a)
                                  ""
                                  (slot-value widget 'separator))
                              a)
               a)))
       "")))))


;;;
;;; Server
;;;

(define (update widget-or-name . params)
  (llog (update "~S ~S" widget-or-name params)
    (and-let*
        ((widget (if (string? widget-or-name)
                     (hash-table-ref/default *widgets* widget-or-name #f)
                     widget-or-name)))
      (widget-update widget params)
      (let ((window (slot-value widget 'window)))
        (window-expose window (or (window-update-widget-dimensions! window)
                                  (slot-value widget 'xrectangle))))
      #t)))

(define (log-watch symlist . params)
  (for-each
   (lambda (x)
     (case (string-ref x 0)
       ((#\-) (llog-unwatch (string->symbol (string-drop x 1))))
       ((#\+) (llog-watch (string->symbol (string-drop x 1))))
       (else (llog-watch (string->symbol x)))))
   (string-split symlist ", ")) 
  #t)

(define bypass-startup-script (make-parameter #f))

(define (mowedline)
  (set! *display* (xopendisplay #f))
  (assert *display*)

  (let ((x-fd (xconnectionnumber *display*))
        (event (make-xevent))
        (done #f))

    (define (quit . params)
      (set! done #t))

    (for-each
     (lambda (widgets) (make <window> 'widgets widgets))
     *command-line-windows*)
    (set! *command-line-windows* (list))

    (unless (null? *default-widgets*)
      (make <window> 'widgets (reverse! *default-widgets*))
      (set! *default-widgets* (list)))

    (and-let* ((_ (not (bypass-startup-script)))
               (path
                (find file-read-access?
                      (L (filepath:join-path (L "~" ".mowedline"))
                         (filepath:join-path (L "~" ".config" "mowedline" "init.scm"))))))
      (eval '(import mowedline))
      (load path))

    (when (null? *windows*)
      (make <window>
        'widgets
        (L (make <text-widget>
             'name "default"
             'flex 1
             'text "mowedline"))))

    (let ((dbus-context
           (dbus:make-context service: 'mowedline.server
                              interface: 'mowedline.interface)))
      (dbus:enable-polling-thread! enable: #f)
      (dbus:register-method dbus-context "update" update)
      (dbus:register-method dbus-context "quit" quit)
      (dbus:register-method dbus-context "log" log-watch))

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

    (define (x-eventloop)
      (unless (> (xpending *display*) 0)
        (thread-wait-for-i/o! x-fd input:))
      (xnextevent *display* event)
      (select (xevent-type event)
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
           ((eval (button-thunk button))))))
      (x-eventloop))

    (define (dbus-eventloop)
      (dbus:poll-for-message)
      (unless done
        (thread-sleep! 0.01)
        (dbus-eventloop)))

    (define (internal-events-eventloop)
      (let loop ()
        ((mailbox-receive! *internal-events*))
        (loop)))

    (let ((x-thread (thread-start! x-eventloop))
          (internal-events-thread (thread-start! internal-events-eventloop)))
      (dbus-eventloop)
      (thread-terminate! x-thread)
      (thread-terminate! internal-events-thread)))
  (xclosedisplay *display*))


;;;
;;; Command Line
;;;

(icla:help-heading
 (sprintf "mowedline version ~A, by John J. Foerch" version))

(icla:define-command-group server-options
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
 ((position value)
  doc: "set the default window position (top or bottom)"
  (window-position (string->symbol value)))
 ((window)
  doc: "make a window containing the foregoing widgets"
  (set! *command-line-windows*
        (cons (reverse! *default-widgets*) *command-line-windows*))
  (set! *default-widgets* (list))))

)
