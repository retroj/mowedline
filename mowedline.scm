
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

(import chicken scheme)

(use srfi-1
     srfi-4 ;; homogeneous numeric vectors
     srfi-13 ;; string
     srfi-14 ;; character sets
     srfi-18 ;; threads
     srfi-69 ;; hash tables
     coops
     (only coops-utils instance-of?)
     data-structures
     (prefix dbus dbus:)
     extras
     filepath
     (prefix imperative-command-line-a icla:)
     list-utils
     mailbox
     miscmacros
     ports
     posix
     xft
     (except xlib make-xrectangle
                  xrectangle-x xrectangle-y
                  xrectangle-width xrectangle-height)
     (prefix xlib-utils xu:)
     xtypes)

(import llog)

(include "version")

(include "utils")


;;;
;;; Globals
;;;

(define current-xcontext (make-parameter #f))

(define xcontexts (list))

(define *widgets* (make-hash-table test: equal?))

(define *default-widgets* (list))

(define *command-line-windows* (list))

(define *internal-events* (make-mailbox))

(define %quit-mowedline #f) ;; will be bound to a quit continuation
(define (quit-mowedline . _)
  (%quit-mowedline #t))

(define (switch-to-desktop desktop)
  (xu:switch-to-desktop (current-xcontext) desktop))


;;;
;;; Button
;;;

(define-record button
  xrectangle handler)


;;;
;;; Window
;;;

(define window-background (make-parameter #f))
(define window-lower (make-parameter #t))
(define window-position (make-parameter 'top))

(define window-get-next-id
  (let ((last -1))
    (lambda ()
      (inc! last)
      last)))

(define-class <window> ()
  ((id initform: (window-get-next-id))
   (xcontext initform: (current-xcontext))
   (position initform: (window-position))
   (height initform: #f)
   (width initform: #f)
   (baseline initform: #f)
   (margin-top initform: 0)
   (margin-right initform: 0)
   (margin-bottom initform: 0)
   (margin-left initform: 0)
   (background initform: (window-background))
   (widgets initform: (list))
   (fonts initform: (list))))

(define (window . args)
  (receive (props widgets)
      (split-properties args)
    (apply make <window> 'widgets widgets props)))

(define (window-create-xwindow xcontext x y width height background)
  (xu:with-xcontext xcontext (display screen window)
    (let ((attr (make-xsetwindowattributes))
          (flags #f)
          (visual (xdefaultvisual display screen))
          (visual-depth COPYFROMPARENT))
      (cond
       ((eq? 'inherit background)
        (set! flags (bitwise-ior CWBACKPIXMAP CWBORDERPIXEL CWOVERRIDEREDIRECT))
        (set-xsetwindowattributes-background_pixmap! attr PARENTRELATIVE))
       ((eq? 'transparent background)
        (let ((vinfo (make-xvisualinfo)))
          (xmatchvisualinfo display screen 32 TRUECOLOR vinfo)
          (set! visual (xvisualinfo-visual vinfo))
          (set! visual-depth (xvisualinfo-depth vinfo))
          (set! flags (bitwise-ior CWBACKPIXEL CWBORDERPIXEL CWCOLORMAP CWOVERRIDEREDIRECT))
          (set-xsetwindowattributes-background_pixel! attr (xblackpixel display screen))
          (set-xsetwindowattributes-colormap!
           attr (xcreatecolormap display window (xvisualinfo-visual vinfo) ALLOCNONE))))
       (else
        (set! flags (bitwise-ior CWBACKPIXEL CWBORDERPIXEL CWOVERRIDEREDIRECT))
        (set-xsetwindowattributes-background_pixel! attr (xblackpixel display screen))))
      (set-xsetwindowattributes-border_pixel! attr (xblackpixel display screen))
      (set-xsetwindowattributes-override_redirect! attr 1)
      (xcreatewindow display window x y width height 0
                     visual-depth INPUTOUTPUT visual
                     flags attr))))

(define-method (initialize-instance (window <window>))
  (call-next-method)
  (xu:with-xcontext (slot-value window 'xcontext)
      (xcontext display screen root)
    (for-each (lambda (widget) (widget-set-window! widget window))
              (slot-value window 'widgets))
    (let* ((shei (xdisplayheight display screen))
           (position (slot-value window 'position))
           (width (or (slot-value window 'width)
                      (- (xdisplaywidth display screen)
                         (slot-value window 'margin-left)
                         (slot-value window 'margin-right))))
           (height (or (slot-value window 'height)
                       (fold max 1 (map widget-preferred-height
                                        (slot-value window 'widgets)))))
           (window-top (case position
                         ((bottom) (- shei (slot-value window 'margin-bottom) height))
                         (else (slot-value window 'margin-top))))
           (xwindow (window-create-xwindow xcontext
                                           (slot-value window 'margin-left)
                                           window-top width height
                                           (slot-value window 'background))))
      (assert xwindow)
      (let* ((xcontext (xu:make-xcontext xcontext window: xwindow)))
        (set! (slot-value window 'xcontext) xcontext)
        (set! (slot-value window 'width) width)
        (set! (slot-value window 'height) height)
        (set! (slot-value window 'baseline)
              (fold max 1 (map widget-preferred-baseline
                               (slot-value window 'widgets))))
        (for-each widget-init (slot-value window 'widgets))
        (window-update-widget-dimensions! window)

        ;; Window Properties
        ;;
        (xstorename display xwindow "mowedline")

        (let ((p (make-xtextproperty))
              (str (xu:make-text-property (get-host-name))))
          (xstringlisttotextproperty str 1 p)
          (xsetwmclientmachine display xwindow p))

        (xu:window-property-set xcontext "_NET_WM_PID"
                                (xu:make-number-property (current-process-id)))
        (xu:window-property-set xcontext "_NET_WM_WINDOW_TYPE"
                                (xu:make-atom-property xcontext "_NET_WM_TYPE_DOCK"))
        (xu:window-property-set xcontext "_NET_WM_DESKTOP"
                                (xu:make-number-property #xffffffff))
        (xu:window-property-set xcontext "_NET_WM_STATE"
                                (xu:make-atom-property xcontext "_NET_WM_STATE_BELOW"))
        (xu:window-property-append xcontext "_NET_WM_STATE"
                                   (xu:make-atom-property xcontext "_NET_WM_STATE_STICKY"))
        (xu:window-property-append xcontext "_NET_WM_STATE"
                                   (xu:make-atom-property xcontext "_NET_WM_STATE_SKIP_TASKBAR"))
        (xu:window-property-append xcontext "_NET_WM_STATE"
                                   (xu:make-atom-property xcontext "_NET_WM_STATE_SKIP_PAGER"))

        (let ((strut-height (+ height (slot-value window 'margin-top)
                               (slot-value window 'margin-bottom))))
          (xu:window-property-set xcontext "_NET_WM_STRUT"
                                  (xu:make-numbers-property
                                   (if (eq? position 'bottom)
                                       (list 0 0 0 strut-height)
                                       (list 0 0 strut-height 0))))
          (xu:window-property-set xcontext "_NET_WM_STRUT_PARTIAL"
                                  (xu:make-numbers-property
                                   (if (eq? position 'bottom)
                                       (list 0 0 0 strut-height 0 0 0 0 0 0 0 0)
                                       (list 0 0 strut-height 0 0 0 0 0 0 0 0 0)))))

        (xu:set-wm-protocols xcontext '(WM_DELETE_WINDOW))

        (when (window-lower)
          (xlowerwindow display xwindow))

        (xselectinput display xwindow
                      (bitwise-ior EXPOSUREMASK
                                   BUTTONPRESSMASK
                                   STRUCTURENOTIFYMASK))
        (xmapwindow display xwindow)
        (xnextevent display (make-xevent))
        (window-expose window)

        (xu:xcontext-data-set! xcontext window)
        (xu:add-event-handler! xcontext
                               CLIENTMESSAGE
                               #f
                               window-handle-event/clientmessage
                               #f)
        (xu:add-event-handler! xcontext
                               EXPOSE
                               EXPOSUREMASK
                               window-handle-event/expose
                               #f)
        (xu:add-event-handler! xcontext
                               BUTTONPRESS
                               BUTTONPRESSMASK
                               window-handle-event/buttonpress
                               #f)
        (push! xcontext xcontexts)))))

(define (window-get-create-font window font)
  (let ((fonts (slot-value window 'fonts)))
    (or (alist-ref font fonts)
        (xu:with-xcontext (slot-value window 'xcontext)
          (display screen)
        (let ((fontref (xft-font-open/name display screen font)))
          (set! (slot-value window 'fonts)
                (cons (cons font fontref)
                      fonts))
          fontref)))))

(define window-expose
  (case-lambda
   ((window xrectangle)
    ;; exposing a given rectangle means drawing all widgets which
    ;; intersect that rectangle, passing the rectangle in to them so they
    ;; can use it as a mask (via a region).
    (xu:with-xcontext (slot-value window 'xcontext) (xcontext display)
      (let ((xwindow (xu:xcontext-window xcontext))
            (widgets (slot-value window 'widgets))
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
              (xcleararea display xwindow
                          p 0 (- m p) (slot-value window 'height)
                          0))))
        (xflush display))))
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
                  (inc! flexsum (max 0 (slot-value widget 'flex)))
                  (and-let* ((wid (widget-preferred-width widget)))
                    (inc! widsum wid)
                    wid))
                widgets))
         (remainder (- (slot-value window 'width) widsum))
         (x 0)
         (rmin #f)  ;; redraw range
         (rmax #f))
    (define (flex-allocate flex)
      (if (zero? flexsum)
          #f
          (let ((flexwid (inexact->exact (round (* (/ flex flexsum) remainder)))))
            (set! remainder (- remainder flexwid))
            (set! flexsum (- flexsum flex))
            flexwid)))
    (for-each
     (lambda (widget wid)
       (let* ((rect (slot-value widget 'xrectangle))
              (wid (or wid (flex-allocate (slot-value widget 'flex))))
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

(define (window-handle-event/clientmessage xcontext event)
  (xu:with-xcontext xcontext (display)
    (let ((WM_PROTOCOLS (xinternatom display "WM_PROTOCOLS" 1))
          (WM_DELETE_WINDOW (xinternatom display "WM_DELETE_WINDOW" 1)))
      (when (and (= WM_PROTOCOLS (xclientmessageevent-message_type event))
                 (= WM_DELETE_WINDOW (first (xu:xclientmessageevent-data-l event))))
        (quit-mowedline)))))

(define (window-handle-event/expose xcontext event)
  (and-let* ((window (xu:xcontext-data xcontext))
             (x (xexposeevent-x event))
             (y (xexposeevent-y event))
             (width (xexposeevent-width event))
             (height (xexposeevent-height event)))
    (window-expose window (make-xrectangle x y width height))))

(define (window-handle-event/buttonpress xcontext event)
  (let ((window (xu:xcontext-data xcontext)))
  (parameterize ((current-xcontext (slot-value window 'xcontext)))
    (and-let* ((widget (window-widget-at-position
                        window (xbuttonpressedevent-x event)))
               (button (widget-button-at-position
                        widget (xbuttonpressedevent-x event))))
      ((button-handler button) widget)))))


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

(define widget-background-color (make-parameter #f))
(define widget-flex (make-parameter 0))

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
  (if (> (slot-value widget 'flex) 0)
      #f
      1))

(define-method (widget-draw (widget <widget>) region)
  (let ((window (slot-value widget 'window)))
    (xu:with-xcontext (slot-value window 'xcontext) (xcontext display)
      (let* ((xwindow (xu:xcontext-window xcontext))
             (wrect (slot-value widget 'xrectangle))
             (x (xrectangle-x wrect))
             (attr (make-xwindowattributes))
             (_ (xgetwindowattributes display xwindow attr))
             (visual (xwindowattributes-visual attr))
             (colormap (xwindowattributes-colormap attr))
             (draw (xftdraw-create display xwindow visual colormap)))
        (define (make-color c)
          (apply make-xftcolor display visual colormap
                 (ensure-list c)))
        (let ((background-color (slot-value widget 'background-color)))
          (xftdraw-set-clip! draw region)
          (if background-color
              (xft-draw-rect draw (make-color background-color) x 0
                             (xrectangle-width wrect)
                             (xrectangle-height wrect))
              (xcleararea display xwindow x 0
                          (xrectangle-width wrect)
                          (xrectangle-height wrect)
                          0)))))))

(define (widget-button-at-position widget x)
  (find
   (lambda (button)
     (let ((rect (button-xrectangle button)))
       (and (>= x (xrectangle-x rect))
            (< x (+ (xrectangle-x rect)
                    (xrectangle-width rect))))))
   (slot-value widget 'buttons)))


;; Spacer
;;
(define-class <spacer> (<widget>)
  ((width initform: #f)))

(define (widget:spacer . args)
  (apply make <spacer> args))

(define-method (widget-preferred-width (widget <spacer>))
  (if (> (slot-value widget 'flex) 0)
      #f
      (or (slot-value widget 'width) 1)))


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

(define-method (initialize-instance (widget <text-widget>))
  (call-next-method)
  ;; apply formatting to default text
  (widget-update widget (list (slot-value widget 'text))))

(define-method (widget-draw (widget <text-widget>) region)
  (let ((window (slot-value widget 'window)))
    (xu:with-xcontext (slot-value window 'xcontext) (xcontext display)
      (let* ((xwindow (xu:xcontext-window xcontext))
             (wrect (slot-value widget 'xrectangle))
             (font (window-get-create-font window (slot-value widget 'font)))
             (x (xrectangle-x wrect))
             (baseline (slot-value window 'baseline))
             (attr (make-xwindowattributes))
             (_ (xgetwindowattributes display xwindow attr))
             (visual (xwindowattributes-visual attr))
             (colormap (xwindowattributes-colormap attr))
             (draw (xftdraw-create display xwindow visual colormap)))
        (define (make-color c)
          (apply make-xftcolor display visual colormap
                 (ensure-list c)))
        (set! (slot-value widget 'buttons) (list))
        (let ((color (make-color (slot-value widget 'color)))
              (background-color (slot-value widget 'background-color)))
          (xftdraw-set-clip! draw region)
          (if background-color
              (xft-draw-rect draw (make-color background-color) x 0
                             (xrectangle-width wrect)
                             (xrectangle-height wrect))
              (xcleararea display xwindow x 0
                          (xrectangle-width wrect)
                          (xrectangle-height wrect)
                          0))
          (let walk ((term (slot-value widget 'text))
                     (fonts (list font))
                     (colors (list color)))
            (cond
             ((string? term)
              (xft-draw-string draw (first fonts) (first colors) x baseline term)
              (inc! x (xglyphinfo-xoff (xft-text-extents display (first fonts) term))))
             ((pair? term)
              (cond
               ((eq? 'button (first term))
                (let ((handler (second term))
                      (buttonx1 x))
                  (walk (cddr term) fonts colors)
                  (push! (make-button (make-xrectangle buttonx1 0
                                                       (- x buttonx1)
                                                       (xrectangle-height wrect))
                                      handler)
                         (slot-value widget 'buttons))))
               ((eq? 'color (first term))
                (walk (cddr term) fonts (cons (make-color (second term)) colors)))
               ((eq? 'font (first term))
                (walk (cddr term) (cons (window-get-create-font window (second term)) fonts) colors))
               (else
                (walk (first term) fonts colors)
                (walk (rest term) fonts colors)))))))))))

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
    (xu:with-xcontext (slot-value window 'xcontext) (display)
      (define (x-extent str font)
        (xglyphinfo-xoff
         (xft-text-extents display
                           (window-get-create-font window font)
                           str)))
      (if (> (slot-value widget 'flex) 0)
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
             (else 0)))))))

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
   (curflags initform: '())))

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
  (receive (fst snd)
      (with-input-from-string (first params)
        (lambda () (values (read) (read))))
    (unless (or (eof-object? fst) (eof-object? snd))
      (hash-table-set! (slot-value widget 'data)
                       fst snd))
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


;; Active Window Title
;;
(define-class <active-window-title> (<text-widget>)
  ())

(define (widget:active-window-title . args)
  (apply make <active-window-title> args))

(define-method (widget-init (widget <active-window-title>))
  (call-next-method)
  (let* ((window (slot-value widget 'window))
         (xcontext (slot-value window 'xcontext)))
    (xu:with-xcontext xcontext (display screen root)
      (let ((root-xcontext (find (lambda (xc) (= root (xu:xcontext-window xc))) xcontexts))
            (net-active-window-atom (xinternatom display "_NET_ACTIVE_WINDOW" 0)))
        (xu:add-event-handler! root-xcontext
                               PROPERTYNOTIFY
                               PROPERTYCHANGEMASK
                               (lambda (xcontext event)
                                 (let ((title (xu:active-window-title root-xcontext)))
                                   (update widget (or title ""))))
                               ;; guard
                               (lambda (event)
                                 (= net-active-window-atom (xpropertyevent-atom event))))
        ;;XXX: we need to make a general interface so that widgets are not
        ;;     clobbering each other's eventmasks
        (xselectinput display root (bitwise-ior PROPERTYCHANGEMASK))
        (widget-update widget (list (or (xu:active-window-title root-xcontext) "")))))))


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

(define startup-script (make-parameter #f))

(define (mowedline)
  (xu:with-xcontext (xu:make-xcontext display: (xopendisplay #f))
      (xcontext display)
    (assert display)

    (push! xcontext xcontexts) ;; root xcontext

    (parameterize ((current-xcontext xcontext))
      (let ((x-fd (xconnectionnumber display))
            (event (make-xevent)))

        (for-each
         (lambda (widgets) (make <window> 'widgets widgets))
         *command-line-windows*)
        (set! *command-line-windows* (list))

        (unless (null? *default-widgets*)
          (make <window> 'widgets (reverse! *default-widgets*))
          (set! *default-widgets* (list)))

        (and-let*
            ((_ (not (bypass-startup-script)))
             (path
              (or (startup-script)
                  (let ((~ (get-environment-variable "HOME")))
                    (find file-read-access?
                          (L (filepath:join-path (L ~ ".mowedline"))
                             (filepath:join-path (L (xdg-config-home)
                                                    "mowedline" "init.scm"))))))))
          (eval '(import mowedline))
          (load path))

        (unless (find (lambda (xc) (instance-of? (xu:xcontext-data xc) <window>))
                      xcontexts)
          (make <window>
            'widgets
            (L (make <text-widget>
                 'name "default"
                 'flex 1
                 'text "mowedline"))))

        (define (client-quit)
          (mailbox-send! *internal-events* quit-mowedline)
          #t)

        (let ((dbus-context
               (dbus:make-context service: 'mowedline.server
                                  interface: 'mowedline.interface)))
          (dbus:enable-polling-thread! enable: #f)
          (dbus:register-method dbus-context "update" update)
          (dbus:register-method dbus-context "quit" client-quit)
          (dbus:register-method dbus-context "log" log-watch))

        (define (x-eventloop)
          (unless (> (xpending display) 0)
            (thread-wait-for-i/o! x-fd input:))
          (xnextevent display event)
          (xu:handle-event event xcontexts)
          (x-eventloop))

        (define (dbus-eventloop)
          (dbus:poll-for-message)
          (thread-sleep! 0.01)
          (dbus-eventloop))

        (define (internal-events-eventloop)
          ((mailbox-receive! *internal-events*))
          (internal-events-eventloop))

        (call/cc
         (lambda (return)
           (set! %quit-mowedline return)
           (thread-start! x-eventloop)
           (thread-start! internal-events-eventloop)
           (dbus-eventloop)))))
    (xclosedisplay display)))

(define (mowedline-start)
  (thread-start! (lambda () (mowedline) (exit))))


;;;
;;; Command Line
;;;

(icla:help-heading
 (sprintf "mowedline version ~A, by John J. Foerch" version))

(icla:define-command-group server-options
 ((q)
  doc: "bypass .mowedline"
  (bypass-startup-script #t))
 ((config path)
  doc: "use config file instead of .mowedline"
  (startup-script path))
 ((text-widget name)
  (push! (make <text-widget>
           'name name)
         *default-widgets*))
 ((clock)
  (push! (make <clock>)
         *default-widgets*))
 ((active-window-title)
  (push! (make <active-window-title>)
         *default-widgets*))
 ((bg color)
  doc: "set default background-color"
  (widget-background-color color))
 ((fg color)
  doc: "set the default text color"
  (text-widget-color color))
 ((flex value)
  doc: "set the default flex value"
  (widget-flex (string->number value)))
 ((position value)
  doc: "set the default window position (top or bottom)"
  (window-position (string->symbol value)))
 ((window)
  doc: "make a window containing the foregoing widgets"
  (set! *command-line-windows*
        (cons (reverse! *default-widgets*) *command-line-windows*))
  (set! *default-widgets* (list))))

)
