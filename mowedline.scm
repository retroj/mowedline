
(import chicken scheme extras foreign)

(use srfi-4 ;; homogeneous numeric vectors
     srfi-69 ;; hash tables
     coops
     dbus
     environments
     filepath
     list-utils
     lolevel
     miscmacros
     posix
     xlib)


;;;
;;; Language
;;;

(define L list)


;;;
;;; Globals
;;;

(define *display* #f)

(define *screen* #f)

(define *windows* (list))

(define *widgets* (make-hash-table test: equal?))


;;;
;;; Window
;;;

(define-class <window> ()
  ((screen initform: (xdefaultscreen *display*))
   (position initform: 'top)
   (height initform: #f)
   (width initform: #f)
   (widgets initform: (list))
   (xwindow)))

(define-method (initialize-instance (window <window>))
  (call-next-method)

  (let* ((screen (slot-value window 'screen))
         (shei (xdisplayheight *display* screen))
         (position (slot-value window 'position))
         (width (or (slot-value window 'width) (xdisplaywidth *display* screen)))
         (height (or (slot-value window 'height)
                     (fold max 1 (map widget-height (slot-value window 'widgets)))))
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
    (set! (slot-value window 'xwindow) xwindow)
    (for-each (lambda (widget) (widget-set-window! widget window))
              (slot-value window 'widgets))

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

    (push! window *windows*)))


;;;
;;; Widgets
;;;

(define-generic (widget-draw widget x wid))
(define-generic (widget-height widget))
(define-generic (widget-update widget params))
(define-generic (widget-width widget))
(define-generic (widget-set-window! widget window))

(define-class <widget> ()
  ((name)
   (flex initform: #f)
   (window)
   (gc)))

(define-method (initialize-instance (widget <widget>))
  (call-next-method)
  (when (hash-table-exists? *widgets* (slot-value widget 'name))
    (error "duplicate widget name"))
  (hash-table-set! *widgets* (slot-value widget 'name) widget))

(define-method (widget-set-window! (widget <widget>) (window <window>))
  (set! (slot-value widget 'window) window))

(define-method (widget-height (widget <widget>)) 1)
(define-method (widget-width (widget <widget>)) 1)

;; Text Widget
;;
(define-class <text-widget> (<widget>)
  ((text initform: "")
   (font initform: (or (get-font "9x15bold")
                       (get-font "*")
                       (error "no font")))))

(define-method (widget-set-window! (widget <text-widget>) (window <window>))
  (call-next-method)
  (let ((gc (xcreategc *display*
                       (slot-value window 'xwindow)
                       0 #f)))
    (xsetbackground *display* gc (xblackpixel *display* (slot-value window 'screen)))
    (xsetforeground *display* gc (xwhitepixel *display* (slot-value window 'screen)))
    (xsetfunction *display* gc GXCOPY)
    (xsetfont *display* gc (xfontstruct-fid (slot-value widget 'font)))
    ;;(xsetregion *display* gc (xcreateregion))
    (set! (slot-value widget 'gc) gc)))

(define-method (widget-draw (widget <text-widget>) x wid)
  ;; XCreateRegion() --> pointer to region
  ;; XUnionRectWithRegion()
  ;; XDestroyRegion()
  ;; (xoffsetregion r x 0)

  ; (xunionrectwithregion rect src dest)
  ; (let ((r (xcreateregion)))
  ;   (xsetregion *display* gc r))
  (let ((text (slot-value widget 'text))
        (baseline (xfontstruct-ascent (slot-value widget 'font))))
    (xdrawimagestring *display*
                      (slot-value (slot-value widget 'window) 'xwindow)
                      (slot-value widget 'gc)
                      x baseline text (string-length text))))

(define-method (widget-height (widget <text-widget>))
  ;; i find even common fonts extend a pixel lower than their
  ;; declared descent.  tsk tsk.
  (let ((font (slot-value widget 'font)))
    (+ (xfontstruct-ascent font) (xfontstruct-descent font) 2)))

(define-method (widget-update (widget <text-widget>) params)
  ;; after update, the caller will call draw.  but efficiency could be
  ;; gained if the caller knew if our width changed, thus determining how
  ;; much of the window needed to be redrawn.
  (set! (slot-value widget 'text) (first params)))

(define-method (widget-width (widget <text-widget>))
  (xtextwidth (slot-value widget 'font)
              (slot-value widget 'text)
              (string-length (slot-value widget 'text))))



;;;
;;; Window Property Utils
;;;

(define (property-type property)
  (vector-ref property 0))
(define (property-format property)
  (vector-ref property 1))
(define (property-data property)
  (vector-ref property 2))
(define (property-count property)
  (vector-ref property 3))

(define (make-atom-property atom-name)
  (let ((data (xinternatom *display* atom-name 0)))
    (let-location ((data unsigned-long data))
      (vector "ATOM" 32
              (location data)    
              1))))

(define (make-number-property number)
  (let-location ((data unsigned-long number))
    (vector "CARDINAL" 32 (location data) 1)))

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
    (vector "CARDINAL" 32 lvec len)))

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
                   (xinternatom *display* (property-type value) 0)
                   (property-format value)
                   PROPMODEREPLACE
                   (property-data value)
                   (property-count value)))

(define (window-property-append win key value)
  (xchangeproperty *display* win
                   (xinternatom *display* key 0)
                   (xinternatom *display* (property-type value) 0)
                   (property-format value)
                   PROPMODEAPPEND
                   (property-data value)
                   (property-count value)))


(define (get-font font-name)
  (let ((font (xloadqueryfont *display* font-name)))
    font))


(define (start-server)
  (set! *display* (xopendisplay #f))
  (assert *display*)

  ;; "Main"
  ;;
  (define (handleexpose xwindow)
    (let* ((window (find (lambda (window)
                           (equal? (slot-value window 'xwindow) xwindow))
                         *windows*))
           (taken 0)
           (flex 0)
           (wids (map (lambda (x)
                        (if* (slot-value x 'flex)
                             (begin (set! flex (+ flex it))
                                    #f)
                             (let ((wid (widget-width x)))
                               (set! taken (+ taken wid))
                               wid)))
                      (slot-value window 'widgets)))
           ;;XXX: we should be using the width of the window, not the screen.
           (remainder (- (xdisplaywidth *display* (slot-value window 'screen))
                         taken))
           (flexunit (if (> flex 0) (/ remainder flex) 0))
           (left 10))
      (for-each
       (lambda (w wid)
         (cond (wid (widget-draw w left wid)
                    (set! left (+ left wid)))
               (else (let ((wid (* flexunit (slot-value w 'flex))))
                       (widget-draw w left wid)
                       (set! left (+ left wid))))))
       (slot-value window 'widgets)
       wids)))

  (define (update . params)
    (printf "*** Received dbus message: ~s~%" params)
    (let* ((name (first params))
           (widget (hash-table-ref *widgets* name)))
      (widget-update widget (cdr params))
      (handleexpose (slot-value (slot-value widget 'window) 'xwindow))))


  (let ((event (make-xevent))
        (return #f))
    (define (quit . params)
      (return #t))

    (define (eventloop)
      (when (> (xpending *display*) 0)
        (xnextevent *display* event)
        (let ((type (xevent-type event)))
          (cond
           ((= type CLIENTMESSAGE)
            (display "closed!\n")
            (return #t))

           ((= type EXPOSE)
            (handleexpose (xexposeevent-window event))
            (display "expose\n"))

           ((= type BUTTONPRESS)
            (display "buttonpress\n")
            (return #t))

           (else
            (display "event ")
            (display (xevent-type event))
            (display "\n")))))
      (dbus:poll-for-message)
      (eventloop))

    (define (start-eventloop ret)
      (set! return ret)
      (eventloop))

    (if* (find file-read-access?
               (L (filepath:join-path (L "~" ".mowedline"))
                  (filepath:join-path (L "~" ".config" "mowedline" "init.scm"))))
         (let ((env (environment-copy (interaction-environment))))
           (environment-extend! env 'make make)
           (load it (lambda (form) (eval form env)))))

    (when (null? *windows*)
      (make <window>
        'widgets
        (L (make <text-widget>
             'name "default"
             'text "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))))

    (define dbus-context
      (dbus:make-context service: 'mowedline.server
                         interface: 'mowedline.interface))
    (dbus:enable-polling-thread! enable: #f)
    (dbus:register-method dbus-context "update" update)
    (dbus:register-method dbus-context "quit" quit)

    (for-each
     (lambda (w)
       (xselectinput *display*
                     (slot-value w 'xwindow)
                     (bitwise-ior EXPOSUREMASK
                                  BUTTONPRESSMASK
                                  STRUCTURENOTIFYMASK))
       (xmapwindow *display* (slot-value w 'xwindow))
       (xnextevent *display* event)
       (handleexpose (slot-value w 'xwindow)))
     *windows*)
    (xflush *display*)
    (call/cc start-eventloop))
  (xclosedisplay *display*))


(unless (member "mowedline.server" (dbus:discover-services))
  (process-fork start-server))

(define dbus-context
  (dbus:make-context service: 'mowedline.server
                     interface: 'mowedline.interface))



;; (put 'make-window 'scheme-indent-function 1)
;; (put 'foreign-lambda* 'scheme-indent-function 2)
;; (put 'let-location 'scheme-indent-function 1)
