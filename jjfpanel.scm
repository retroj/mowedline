
(import chicken scheme extras foreign)

(use srfi-4 ;; homogeneous numeric vectors
     coops
     lolevel
     miscmacros
     posix
     xlib)


;;;
;;; Globals
;;;

(define *display* (xopendisplay #f))
(assert *display*)

(define *screen* #f)

(define *window* #f)


;;;
;;; Widgets
;;;

(define-generic (widget-draw widget x wid))
(define-generic (widget-width widget))

(define-class <widget> ()
  ((name)
   (flex initform: #f)
   (gc)))

(define-method (widget-width (widget <widget>)) 0)

;; Text Widget
;;
(define-class <text-widget> (<widget>)
  ((text initform: "")
   (font)))

(define-method (initialize-instance (widget <text-widget>))
  (call-next-method)
  (let ((gc (xcreategc *display* *window* 0 #f)))
    (xsetbackground *display* gc (xblackpixel *display* *screen*))
    (xsetforeground *display* gc (xwhitepixel *display* *screen*))
    (xsetfunction *display* gc GXCOPY)
    (xsetfont *display* gc (xfontstruct-fid (slot-value widget 'font)))
    (set! (slot-value widget 'gc) gc)))

(define-method (widget-draw (widget <text-widget>) x wid)
  (let ((text (slot-value widget 'text))
        (baseline (xfontstruct-ascent (slot-value widget 'font))))
    (xdrawimagestring *display*
                      *window*
                      (slot-value widget 'gc)
                      x baseline text (string-length text))))

(define-method (widget-width (widget <text-widget>))
  (xtextwidth (slot-value widget 'font)
              (slot-value widget 'text)
              (string-length (slot-value widget 'text))))


(define *widgets* (list))



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

(define (set-struts win strut-spec)
  (let ((values ((foreign-lambda* c-pointer ((u32vector s))
                   "unsigned long strut[12] ="
                   "    { s[0], s[1], s[2], s[3], s[4], s[5],"
                   "      s[6], s[7], s[8], s[9], s[10], s[11] };"
                   "C_return(strut);")
                 (list->u32vector strut-spec))))
    (window-property-set win "_NET_WM_STRUT_PARTIAL"
                         (vector "CARDINAL" 32 values 12))))



(define (get-font font-name)
  (let ((font (xloadqueryfont *display* font-name)))
    (assert font)
    font))


(let* ((screen (xdefaultscreen *display*))
       (swid (xdisplaywidth *display* screen))
       (shei (xdisplayheight *display* screen))
       (attr (make-xsetwindowattributes))
       (vis (make-visual))
       (font (get-font "-misc-fixed-bold-*-*-*-*-100-*-*-*-*-*-*"))
       ;; i find even these common fonts extend a pixel lower than their
       ;; declared descent.  tsk tsk.
       (whei (+ (xfontstruct-ascent font) (xfontstruct-descent font) 2))
       (position 'top)
       (window-top (case position
                     ((bottom) (- shei whei))
                     (else 0))))
  (set-xsetwindowattributes-background_pixel! attr (xblackpixel *display* screen))
  (set-xsetwindowattributes-border_pixel! attr (xblackpixel *display* screen))
  (set-xsetwindowattributes-override_redirect! attr 1)
  (set-visual-class! vis COPYFROMPARENT)

  (set! *screen* screen)
  (set! *window* (xcreatewindow
                  *display*
                  (xrootwindow *display* screen)
                  0 window-top swid whei 0
                  (xdefaultdepth *display* screen)
                  INPUTOUTPUT vis
                  (bitwise-ior CWBACKPIXEL CWBORDERPIXEL CWOVERRIDEREDIRECT)
                  attr))
  (assert *window*)

  ;; Window Properties
  ;;
  (xstorename *display* *window* "jjfpanel")

  (let ((p (make-xtextproperty))
        (str (make-text-property (get-host-name))))
    (xstringlisttotextproperty str 1 p)
    (xsetwmclientmachine *display* *window* p))

  (window-property-set *window* "_NET_WM_PID"
                       (make-number-property (current-process-id)))
  (window-property-set *window* "_NET_WM_WINDOW_TYPE"
                       (make-atom-property "_NET_WM_TYPE_DOCK"))
  (window-property-set *window* "_NET_WM_DESKTOP"
                       (make-number-property #xffffffff))
  (window-property-set *window* "_NET_WM_STATE"
                       (make-atom-property "_NET_WM_STATE_BELOW"))
  (window-property-append *window* "_NET_WM_STATE"
                          (make-atom-property "_NET_WM_STATE_STICKY"))
  (window-property-append *window* "_NET_WM_STATE"
                          (make-atom-property "_NET_WM_STATE_SKIP_TASKBAR"))
  (window-property-append *window* "_NET_WM_STATE"
                          (make-atom-property "_NET_WM_STATE_SKIP_PAGER"))

  ;; Struts: left, right, top, bottom,
  ;;         left_start_y, left_end_y, right_start_y, right_end_y,
  ;;         top_start_x, top_end_x, bottom_start_x, bottom_end_x
  ;;
  ;; so for a top panel, we set top, top_start_x, and top_end_x.
  (set-struts *window*
              (if (eq? position 'bottom)
                  (list 0 0 0 whei 0 0 0 0 0 0 0 0)
                  (list 0 0 whei 0 0 0 0 0 0 0 0 0)))

  (let ((d-atom (xinternatom *display* "WM_DELETE_WINDOW" 1)))
    (let-location ((atm unsigned-long d-atom))
      (xsetwmprotocols *display* *window* (location atm) 1)))


  ;; "Main"
  ;;
  (define (handleexpose)
    (let* ((taken 0)
           (flex 0)
           (wids (map (lambda (x)
                        (if* (slot-value x 'flex)
                             (begin (set! flex (+ flex it))
                                    #f)
                             (let ((wid (widget-width x)))
                               (set! taken (+ taken wid))
                               wid)))
                      *widgets*))
           (remainder (- swid taken))
           (flexunit (if (> flex 0) (/ remainder flex) 0))
           (left 10))
      (for-each
       (lambda (w wid)
         (cond (wid (widget-draw w left wid)
                    (set! left (+ left wid)))
               (else (let ((wid (* flexunit (slot-value w 'flex))))
                       (widget-draw w left wid)
                       (set! left (+ left wid))))))
       *widgets*
       wids)))

  (let ((event (make-xevent)))
    (define (eventloop return)
      (xnextevent *display* event)
      (let ((type (xevent-type event)))
        (cond
         ((= type CLIENTMESSAGE)
          (display "closed!\n")
          (return #t))

         ((= type EXPOSE)
          (handleexpose)
          (display "expose\n"))

         ((= type BUTTONPRESS)
          (display "buttonpress\n")
          (return #t))

         (else
          (display "event ")
          (display (xevent-type event))
          (display "\n"))))
      (eventloop return))


    (push! (make <text-widget>
             'name "some-text"
             'text "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
             'font font)
           *widgets*)
    (xselectinput *display* *window*
                  (bitwise-ior EXPOSUREMASK
                               BUTTONPRESSMASK
                               STRUCTURENOTIFYMASK))
    (xmapwindow *display* *window*)
    (xnextevent *display* event)
    (handleexpose)
    (xflush *display*)
    (call/cc eventloop)))

(xclosedisplay *display*)
