
(import chicken scheme extras foreign)

(use miscmacros
     srfi-4  ;; homogeneous numeric vector datatypes
     srfi-69 ;; hash tables
     lolevel
     posix
     xlib)


(define *display* (xopendisplay #f))
(assert *display*)


(define (xtextproperty-make textp)
  (let ((tp (make-xtextproperty)))
    (set-xtextproperty-value! tp (location textp))
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

(define (set-struts win strut-callback)
  (xchangeproperty *display* win
                   (xinternatom *display* "_NET_WM_STRUT_PARTIAL" 0)
                   (xinternatom *display* "CARDINAL" 0)
                   32
                   PROPMODEREPLACE
                   (strut-callback)
                   12))

(define (make-atom-property atom-name)
  (let ((data (xinternatom *display* atom-name 0)))
    (let-location ((data unsigned-long data))
      (vector "ATOM" 32
              (location data)    
              1))))

(define (make-number-property number)
  (let-location ((data unsigned-long number))
    (vector "CARDINAL" 32 (location data) 1)))


(define (property-type property)
  (vector-ref property 0))
(define (property-format property)
  (vector-ref property 1))
(define (property-data property)
  (vector-ref property 2))
(define (property-count property)
  (vector-ref property 3))


(let* ((screen (xdefaultscreen *display*))
       (root (xrootwindow *display* screen))
       (swid (xdisplaywidth *display* screen))
       (shei (xdisplayheight *display* screen))
       (attr (make-xsetwindowattributes))
       (vis (make-visual)))
  (set-xsetwindowattributes-background_pixel! attr (xblackpixel *display* screen))
  (set-xsetwindowattributes-border_pixel! attr (xblackpixel *display* screen))
  (set-xsetwindowattributes-override_redirect! attr 1)
  (set-visual-class! vis COPYFROMPARENT)

  (let ((win (xcreatewindow
              *display* root
              0 0 swid 20 0
              (xdefaultdepth *display* screen)
              INPUTOUTPUT vis
              (bitwise-ior CWBACKPIXEL CWBORDERPIXEL CWOVERRIDEREDIRECT)
              attr)))
    (assert win)

    ;;;
    ;;; Window Properties
    ;;;
    (xstorename *display* win "jjfpanel")

    (let ((p (make-xtextproperty))
          (str (xtextproperty-make (get-host-name))))
      (xstringlisttotextproperty str 1 p)
      (xsetwmclientmachine *display* win p))

    (window-property-set win "_NET_WM_PID"
                         (make-number-property (current-process-id)))
    (window-property-set win "_NET_WM_WINDOW_TYPE"
                         (make-atom-property "_NET_WM_TYPE_DOCK"))
    (window-property-set win "_NET_WM_DESKTOP"
                         (make-number-property #xffffffff))
    (window-property-set win "_NET_WM_STATE"
                         (make-atom-property "_NET_WM_STATE_BELOW"))
    (window-property-append win "_NET_WM_STATE"
                            (make-atom-property "_NET_WM_STATE_STICKY"))
    (window-property-append win "_NET_WM_STATE"
                            (make-atom-property "_NET_WM_STATE_SKIP_TASKBAR"))
    (window-property-append win "_NET_WM_STATE"
                            (make-atom-property "_NET_WM_STATE_SKIP_PAGER"))

    (set-struts win
                (foreign-lambda* c-pointer ()
                  "unsigned long strut[12] = { 0, 0, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0 };"
                  "return(strut);"))

    (xselectinput *display* win
                  (bitwise-ior
                   EXPOSUREMASK
                   BUTTONPRESSMASK
                   STRUCTURENOTIFYMASK))

    (let ((d-atom (xinternatom *display* "WM_DELETE_WINDOW" 1)))
      (let-location ((atm unsigned-long d-atom))
        (xsetwmprotocols *display* win (location atm) 1)))


    (let ((font (xloadfont *display* "-misc-fixed-bold-*-*-*-*-100-*-*-*-*-*-*")))
      (assert font)
      (let ((gc (xcreategc *display* win 0 #f))
            (event (make-xevent)))
        (xsetbackground *display* gc (xblackpixel *display* screen))
        (xsetforeground *display* gc (xwhitepixel *display* screen))
        (xsetfunction *display* gc GXCOPY)
        (xsetfont *display* gc font)

        (define (handleexpose)
          (let ((text "[j-e2,s,m-e267,lam-e23]"))
            (xdrawstring *display* win gc 10 18 text (string-length text))))

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

        (xmapwindow *display* win)
        (xnextevent *display* event)
        (handleexpose)
        (xflush *display*)
        (call/cc eventloop)))))

(xclosedisplay *display*)
