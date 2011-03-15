
;; mowedline: programmable status bar for X
;; Copyright (C) 2011  John J. Foerch
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(import chicken scheme extras foreign)

(use srfi-1
     srfi-4 ;; homogeneous numeric vectors
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


(define version "0.0")


;;;
;;; Language
;;;

(define L list)
(define rest cdr)


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


(define (start-client commands)
  (define dbus-context
    (dbus:make-context service: 'mowedline.server
                       interface: 'mowedline.interface))
  )


(define server-options
  '((("text-widget" name))
    (("bg" color))
    (("fg" color))
    (("screen" screen))
    (("position" position))))

(define client-options
  '((("quit"))
    (("read" widget source))
    (("update" widget value))))

(define special-options
  '((("help")
     "displays this help"
     (let ((longest
            (fold max 0
                  (map
                   (lambda (def)
                     (apply + 2 (string-length (command-name def))
                            (* 3 (length (command-args def)))
                            (map (compose string-length symbol->string)
                                 (command-args def))))
                   (append server-options client-options special-options))))
           (docspc 4))
       (define (help-section option-group)
         (for-each
          (lambda (def)
            (let ((col1 (apply string-append " -" (command-name def)
                               (map (lambda (a)
                                      (string-append " <" (symbol->string a) ">"))
                                    (command-args def)))))
              (display col1)
              (dotimes (_ (+ docspc (- longest (string-length col1)))) (display " "))
              (printf "~A~%" (command-doc def))))
          option-group))
       (printf "mowedline version ~A, by John J. Foerch~%" version)
       (printf "~%SPECIAL OPTIONS  (evaluate first one and exit)~%~%")
       (help-section special-options)
       (printf "~%SERVER OPTIONS  (only valid when starting the server)~%~%")
       (help-section server-options)
       (printf "~%CLIENT OPTIONS~%~%")
       (help-section client-options)
       (newline)))
    (("version")
     "prints the version"
     (printf "mowedline version ~A, by John J. Foerch~%" version))))

(define (command-name command-def)
  (caar command-def))

(define (command-args command-def)
  (cdar command-def))

(define (command-doc command-def)
  (string-join (take-while string? (cdr command-def)) "\n"))

(define (command-body command-def)
  (drop-while string? (cdr command-def)))


(define (make-commands-stucture)
  (vector '() '() '()))

(define (optype-internal-index optype)
  (case optype
    ((server-options) 0)
    ((client-options) 1)
    ((special-options) 2)))

(define (add-command! commands optype command)
  (let ((k (optype-internal-index optype)))
    (vector-set! commands k (cons command (vector-ref commands k)))))

(define (get-server-commands commands)
  (reverse (vector-ref commands 0)))

(define (get-client-commands commands)
  (reverse (vector-ref commands 1)))

(define (get-special-commands commands)
  (reverse (vector-ref commands 2)))

(define (mkcmd op args)
  (cons op args))

(define parse-command-line
  (case-lambda
   ((input count out)
    (if (null? input)
        out
        (let* ((opsym (first input))
               (input (rest input))
               (count (- count 1))
               (op (string-trim opsym #\-))
               (def #f)
               (optype (find (lambda (optype)
                               (set! def (assoc op (eval optype)
                                                (lambda (a b) (equal? a (car b)))))
                               def)
                             '(server-options client-options special-options))))
          (unless def
            (error (sprintf "unexpected symbol ~S~%" opsym)))
          (let ((narg (- (length (car def)) 1)))
            (when (< count narg)
              (error (sprintf "~A requires ~A arguments, but only ~A were given"
                              op narg count)))
            (let ((command (mkcmd op (take input narg))))
              (add-command! out optype command)
              (parse-command-line (list-tail input narg) (- count narg) out))))))
   ((input . args) (parse-command-line input (length input) (make-commands-stucture)))))

(let* ((commands (parse-command-line (command-line-arguments)))
       (server-commands (get-server-commands commands))
       (client-commands (get-client-commands commands))
       (special-commands (get-special-commands commands)))
  (cond
   ((not (null? special-commands))
    (let* ((cmd (first special-commands))
           (def (assoc (car cmd) special-options
                       (lambda (a b) (equal? a (car b))))))
      (eval
       `(let ,(zip (command-args def) (cdr cmd))
          ,@(command-body def))))
    (unless (null? (rest special-commands))
      (printf "~%Warning: the following commands were ignored:~%")
      (for-each
       (lambda (x) (printf "  ~S~%" x))
       (rest special-commands))))
   ((member "mowedline.server" (dbus:discover-services))
    (when (not (null? server-commands))
      (printf "Warning: the following commands were ignored because the server is already running:~%")
      (for-each
       (lambda (x) (printf "  ~S~%" x))
       server-commands))
    (start-client client-commands))
   (else
    (when (not (null? server-commands))
      ;; put server-commands somewhere where start-server can see them
      )
    (process-fork start-server)
    ;; wait for the server to be ready?
    (start-client client-commands))))

;; (put 'make-window 'scheme-indent-function 1)
;; (put 'foreign-lambda* 'scheme-indent-function 2)
;; (put 'let-location 'scheme-indent-function 1)