
(import chicken scheme)

(use (srfi 1)
     coops
     imlib2
     imlib2-xlib
     xft
     xlib
     (prefix xlib-utils xu:))

;; Active Window Icon
;;
(define-class <active-window-icon> (<widget>)
  ((%icon: initform: #f)))

(define (widget:active-window-icon . args)
  (apply make <active-window-icon> args))

(define-method (widget-init (widget <active-window-icon>))
  (call-next-method)
  (let* ((window (slot-value widget %window:))
         (xcontext (slot-value window %xcontext:)))
    (xu:with-xcontext xcontext (display screen root)
      (let* ((root-xcontext (find (lambda (xc) (= root (xu:xcontext-window xc))) xcontexts))
             (net-active-window-atom (xinternatom display "_NET_ACTIVE_WINDOW" 0)))
        (xu:add-event-handler! root-xcontext
                               PROPERTYNOTIFY
                               PROPERTYCHANGEMASK
                               (lambda (xcontext event)
                                 (update widget (active-window-icon-get-icon widget xcontext)))
                               ;; guard
                               (lambda (event)
                                 (= net-active-window-atom (xpropertyevent-atom event))))
        (xu:update-event-mask! root-xcontext)
        (widget-update widget (list (active-window-icon-get-icon widget root-xcontext)))))))

(define-method (widget-draw (widget <active-window-icon>) region)
  (let ((window (slot-value widget %window:)))
    (xu:with-xcontext (slot-value window %xcontext:) (xcontext display screen)
      (let* ((xwindow (xu:xcontext-window xcontext))
             (wrect (slot-value widget %xrectangle:))
             (x (xrectangle-x wrect))
             (attr (make-xwindowattributes))
             (_ (xgetwindowattributes display xwindow attr))
             (visual (xwindowattributes-visual attr))
             (colormap (xwindowattributes-colormap attr))
             (draw (xftdraw-create display xwindow visual colormap)))
        (define (make-color c)
          (apply make-xftcolor display visual colormap
                 (ensure-list c)))
        (let ((background-color (slot-value widget background-color:)))
          (if background-color
              (xft-draw-rect draw (make-color background-color) x 0
                             (xrectangle-width wrect)
                             (xrectangle-height wrect))
              (xcleararea display xwindow x 0
                          (xrectangle-width wrect)
                          (xrectangle-height wrect)
                          0)))
        (and-let* ((img (slot-value widget %icon:)))
          (imlib-context-set-display display)
          (imlib-context-set-visual visual)
          (imlib-context-set-colormap colormap)
          (imlib-context-set-drawable xwindow)
          (imlib-render-image-on-drawable img x 0))))))

(define-method (widget-preferred-width (widget <active-window-icon>))
  (cond
   ((> (slot-value widget flex:) 0) #f)
   ((slot-value widget %icon:) (slot-value (slot-value widget %window:) height:))
   (else 0)))

(define-method (widget-update (widget <active-window-icon>) params)
  (let ((icon (first params)))
    (set! (slot-value widget %icon:)
          (if (and icon (not (string? icon)))
              icon
              #f))))

(define (active-window-icon-get-icon widget root-xcontext)
  (xu:with-xcontext root-xcontext (display screen)
    (and-let* ((w (xu:get-active-window root-xcontext))
               ;;XXX: implement an icon cache?
               (icons (xu:window-get-icons* display screen w))
               (_ (not (null? icons)))
               (widget-height (slot-value (slot-value widget %window:) height:))
               (ximg (fold
                      (lambda (x best)
                        (let ((x-height (ximage-height x))
                              (best-height (ximage-height best)))
                          (cond
                           ((= best-height widget-height) best)
                           ((= x-height widget-height) x)
                           ((> x-height best-height) x)
                           (else best))))
                      (first icons)
                      (cdr icons))))
      (imlib-context-set-display display)
      (imlib-context-set-visual (xdefaultvisual display screen))
      (image-scale
       (imlib-create-image-from-ximage ximg #f 0 0
                                       (ximage-width ximg)
                                       (ximage-height ximg)
                                       #f)
       widget-height widget-height))))


;; Active Window Title
;;
(define-class <active-window-title> (<text-widget>)
  ())

(define (widget:active-window-title . args)
  (apply make <active-window-title> args))

(define-method (widget-init (widget <active-window-title>))
  (call-next-method)
  (let* ((window (slot-value widget %window:))
         (xcontext (slot-value window %xcontext:)))
    (xu:with-xcontext xcontext (display root)
      (let* ((root-xcontext (find (lambda (xc) (= root (xu:xcontext-window xc))) xcontexts))
             (net-active-window-atom (xinternatom display "_NET_ACTIVE_WINDOW" 0)))
        (xu:add-event-handler! root-xcontext
                               PROPERTYNOTIFY
                               PROPERTYCHANGEMASK
                               (lambda (xcontext event)
                                 (update widget
                                         (or (and-let* ((w (xu:get-active-window xcontext)))
                                               (xu:window-get-title* display w))
                                             "")))
                               ;; guard
                               (lambda (event)
                                 (= net-active-window-atom (xpropertyevent-atom event))))
        (xu:update-event-mask! root-xcontext)
        (widget-update widget
                       (list
                        (or (and-let* ((w (xu:get-active-window root-xcontext)))
                              (xu:window-get-title* display w))
                            "")))))))
