
;; Spacer
;;
(define-class <spacer> (<widget>)
  ((width: initform: #f)))

(define (widget:spacer . args)
  (apply make <spacer> args))

(define-method (widget-preferred-width (widget <spacer>))
  (if (> (slot-value widget flex:) 0)
      #f
      (or (slot-value widget width:) 1)))


;; Text Widget
;;
(define text-widget-font (make-parameter "mono-10:bold"))
(define text-widget-color (make-parameter (list 1 1 1 1)))
(define text-widget-format (make-parameter identity))

(define-class <text-widget> (<widget>)
  ((text: initform: "")
   (font: initform: (text-widget-font))
   (color: initform: (text-widget-color))
   (format: initform: (text-widget-format))))

(define (widget:text . args)
  (apply make <text-widget> args))

(define-method (initialize-instance (widget <text-widget>))
  (call-next-method)
  ;; apply formatting to default text
  (widget-update widget (list (slot-value widget text:))))

(define-method (widget-draw (widget <text-widget>) region)
  (let ((window (slot-value widget %window:)))
    (xu:with-xcontext (slot-value window %xcontext:) (xcontext display)
      (let* ((xwindow (xu:xcontext-window xcontext))
             (wrect (slot-value widget %xrectangle:))
             (font (window-get-create-font window (slot-value widget font:)))
             (x (xrectangle-x wrect))
             (baseline (slot-value window %baseline:))
             (attr (make-xwindowattributes))
             (_ (xgetwindowattributes display xwindow attr))
             (visual (xwindowattributes-visual attr))
             (colormap (xwindowattributes-colormap attr))
             (draw (xftdraw-create display xwindow visual colormap)))
        (define (make-color c)
          (apply make-xftcolor display visual colormap
                 (ensure-list c)))
        (set! (slot-value widget %buttons:) (list))
        (let ((color (make-color (slot-value widget color:)))
              (background-color (slot-value widget background-color:)))
          (xftdraw-set-clip! draw region)
          (if background-color
              (xft-draw-rect draw (make-color background-color) x 0
                             (xrectangle-width wrect)
                             (xrectangle-height wrect))
              (xcleararea display xwindow x 0
                          (xrectangle-width wrect)
                          (xrectangle-height wrect)
                          0))
          (let walk ((term (slot-value widget text:))
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
                         (slot-value widget %buttons:))))
               ((eq? 'color (first term))
                (walk (cddr term) fonts (cons (make-color (second term)) colors)))
               ((eq? 'font (first term))
                (walk (cddr term) (cons (window-get-create-font window (second term)) fonts) colors))
               (else
                (walk (first term) fonts colors)
                (walk (rest term) fonts colors)))))))))))

(define-method (widget-preferred-baseline (widget <text-widget>))
  (xftfont-ascent (window-get-create-font
                   (slot-value widget %window:)
                   (slot-value widget font:))))

(define-method (widget-preferred-height (widget <text-widget>))
  ;; i find even common fonts extend a pixel lower than their
  ;; declared descent.  tsk tsk.
  (let ((font (window-get-create-font
               (slot-value widget %window:)
               (slot-value widget font:))))
    (+ (xftfont-ascent font) (xftfont-descent font) 2)))

(define-method (widget-preferred-width (widget <text-widget>))
  (let ((window (slot-value widget %window:)))
    (xu:with-xcontext (slot-value window %xcontext:) (display)
      (define (x-extent str font)
        (xglyphinfo-xoff
         (xft-text-extents display
                           (window-get-create-font window font)
                           str)))
      (if (> (slot-value widget flex:) 0)
          #f
          (let walk ((term (slot-value widget text:))
                     (font (slot-value widget font:)))
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
  (set! (slot-value widget text:)
        ((slot-value widget format:) (first params))))


;; Clock
;;
(define (clock-thread widget)
  (lambda ()
    (let loop ()
      (let* ((time (seconds->local-time (current-seconds)))
             (s (vector-ref time 0)))
        (gochan-send
         *internal-events*
         (lambda ()
           (update widget
                   (time->string time (slot-value widget time-format:)))))
        (thread-sleep! (- 60 s)))
      (loop))))

(define-class <clock> (<text-widget>)
  ((time-format: initform: "%a %b %e %H:%M %Z %Y")))

(define (widget:clock . args)
  (apply make <clock> args))

(define-method (widget-init (widget <clock>))
  (call-next-method)
  (thread-start! (make-thread (clock-thread widget))))


;; Flags
;;
(define-class <flags> (<text-widget>)
  ((flags: initform: '())
   (%curflags: initform: '())))

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
       (set! (slot-value widget %curflags:)
             (lset-union equal? (slot-value widget %curflags:) tokens)))
      ((remove)
       (set! (slot-value widget %curflags:)
             (lset-difference equal? (slot-value widget %curflags:) tokens)))
      ((replace)
       (set! (slot-value widget %curflags:) tokens)))
    (let ((curflags (slot-value widget %curflags:)))
      (set!
       (slot-value widget text:)
       ((slot-value widget format:)
        (fold (lambda (flagdef result)
                (let ((flag (first flagdef))
                      (r (rest flagdef)))
                  (if (member flag curflags)
                      (cons r result)
                      result)))
              '()
              (reverse (slot-value widget flags:))))))))


;; Map
;;
(define (map-format-pair k v)
  (string-append (->string k) "=" (->string v)))

(define-class <map> (<text-widget>)
  ((%data: initform: (make-hash-table))
   (format-pair: initform: map-format-pair)
   (separator: initform: ",")))

(define (widget:map . args)
  (apply make <map> args))

(define-method (widget-update (widget <map>) params)
  (receive (fst snd)
      (with-input-from-string (first params)
        (lambda () (values (read) (read))))
    (unless (or (eof-object? fst) (eof-object? snd))
      (hash-table-set! (slot-value widget %data:)
                       fst snd))
    (set!
     (slot-value widget text:)
     ((slot-value widget format:)
      (hash-table-fold
       (slot-value widget %data:)
       (lambda (k v a)
         (let ((part ((slot-value widget format-pair:) k v)))
           (if part
               (string-append part
                              (if (string-null? a)
                                  ""
                                  (slot-value widget separator:))
                              a)
               a)))
       "")))))
