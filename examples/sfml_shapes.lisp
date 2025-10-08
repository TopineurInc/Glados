(define (render-loop window shape)
  (if (sfml-window-is-open window)
      (begin
        (sfml-window-clear window 25 25 35)
        (sfml-window-draw window shape)
        (sfml-window-display window)
        (render-loop window shape))
      (sfml-close-window window)))

(define window (sfml-create-window 800 600 "SFML manual demo"))
(define rect (sfml-create-rectangle 180 120))

(sfml-shape-set-position rect 310 240)
(sfml-shape-set-fill-color rect 255 120 0)

(render-loop window rect)
