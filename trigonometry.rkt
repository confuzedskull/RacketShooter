#lang racket

(require picturing-programs)

(define background (empty-scene 500 500))

(define-struct rotation_point (image x y))
(define-struct object (image x y))
(define-struct mouse_posn (x y))
(define-struct scene (xp yp xm ym angle))

(define pointer (rotate-cw (triangle 25 "outline" "red")))

(define current (make-scene 250 250 375 125 0))

(define (distance x2 y2 x1 y1)
(sqrt (+ (* (- x2 x1)
            (- x2 x1))
         (* (- y2 y1)
            (- y2 y1)))))

(define (draw-handle current) (place-image (rotate (scene-angle current) pointer) (scene-xp current) (scene-yp current)
                                                   (place-image (above/align "center"(text "m" 10 "green") 
                                                                             (text (number->string (cond [(and (< (scene-ym current) (scene-yp current)) (>= (scene-xm current) (scene-xp current)))
                                                                                             (- 90(/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [(and (<= (scene-ym current) (scene-yp current)) (<= (scene-xm current) (scene-xp current)))
                                                                                             (+ 90 (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [(and (> (scene-ym current) (scene-yp current)) (<= (scene-xm current) (scene-xp current)))
                                                                                             (- 270 (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [(and (> (scene-ym current) (scene-yp current)) (> (scene-xm current) (scene-xp current)))
                                                                                             (+ 270 (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [else (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current)))]
                                                                                            ))10 "black"))
                                                        
                                           (scene-xm current)
                                           (scene-ym current)
                               (place-image (text "k" 10 "blue")
                                           (scene-xm current)
                                           (scene-yp current) (place-image (text "p" 10 "red")
                                                                               (scene-xp current) 
                                                                               (scene-yp current) background)))))


(define (mouse-handle current x y mouse-event) (cond [(string=? mouse-event "move")
                                                  (make-scene (scene-xp current) (scene-yp current) x y
                                                     (cond [(and (< (scene-ym current) (scene-yp current)) (>= (scene-xm current) (scene-xp current)))
                                                                                             (- 90 (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [(and (<= (scene-ym current) (scene-yp current)) (< (scene-xm current) (scene-xp current)))
                                                                                             (+ 90 (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [(and (> (scene-ym current) (scene-yp current)) (<= (scene-xm current) (scene-xp current)))
                                                                                             (- 270 (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [(and (> (scene-ym current) (scene-yp current)) (> (scene-xm current) (scene-xp current)))
                                                                                             (+ 270 (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current))))]
                                                                                            [else (/ (* 90 (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-ym current)))
                                                             (distance (scene-xm current) (scene-ym current) (scene-xp current) (scene-yp current)))]))]
                                                                                            
                                                      
                                                     [else current]))


(define triangle_dots (big-bang current
                                  (on-draw draw-handle)
                                  (on-mouse mouse-handle)))

                                  