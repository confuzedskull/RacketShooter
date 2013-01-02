;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |multiple shots|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require picturing-programs)

(define object_image (circle 5 "solid" "black"))
(define background  (rectangle 500 500 "outline" "black"))
(define barrage-init empty)

(define object_x 250)
(define object_y 250)

(define (render-barrage barrage) (cond [(empty? barrage) (empty-scene 500 500)]
                                    [(cons? barrage) (cons-render-barrage barrage)]))


(define (cons-render-barrage barrage)
  (overlay (place-image object_image
                        (posn-x (first barrage))
                        (posn-y (first barrage))
                                            background)
           (render-barrage (rest barrage))))


(define (new-barrage barrage x y)
  (cons (new-projectile x y) barrage))

(define (new-projectile x y)
  (make-posn x y))

(define (mouse-handle barrage x y mouse-event)
  (cond [(string=? mouse-event "button-down")
         (cons (make-posn x y) barrage)]
        [else barrage]))

(define (key-handle barrage key)
  (cond [(string=? key " ")
         (cons (make-posn object_x object_y) barrage)]
        [else barrage]))

(define (projectile-tick current)
  (if (empty? current)
      barrage-init
  (make-posn (posn-x current)
(- (posn-y current) 1))))

(define (barrage-tick barrage)
  (cond [(empty? barrage) empty]
        [(cons? barrage) (cons-barrage-tick barrage)]))

(define (cons-barrage-tick barrage)
  (cons (projectile-tick (first barrage))
        (barrage-tick (rest barrage))))

(define-struct game (barrage))

(define (game-tick game)
  (make-game (barrage-tick (game-barrage game))))

(define (game-render game)
  (overlay (render-barrage (game-barrage game))
           (empty-scene 100 100)))

(define undo (big-bang barrage-init
  (on-draw render-barrage)
  (on-mouse mouse-handle)
  (on-key key-handle)
  (on-tick barrage-tick)))
  
  