#lang racket
(require picturing-programs)

;image definitions
(define player_image (rotate-cw (triangle 25 "solid" "black")))
(define projectile_image (rotate-cw (rectangle 5 10 "solid" "red")))
(define enemy_image (rectangle 50 50 "solid" "green"))
(define enemy_image_yellow (rectangle 50 50 "solid" "yellow"))
(define enemy_image_orange (rectangle 50 50 "solid" "orange"))
(define enemy_image_red (rectangle 50 50 "solid" "red"))
(define background (empty-scene 500 500))
(define crosshair (overlay (line 25 0 "red") (line 0 25 "red")))

;calculations
(define (distance x2 y2 x1 y1)
(sqrt (+ (* (- x2 x1)
            (- x2 x1))
         (* (- y2 y1)
            (- y2 y1)))))

;structure definitions

(define-struct player (image x y direction action status))
(define-struct projectile (image x y direction action))
(define-struct enemy (image x y action status))
(define-struct scene (player projectile enemy mouse_x mouse_y image))

;default settings
(define current (make-scene (make-player player_image 250 250 0 "stationary" 4)
                            (make-projectile projectile_image 250 250 0 "stationary")
                            (make-enemy enemy_image 250 375 "stationary" 4)
                            375
                            125
                            background))

;rendering

(define (render-player current) (rotate (player-direction (scene-player current)) (player-image (scene-player current))))

(define (render-projectile current) (cond ;[(string=? (projectile-action (scene-projectile current)) "stationary")
                                           ;()
                                          [(string=? (projectile-action (scene-projectile current)) "fired")                                     
                                            (place-image (rotate (projectile-direction (scene-projectile current)) (projectile-image (scene-projectile current)))
                                            (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))
                                            (scene-image current))]
                                          [else (scene-image current)]))
(define (render-enemy current) (place-image crosshair
                                            (scene-mouse_x current) (scene-mouse_y current)
                                            (place-image (render-player current)
                                            (player-x (scene-player current)) (player-y (scene-player current))
                                            (place-image (enemy-image (scene-enemy current))
                                                         (enemy-x (scene-enemy current))
                                                         (enemy-y (scene-enemy current))
                                            (render-projectile current))))) 
(define (render-scene current) (place-image crosshair
                                            (scene-mouse_x current) (scene-mouse_y current)
                                            (place-image (render-player current)
                                            (player-x (scene-player current)) (player-y (scene-player current))
                                            
                                            (render-enemy current))))


;controls

(define (move_player_left current) (make-player (player-image (scene-player current)) (- (player-x (scene-player current)) 10) (player-y (scene-player current)) (player-direction (scene-player current)) "moved left" (player-status (scene-player current))))

(define (move_player_right current) (make-player (player-image (scene-player current)) (+ (player-x (scene-player current)) 10) (player-y (scene-player current)) (player-direction (scene-player current)) "moved right" (player-status (scene-player current))))

(define (move_player_down current) (make-player (player-image (scene-player current)) (player-x (scene-player current)) (+ (player-y (scene-player current)) 10) (player-direction (scene-player current)) "moved down" (player-status (scene-player current))))

(define (move_player_up current) (make-player (player-image (scene-player current)) (player-x (scene-player current)) (- (player-y (scene-player current)) 10) (player-direction (scene-player current)) "moved up" (player-status (scene-player current))))

(define (move_projectile_left current) (make-projectile (projectile-image (scene-projectile current)) (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))(projectile-direction (scene-projectile current)) "moving_left"))

(define (move_projectile_right current) (make-projectile (projectile-image (scene-projectile current)) (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))(projectile-direction (scene-projectile current)) "moving_right"))

(define (move_projectile_down current) (make-projectile (projectile-image (scene-projectile current)) (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))(projectile-direction (scene-projectile current)) "moving_down"))

(define (move_projectile_up current) (make-projectile (projectile-image (scene-projectile current)) (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))(projectile-direction (scene-projectile current)) "moving_up"))
;keyboard
(define (player_controls current key) (cond [(and (string=? key "a") (> (player-x (scene-player current)) 13))
                                    (make-scene (move_player_left current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [(and (string=? key "d") (< (player-x (scene-player current)) 487))
                                    (make-scene  (move_player_right current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [(and (string=? key "s") (< (player-y (scene-player current)) 487))
                                    (make-scene (move_player_down current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [(and (string=? key "w") (> (player-y (scene-player current)) 13))
                                    (make-scene (move_player_up current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [(string=? key "left")
                                    (make-scene (scene-player current) (move_projectile_left current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [(string=? key "right")
                                    (make-scene (scene-player current) (move_projectile_right current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [(string=? key "up")
                                    (make-scene (scene-player current) (move_projectile_up current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [(string=? key "down")
                                    (make-scene (scene-player current) (move_projectile_down current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-image current))]
                                   [else current]))


;mouse
(define (mouse-handle current x y mouse-event) (cond [(string=? mouse-event "button-down")
                                                      (make-scene (scene-player current)
                                                                  (make-projectile (projectile-image (scene-projectile current)) (player-x (scene-player current)) (player-y (scene-player current)) (player-direction (scene-player current)) "fired")
                                                                  (scene-enemy current)
                                                                  x
                                                                  y
                                                                  (scene-image current))]
                                                     [(string=? mouse-event "button-up")
                                                      (make-scene (scene-player current)
                                                                  (make-projectile (projectile-image (scene-projectile current)) (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current)) (projectile-direction (scene-projectile current)) "moving")
                                                                  (scene-enemy current)
                                                                  (scene-mouse_x current)
                                                                  (scene-mouse_y current)
                                                                  (scene-image current))]

                                                     [(string=? mouse-event "move")
                                                  (make-scene (make-player (player-image (scene-player current)) (player-x (scene-player current)) (player-y (scene-player current)) 
                                                     (cond [(and (< (scene-mouse_y current) (player-y (scene-player current))) (>= (scene-mouse_x current) (player-x (scene-player current))))
                                                                                             (- 90 (/ (* 90 (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (scene-mouse_y current)))
                                                             (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [(and (<= (scene-mouse_y current) (player-y (scene-player current))) (< (scene-mouse_x current) (player-x (scene-player current))))
                                                                                             (+ 90 (/ (* 90 (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (scene-mouse_y current)))
                                                             (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [(and (> (scene-mouse_y current) (player-y (scene-player current))) (<= (scene-mouse_x current) (player-x (scene-player current))))
                                                                                             (- 270 (/ (* 90 (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (scene-mouse_y current)))
                                                             (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [(and (> (scene-mouse_y current) (player-y (scene-player current))) (> (scene-mouse_x current) (player-x (scene-player current))))
                                                                                             (+ 270 (/ (* 90 (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (scene-mouse_y current)))
                                                             (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [else (/ (* 90 (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (scene-mouse_y current)))
                                                             (distance (scene-mouse_x current) (scene-mouse_y current) (player-x (scene-player current)) (player-y (scene-player current))))])
                                                     (player-action (scene-player current))
                                                     (player-status (scene-player current)))
                                                              (scene-projectile current)
                                                              (scene-enemy current)
                                                              x y (scene-image current))]
                                                                                                                                               
                                                     [else current]))

;move projectile difference of x y mouse
;tick-handle: function for actions over time
(define (tick-handle current) (cond [(and (and (> (player-x (scene-player current)) 13) (< (player-x (scene-player current)) 487))
                                          (and (> (player-y (scene-player current)) 13) (< (player-y (scene-player current)) 487)))
                                     (cond [(string=? (player-action (scene-player current)) "moved left")
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (+ (player-x (scene-player current)) (/ (- 0 (player-x (scene-player current))) 50))
                                                              (player-y (scene-player current))
                                                              (player-direction (scene-player current))
                                                              "moved left"
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    [(string=? (player-action (scene-player current)) "moved right")
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (+ (player-x (scene-player current)) (/ (- 500 (player-x (scene-player current))) 50))
                                                              (player-y (scene-player current))
                                                              (player-direction (scene-player current))
                                                              (player-action (scene-player current))
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    [(string=? (player-action (scene-player current)) "moved up")
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (player-x (scene-player current))
                                                              (+ (player-y (scene-player current)) (/ (- 0 (player-y (scene-player current))) 50))
                                                              (player-direction (scene-player current))
                                                              (player-action (scene-player current))
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    [(string=? (player-action (scene-player current)) "moved down")
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (player-x (scene-player current))
                                                              (+ (player-y (scene-player current)) (/ (- 500 (player-y (scene-player current))) 50))
                                                              (player-direction (scene-player current))
                                                              "moved down"
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    [else current])]
                                    [(string=? (projectile-action (scene-projectile current)) "fired")
                                     (make-scene (scene-player current)
                                                 (make-projectile (projectile-image (scene-projectile current))
                                                                  (+ (projectile-x (scene-projectile current)) (/ (- (scene-mouse_x current)(projectile-x (scene-projectile current))) 6))
                                                                  (+ (projectile-y (scene-projectile current)) (/ (- (scene-mouse_y current) (projectile-y (scene-projectile current))) 6))
                                                                  (projectile-direction (scene-projectile current))
                                                                  "fired")
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    [(<= (distance (enemy-x (scene-enemy current)) (enemy-y (scene-enemy current))
                                               (player-x (scene-player current)) (player-y (scene-player current))) 18)
                                     (make-scene (make-player 
                                                  (player-image (scene-player current))
                                                  (player-x (scene-player current))
                                                  (player-y (scene-player current))
                                                  (player-direction (scene-player current))
                                                  (player-action (scene-player current))
                                                  (- (player-status (scene-player current)) 1))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    [(< (enemy-status (scene-enemy current)) 4)
                                     (make-scene (scene-player current)
                                                 (scene-projectile current)
                                                 (make-enemy enemy_image_red
                                                             (enemy-x (scene-enemy current))
                                                             (enemy-y (scene-enemy current))
                                                             "moving"
                                                             (enemy-status (scene-enemy current)))
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    
                                    [(<= (distance (enemy-x (scene-enemy current)) (enemy-y (scene-enemy current))
                                               (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))) 25)
                                     (make-scene (scene-player current)
                                                 (scene-projectile current)
                                                 (make-enemy (scale 0.99 (enemy-image (scene-enemy current)))
                                                             (enemy-x (scene-enemy current))
                                                             (enemy-y (scene-enemy current))
                                                             "shrinking"
                                                             (enemy-status (scene-enemy current)))
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-image current))]
                                    
                                    
                                    [else current]))

;starter
(define (shooter) (big-bang current
                            (on-draw render-scene)
                            (on-mouse mouse-handle)
                            (on-key player_controls)
                            (on-tick tick-handle 1/60)))