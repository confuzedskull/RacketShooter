;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |shooter r5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;Shooter by James Nakano
;;revision 5
;;;changelog:
;;;;moving enemy
;;;;changed player movement
;;;;fixed enemy life change
;;;;enemy health render
;;;;added mouse click position to mouse structure definitions
;;;;started experimental list-based data storage


(require picturing-programs)

;<image definitions>
(define player_image (rotate-cw (triangle 25 "solid" "black")))
(define projectile_image (rotate-cw (rectangle 5 10 "solid" "red")))
(define enemy_image (rectangle 50 50 "solid" "green"))
(define enemy_image_yellow (rectangle 50 50 "solid" "yellow"))
(define enemy_image_orange (rectangle 50 50 "solid" "orange"))
(define enemy_image_red (rectangle 50 50 "solid" "red"))
(define background (rectangle 500 500 "outline" "bla
ck"))
(define crosshair (overlay (line 25 0 "red") (line 0 25 "red")))
(define win_screen (overlay/align "center" "center" (text "You Win!" 50 "green") background))
(define lose_screen (overlay/align "center" "center" (text "Game Over" 50 "red") background))


;<calculations>
;;basic distance formula
(define (distance x2 y2 x1 y1)
(sqrt (+ (* (- x2 x1)
            (- x2 x1))
         (* (- y2 y1)
            (- y2 y1)))))

(define (distance_enemy-projectile current) (distance (enemy-x (scene-enemy current)) (enemy-y (scene-enemy current))
                                                      (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))))

(define (distance_enemy-barrage current) (distance (enemy-x (scene-enemy current)) (enemy-y (scene-enemy current))
                                                   (posn-x (first (scene-barrage current))) (posn-y (first (scene-barrage current)))))

(define (average_enemy_radius current) (/ (+ (image-width (enemy-image (scene-enemy current)))(image-height (enemy-image (scene-enemy current))))4))

(define (projectile_hit_enemy? current) (if (or (<= (distance_enemy-projectile current) (average_enemy_radius current))
                                                (<= (distance_enemy-barrage current) (average_enemy_radius current)))  
                                     true
                                     false))

(define (projectile_hit_mouse? current) (if (<= (distance (first (mouse_x-clicked (scene-mouse_x current))) (first (mouse_y-clicked (scene-mouse_y current)))
                                               (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))) 30)
                                     true
                                     false))

(define (player_touch_wall? current) (cond [(and (> (player-x (scene-player current)) 12.5) (< (player-x (scene-player current)) 487.5)
                                            (> (player-y (scene-player current)) 12.5) (< (player-y (scene-player current)) 487.5))
                                            false]
                                           [else true]))

(define (player_touch_enemy? current) (cond [(<= (distance (enemy-x (scene-enemy current)) (enemy-y (scene-enemy current))
                                               (player-x (scene-player current)) (player-y (scene-player current))) (+ (/ (+ (image-width (enemy-image (scene-enemy current)))
                                                                                                                             (image-height (enemy-image (scene-enemy current))))4)
                                                                                                                       17))
                                             true]
                                            [else false]))

;;not used yet
(define (player_moving? current) (if (or (string=? (player-action (scene-player current)) "moved left")
     (string=? (player-action (scene-player current)) "moved right")
     (string=? (player-action (scene-player current)) "moved up")
     (string=? (player-action (scene-player current)) "moved down"))
                                     true
                                     false))

;<structure definitions>
(define barrage-init empty)
(define-struct player (image x y direction action status))
(define-struct projectile (image x y direction action))
(define-struct enemy (image x y action status))
;;mouse_
(define-struct mouse_x (moving clicked))
(define-struct mouse_y (moving clicked))
(define-struct scene (player projectile enemy mouse_x mouse_y barrage))

;list of mouse positions
(define mouse_posn-init (list 0))


;default settings
(define current (make-scene (make-player player_image 250 250 0 "stationary" 100)
                            (make-projectile projectile_image 250 250 0 "stationary")
                            (make-enemy enemy_image 375 125 "stationary" 200)
                            (make-mouse_x 250 mouse_posn-init)
                            (make-mouse_y 125 mouse_posn-init)
                            (list (make-posn 250 250))))

;<rendering definitions>
;;crosshair
(define (render-crosshair current) (place-image crosshair
                                                (mouse_x-moving (scene-mouse_x current))
                                                (mouse_y-moving (scene-mouse_y current))
                                                background))
;;player
(define (render-player current) (place-image (rotate (player-direction (scene-player current)) (player-image (scene-player current)))
                                             (player-x (scene-player current))
                                             (player-y (scene-player current))
                                                 background))
;;projectile
(define (render-projectile current) (place-image (rotate (projectile-direction (scene-projectile current)) (projectile-image (scene-projectile current)))
                                                 (projectile-x (scene-projectile current))
                                                 (projectile-y (scene-projectile current))
                                                 background))
                                                 
;;barrage
(define (render-barrage barrage current) (cond 
                                   [(empty? barrage) background]
                                   [(cons? barrage) (cons-render-barrage barrage current)]))

(define (cons-render-barrage barrage current)
  (overlay (place-image (rotate (projectile-direction (scene-projectile current)) (projectile-image (scene-projectile current)))
                        (posn-x (first barrage))
                        (posn-y (first barrage))
                                             background)
           (render-barrage (rest barrage) current)))

;;enemy
(define (render-enemy current) (place-image (enemy-image (scene-enemy current))
                                                         (enemy-x (scene-enemy current))
                                                         (enemy-y (scene-enemy current))
                                            background))

;;scene
(define (win-lose? current) (cond [(<= (player-status (scene-player current)) 0)
                                   lose_screen]
                                  [(<= (enemy-status (scene-enemy current)) 0)
                                   win_screen]
                                  [else background]))

(define (render-scene current) (place-image crosshair
                                                (mouse_x-moving (scene-mouse_x current))
                                                (mouse_y-moving (scene-mouse_y current))
                                                (overlay 
                                (win-lose? current)
                                (render-player current)
                                (render-enemy current)
                                (render-barrage (scene-barrage current) current)
                                ;(render-projectile current)
                                            (overlay/align "left" "bottom" (beside (text (cond [(<= (player-status (scene-player current)) 0) "player dead" ]
                                                                                                [else (string-append "player:"(number->string (player-status (scene-player current))))]) 10 "black")
                                                                                   (text (cond [(<= (enemy-status (scene-enemy current)) 0) "enemy dead" ]
                                                                                               [else (string-append "enemy:"(number->string (enemy-status (scene-enemy current))))]) 10 "black"))
                                             background))))


;<controls>

;;"player-action" definitions
(define (move_player_left current) (make-player (player-image (scene-player current))
                                                (- (player-x (scene-player current)) 10)
                                                (player-y (scene-player current))
                                                (player-direction (scene-player current))
                                                ;"moved left"
                                                (player-action (scene-player current))
                                                (player-status (scene-player current))))

(define (move_player_right current) (make-player (player-image (scene-player current))
                                                 (+ (player-x (scene-player current)) 10)
                                                 (player-y (scene-player current))
                                                 (player-direction (scene-player current))
                                                 ;"moved right"
                                                 (player-action (scene-player current))
                                                 (player-status (scene-player current))))

(define (move_player_down current) (make-player (player-image (scene-player current))
                                                (player-x (scene-player current))
                                                (+ (player-y (scene-player current)) 10)
                                                (player-direction (scene-player current))
                                                ;"moved down"
                                                (player-action (scene-player current))
                                                (player-status (scene-player current))))

(define (move_player_up current) (make-player (player-image (scene-player current))
                                              (player-x (scene-player current))
                                              (- (player-y (scene-player current)) 10)
                                              (player-direction (scene-player current))
                                              ;"moved up" 
                                              (player-action (scene-player current))
                                              (player-status (scene-player current))))

;;;to be implemented
(define (new-barrage barrage x y)
  (cons (new-projectile x y) barrage))

(define (new-projectile x y)
  (make-posn x y))

;;keyboard
(define (player_controls current key) (cond [(and (string=? key "a") (> (player-x (scene-player current)) 12))
                                    (make-scene (move_player_left current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-barrage current))]
                                   [(and (string=? key "d") (< (player-x (scene-player current)) 487))
                                    (make-scene  (move_player_right current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-barrage current))]
                                   [(and (string=? key "s") (< (player-y (scene-player current)) 487))
                                    (make-scene (move_player_down current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-barrage current))]
                                   [(and (string=? key "w") (> (player-y (scene-player current)) 12))
                                    (make-scene (move_player_up current) (scene-projectile current) (scene-enemy current) (scene-mouse_x current) (scene-mouse_y current) (scene-barrage current))]
                                   
                                   [else current]))


;;mouse

(define (mouse-handle current x y mouse-event) (cond [(string=? mouse-event "button-down")
                                                      (make-scene (scene-player current)
                                                                  (make-projectile (projectile-image (scene-projectile current)) (player-x (scene-player current)) (player-y (scene-player current)) (player-direction (scene-player current)) "fired")
                                                                  (scene-enemy current)
                                                                  (make-mouse_x (mouse_x-moving (scene-mouse_x current)) (cons x (mouse_x-clicked (scene-mouse_x current))))
                                                                  (make-mouse_y (mouse_y-moving (scene-mouse_y current)) (cons y (mouse_y-clicked (scene-mouse_y current))))
                                                                  (scene-barrage current))]
                                                     
                                                    ; [(string=? mouse-event "button-up")
                                                     ; (make-scene (scene-player current)
                                                      ;            (make-projectile (projectile-image (scene-projectile current)) (player-x (scene-player current)) (player-y (scene-player current)) (player-direction (scene-player current)) "fired")
                                                       ;           (scene-enemy current)
                                                        ;          (scene-mouse_x current)
                                                         ;         (scene-mouse_y current)
                                                          ;        (cons (make-posn (player-x (scene-player current)) (player-y (scene-player current)))
                                                           ;       empty))]
                                                     

                                                     [(string=? mouse-event "move")
                                                  (make-scene (make-player (player-image (scene-player current)) (player-x (scene-player current)) (player-y (scene-player current)) 
                                                     (cond [(and (< y (player-y (scene-player current))) (>= x (player-x (scene-player current))))
                                                                                             (- 90 (/ (* 90 (distance x y (player-x (scene-player current)) y))
                                                             (distance x y (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [(and (<= y (player-y (scene-player current))) (< x (player-x (scene-player current))))
                                                                                             (+ 90 (/ (* 90 (distance x y (player-x (scene-player current)) y))
                                                             (distance x y (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [(and (> y (player-y (scene-player current))) (<= x (player-x (scene-player current))))
                                                                                             (- 270 (/ (* 90 (distance x y (player-x (scene-player current)) y))
                                                             (distance x y (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [(and (> y (player-y (scene-player current))) (> x (player-x (scene-player current))))
                                                                                             (+ 270 (/ (* 90 (distance x y (player-x (scene-player current)) y))
                                                             (distance x y (player-x (scene-player current)) (player-y (scene-player current)))))]
                                                                                            [else (/ (* 90 (distance x y (player-x (scene-player current)) y))
                                                             (distance x y (player-x (scene-player current)) (player-y (scene-player current))))])
                                                     (player-action (scene-player current))
                                                     (player-status (scene-player current)))
                                                              (scene-projectile current)
                                                              (scene-enemy current)
                                                              (make-mouse_x x (mouse_x-clicked (scene-mouse_x current))) 
                                                              (make-mouse_y y (mouse_y-clicked (scene-mouse_y current)))
                                                              (scene-barrage current))]
                                                     
                                                                                                                                               
                                                     [else current]))
;;;to be implemented
;enemy motion
;(define enemy-init enemy)
;enemy-tick
;(define (enemy-tick enemy)
 ; (cond [(empty? enemy) empty]
  ;      [(cons? enemy) (cons-enemy-tick enemy)]))

;(define (cons-enemy-tick enemy)
 ; (cond [(enemy? (first enemy))
  ;       (cons (move-enemy (first enemy))
   ;            (enemy-tick (rest enemy)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;tick-handle: function for actions over time

;;projectile
(define (projectile-tick barrage current)
  (if (empty? barrage)
      empty
  (make-posn ;(+ (posn-x (first (scene-barrage current))) (/ (- (first (mouse_x-clicked (scene-mouse_x current))) (projectile-x (scene-projectile current))) 10))
             ;(+ (posn-y (first (scene-barrage current))) (/ (- (first (mouse_y-clicked (scene-mouse_y current))) (projectile-y (scene-projectile current))) 10)))
             (+ (posn-x barrage) (/ (- (first (mouse_x-clicked (scene-mouse_x current))) (projectile-x (scene-projectile current))) 10))
             (+ (posn-y barrage) (/ (- (first (mouse_y-clicked (scene-mouse_y current))) (projectile-y (scene-projectile current))) 10)))
  ))
;;barrage
(define (barrage-tick barrage current)
  (cond [(empty? barrage) empty]
        [(cons? barrage) (cons-barrage-tick barrage current)]))

(define (cons-barrage-tick barrage current)
  (cons (projectile-tick (first barrage) current)
        (barrage-tick (rest barrage) current)))
;;enemy
;(define (enemy-next-loc loc dir)
 ; (cond [(string=? dir "moving right")
  ;       (make-posn (+ (posn-x loc) enemy_speed) (posn-y loc))]
   ;     [(string=? dir "moving left")
    ;     (make-posn (- (posn-x loc) enemy_speed) (posn-y loc))]
     ;   [(string=? dir "moving down")
      ;   (make-posn (posn-x loc) (+ (posn-y loc) enemy_height y_interval))]))

;(define (enemy-next-dir loc dir)
 ; (cond [(string=? dir "moving right")
  ;       (if (adjacent-right-wall loc)
   ;          "moving down"
    ;         "moving right")]
     ;   [(string=? dir "moving left")
      ;   (if (adjacent-left-wall? loc)
       ;      "moving up"
        ;     "moving left")]
        ;[(string=? dir "moving down")
      ;   (cond [(adjacent-left-wall? loc) "moving right"]
       ;        [(adjacent-right-wall? loc) "moving left"]
        ;       [else "moving down"])]
        ;[(string=? dir "moving up")
         ;(cond [(adjacent-left-wall? loc) "moving right"]
          ;     [(adjacent-right-wall? loc) "moving left"]
           ;    [else "moving up"])]))

;(define (adjacent-left-wall? loc)
;(touch-left-wall? (posn-x (enemy-next-loc loc "left"))))

;(define (adjacent-right-wall? x y)
;(touch-left-wall? (posn-x (enemy-next-loc loc "right"))))

(define (tick-handle current) (cond [(or (and (string=? (player-action (scene-player current)) "moved left")
                                              (> (player-x (scene-player current)) 12)
                                              (false? (player_touch_enemy? current)))
                                         (> (player-x (scene-player current)) 488))
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (+ (player-x (scene-player current)) (/ (- 12 (player-x (scene-player current))) 30))
                                                              (player-y (scene-player current))
                                                              (player-direction (scene-player current))
                                                              (player-action (scene-player current))
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-barrage current))]
                                    [(or (and (string=? (player-action (scene-player current)) "moved right")
                                              (< (player-x (scene-player current)) 488)
                                              (false? (player_touch_enemy? current)))
                                         (< (player-x (scene-player current)) 12))
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (+ (player-x (scene-player current)) (/ (- 488 (player-x (scene-player current))) 30))
                                                              (player-y (scene-player current))
                                                              (player-direction (scene-player current))
                                                              (player-action (scene-player current))
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-barrage current))]
                                    [(or (and (string=? (player-action (scene-player current)) "moved up")
                                              (> (player-y (scene-player current)) 12)
                                              (false? (player_touch_enemy? current)))
                                         (> (player-y (scene-player current)) 488))
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (player-x (scene-player current))
                                                              (+ (player-y (scene-player current)) (/ (- 12 (player-y (scene-player current))) 30))
                                                              (player-direction (scene-player current))
                                                              (player-action (scene-player current))
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-barrage current))]
                                    [(and (string=? (player-action (scene-player current)) "moved down")
                                     (< (player-y (scene-player current)) 488)
                                     (false? (player_touch_enemy? current)))
                                     (make-scene (make-player (player-image (scene-player current))
                                                              (player-x (scene-player current))
                                                              (+ (player-y (scene-player current)) (/ (- 488 (player-y (scene-player current))) 30))
                                                              (player-direction (scene-player current))
                                                              (player-action (scene-player current))
                                                              (player-status (scene-player current)))
                                                 (scene-projectile current)
                                                 (scene-enemy current)
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (scene-barrage current))]
                                    
                                    
                                    [(player_touch_enemy? current)
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
                                                 (scene-barrage current))]
                                    
                                    [(projectile_hit_enemy? current)
                                     (make-scene (scene-player current)
                                                 (make-projectile 
                                                  (projectile-image (scene-projectile current))
                                                  (player-x (scene-player current))
                                                  (player-y (scene-player current))
                                                  (projectile-direction (scene-projectile current))
                                                  "stationary")
                                                 (make-enemy (cond [(= (enemy-status (scene-enemy current)) 150) 
                                                                    (scale 0.95 enemy_image_yellow)]
                                                                   [(= (enemy-status (scene-enemy current)) 100) 
                                                                    (scale 0.90 enemy_image_orange)]
                                                                   [(= (enemy-status (scene-enemy current)) 50) 
                                                                    (scale 0.85 enemy_image_red)]
                                                                   [(and (> (enemy-status (scene-enemy current)) 0) 
                                                                    (< (enemy-status (scene-enemy current)) 50)) 
                                                                    (scale 0.90 (enemy-image (scene-enemy current)))]
                                                                 [else (scale 1.05 (enemy-image (scene-enemy current)))])
                                                             (enemy-x (scene-enemy current))
                                                             (enemy-y (scene-enemy current))
                                                             (enemy-action (scene-enemy current))
                                                             (- (enemy-status (scene-enemy current)) 5))
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 ;(cons (make-posn (player-x (scene-player current)) (player-y (scene-player current))) empty))]
                                                 (cons (make-posn (projectile-x (scene-projectile current)) (projectile-y (scene-projectile current))) empty))]
                                                 ;(scene-barrage current))]
                                    
                                    [(projectile_hit_mouse? current)
                                     (make-scene (scene-player current)
                                                 (make-projectile 
                                                  (projectile-image (scene-projectile current))
                                                  (player-x (scene-player current))
                                                  (player-y (scene-player current))
                                                  (projectile-direction (scene-projectile current))
                                                  "stationary")
                                                (scene-enemy current)
                                                (scene-mouse_x current)
                                                (scene-mouse_y current)
                                                (scene-barrage current))]
                                    
                                    [(string=? (projectile-action (scene-projectile current)) "fired")
                                    (make-scene (scene-player current)
                                                (make-projectile 
                                                 (projectile-image (scene-projectile current))
                                                 (+(projectile-x (scene-projectile current)) (/ (- (first (mouse_x-clicked (scene-mouse_x current))) (projectile-x (scene-projectile current))) 1))
                                                 (+(projectile-y (scene-projectile current)) (/ (- (first (mouse_y-clicked (scene-mouse_y current))) (projectile-y (scene-projectile current))) 1))
                                                 (projectile-direction (scene-projectile current))
                                                (projectile-action (scene-projectile current)))
                                                (scene-enemy current)
                                                (scene-mouse_x current)
                                                (scene-mouse_y current)
                                                (barrage-tick (list (make-posn (projectile-x (scene-projectile current))
                                                                               (projectile-y (scene-projectile current))))current))]
                                    ;(barrage-tick (scene-barrage current) current))]
                                  
                                     [(= (+ 1 1) 2)
                                      (make-scene (scene-player current)
                                                 (scene-projectile current)
                                                 (make-enemy (rotate 10 (enemy-image (scene-enemy current)))
                                                             (cond [(< (enemy-x (scene-enemy current)) 500)
                                                                    (+ (enemy-x (scene-enemy current)) 1)]
                                                                   [(>= (enemy-x (scene-enemy current)) 500)
                                                                    0]
                                                                   )
                                                             (cond [(< (enemy-y (scene-enemy current)) 500)
                                                                    (+ (enemy-y (scene-enemy current)) 1)]
                                                                   [(>= (enemy-y (scene-enemy current)) 500)
                                                                    0]
                                                                   )
                                                             (enemy-action (scene-enemy current))
                                                             (enemy-status (scene-enemy current)))
                                                 (scene-mouse_x current)
                                                 (scene-mouse_y current)
                                                 (barrage-tick (scene-barrage current) current))]
                                                 ;(scene-barrage current))]
                                    [else current]))

;starter
(define (shooter) (big-bang current
                            (on-draw render-scene)
                            (on-mouse mouse-handle)
                            (on-key player_controls)
                            (on-tick tick-handle 1/60)))