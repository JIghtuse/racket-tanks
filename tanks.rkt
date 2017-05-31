;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tanks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A simple tank game

;; =================
;; Constants:
(define WIDTH 640)
(define HEIGHT WIDTH)
(define BASE (bitmap "base.png"))
(define BASE-X (/ WIDTH 2))
(define BASE-Y (- HEIGHT (/ (image-height BASE) 2)))


(define MTS (place-image BASE BASE-X BASE-Y
                         (empty-scene WIDTH HEIGHT "black")))

(define TANK-R (bitmap "tank.png"))
(define TANK-U (rotate 90 TANK-R))
(define TANK-L (rotate 90 TANK-U))
(define TANK-D (rotate 90 TANK-L))


(define BULLET-R (bitmap "bullet.png"))
(define BULLET-U (rotate 90 BULLET-R))
(define BULLET-L (rotate 90 BULLET-U))
(define BULLET-D (rotate 90 BULLET-L))

(define GAME-OVER (text "GAME OVER" 50 "red"))
(define TANK-SPEED 3)    ;pixel per tick
(define BULLET-SPEED 5)  ;pixel per tick
(define TANK-HALF (/ (image-width TANK-R) 2))

;; =================
;; Data definitions:

;; Direction is one of:
;;  - "up"
;;  - "down"
;;  - "right"
;;  - "left"
;; interp: is direction of object on scene

#;
(define (fn-for-direction d)
  (cond [(string=? "up" d) (...)]
        [(string=? "down" d) (...)]
        [(string=? "rigth" d) (...)]
        [(string=? "left" d) (...)]))

(define-struct tank (p d m))
;; Tank is (make-tank Posn Direction Boolean)
;; interp. a tank with p current position on scene, d current direction, m indicates whether tank is moving
;; example of Tank placed on scene (place-image TANK (- WIDTH TANK-HALF) TANK-HALF MTS)

(define TANK0 (make-tank (make-posn (- WIDTH TANK-HALF) TANK-HALF)
                         "down"
                         #false))
(define TANK1 (make-tank (make-posn (- WIDTH TANK-HALF) 50)
                         "up"
                         #true))
#;
(define (fn-for-tank t)
  (... (posn-x (tank-p t))
       (posn-y (tank-p t))
       (tank-d t)
       (tank-m t)))

(define-struct bullet (p d))
;; Bullet is (make-bullet Posn Direction)
;; interp. a bullet with p current position on scene, d current direction

(define BULLET1 (make-bullet (make-posn 320 320)
                             "down"))
(define BULLET2 (make-bullet (make-posn BASE-X BASE-Y)
                             "up"))
(define BULLET3 (make-bullet (make-posn BASE-X BASE-Y)
                             "up"))

#;
(define (fn-for-bullet t)
  (... (posn-x (bullet-p t))
       (posn-y (bullet-p t))
       (bullet-d t)))

;; BulletState is one of
;; - Bullet
;; - false
;; interp: BulletStat is Bullet position or missing bullet

(define BS0 false)
(define BS1 BULLET1)
(define BS2 BULLET2)

#;
(define (fn-for-bullet-state bs)
  (cond [(false? bs) (...)]
        [(bullet? bs) (... bs)]))

(define-struct world (t bs))
;; World (make-world Tank BulletState)
;; interp. is a game world

(define WORLD0 (make-world TANK0 BS0))
(define WORLD1 (make-world TANK1 BS1))
(define WORLD2 (make-world TANK1 BS0))
(define WORLD3 (make-world TANK0 BS1))
(define WORLD4 (make-world TANK0 BS2))

#;
(define (fn-for-world w)
  (... (world-tank w)
       (world-bullet-state w)))

;; =================
;; Functions:

;; World -> World
;; start the world with (main WORLD0)
;; 
(define (main w)
  (big-bang w                                       ; World
            (on-tick   next-world)                  ; World -> World
            (to-draw   draw-world)                  ; World -> Image
            (stop-when game-over? draw-game-over)   ; World -> Boolean
            (on-key    handle-key)))                ; World KeyEvent -> World

;; Tank -> Tank
;; produce the next tank
(check-expect (next-tank TANK0) TANK0)
(check-expect (next-tank TANK1) (make-tank (make-posn (- WIDTH TANK-HALF) (- 50 TANK-SPEED))
                                           "up"
                                           #true))

;(define (next-tank t) t)    ;stub

(define (next-tank t)
  (if (tank-m t)
      (make-tank (move-tank (tank-p t) (tank-d t) ) (tank-d t) (tank-m t))
      t))

;; Posn Direction -> Posn
;; produce Posn of Tank
(check-expect (move-tank (make-posn 5 10) "up")
              (make-posn 5 (- 10 TANK-SPEED)))
(check-expect (move-tank (make-posn 5 10) "down")
              (make-posn 5 (+ 10 TANK-SPEED)))
(check-expect (move-tank (make-posn 5 10) "left")
              (make-posn (- 5 TANK-SPEED) 10))
(check-expect (move-tank (make-posn 5 10) "right")
              (make-posn (+ 5 TANK-SPEED) 10))

;(define (move-tank p d) p)    ;stub 

(define (move-tank p d)
  (cond [(string=? "up" d)
         (make-posn (posn-x p) (- (posn-y p) TANK-SPEED))]
        [(string=? "down" d)
         (make-posn (posn-x p) (+ (posn-y p) TANK-SPEED))]
        [(string=? "right" d)
         (make-posn (+ (posn-x p) TANK-SPEED) (posn-y p))]
        [(string=? "left" d)
         (make-posn (- (posn-x p) TANK-SPEED) (posn-y p))]))

;; BulletState -> BulletState
;; produce the next BulletState
(check-expect (next-bullet-state BS0) BS0)
(check-expect (next-bullet-state BS1) (make-bullet (make-posn 320 (+ 320 BULLET-SPEED))
                                                   "down"))

;(define (next-bullet-state bs) bs)    ;stub

(define (next-bullet-state bs)
  (cond [(false? bs) bs]
        [(bullet? bs) (make-bullet (move-bullet (bullet-p bs) (bullet-d bs)) (bullet-d bs))]))

;; Bullet -> Boolean
;; produce false when bullet reaches scene border otherwise no changes

(define (bullet-border-reached? b) false)    ;stub

;; Posn Direction -> Posn
;; produce Posn of bullet
(check-expect (move-bullet (make-posn 5 10) "up")
              (make-posn 5 (- 10 BULLET-SPEED)))
(check-expect (move-bullet (make-posn 5 10) "down")
              (make-posn 5 (+ 10 BULLET-SPEED)))
(check-expect (move-bullet (make-posn 5 10) "left")
              (make-posn (- 5 BULLET-SPEED) 10))
(check-expect (move-bullet (make-posn 5 10) "right")
              (make-posn (+ 5 BULLET-SPEED) 10))

;(define (move-bullet p d) p)    ;stub 

(define (move-bullet p d)
  (cond [(string=? "up" d)
         (make-posn (posn-x p) (- (posn-y p) BULLET-SPEED))]
        [(string=? "down" d)
         (make-posn (posn-x p) (+ (posn-y p) BULLET-SPEED))]
        [(string=? "right" d)
         (make-posn (+ (posn-x p) BULLET-SPEED) (posn-y p))]
        [(string=? "left" d)
         (make-posn (- (posn-x p) BULLET-SPEED) (posn-y p))]))

;; World -> World
;; produce the next world
(check-expect (next-world WORLD0) (make-world (next-tank (world-t WORLD0)) (next-bullet-state (world-bs WORLD0))))
(check-expect (next-world WORLD1) (make-world (next-tank (world-t WORLD1)) (next-bullet-state (world-bs WORLD1))))
(check-expect (next-world WORLD2) (make-world (next-tank (world-t WORLD2)) (next-bullet-state (world-bs WORLD2))))
(check-expect (next-world WORLD3) (make-world (next-tank (world-t WORLD3)) (next-bullet-state (world-bs WORLD3)))) 

;(define (next-world w) w)    ;stub

(define (next-world w)
  (make-world
   (next-tank (world-t w))
   (next-bullet-state (world-bs w))))

;; World -> Image
;; render next world state
(check-expect (draw-world WORLD0) (draw-tank (world-t WORLD0)
                                             (draw-bullet-state (world-bs WORLD0)
                                                                MTS)))
(check-expect (draw-world WORLD1) (draw-tank (world-t WORLD1)
                                             (draw-bullet-state (world-bs WORLD1)
                                                                MTS)))

(check-expect (draw-world WORLD2) (draw-tank (world-t WORLD2)
                                             (draw-bullet-state (world-bs WORLD2)
                                                                MTS)))

;(define (draw-world w) empty-image)    ;stub

(define (draw-world w)
  (draw-tank (world-t w)
             (draw-bullet-state (world-bs w)
                                MTS)))


;; Tank Image -> Image
;; render tank on given image 
(check-expect (draw-tank TANK0 MTS) (place-image TANK-D (posn-x (tank-p TANK0)) (posn-y (tank-p TANK0)) MTS))
(check-expect (draw-tank TANK1 MTS) (place-image TANK-U (posn-x (tank-p TANK1)) (posn-y (tank-p TANK1)) MTS))

;(define (draw-tank t i) empty-image)   ;stub

(define (draw-tank t i)
  (place-image
   (cond [(string=? "up" (tank-d t)) TANK-U]
         [(string=? "down" (tank-d t)) TANK-D]
         [(string=? "right" (tank-d t)) TANK-R]
         [(string=? "left" (tank-d t)) TANK-L])
   (posn-x (tank-p t))           
   (posn-y (tank-p t))
   i))

;; BulletState Image -> Image
;; render BulletState on given image 
(check-expect (draw-bullet-state BS0 MTS) MTS)
(check-expect (draw-bullet-state BS1 MTS) (place-image BULLET-D (posn-x (bullet-p BS1)) (posn-y (bullet-p BS1)) MTS))

;(define (draw-bullet-state bs i) empty-image)   ;stub

(define (draw-bullet-state bs i)
  (cond [(false? bs) i]
        [(bullet? bs)
         (place-image
          (cond [(string=? "up" (bullet-d bs)) BULLET-U]
                [(string=? "down" (bullet-d bs)) BULLET-D]
                [(string=? "rigth" (bullet-d bs)) BULLET-R]
                [(string=? "left" (bullet-d bs)) BULLET-L])
          (posn-x (bullet-p bs))           
          (posn-y (bullet-p bs))
          i)]))

;; World -> Boolean
;; produce true if bullet hits base otherwise return false 
(check-expect (game-over? WORLD0) false)
(check-expect (game-over? WORLD4) true)

;(define (game-over? w) false)   ;stub

(define (game-over? w)
  (and (bullet? (world-bs w))
       (close-enough? (make-posn BASE-X BASE-Y) (bullet-p (world-bs w)))))

;; Posn Posn -> Boolean
;; produce true if points close enough
(check-expect (close-enough? (make-posn 0 0) (make-posn 1 1)) true)
(check-expect (close-enough? (make-posn 0 0) (make-posn 100 100)) false)

;(define (close-enough? a b) false)   ;stub

(define (close-enough? a b)
  (< (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
              (sqr (- (posn-y a) (posn-y b)))))
     (/ (image-height BASE) 2)))

;; World -> Image
;; render GAME OVER 

(define (draw-game-over w)
  (overlay GAME-OVER (draw-world w)))

;; World KeyEvent -> World
;; move tank on arrow keys fires bullet on spacebar
(check-expect (handle-key WORLD0 "e") WORLD0)
(check-expect (handle-key WORLD0 "up")
              (make-world
               (make-tank (tank-p (world-t WORLD0)) "up" true)
               (world-bs WORLD0)))
(check-expect (handle-key WORLD0 "down")
              (make-world
               (make-tank (tank-p (world-t WORLD0)) "down" true)
               (world-bs WORLD0)))
(check-expect (handle-key WORLD0 "left")
              (make-world
               (make-tank (tank-p (world-t WORLD0)) "left" true)
               (world-bs WORLD0)))
(check-expect (handle-key WORLD0 "right")
              (make-world
               (make-tank (tank-p (world-t WORLD0)) "right" true)
               (world-bs WORLD0)))
(check-expect (handle-key WORLD0 " ")
              (make-world
               (world-t WORLD0)
               (make-bullet (tank-p (world-t WORLD0)) (tank-d (world-t WORLD0)))))

(check-expect (handle-key WORLD1 "e") WORLD1)
(check-expect (handle-key WORLD1 "up")
              (make-world
               (make-tank (tank-p (world-t WORLD1)) "up" true)
               (world-bs WORLD1)))
(check-expect (handle-key WORLD1 "down")
              (make-world
               (make-tank (tank-p (world-t WORLD1)) "down" true)
               (world-bs WORLD1)))
(check-expect (handle-key WORLD1 "left")
              (make-world
               (make-tank (tank-p (world-t WORLD1)) "left" true)
               (world-bs WORLD1)))
(check-expect (handle-key WORLD1 "right")
              (make-world
               (make-tank (tank-p (world-t WORLD1)) "right" true)
               (world-bs WORLD1)))
(check-expect (handle-key WORLD1 " ")
              WORLD1)

;(define (handle-key w ke) w) ;stub
                                                            
(define (handle-key w ke)
  (cond [(key=? ke "up")
         (make-world (make-tank (tank-p (world-t w)) "up" true) (world-bs w))]
        [(key=? ke "down")
         (make-world (make-tank (tank-p (world-t w)) "down" true) (world-bs w))]
        [(key=? ke "left")
         (make-world (make-tank (tank-p (world-t w)) "left" true) (world-bs w))]
        [(key=? ke "right")
         (make-world (make-tank (tank-p (world-t w)) "right" true) (world-bs w))]
        [(key=? ke " ")
         (make-world
          (world-t w)
          (cond [(false? (world-bs w)) (make-bullet (tank-p (world-t w)) (tank-d (world-t w)))]
                [(bullet? (world-bs w)) (world-bs w)]))]
        [else w]))


