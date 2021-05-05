#lang racket
(require 2htdp/universe 2htdp/image lang/posn)

(define (start-snake)
  (big-bang (pit (snake "right" (list (make-posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo))
                 (list (new-obstacle)
                       (new-obstacle)
                       (new-obstacle)
                       (new-obstacle)))
    (on-tick next-pit TICK-TIME)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))


;;Main structures
(struct pit (snake goos obstacles))
(struct snake (direction segments))
(struct goo (location expire type))
(struct obstacle (location expire))

;;Constants
(define TICK-TIME 0.2)
(define SIZE 650)
(define OBSTACLE-IMAGE (square 50 "solid" "black"))
(define GOO-IMAGE (circle 20 "solid" "blue"))
(define BIG-GOO-IMAGE (circle 20 "solid" "green"))
(define EXPIRATION-TIME 10)
(define OBSTACLE-EXPIRATION-TIME 20)
(define MOUNT-SCENE (empty-scene SIZE SIZE))
(define SEGMENT-IMAGE (square 50 "solid" "green"))
(define SEGMENT-SIZE 49)
(define HEAD-UP-IMAGE (square 50 "solid" "green"))
(define HEAD-DOWN-IMAGE (square 50 "solid" "green"))
(define HEAD-RIGHT-IMAGE (square 50 "solid" "green"))
(define HEAD-LEFT-IMAGE (square 50 "solid" "green"))
(define ENDGAME-TEXT-SIZE 50)
(define ENDGAME-TEXT "Game Over\nScore: ")


;;Start clock-tick
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define obstacles (pit-obstacles w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (if (string=? (goo-type goo-to-eat) "big")
          (pit (grow (grow snake)) (age-goo (eat goos goo-to-eat)) (handle-obstacles obstacles))
          (pit (grow snake) (age-goo (eat goos goo-to-eat)) (handle-obstacles obstacles)))
      (pit (slither snake) (age-goo goos) (handle-obstacles obstacles))))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (close? s g)
  (posn=? s (goo-location g)))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (grow sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (snake-segments sn))))

(define (slither sn)
  (snake (snake-direction sn)
         (cons (next-head sn) (all-but-last (snake-segments sn)))))

(define (all-but-last segments)
  (cond [(empty? (rest segments)) empty]
        [else (cons (first segments) (all-but-last (rest segments)))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define direction (snake-direction sn))
  (cond [(string=? direction "up") (posn-move head 0 -1)]
        [(string=? direction "down") (posn-move head 0 1)]
        [(string=? direction "right") (posn-move head 1 0)]
        [(string=? direction "left") (posn-move head -1 0)]))

(define (posn-move p dx dy)
  (make-posn (+ (posn-x p) dx)
             (+ (posn-y p) dy)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond [(empty? goos) (empty)]
        [else (cons (decay (first goos)) (rest goos))]))


(define (renew goos)
  (define (renew-check goos)
    (cond [(empty? goos) empty]
           [(rotten? (first goos))
            (cond [(< (random) 0.9)
                   (cons (fresh-goo) (renew-check (rest goos)))]
                  [else (renew-check (rest goos))])]
           [else
            (cons (first goos) (renew-check (rest goos)))]))
  
  (if (< (random) 0.01)
      (cons (fresh-goo) (renew-check goos))
      (renew-check goos)))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (fresh-goo)
  (goo (make-posn (add1 (random (sub1 (quotient SIZE SEGMENT-SIZE))))
                  (add1 (random (sub1 (quotient SIZE SEGMENT-SIZE)))))
       EXPIRATION-TIME
       (if (< (random) 0.2)
           "big"
           "small")))

(define (handle-obstacles obstacles)
  (cond [(empty? obstacles) empty]
        [else (if (broken? (first obstacles))
                  (cons (new-obstacle) (handle-obstacles (rest obstacles)))
                  (cons (age-obstacle (first obstacles)) (handle-obstacles (rest obstacles))))]))

(define (new-obstacle)
  (obstacle (make-posn (add1 (random (sub1 (quotient SIZE SEGMENT-SIZE))))
                  (add1 (random (sub1 (quotient SIZE SEGMENT-SIZE)))))
       OBSTACLE-EXPIRATION-TIME))

(define (broken? o)
  (zero? (obstacle-expire o)))

(define (age-obstacle o)
  (obstacle (obstacle-location o) (- (obstacle-expire o) 1)))
;;End clock-tick

;;Start key-events
(define (direct-snake w key)
  (cond [(direction? key) (world-change-direction w key)]
        [else w]))

(define (direction? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "right")
      (key=? x "left")))

(define (world-change-direction w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-direction? (snake-direction the-snake) d)
              (cons? (rest (snake-segments the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-direction the-snake d) (pit-goos w) (pit-obstacles w))]))

(define (opposite-direction? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))
;;End key-events

;;Start rendering
(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w)
                               (obstacle-list+scene (pit-obstacles w) MOUNT-SCENE))))

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEGMENT-IMAGE scene))
  (define direction (snake-direction snake))
  (img+scene (snake-head snake)
             (cond [(string=? "up" direction) HEAD-UP-IMAGE]
                   [(string=? "down" direction) HEAD-DOWN-IMAGE]
                   [(string=? "right" direction) HEAD-RIGHT-IMAGE]
                   [(string=? "left" direction) HEAD-LEFT-IMAGE])
             snake-body-scene))

(define (img-list+scene posns images scene)
  (cond [(empty? posns) scene]
        [else (cond [(list? images)
                     (img+scene
                         (first posns)
                         (first images)
                         (img-list+scene (rest posns) (rest images) scene))]
                    [else (img+scene
                         (first posns)
                         images
                         (img-list+scene (rest posns) images scene))])]))

(define (img+scene posn image scene)
  (place-image image
               (* (posn-x posn) SEGMENT-SIZE)
               (* (posn-y posn) SEGMENT-SIZE)
               scene))

(define (goo-list+scene goos scene)
  (define (get-imgs-from-goo goos)
    (cond [(empty? goos) empty]
          [else (if (string=? (goo-type (first goos)) "big")
                    (cons BIG-GOO-IMAGE
                          (get-imgs-from-goo (rest goos)))
                     (cons GOO-IMAGE
                          (get-imgs-from-goo (rest goos))))]))
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-location (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) (get-imgs-from-goo goos) scene))

(define (obstacle-list+scene obstacles scene)
  (define (get-posns-from-obstacle obstacles)
    (cond [(empty? obstacles) empty]
          [else (cons (obstacle-location (first obstacles))
                      (get-posns-from-obstacle (rest obstacles)))]))
  (img-list+scene (get-posns-from-obstacle obstacles) OBSTACLE-IMAGE scene))
;;End rendering

;;Start endgame
(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake) (obstacle-colliding? snake w)))

(define (render-end w)
  (define sn (pit-snake w))
  (define txt (string-append ENDGAME-TEXT (number->string (length (snake-segments sn)))))
  (overlay (text txt ENDGAME-TEXT-SIZE "black")
           MOUNT-SCENE)) ;;(render-snake-world w)  <- from book

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (obstacle-colliding? snake w)
  (cons? (member (snake-head snake) (pit-obstacles w))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (>= 0 x) (>= x (quotient SIZE SEGMENT-SIZE))
      (>= 0 y) (>= y (quotient SIZE SEGMENT-SIZE))))
;;End endgame

;;Start auxiliary functions
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (snake-head sn)
  (first (snake-segments sn)))

(define (snake-body sn)
  (rest (snake-segments sn)))

(define (snake-tail sn)
  (last (snake-segments sn)))

(define (snake-change-direction sn d)
  (snake d (snake-segments sn)))

(define (decay g)
  (goo (goo-location g) (- (goo-expire g) 1) (goo-type g)))
;;End auxiliary functions