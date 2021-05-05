#lang racket
(require 2htdp/universe 2htdp/image lang/posn)

(define (start)
  (big-bang (initialize-orc-world)
    (on-key player-acts-on-monsters)
    (to-draw render-orc-battle)
    (stop-when end-of-orc-battle? render-end)))

;;structs
(struct orc-world (player monsters attack# target) #:mutable #:transparent)
(struct player (health agility strength) #:mutable #:transparent)

(struct monster (image [health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)

;;constants
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)
(define HEALING 5)
(define ATTACKS# 3)
(define STAB-DAMAGE 10)
(define FLAIL-DAMAGE 10)
(define MONSTERS-PER-ROW 3)
(define MONSTER# 6)
(define MONSTER-HEALTH0 15)
(define CLUB-STRENGTH 7)
(define SLIMINESS 3)
(define BRIGAND-HEALTH-DAMAGE 7)
(define BRIGAND-AGILITY-DAMAGE 7)
(define BRIGAND-STRENGTH-DAMAGE 7)


(define PLAYER-IMAGE (circle 30 "solid" "black"))
(define ORC-IMAGE (square 40 "solid" "green"))
(define HYDRA-IMAGE (circle 28 "solid" "blue"))
(define SLIME-IMAGE (circle 14 "solid" "green"))
(define BRIGAND-IMAGE (square 60 "solid" "black"))
(define TARGET (circle 34 "solid" "yellow"))
(define V-SPACER (rectangle 0 20 "solid" "white"))
(define H-SPACER (rectangle 20 0 "solid" "white"))
(define REMAINING "Attacks remainig: ")
(define INSTRUCTION-TEXT-SIZE 15)
(define ATTACK-COLOR "red")
(define INSTRUCTION-TEXT (text "s stab \nh heal\na recover agility\no recover strength \nf flail \ne end turn \nn new world \n arrows select target" 15 "black"))
(define DEAD-TEXT (text "DEAD" 20 "black"))
(define STRENGTH-COLOR "blue")
(define AGILITY "AGILITY")
(define AGILITY-COLOR "green")
(define HEALTH "HEALTH")
(define HEALTH-COLOR "red")
(define STRENGTH "STRENGTH")
(define MONSTER-COLOR "red")
(define MESSAGES-SIZE 15)
(define MESSAGE-COLOR "black")
(define LOSE "Your Dad")
(define WIN "You Win")
(define HEALTH-BAR-WIDTH 40)
(define HEALTH-BAR-HEIGHT 4)
(define LABEL-SIZE (quotient HEALTH-BAR-WIDTH 4))


;;Start main functions
  ;;Start Initialize

(define (initialize-orc-world)
  ;(print "Initialize")
  (define player0 (initialize-player))
  (define monsters0 (initialize-monsters))
  (orc-world player0 monsters0 (random-number-of-attacks player0) 0))

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

(define (initialize-monsters)
  (build-list
    MONSTER#
    (lambda (_)
      (define health (random+ MONSTER-HEALTH0))
      (case (random 4)
        [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
        [(1) (hydra HYDRA-IMAGE health)]
        [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
        [(3) (brigand BRIGAND-IMAGE health)]))))
  ;;End Initialize

  ;;Start player's move
(define (player-acts-on-monsters w k)
  (cond
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "a" k) (speed-up w)]
    [(key=? "o" k) (strengthen w)]
    [(key=? "f" k) (flail w)]
    [(key=? "e" k) (end-turn w)]
    [(key=? "n" k) (initialize-orc-world)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "down" k) (move-target w (+ MONSTERS-PER-ROW))]
    [(key=? "up" k) (move-target w (- MONSTERS-PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

(define (end-turn w)
  (set-orc-world-attack#! w 0))

(define (heal w)
  (decrease-attack#! w)
  (player-health+! (orc-world-player w) HEALING))

(define (speed-up w)
  (decrease-attack#! w)
  (player-agility+! (orc-world-player w) HEALING))

(define (strengthen w)
  (decrease-attack#! w)
  (player-strength+! (orc-world-player w) HEALING))

(define (stab w)
  (decrease-attack#! w)
  (define target
    (list-ref (orc-world-monsters w) (orc-world-target w)))
  (define damage
    (random-quotient (player-strength (orc-world-player w))
                     STAB-DAMAGE))
  (damage-monster target damage))

(define (flail w)
  (decrease-attack#! w)
  (define target (current-target w))
  (define alive (filter monster-alive? (orc-world-monsters w)))
  (define pick#
    (min
     (random-quotient (player-strength (orc-world-player w))
                      FLAIL-DAMAGE)
     (length alive)))
  (define getem (cons target (take alive pick#)))
  (for-each (lambda(m) (damage-monster m 1)) getem))

(define (decrease-attack#! w)
  (set-orc-world-attack#! w (sub1 (orc-world-attack# w))))

(define (damage-monster m delta)
  (set-monster-health! m (interval- (monster-health m) delta)))

(define (current-target w)
  (list-ref (orc-world-monsters w) (orc-world-target w)))

(define (move-target w delta)
  (define new (+ (orc-world-target w) delta))
  (set-orc-world-target! w (modulo new MONSTER#)))
  ;;End player's move

  ;;Start monsters' move
(define (give-monster-turn-if-attack#=0 w)
  (when (zero? (orc-world-attack# w))
    (define player (orc-world-player w))
    (all-monsters-attack-player player (orc-world-monsters w))
    (set-orc-world-attack#! w (random-number-of-attacks player))))

(define (all-monsters-attack-player player monsters)
  (define (one-monster-attacks-player m)
    (cond
      [(orc? m)
       (player-health+! player (random- (orc-club m)))]
      [(hydra? m)
       (player-health+! player (random- (monster-health m)))]
      [(slime? m)
       (player-health+! player -1)
      (player-agility+! player
                       (random- (slime-sliminess m)))]
      [(brigand? m)
       (case (random 3)
         [(0) (player-health+! player (random- BRIGAND-HEALTH-DAMAGE))]
         [(1) (player-agility+! player (random- BRIGAND-AGILITY-DAMAGE))]
         [(2) (player-strength+! player (random- BRIGAND-STRENGTH-DAMAGE))])]))
  (define live-monsters (filter monster-alive? monsters))
  (for-each one-monster-attacks-player live-monsters))
  ;;End monsters' move

  ;;Start render
(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

(define (render-orc-world w t additional-text)
  (define i-player (render-player (orc-world-player w)))
  (define i-monsters (render-monsters (orc-world-monsters w) t))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (above i-monsters
                        V-SPACER V-SPACER V-SPACER
                        additional-text)
                  H-SPACER)
          V-SPACER))

(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT))

(define (render-player p)
  (define strength (player-strength p))
  (define agility (player-agility p))
  (define health (player-health p))
  (above/align
    "left"
    (status-bar strength MAX-STRENGTH STRENGTH-COLOR STRENGTH)
    (text (number->string strength) 10 "black")
    V-SPACER
    (status-bar agility MAX-AGILITY AGILITY-COLOR AGILITY)
    (text (number->string agility) 10 "black")
    V-SPACER
    (status-bar health MAX-HEALTH HEALTH-COLOR HEALTH)
    (text (number->string health) 10 "black")
    V-SPACER V-SPACER V-SPACER
    PLAYER-IMAGE))


(define (render-monsters monsters with-target)
  (define target
    (if (number? with-target)
        (list-ref monsters with-target)
        'a-silly-symbol-that-cannot-be-equal-to-an-orc))

  (define (render-one-monster m)
    (define image
      (if (eq? m target)
          (overlay (monster-image m) TARGET)
          (monster-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (= health 0)
          (overlay DEAD-TEXT (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))

  (arrange (map render-one-monster monsters)))


(define (arrange monsters)
  (cond
    [(empty? monsters) empty-image]
    [(< (length monsters) MONSTERS-PER-ROW) (apply beside monsters)]
    [else (define r (apply beside (take monsters MONSTERS-PER-ROW)))
          (above r (arrange (drop monsters MONSTERS-PER-ROW)))]))

(define (message str)
  (text str MESSAGES-SIZE MESSAGE-COLOR))
  ;;End render

  ;;Start endgame
(define (render-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))

(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (win? w)
  (all-dead? (orc-world-monsters w)))

(define (lose? w)
  (player-dead? (orc-world-player w)))

(define (player-dead? p)
  (or (= (player-health p) 0)
      (= (player-agility p) 0)
      (= (player-strength p) 0)))

(define (all-dead? monsters)
  (not (ormap monster-alive? monsters)))

(define (monster-alive? m)
  (> (monster-health m) 0))
  ;;End endgame
;;End main functions

;;Start stat ajust 
(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define player-health+!
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-agility+!
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define player-strength+!
  (player-update! set-player-strength! player-strength MAX-STRENGTH))
;;End stat ajust


;;auxiliary functions
(define (status-bar current-value max-value color label)
  (define width (* (/ current-value max-value) HEALTH-BAR-WIDTH))
  (define fill (rectangle width HEALTH-BAR-HEIGHT 'solid color))
  (define box (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGHT 'outline "black"))
  (define bar (overlay/align "left" "top" fill box))
  (beside bar H-SPACER (text label LABEL-SIZE color)))

(define (interval+ x y mx)
  (define total (+ x y))
  (cond [(> total mx) mx]
        [(< total 0) 0]
        [else total]))

(define (interval- x y)
  (define total (- x y))
  (if (< total 0)
      0
      total))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random+ n)
  (add1 (random n)))

(define (random- n)
  (* -1 (add1 (random n))))

