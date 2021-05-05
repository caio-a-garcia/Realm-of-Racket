#lang racket
(require 2htdp/universe 2htdp/image lang/posn)

;;Tic Tac Toe
(define (start)
  (big-bang (initialize-board)
    (on-key select-and-play)
    (to-draw render)
    (stop-when end? render)))

(struct ttt (board player target)#:transparent)
(struct action (player position)#:transparent)

;;constants
(define SIZE 300)
(define SQUARE-SIZE (quotient SIZE 3.0))
(define ROW 3)
(define INITIAL-BOARD (list 0 0 0 0 0 0 0 0 0))
(define EMPTY-SQUARE (square (quotient SIZE 3.0) "outline" "white"))
(define X (place-image/align (text "X" (quotient SIZE 3.0) "black")
                             0 0 "left" "top"
                             EMPTY-SQUARE))
(define O (place-image/align (text "O" (quotient SIZE 3.0) "black")
                             0 0 "left" "top"
                             EMPTY-SQUARE))
(define TARGET (square (quotient SIZE 3.0) "outline" "yellow"))

;;Start main
  ;;Initialize
(define (initialize-board)
  (ttt INITIAL-BOARD "X" 0))

  ;;Start player interaction
(define (select-and-play w k)
  (cond
    [(and (equal? (current-target w) 0)(key=? "\r" k)) (choose w)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "down" k) (move-target w (+ ROW))]
    [(key=? "up" k) (move-target w (- ROW))]
    [(key=? "n" k) (initialize-board)]
    [else w]))

(define (current-target w)
  (list-ref (ttt-board w) (ttt-target w)))

(define (move-target w delta)
  (define target (ttt-target w))
  (define new (if (= (abs delta) 3)
                  (modulo (+ target delta) 9)
                  (+ (modulo (+ target delta) 3) (* 3 (quotient target 3)))))
  (ttt (ttt-board w) (ttt-player w) new))

(define (choose w)
  (define target (ttt-target w))
  (define board (ttt-board w))
  (define player (ttt-player w))
  (define new-board (list-set board target player))
  (ttt new-board (return-next-player player) target))
  ;;End player interaction

  ;;Start render
(define (render w)
  (define board (ttt-board w))
  (show-choice w
               (arrange
                (map pick-image board))))

(define (pick-image element)
  (cond [(equal? element "X") X]
        [(equal? element "O") O]
        [else EMPTY-SQUARE]))

(define (arrange elements)
  (cond
    [(empty? elements) empty-image]
    [(< (length elements) ROW) (apply beside elements)]
    [else (define r (apply beside (take elements ROW)))
          (above r (arrange (drop elements ROW)))]))

(define (show-choice w scene)
  (define target (ttt-target w))
  (define x (+ (* (modulo target 3) SQUARE-SIZE) (quotient SQUARE-SIZE 2.0)))
  (define y (+ (* (quotient target 3) SQUARE-SIZE) (quotient SQUARE-SIZE 2.0)))
  (place-image TARGET x y scene))
  
  ;;End render
                            
  ;;Start endgame
(define (end? w)
  (ormap evaluate-branch (get-branches w end-tree)))
  
(define (get-player w t)
  (list-ref (ttt-board w) t))
  
(define (get-branch w branch)
  (cond [(empty? branch) empty]
        [else (cons (get-player w (car branch))
                    (get-branch w (cdr branch)))]))

(define (get-branches w tree)
  (cond [(empty? tree) empty]
        [else (cons (get-branch w (car tree))
                    (get-branches w (cdr tree)))]))

(define (evaluate-branch branch)
  (and (equal? (car branch) (cadr branch))
       (equal? (car branch) (caddr branch))
       (not (equal? (car branch) 0))))

(define end-tree (list '(0 1 2) '(3 4 5) '(6 7 8)
                       '(0 3 6) '(1 4 7) '(2 5 8)
                       '(0 4 8) '(2 4 6)))
  ;;End endgame
;;End main

;;auxiliary functions
(define (return-next-player player)
  (cond [(string=? player "X") "O"]
        [else "X"]))