#lang racket
(require 2htdp/universe 2htdp/image)

(define (start lower upper)
  (big-bang (interval lower upper)
    (on-key deal-with-guess)
    (to-draw render)
    (stop-when single? render-last-scene)))

(struct interval (small big))

(define counter 0)

(define TEXT-SIZE 25)
(define TEXT-X 20)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 400)
(define WIDTH 600)
(define HEIGHT 400)
(define SIZE 50)

(define HELP-TEXT
  (text "up for larger numbers, down for smaller ones"
        TEXT-SIZE
        "blue"))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))
(define COLOR "red")

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))


(define (smaller w)
  (set! counter (+ counter 1))
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

(define (bigger w)
  (set! counter (+ counter 1))
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (render w)
  (overlay (text (score-string w) SIZE COLOR) MT-SC))

(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (score-string w)
  (define this-guess (number->string (guess w)))
  (string-append "Current Guess: " this-guess " Tries: " (number->string counter)))