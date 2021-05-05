#lang racket
(struct vehicle (name wheels) #:transparent)
(define carro1 (vehicle 'caminhonete 4))
(define moto1 (vehicle 'hd 2))
(define bike1 (vehicle 'minha-bike 2))
(define outro (vehicle 'patins 2))
  

(struct vehicles (cars motorcycles bikes others) #:transparent)
(define home-cars
  (vehicles (list carro1 empty)
            (list moto1 empty)
            (list bike1 empty)
            (list outro empty)))