#!/usr/bin/guile \
-e main -s
!#
;; usage avec guile :
;; $ guile -e main racine.scm 3 50

;; Depuis emacs avec geiser :
;; M-x run-geiser  pour démarrer guile
;; Puis C-c C-l racine.scm RET
;; Après on peut jouer avec depuis l'interprêteur :
;; scheme@(guile-user)> (racine 10 30)

;; TODO: utiliser  (ice-9 match)
;; Ça permetrait de simpifier un peu (surtout dans main)

(define *exp-base* 0)
(define *base-racine* 0)
(define *carre-base* 0)

(define def-base 
  (lambda b
    "Set constants of the calculus"
    (if (not (null? b))
        (let ((base (car b)))
          (begin
            (set! *base-racine* (expt 10 base))
            (set! *carre-base* (expt 10 (* base 2)))
            (set! *exp-base* base))))
  *exp-base*))

(def-base 10) 

(define-inlinable (mean a b)
  "Compute the floor of average of two integer"
  (quotient (+ a b) 2))

(define (sqrt-floor nb)
  "Compute the square root floor of an integer"
    (if (< nb 2)  
         nb
        (let loop ((inf 1)
                   (sup nb))
          (if (= inf (- sup 1))
              inf
              (let ((med (mean inf sup)))
                (if (> (* med med) nb)
                    (loop inf med)
                    (loop med sup)))))))

(define (iterations p)
  "Compute the number of iterations"
  (if (zero? (modulo p *exp-base*))
      (- (quotient p *exp-base*) 1)
      (quotient p *exp-base*)))

(define (newton u v)
  "Compute the next decimal packet"
  (let loop ((k (+ 1 (quotient v u))))
    (let ((kplus (quotient (+ (* k k) v)
                          (+ (* 2 k) u))))
      (if (= k kplus)
          k
          (loop kplus)))))

(define (rac nb p0)
  "Compute the list of floor and decimal packet of the square root"
  (let ((k0 (sqrt-floor nb)))
    (if (or (zero? p0) (= (* k0 k0) nb))
        (list k0)
        (cons k0
         (let loop ((p (iterations p0))
                    (u (* 2 *base-racine* k0))
                    (v (* *carre-base* (- nb (* k0 k0)))))
           (let ((k (newton u v)))
             (if (zero? p)
                 (list k)
                 (cons k (loop
                          (- p 1)
                          (* *base-racine* (+ u (* 2 k)))
                          (* *carre-base* (- v (* (+ u k) k))))))))))))

(define (conv-frac n)
  "Convert a number to a string filling with zero to fit *exp⁻base* character"
  (let* ((s (number->string n))
         (delta (- *exp-base* (string-length s))))
    (if (zero? delta)
        s
        (string-append (make-string delta #\0) s))))


(define (racine nb p)
  "Return the square root as a string"
  (let* ((ls (rac nb p))
         (floor (car ls))
         (fracs (cdr ls)))
    (apply
     string-append
     (cons
      (number->string floor)
      (if (null? fracs)
          '()
          (cons "." (map conv-frac fracs)))))))


(define (usage prog) 
  "Display usage then exit"
  (display "usage : ")
  (display prog)
  (display " nb [précision [granularité]]")
  (newline)
  (exit 1))

(define (conversion str)
  "Convert a string to an integer. Check if argument is positive"
    (let ((n (string->number str)))
      (if (or (not (integer? n)) (< n 0))
          (begin
            (display "Les nombres doivent être des entiers positifs")
            (newline)
            (exit 1))
          n)))
          
(define (print x)
  "Display an object follow by a new line"
  (begin
    (display x)
    (newline)))

(define (main arguments)
  "The entry point of the program"
  (let* ((args (cdr arguments))
         (prog (car arguments))
         (n (length args)))
      (case n 
        ((1) (let ((nb (conversion (car args))))
               (print (racine nb 30))))
        ((2) (let ((nb (conversion (car args)))
                   (p (conversion (cadr args))))
               (print (racine nb p))))
        ((3) (let ((nb (conversion (car args)))
                   (p (conversion (cadr args)))
                   (g (conversion (caddr args))))
               (begin
                 (if (<= g 0) (usage prog))
                 (def-base g)
                 (print (racine nb p)))))
        (else (usage prog)))))
