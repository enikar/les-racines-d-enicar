;; 2020/07/12
;; Remake de ma racine carrée en common lisp.
;; Version itérative, qui utilise loop.

;; Usage :
;; > (load "lracine")
;; > (lrac:racine 2 20)
;; Ou de depuis slime :
;; M-x slime (lance sbcl)
;; C-c C-l  RET (charge le fichier)
;; Puis depuis sbcl :
;; CL-USER> (lrac:racine 3 20)

;; Ou depuis le shell :
;; $ sbcl --noinform --load lracine.lisp --eval '(lrac:main)' --quit 3 50

;; Pour faire un exécutable :
;; $ sbcl --noinform --load lracine.lisp --eval "(save-lisp-and-die \"lracine\" :toplevel #'lrac:main :executable T :compression T)"

;; ou utiliser make-lracine.lisp qui automatise la création de l'exécutable.

;; TODO: faire une structure env qui contiendrait les constante du calculs
;;       Plutôt que d'utiliser des variables globales.
;; TODO: Utiliser le système de condition pour ne pas se servir de exit
;;       qui n'est pas portable.

(defpackage :lrac
  (:use :common-lisp)
  (:export
   #:def-base
   #:racine
   #:rac
   #:sqrt-floor
   #:main))

;; for command-line-arguments
(require 'uiop)

(in-package :lrac)


;; Peut-être serait-il possible de calculer automatiquement les valeurs
;; base-racine et carre-base en fonction  de exp-base lors de la création
;; d'un env
;; (defstruct env
;;   exp-base
;;   base-racine
;;   carre-base)

(defvar *exp-base* 0)
(defvar *base-racine* 0)
(defvar *carre-base* 0)

(defun def-base (&optional base)
  (if base
      (progn (setq *exp-base* base)
             (setq *base-racine* (expt 10 base))
             (setq *carre-base* (expt *base-racine* 2))))
  *exp-base*)

(def-base 10)

(defmacro quotient (a b)
  `(floor ,a ,b))

(defmacro mean (a b)
  `(floor (+ ,a ,b) 2))

;; (defun sqrt-floor (nb)
;;   "Compute the integer part of square root of nb."
;;   (if (< nb 2)
;;       nb
;;       (loop
;;         for test = T  then (> (* med med) nb)
;;         for inf = 1  then (if test inf med)
;;         for sup = nb then (if test med sup)
;;         for med = (mean inf sup)
;;         when (= (1+ inf)  sup)
;;           return inf)))
;; Recursive version
(defun sqrt-floor (nb)
  "Compute the integer part of square root of nb."
  (labels ((bissection (inf sup)
             (if (= (1+ inf) sup)
                 inf
                 (let ((med (mean inf sup)))
                   (if (> (* med med) nb)
                       (bissection inf med)
                       (bissection med sup))))))
    (if (< nb 2)
        nb
        (bissection 1 nb))))

(defun newton (u v)
  "Compute the next decimal packet."
  (flet ((next-k (k)
           (quotient (+ (* k k) v)
                     (+ (* 2 k) u))))
    (loop
      for k = (1+ (quotient v u))
        then kplus
      for kplus = (next-k k)
      when (or (= k kplus)
               (and (zerop kplus) (= 1 k)))
        return kplus)))

(defun iterations (p0)
  "Compute the number of iterations"
  (let ((p (quotient p0 *exp-base*)))
    (if (zerop (mod p0 *exp-base*))
        p
        (1+ p))))

(defun rac (nb p0)
  "Compute a list that consists of integer part and decimal packets."
  (let ((k0 (sqrt-floor nb)))
    (if (or (<= p0 0) (< nb 0) (= (* k0 k0) nb))
        (list k0)
        (cons
         k0
         (loop
           for v = (* *carre-base* (- nb (* k0 k0)))
             then (* *carre-base* (- v (* (+ u k) k)))
           for u = (* 2 *base-racine* k0)
             then (* *base-racine* (+ u (* 2 k)))
           for k = (newton u v)
           repeat (iterations p0) collect k)))))

(defun conv-frac (x)
  (let* ((s (format NIL "~D" x))
         (delta (- *exp-base* (length s))))
   (if (zerop delta)
       s
       (let ((y (make-sequence 'string
                               delta
                               :initial-element #\0) ))
         (concatenate 'string y s)))))

(defun racine (n p)
  "Print the square root of the positive integer n with at least p decimals."
  (destructuring-bind (floor &rest fracs) (rac n p)
   (apply
    #'concatenate
    (cons 'string
      (cons (format nil "~D" floor)
          (if fracs
              (cons "." (mapcar #'conv-frac fracs))
              NIL))))))

(defun usage ()
  "Print usage and exit"
  (let ((prog-name (car sb-ext:*posix-argv*)))
    (format T "~&Usage: ~A nb [precision [granularity]]~%" prog-name)
    (sb-ext:exit :code 1)))

(defun convert (s)
  "Convert a string to an integer. Check if argument is positive"
  (let ((n (ignore-errors (parse-integer s))))
    (if (or (not (integerp n)) (< n 0))
        (progn
          (format T "~&Numbers must be positive integers~%")
          (usage))
        n)))

(defun put-str-ln (x)
  (format T "~&~A~%" x))

(defun main ()
  "The entry point of the program"
  (let* ((args (uiop:command-line-arguments))
         (n (length args)))
    (case n
      ((1) (let ((nb (convert (car args))))
             (put-str-ln (racine nb 30))))
      ((2) (let ((nb (convert (car args)))
                 (p (convert (cadr args))))
             (put-str-ln (racine nb p))))
      ((3) (let ((nb (convert (car args)))
                 (p (convert (cadr args)))
                 (g (convert (caddr args))))
             (progn
               (when (<= g 0) (usage))
               (def-base g)
               (put-str-ln (racine nb p)))))
      (OTHERWISE (usage)))))
