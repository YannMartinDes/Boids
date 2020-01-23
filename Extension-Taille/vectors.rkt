
#lang racket/gui

(provide
 vect x-coord y-coord sqr-norm norm extern-prod scalar-prod cross-prod
 unitary vect-sum vect-diff vect-mean torify angle-vects rotate-vect
 math->canevas canevas->math vect-sqrdist inlist?)

(define (vect x y) ;number*number -> vector
  ;Construit un vecteur 2D a partir de ses coordonnees 
  (list x y))  ;Representer par une liste a 2 elements

(define (x-coord v) ;vector -> number
  (car v)) ;Retourne la coordonnée x d'un vecteur

(define (y-coord v) ;vector -> number
  (cadr v));Retourne la coordonnée y d'un vecteur

(define (sqr-norm v) ;vector -> number
  (sqr (norm v))) ;retourne la norme au carre d'un vecteur

(define (norm v) ;vector -> number
  (sqrt (+ (sqr (x-coord v)) (sqr (y-coord v))))) ;la norme d'un vecteur

(define (extern-prod k v) ;number * vector -> vector
  (vect (* k (x-coord v)) (* k (y-coord v)))) ;homothetie d'un vecteur 

(define (scalar-prod v1 v2);vector * vector -> number
  ;le produit scalaire 
  (+ (* (x-coord v1) (x-coord v2)) (* (y-coord v1) (y-coord v2))))

(define (cross-prod v1 v2);vector * vector -> number
  ;le produit en croix de deux vecteurs 2D
  (- (* (x-coord v1) (y-coord v2)) (* (x-coord v2) (y-coord v1))))

(define (unitary v);vector -> vector
  ;le vecteur unitaire de v (de norme 1)
  (vect (/ (x-coord v) (norm v)) (/ (y-coord v) (norm v))))


(define (vect-sum . L) ;vector * ... * vector -> vector
  ;La somme des vecteur donnés en argument
  (vect (apply + (map x-coord L)) (apply + (map y-coord L))))
  
(define (vect-diff v1 v2) ;vector * vector -> vector
  ;La difference entre deux vecteur v1 et v2
  (vect (- (x-coord v1) (x-coord v2)) (- (y-coord v1) (y-coord v2))))

(define (vect-mean L) ;vector list -> vector
  (if (empty? L) ;Le barycentre d'une liste de vecteur 
      (error "Erreur la liste est vide") ;cas d'une liste vide 
      (let [(N (length L))]
        (vect (/ (apply + (map x-coord L)) N) (/ (apply + (map y-coord L)) N)))))

(define (torify v L H) ;vector * number * number -> vector
  ;renvoie le vecteur en coordonnees canonique {(x,y) E [-L/2;L/2]x[-H/2;H/2] }
  ;Avec L la largeur du tore et H sa hauteur
  
  (define (torify-val x L) ;number * number -> number 
    (if (and (<= x (/ L 2)) (>= x (- (/ L 2)))) ;retourne la valeur canonique de x
        x ;retourne x si dans l'intervalle [-L/2;L/2]
        (torify-val ((if (> x 0) - +) x L) L)))

  ;(define (torify-val2 x L) NE MARCHE PAS POUR (x = 2 et L = 4) 
  ;(modulo x ((if (even? (floor (/ x (/ L )))) + -) (/ L 2))))
  
  (math->canevas (vect (torify-val (x-coord v) L) (torify-val (y-coord v) H)) L H))

(define (angle-vects v1 v2);vector * vector -> number
  ;renvoie l'angle dans [-pi;pi] entre v1 et v2
  (let [(crossprod (cross-prod v1 v2))]
    (* (if (= 0 crossprod) 1 (/ crossprod (abs crossprod))) ;1 pour gerer la division par 0 
           (acos (/ (scalar-prod v1 v2) (* (norm v1) (norm v2)))))
       ;peu optimise
       ))

(define (rotate-vect v theta);vector * number -> vector
  ;effectue une rotation de theta (E [-pi,pi]) sur V
  (vect (- (* (x-coord v) (cos theta)) (* (y-coord v) (sin theta)))
        (+ (* (x-coord v) (sin theta)) (* (y-coord v) (cos theta)))))

(define (math->canevas v L H);vector * number * number -> vector 
  ;passe des coordonnees mathematiques aux coordonnees du canevas
  (vect (+ (x-coord v) (/ L 2)) (- (/ H 2) (y-coord v) )))

(define (canevas->math v L H);vector * number * number -> vector
  ;passe des coordonnees du canevas aux coordonnees mathematiques
  (vect (- (x-coord v) (/ L 2)) (- (/ H 2) (y-coord v) )))

(define (vect-sqrdist v1 v2);vector * vector -> number
  (sqr-norm (vect-diff v1 v2)));la distance au carré séparant v1 et v2

(define (inlist? el L) ;number * list -> boolean
  (cond ;retourne True si el est dans list sinon False
    [(empty? L) #f]
    [(= (car L) el) #t]
    [else (inlist? el (cdr L))]))
  
