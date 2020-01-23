#lang racket/gui
(require "vectors.rkt" "params.rkt")
(provide boid% draw-boids update-boids remove-all-boids 1/dt BOIDS)


(define (zone? boid boid_cible);vector * vector -> number
  ;ici boid et boid_cible sont les positions des boids
  
  (cond ;La zone dans laquelle se trouve le boid cible 
    [(equal? boid boid_cible) 4];Si le boid_cible est le boid lui meme 
    [(<= (vect-sqrdist boid boid_cible) (sqr rd)) 1];Zone distanciation
    [(<= (vect-sqrdist boid boid_cible) (sqr ra)) 2];Zone alignement
    [(<= (vect-sqrdist boid boid_cible) (sqr rc)) 3];Zone cohésion
    [else 4]));Aucune zone


(define (zone_List boid BOIDS);boid object * List boid -> List of 4 List of boids
  ;Retourne 4 listes pour chaque zones contenant les boids qui s'y trouvent
  (define (iter boid BOIDS LD LA LC)
    (if (empty? BOIDS)
        (list LD LA LC)
        (let* [(boid_pos (send boid get-pos))
               (boid_cible_pos (send (car BOIDS) get-pos))
               (boid_cible (car BOIDS))
               (zone (zone? boid_pos boid_cible_pos))]

          (if (send boid can-see? boid_cible);si notre boid voit le boid_cible 
              (cond
                [(= zone 1) (iter boid (cdr BOIDS) (cons boid_cible LD) LA LC)]
                [(= zone 2) (iter boid (cdr BOIDS) LD (cons boid_cible LA) LC)]
                [(= zone 3) (iter boid (cdr BOIDS) LD LA (cons boid_cible LC))]
                [else (iter boid (cdr BOIDS) LD LA LC)])
              
              (iter boid (cdr BOIDS) LD LA LC)))));sinon on passe au suivant 
          
  (iter boid BOIDS '() '() '()))
          
;(define (zone_all? boid_pos BOIDS) ;boid object * List boid -> List number 
;  ;Retourne la liste des zones de chaque boid de BOIDS 
;  (define (iter boid_pos BOIDS ZONE-LIST)
;    (if (empty? BOIDS)
;        ZONE-LIST
;        (let [(boid_cible_pos (send (car BOIDS) get-pos))]
;          (iter boid_pos (cdr BOIDS) (cons (zone? boid_pos boid_cible_pos) ZONE-LIST)))))
;  (iter boid_pos BOIDS '()))

(define (rotate-cap wmax vit vbut); number * vector * vector -> vector
  ;Plafonne vbut selon la vitesse de rotation wmax 
  (let [(angle (angle-vects vit vbut))]
    (if (< wmax (abs (if (real? angle) angle 0)))
        (rotate-vect vit ((if (<= 0 angle) + -) wmax))
        vbut)))


(define (rotate-random wmax vit);angle * vect -> vect
  ;Effectue une rotation de + ou - un angle entre 0 et wmax sur vit
  (let [(angle (* (random) wmax))
        (signe (if (= (random 2) 0) + -))]
    (rotate-vect vit (signe angle))));vit de norme v0
  
 
;####################################################

(define (Liste-vitesse L_boids);List boids -> list vector 
  ;La liste des vecteur vitesse 
  (define (iter Lvit L_boids)
    (if (empty? L_boids)
        Lvit
        (iter (cons (send (car L_boids) get-speed) Lvit) (cdr L_boids))))
  (iter '() L_boids))


(define (Liste-pos L_boids);List boids -> list vector 
  ;La liste des positions 
  (define (iter Lpos L_boids)
    (if (empty? L_boids)
        Lpos
        (iter (cons (send (car L_boids) get-pos) Lpos) (cdr L_boids))))
  (iter '() L_boids))


;Distanciation

(define (wd boid_pos L_pos);vector * vector list -> vector 
  ;Calcule wd 
  (extern-prod (- 1) (vect-diff (vect-mean L_pos) boid_pos)))


(define (vbut-d v0 wd);number * vector -> vector 
  ;Le vecteur but lors d'une distanciation
  (define norme_wd (norm wd))
  (extern-prod v0 (vect (/ (x-coord wd) norme_wd) (/ (y-coord wd) norme_wd)))) ;On obtient vit de norme v0 


;Cohesion

(define (wc boid_pos L_pos);vector * vector list -> vector 
  ;Calcule wc
  (vect-diff (vect-mean L_pos) boid_pos))


(define (vbut-c v0 wc);number * vector -> vector 
  ;Le vecteur but lors d'une cohésion
  (define norme_wc (norm wc))
  (extern-prod v0 (vect (/ (x-coord wc) norme_wc) (/ (y-coord wc) norme_wc)))) ;On obtient vit de norme v0 


;Alignement

(define (wa L-vit);vector list -> vector
  ;Calcule wa 
  (vect-mean L-vit))


(define (vbut-a v0 wa);number * vector -> vector 
  ;Le vecteur but lors d'un alignement
  (define norme_wa (norm wa))
  (extern-prod v0 (vect (/ (x-coord wa) norme_wa) (/ (y-coord wa) norme_wa)))) ;On obtient vit de norme v0


(define (vbut-ac wa wc v0);vector * vector * number -> vector
  (let [(nwa (norm wa));Calcule vbut lors d'un alignement et cohesion
        (nwc (norm wc))]
    (extern-prod v0 ;On obtient vit de norme v0
                 (vect-sum
                  (vect (/ (x-coord wa) (* 2 nwa)) (/ (y-coord wa) (* 2 nwa)))
                  (vect (/ (x-coord wc) (* 2 nwc)) (/ (y-coord wc) (* 2 nwc)))
                  ))))

;La taille d'un boid :
(define Tboid 20)


;Les brushs :
(define G-brush (new brush% (color "green") (style 'solid)))
(define R-brush (new brush% (color "red") (style 'solid)))
(define Y-brush (new brush% (color "yellow") (style 'solid)))
(define B-brush (new brush% (color "blue") (style 'solid)))


;La liste des boids crees
(define BOIDS '())

;###############################################

;La classe des boids
(define boid%
  (class object%

    (init-field
     (vit (extern-prod v0 '(1 0)))
     (pos '(0 0))
     (boid-brush G-brush))

    (define/public (get-pos) ;recupere la position du boid
      pos)

    (define/public (get-speed) ;recupere la vitesse du boid
      vit)

    ;###############################

    (define/public (advance L H)
      (begin (desired-speed);Avance le boid selon son vecteur vitesse et dt 
             (set! pos (torify (canevas->math (vect-sum pos vit) L H) L H))))

    
    (define/public (draw dc);dessine le boid en utilisant le peintre dc
      (let [(p1 (make-object point% (x-coord pos) (+ (y-coord pos) (/ Tboid 2))))
            (p2 (make-object point% (+ (x-coord pos) (/ Tboid 2)) (- (y-coord pos) (/ Tboid 2))))
            (p3 (make-object point% (- (x-coord pos) (/ Tboid 2)) (- (y-coord pos) (/ Tboid 2))))]
        ;Pour changer la couleur du boid (le brush change dans desire-speed)
        (send dc set-brush boid-brush)
        (send dc draw-polygon (list p1 p2 p3))))

    
    (define/public (can-see? boid);renvoie vrai si peut voir boid
      (let [(v2 (vect-diff (send boid get-pos) pos))]
        (if (equal? v2 '(0 0))
            #t
            (let [(angle-vue (angle-vects vit v2))]
            (if (<= (abs (if (real? angle-vue) angle-vue 0))
                    (/ Ov 2)) #t #f)))))

    
    (define/public (desired-speed)
      ;Calcul le vecteur vbut
      (set! vit (extern-prod v0 (unitary vit)))
      (let* [(vbut '(0 0))
             (ZoneList (zone_List this BOIDS))
             (LD (car ZoneList));liste des boids dans la zone de distanciation
             (LA (cadr ZoneList));liste des boids dans la zone d'alignement
             (LC (caddr ZoneList))];liste des boids dans la zone de cohésion
        (cond
          [(not (empty? LD)); vbut = wd
           (set! vbut (vbut-d v0 (wd pos (Liste-pos LD) ) ))
           (set! boid-brush R-brush)]
       
          [(and (not (empty? LA)) (not (empty? LC)));vbut = wa * wc
           (set! vbut (vbut-ac (wa (Liste-vitesse LA)) (wc pos (Liste-pos LC)) v0))
           (set! boid-brush B-brush)]

          [(not (empty? LA));vbut = wa
           (set! vbut (vbut-a v0 (wa (Liste-vitesse LA))))
           (set! boid-brush B-brush)]

          [(not (empty? LC));vbut = wc 
           (set! vbut (vbut-c v0 (wc pos (Liste-pos LC))))
           (set! boid-brush Y-brush)]

          [else (set! vbut (rotate-random wmax vit))
                (set! boid-brush G-brush)]);vbut = random rotation de vit 

        (set! vit (rotate-cap wmax vit vbut))));Mise en place du cap de la vitesse de rotation 
        
    (set! BOIDS (cons this BOIDS))
    (super-new)))

;##############################################

;La fonction de dessin
(define (draw-boids dc) ;rajoute les boids sur le canvas
  (map (lambda(x) (send x draw dc)) BOIDS))

;La fonction d'actualisation
(define (update-boids L H); Effectue un pas dans la simulation
  (map (lambda (x) (send x advance L H)) BOIDS))

;utiliser surement (map (lambda (x) (send x advance)) BOIDS)

;La fonction reset
(define (remove-all-boids)
  (set! BOIDS empty))