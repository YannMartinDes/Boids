#lang racket/gui
(require "boids.rkt")

(define myfont (make-object font% 13.0 'default)) ;La font des caractères dans les boutons

(define play #t);Utiliser pour le caractère du bouton pause/play

(define FRAME;La Frame qui contiendra le canvas et les boutons
  (new frame% (label "Projet de Base")))

(define HPANEL
  (new horizontal-panel%
       (parent FRAME)
       (alignment '(center top))))

(define VPANEL1 ;Le panneau vertical qui contient les boutons (il n'est pas étirable)
  (new vertical-panel%
       (parent HPANEL)
       (alignment '(center center))
       (stretchable-width #f)))

(define VPANEL2
  (new vertical-panel%
       (parent HPANEL)
       (alignment '(right center))))


;########################### BOUTONS #####################################


(define BOUTON+1 ;Le bouton pour ajouter un boid
  (new button%
       (parent VPANEL1)
       (label "+1")
       (font myfont)
       (min-width 50)
       (min-height 50)
       (callback (lambda (obj c)
                   (when play 
                     (new boid% ;Sa position est aléatoire entre 0 et la taille du canvas
                          (pos (list (random (L))
                                     (random (H))
                                     ))))))))

(define BOUTONPausePlay 
  (new button% ;Le bouton pour mettre en pause ou play 
       (parent VPANEL1)
       (label "⏸")
       (font myfont)
       (min-width 50)       
       (min-height 50)
       (callback (lambda(button evt)
                   (if play
                       (send BOUTONPausePlay set-label "►")
                       (send BOUTONPausePlay set-label "⏸"))
                   (set! play (not play))))))


(define BOUTONStop
  (new button% ;Le bouton stop qui retire tout les boids du canvas et du BOIDS
       (parent VPANEL1)
       (font myfont)
       (label "■")
       (min-width 50)
       (min-height 50)
       (callback (lambda (obj c)
                   (remove-all-boids)))))


(define main-canvas    ;Le canvas 
  (new canvas% 
       (parent VPANEL2)
       (min-width 500)
       (min-height 500)
       (paint-callback (lambda (c dc)
                         (update-boids (L) (H))
                         (draw-boids dc)))))


(define H (lambda () (send main-canvas get-height))) ;La hauteur du canvas
(define L (lambda () (send main-canvas get-width))) ;La largeur du canvas

(define (counter)
  ;La fonction parallèle qui refresh le canvas selon la frequence
  (when play
    (send main-canvas refresh-now)
    (sleep 1/dt))
  (counter))

(define T (thread counter))

(send FRAME show #t)