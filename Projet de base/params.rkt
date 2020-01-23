#lang racket/gui

;On envoie les donnees au programme utilisant ce module. 
(provide Ov rd ra rc v0 wmax 1/dt)


;###########Initialisation###############

(define Ov (/ pi 2));Angle de vue
(define rd 20);Zone de distanciation
(define ra 60);Zone d'alignement
(define rc 100);Zone de cohesion
(define v0 20);Vitesse 
(define wmax (/ pi 4));Vitesse de la rotation
(define 1/dt 0.10);Frequence 

;############################GRAPHIQUE ########################

(define FRAME ;La fenetre des parametres 
  (new frame%
       (label "Paramètres")))

(define HPANEL ;Paneau horizontal contenant VPANEL1 et VPANEL2
  (new horizontal-panel%
       (parent FRAME)
       (alignment '(center center))))

(define VPANEL1;Paneau vertical gauche
  (new vertical-panel%
       (parent HPANEL)
       (alignment '(center top))))

(define VPANEL2;Paneau vertical droit
  (new vertical-panel%
       (parent HPANEL)
       (alignment '(center top))))

(define Msg-θv ;Le titre de l'angle θv
  (new message%
       (parent VPANEL1)
       (label "Angle de vue d'un boid")
       (min-width 200)))

(define sliderL-θv;Le slider de l'angle θv
  (new slider%
       (parent VPANEL1)
       (label "θv :")
       (min-value 0)
       (max-value 180)
       (callback (lambda (slider evt)
                   (set! Ov (degrees->radians (send sliderL-θv get-value)))))))
                    

(define Msg-rc;Le titre de la zone rc 
  (new message%
       (parent VPANEL1)
       (label "Zone de cohésion")
       (min-width 200)))

(define sliderL-rc;Le slider de la zone rc 
  (new slider%
       (parent VPANEL1)
       (label "rc :")
       (min-value 0)
       (max-value 100)
       (callback (lambda (slider evt)
                   (set! rc (send sliderL-rc get-value))))))

(define Msg-ra;Le titre de la zone rd
  (new message%
       (parent VPANEL1)
       (label "Zone d'alignement")
       (min-width 200)))

(define sliderL-ra;Le slider de la zone ra
  (new slider%
       (parent VPANEL1)
       (label "ra :")
       (min-value 0)
       (max-value 100)
       (callback (lambda (slider evt)
                   (set! ra (send sliderL-ra get-value))))))

(define Msg-rd;Le titre de la zone rd
  (new message%
       (parent VPANEL1)
       (label "Zone de distanciation")
       (min-width 200)))

(define sliderL-rd;Le slider de la zone rd
  (new slider%
       (parent VPANEL1)
       (label "rd :")
       (min-value 0)
       (max-value 100)
       (callback (lambda (slider evt)
                   (set! rd (send sliderL-rd get-value))))))

(define Msg-v0;Le titre de la vitesse v0
  (new message%
       (parent VPANEL2)
       (label "Vitesse des boids")
       (min-width 200)))

(define sliderR-v0 ;Le slider de la vitesse v0
  (new slider%
       (parent VPANEL2)
       (label "v0 :")
       (min-value 1)
       (max-value 100)
       (callback (lambda (slider evt)
                   (set! v0 (send sliderR-v0 get-value))))))

(define Msg-ωmax;Le titre de la vitesse de rotation ωmax
  (new message%
       (parent VPANEL2)
       (label "Vitesse de rotation d'un boid")
       (min-width 200)))

(define sliderR-ωmax;Le slider de la vitesse de rotation ωmax
  (new slider%
       (parent VPANEL2)
       (label "ω  :")
       (min-value 1)
       (max-value 180)
       (callback (lambda (slider evt)
                   (set! wmax (degrees->radians(send sliderR-ωmax get-value)))))))


(define Msg-dt;Le titre de la frequence 
  (new message%
       (parent VPANEL2)
       (label "Fréquence d'échantillonage 1/dt")
       (min-width 200)))

(define sliderR-dt;Le slider de la frequence
  (new slider%
       (parent VPANEL2)
       (label "dt :")
       (min-value 1)
       (max-value 40)
       (callback (lambda (slider evt)
                   (set! 1/dt (/ 1 (send sliderR-dt get-value)))))))

;On ouvre automatiquement la fenetre des parametres
(send FRAME show #t)