(require "cl-collider")

(in-package :ccl)

(advise ccl::resolve-address (let ((arglist (list* :address-family
						   :internet
						   arglist)))
                               (:do-it))
        :when :around
        :name :internet)

(in-package :sc)

(defugen (trill-raw "TrillRaw")
    (&key (i2c-bus 1) (i2c-address #x20) (noise-threshold 0) (pre-scaler 2) (reset-baseline 0.0))
  ((:kr (multinew new 'multiout-ugen 11
		  i2c-bus i2c-address noise-threshold pre-scaler reset-baseline))))

(defugen (trill-centroids "TrillCentroids")
    (&key (i2c-bus 1) (i2c-address #x20) (noise-threshold 0) (pre-scaler 2) (reset-baseline 0.0))
  ((:kr (multinew new 'multiout-ugen 11
		  i2c-bus i2c-address noise-threshold pre-scaler reset-baseline))))

(in-package :sc-user)

(defun exp-rand (mi ma)
  (lin-exp (random 1.0) 0 1.0 mi ma))

(defun ranged-random (mi ma)
  (+ mi (random (- ma mi))))

(defmacro repeat (n &body expr)
  `(loop :repeat ,n
	 :collect ,@expr))

(setf *s*
      (make-external-server "bela"
			    :port 5000
			    :server-options (make-server-options
					     :block-size 16
					     :realtime-mem-size (* 8192 4))))

(unless (boot-p *s*)
  (loop :initially (server-boot *s*)
	:with start := (get-universal-time)
	:do (sleep 1)
	:until (or (boot-p *s*) (> (get-universal-time) (+ start 10)))))

;;;

(defparameter *parafuso* (buffer-alloc 2048))

(buffer-setn *parafuso*
	     (coerce (sc::vector-in-wavetable-format
		      (env-as-signal (env '(0.57821953 0.009427071 -0.036792517 0.4255371
					    0.33361423 -0.71881485 -0.28034806 -0.8233911
					    -0.24736154 -0.5063758 -0.5115534 0.28532314
					    0.6649147 0.6877264 -0.56529486 0.31029904)
					  '(5.491031 7.9066033 1.5847538 7.903015
					    1.2321858 14.387738 6.069192 16.594774
					    5.243026 12.321582 11.248147 4.8787475
					    5.403677 16.675295 1.0069734)
					  '(9 -6 13 -16 5 14 10 0 6 9 0 2 15 -16 -7))
				     1024))
		     'list))

(proxy :trill
       (with-controls ((reset-baseline 1))
	 (let ((raw-vals (trill-raw.kr :i2c-bus 1
				       :i2c-address 48
				       :noise-threshold 0.0068
				       :pre-scaler 5
				       :reset-baseline reset-baseline)))
	   (+

	    ;; parafuso
	    (let ((trig (- (elt raw-vals 0) .09)))
	      (pan2.ar (leak-dc.ar
			(* (env-gen.kr (adsr 0.001 .3 .1 .6 .9)
				       :gate trig
				       :level-scale 5)
			   (mix (moog-ff.ar (osc.ar *parafuso*
						    (let ((freq (midicps
								 (demand.kr trig trig
									    (d-rand '(20 21 30))))))
						      (list freq (* 2.01 freq) (* 2.33 freq) (* 2.45 freq)))
						    0 '(.8 .5 .4 .3))
					    (env-gen.kr (asr 3 600 .1) :level-bias 220 :gate trig)
					    1))))))

	    ;; curvos grandes
	    (* (env-gen.kr (adsr .001 .2 .1 1)
			   :gate (- (elt raw-vals 1) .09)
			   :level-scale 4)
	       (dyn-klank.ar (list (mapcar (lambda (x) (* x .55706))
					   '(800 1071 1153 1723))
				   nil
				   (loop :repeat 4 :collect 0.429015))
			     (brown-noise.ar .002)))

	    (* (env-gen.kr (asr 1.6 1.5 1.2)
			   :gate (- (elt raw-vals 2) .09)
			   :level-scale 7)
	       (select-x-focus.ar (range (lf-tri.kr .175) 0 1)
				  (list 
				   (mix (sin-osc.ar (mapcar #'midicps '(50 51.25 72 73.25 83.25 84.25)) 0 .008))
				   (mix (sin-osc.ar (mapcar #'midicps '(50 51 60 70.125 71.125 80.125)) 0 .008)))
				  .75))

	    ;; curvos pequenos
	    (* (env-gen.kr (asr 1.3 1.1 1.4)
			   :gate (- (elt raw-vals 3) .11)
			   :level-scale 10)
	       (select-x-focus.ar (range (lf-tri.kr .2125) 0 1)
				  (list (sin-osc.ar (midicps 73.5) 0 0.1)
					(sin-osc.ar (midicps 76) 0 0.1)
					(sin-osc.ar (midicps 99.25) 0 0.1)))
	       (splay.ar (sin-osc.ar (mapcar #'midicps '(74 75 99.75)) 0 '(.1 .15 .08))))

	    (* (env-gen.kr (asr 1.3 1.1 1.4)
			   :gate (- (elt raw-vals 4) .19)
			   :level-scale 10)
	       (select-x-focus.ar (range (lf-tri.kr .2125) 0 1)
				  (list (sin-osc.ar (midicps 73.25) 0 0.1)
					(sin-osc.ar (midicps 76.75) 0 0.1)
					(sin-osc.ar (midicps 98) 0 0.1)))
	       (splay.ar (sin-osc.ar (mapcar #'midicps '(74 75.5 97.125)) 0 '(.1 .14 .09))))

	    
	    ;; main
	    (* (env-gen.kr (asr .001 1 1.5)
			   :gate (- (elt raw-vals 10) .085)
			   :level-scale 10)
	       (let ((trig (dust.kr (env-gen.kr (asr 5 14 1.5 1)
						:gate (- (elt raw-vals 10) .085)))))
		 (freeverb.ar (hpf.ar (+
				       ;; first sound
				       (let ((first-trig (trig.kr (- (elt raw-vals 10) .085) .0001)))
					 (* (env-gen.kr (perc) :gate first-trig)
					    (sos.ar (k2a.ar first-trig)
						    0 2 0 (demand.kr first-trig 1 (d-white 1.45 1.6))
						    '(-0.9995 -0.9995))))
				       ;;others
				       (* .45 (sos.ar (env-gen.ar (perc .001 .5) :gate trig)
						      0 .05 0 (demand.kr trig 1 (d-rand '(1.45 1.6 1.5 1.7 1.8 1.85) +inf+)) '(-0.9991 -0.9995))))
				      1000))))))))

(loop (sleep 60)
      (format t "Baseline reset.")
      (ctrl :trill :baseline-reset 1))

;;; color codes for connecting the elements to Trill Craft channels
;;; planta - castanho 10
;;; parafuso - preto 0
;;; grande 1 - branco 1
;;; grande 2 - cinzento 2
;;; pequeno 1 - azul 3
;;; pequeno 2 - roxo 4
