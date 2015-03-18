(defun Degrees->Radians	(numberOfDegrees)
  (* pi (/ numberOfDegrees 180.0))
)


(defun Srastoinie (JOrast Vgradus)
  (atof
    (rtos (* JOrast (cos (degrees->radians (- 90 Vgradus))))
	  2
	  6
    )
  )
)

(defun Hvisota (JOrast Vgradus)
  (atof
    (rtos (* JOrast (sin (degrees->radians (- 90 Vgradus))))
	  2
	  6
    )
  )
)

(defun C:r45loadpoints ()
  (setvar "CMDECHO" 0)
  (setvar "angdir" 1)
  (setvar "angbase" 0.0000)
  (setvar "DIMZIN" 0)
  (setq	fname (getfiled	"Выберете фаил с форматом *.dat"
			"C:\\TAX\\"
			"dat;txt;*"
			16
	      )
  )
					;получение сведений
  (setq priborpoi (getpoint "\nУкажите точку где стоял прибор:"))
  (setq punktpoi (getpoint "\nУкажите точку расположения ориентира:"))
  (setq osnH (atof (getstring "\nОбозначте высоту H:")))
  (setq osnI (atof (getstring "\nОбозначте высоту прибора:")))
  (setq osnV (atof (getstring "\nОбозначте высоту вешки:")))
  (setq ugol (angle priborpoi punktpoi))
  (command "_CLAYER" "Пикет")
  (command "_circle" priborpoi "0.2" "")
  (setq ofi (open fname "r"))
					;Читание файла
					;Цикл построчного данных
  (while
    (setq nli (read-line ofi))
     (setq numpic2 (fix (atof (substr nli 28 4))))
     (setq rastoi2 (atof (substr nli 43 8)))
     (setq gorizon (substr nli 59 8))
     (setq vertical (substr nli 71 8))
     (setq podvesh 0)
     (if (/= "" (substr nli 81 5))
       (setq podvesh (atof (substr nli 81 5)))
     )
					;Если строрка пустая 
     (if (= (substr nli 43 8) "        ")
       (progn
	 (setq ihek (substr nli 4 4))
	 (princ (strcat "В ячейке №" ihek))
	 (setq kkkl
		(getstring
		  " нет данных.Другой участок(Д),Поправка(П),Продолжить(Н),Cтрока(С)<Н>"
		)
	 )

	 (if (or (= kkkl "с") (= kkkl "С"))
	   (progn
	     (Setq nomih (getstring
			   "Введите номер ичейки:"
			 )
	     )
	     (setq ofi (open fname "r"))
	     (while (/= (substr nli 4 4) nomih)
	       (setq nli (read-line ofi))
	     )
	   )
	 )

	 (if (or (= kkkl "д") (= kkkl "Д"))
	   (progn
	     (setq
	       priborpoi (getpoint "\nУкажите точку где стоял прибор:")
	     )
	     (setq punktpoi
		    (getpoint "\nУкажите точку расположения ориентира:")
	     )
	     (setq osnH (atof (getstring "\nОбозначте высоту H:")))
	     (setq osnI (atof (getstring "\nОбозначте высоту прибора:")))
	     (setq osnV (atof (getstring "\nОбозначте высоту вешки:")))
	     (command "_CLAYER" "Пикет")
	     (command "_circle" priborpoi "0.2" "")
	     (setq nli (read-line ofi))
	     (setq numpic2 (fix (atof (substr nli 28 4))))
	     (setq rastoi2 (atof (substr nli 43 8)))
	     (setq gorizon (substr nli 59 8))
	     (setq vertical (substr nli 71 8))
	     (setq podvesh 0)
	     (if (/= "" (substr nli 81 5))
	       (setq podvesh (atof (substr nli 81 5)))
	     )
	     (setq ugol (angle priborpoi punktpoi))
	     (setq kkkl nil)
	   )
	 )
	 (if (or (= kkkl "п") (= kkkl "П"))
	   (progn (setq	gorizondes
			 (+ (atof (substr gorizon 1 3))
			    (/ (atof (substr gorizon 5 2)) 60)
			    (/ (atof (substr gorizon 7 2)) 3600)
			 )
		  )
		  (setq
		    ugol (+ ugol (Degrees->Radians gorizondes))
		  )
	   )
	 )

       )
     )					;Конец условия 

     (setq gorizongrad (atof (substr gorizon 1 3)))
     (setq gorizonmin (atof (substr gorizon 5 2)))
     (setq gorizonsec (atof (substr gorizon 7 2)))

     (setq verticalgrad (atof (substr vertical 1 3)))
     (setq verticalmin (atof (substr vertical 5 2)))
     (setq verticalsec (atof (substr vertical 7 2)))
					;Перевод из град в десятичные градусы
     (setq
       gorizon2	(+ gorizongrad (/ gorizonmin 60) (/ gorizonsec 3600))
     )
     (setq vertical2
	    (+ verticalgrad (/ verticalmin 60) (/ verticalsec 3600))
     )
					;конец перевода из град в десятичные градусы
     (setq natrast (Srastoinie rastoi2 vertical2))
					;расчитывание высоты 
     (setq
       visotbI
	(- (+ (- (+ osnI (Hvisota rastoi2 vertical2)) osnV) osnH)
	   podvesh
	)
     )
					;конец расчитывания высоты
					;Расчитывание точки 
     (setq coordinata
	    (polar priborpoi
		   (- (- 0 (- (Degrees->Radians gorizon2) ugol)) 0.32332)
		   natrast
	    )
     )
					;Конец расчитывание точки 
     (command "_CLAYER" "Пикет")
     (command "_circle" coordinata "0.1" "")
     (command "_TEXT"
	      "_R"
	      (mapcar
		'-
		coordinata
		'(0.25 0.18)
	      )
	      "0.35"
	      "0"
	      numpic2
     )
     (command "_CLAYER" "Отметки")
     (command "_circle" coordinata "0.1" "")
     (command "_TEXT"
	      (mapcar
		'+
		coordinata
		'(0.5 0)
	      )
	      "0.85"
	      "0"
	      (rtos visotbI 2 2)
     )
  )
  (close ofi)
  (princ)
)

