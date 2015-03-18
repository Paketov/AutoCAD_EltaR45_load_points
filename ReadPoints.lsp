

(defun C:загрузточ ()
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
  (if (= (nth 2 priborpoi) 0)
    (progn
      (setq osnH (atof (getstring "\nОбозначте высоту H:")))
      (setq priborpoi (list (nth 0 priborpoi) (nth 1 priborpoi) osnH))
    )
    (setq osnH (nth 2 priborpoi))
  )
  (setq osnI (atof (getstring "\nОбозначте высоту прибора:")))
  ;(setq osnI 1.475)
  (setq osnV (atof (getstring "\nОбозначте высоту вешки:")))
  ;(setq osnV 2)
  (command "_CLAYER" "Пикет")
  (command "_circle" priborpoi "0.2" "")
  (setq ofi (open fname "r"))
  (setq popravka '(0 0 0))
	 
   ;Чтение файла
   ;Цикл построчного считывания 
  (setq Continue nil)
  (while
    (setq nli (read-line ofi))
     (setq numpic2 (fix (atof (substr nli 28 4))))
     (setq rastoi2 (atof (substr nli 43 8)))
     (setq gorizon (substr nli 59 8))
     (setq vertical (substr nli 71 8))
   ;Если строка пустая 
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
	     (setq punktpoi (getpoint
			      "\nУкажите точку расположения ориентира:"
			    )
	     )
	     (if (= (nth 2 priborpoi) 0)
	       (progn
		 (setq osnH (atof (getstring "\nОбозначте высоту H:")))
		 (setq priborpoi (list (nth 0 priborpoi)
				       (nth 1 priborpoi)
				       osnH
				 )
		 )
	       )
	       (setq osnH (nth 2 priborpoi))
	     )
	     (setq osnI (atof (getstring "\nОбозначте высоту прибора:")))
	     (setq osnV (atof (getstring "\nОбозначте высоту вешки:")))
	     (command "_CLAYER" "Пикет")
	     (command "_circle" priborpoi "0.2" "")
	     (setq nli (read-line ofi))
	     (setq numpic2 (fix (atof (substr nli 28 4))))
	     (setq rastoi2 (atof (substr nli 43 8)))
	     (setq gorizon (substr nli 59 8))
	     (setq vertical (substr nli 71 8))
		 (setq popravka '(0 0 0))
	     (setq kkkl nil)
	   )
	 )
	 (if (or (= kkkl "п") (= kkkl "П"))
	   	 (setq popravka
		(list (atof (substr gorizon 1 3))
		      (atof (substr gorizon 5 2))
			  (atof (substr gorizon 7 2))
		)
	 )
	 )
	 (if (or (= kkkl "н") (= kkkl "Н") (= kkkl ""))
	   (setq Continue T)
	 )
       )
     ) ;Конец условия 

	 
     (if (not Continue)
       (progn
	 (setq gorizongrad
		(list (atof (substr gorizon 1 3))
		      (atof (substr gorizon 5 2))
		      (atof (substr gorizon 7 2))
		)
	 )

	 (setq verticalgrad
		(list (atof (substr vertical 1 3))
		      (atof (substr vertical 5 2))
		      (atof (substr vertical 7 2))
		)
	 )

	 (setq retcoord	(exetut	gorizongrad  rastoi2	  verticalgrad
				osnI	     osnV	  '(0 0 0)
				punktpoi     priborpoi  popravka
			       )
	 )
   ;Конец расчитывание точки 
	 (command "_CLAYER" "Пикет")
	 (command "_circle" retcoord "0.1" "")
	 (command "_TEXT"
		  "_R"
		  (mapcar
		    '-
		    retcoord
		    '(0.25 0.18)
		  )
		  "0.35"
		  "0"
		  (itoa numpic2)
	 )
	 (command "_CLAYER" "Отметки")
	 (command "_circle" retcoord "0.1" "")
	 (command "_TEXT"
		  (mapcar
		    '+
		    retcoord
		    '(0.5 0)
		  )
		  "0.85"
		  "0"
		  (rtos (nth 2 retcoord) 2 2)
	 )
       )
     )
     (setq Continue nil)
  )
  (close ofi)
  (princ)
)



(defun DegrToRad (Degr)
  (* pi (/ Degr 180.0))
)

(defun znak (num)
  (if (/= num 0)
    (if	(> num 0)
      (setq ret 1)
      (setq ret -1)
    )
    (setq ret 0)
  )
  (eval ret)
)
(defun tan (num)
  (/ (sin num) (cos num))
)

(defun exetut (xyugol nrast zugol i_visota v_visota nahal_izm punkt
	       tokinst popravka)
   ;Функция вычисления координат точки по входным данным
   ;Возвращает список с координатами точки
  (if (and
	(> (nth 0 zugol) 0)
	(> (nth 1 zugol) 0)
	(> (nth 2 zugol) 0)
      )
    (setq Q_ 1)
    (setq Q_ -1)
  )
    (setq popravka (DegrToRad (+ (nth 0 popravka)
			 (/ (nth 1 popravka) 60)
			 (/ (nth 2 popravka) 3600)
		      )
	   )
  )

   ;Горизонтальный угол переводим в радианы
  (setq	T_ (DegrToRad (+ (nth 0 xyugol)
			 (/ (nth 1 xyugol) 60)
			 (/
			   (nth 2 xyugol)
			   3600
			 )
		      )
	   )
  )
    (if (> popravka pi)
    (setq T_ (- T_ (- (* pi 2)  popravka)))
	(setq T_ (+ T_ popravka))
  )
   ;Начальное измерение переводим в радианы
  (setq	S_ (DegrToRad (+ (nth 0 nahal_izm)
			   (/ (nth 1 nahal_izm) 60)
			   (/ (nth 2 nahal_izm) 3600)
		      )
	   )
  )

   ;Вертикальный угол в радианы
  (if (> (nth 0 zugol) 45)
    (setq W_ (DegrToRad	(+ (nth 0 zugol)
			   (/ (nth 1 zugol) 60)
			   (/
			     (nth 2
				  zugol
			     )
			     3600
			   )
			)
	     )
    )
    (setq W_ (DegrToRad	(- 90
			   (* (+ (abs (nth 0 zugol))
				 (abs (/
					(nth 1 zugol)
					60
				      )
				 )
				 (abs (/
					(nth 2 zugol)
					3600
				      )
				 )
			      )
			      Q_
			   )
			)
	     )
    )
  )
   ;Конец преобразования вертикального угла в радианы
  (setq U_ (- T_ S_))
   ;Расстояние приведённое
  (if (not (= nrast 0))
    (setq R_ (-	(* nrast (sin W_))
		(/ (* 0.734 (expt (* nrast (sin W_)) 2))
		   (expt 10 10)
		)
	     )
    )
  )

  ;;Координаты пункта
  (setq AH_ (- (nth 0 punkt) (nth 0 tokinst)))
  (setq AI_ (- (nth 1 punkt) (nth 1 tokinst)))
  (setq AJ_ (+ (atan (/ AI_ AH_)) (/ (* pi (- 1 (znak AH_))) 2)))
  ;;Координаты пункта

  (setq V_ (+ U_ AJ_))
  (setq X_ (* R_ (cos V_)))
  (setq Y_ (* R_ (sin V_)))
  (setq Z_ (- (+ (* nrast (cos W_)) i_visota) v_visota))
   ;Возвращаем координаты точки
  (list	(+ Y_ (nth 1 tokinst))
	(+ X_ (nth 0 tokinst))
	(+ Z_ (nth 2 tokinst))
  )
)
