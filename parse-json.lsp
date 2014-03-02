;;parse-json -  input: string - output: list
;;map the string
(defun parse-json (string)
		(string-trim " " string)
		(parse-object(map 'list #'identity string))
)				
		
;;parse-object - input: list - output: list (without {})
;;if first and last elementes are {} call parse-member
(defun parse-object(l)			
			(cond 	((null  l) nil)
					((and (equal (first l) #\{)(equal (car (last l)) #\})) 
						(parse-member(nthcdr 1 (butlast l)))
					)
			)
)

;;parse-member - input: list - output: list till element comma.
;;if there's no comma, all the list is a pair, otherwise, the list is ;;splitted using comma as separator.
;;check if json-pair is a pair
(defun parse-member(l)
			(cond 	((null (position #\, l)) (parse-pair l))
					((json-pair(subseq l 0 (position #\, l)))
						(append 	
						(parse-pair(subseq l 0 (position #\, l)))
						(parse-member(subseq l (1+ (position #\, l))))
						)
					)
			)
)

(defun json-pair(l)
	(cond 	((null (position #\: l)) nil)	
			((and
				(json-string(subseq l 0 (position #\: l)))
				(json-value(subseq l (1+ (position #\: l)))))
				(
				;fare cose
				))))

(defun json-string(l)
	(cond	((null (position #\" l)) nil)
			((json-chars(subseq l (1+(position #\" l))))
				(
				;fare qualcosa
				)
			)
	)
)

(defun json-chars(l)
	(cond 	((null (position #\" l)) nil)
			((
			
(defun json-value(l)
	t)
	

			
(defun parse-pair(l)
			(cond ((null (position #\: l)) nil)
				  (t 	(parse-string (subseq l 0 (position #\: l)))
						(parse-value (subseq l (1+ (position #\: l)))))
			)
)

(defun parse-string(l)
			(cond ((null (position #\" l)) nil)
				  (t (parse-chars-atom(subseq l (1+ (position #\" l)) (or(position #\" l :start (1+ (position #\" lista)))(length l) ))))
			)
)
		
		
(defun parse-chars-atom(l)
	(parse-chars l)
)

(defun parse-chars(l)
	(cond ((null (position #\\ l))
			(parse-char(car l))
			(parse-chars(cdr l))
			)
		  ((null l) nil)
		  (t (parse-quotes(car (subseq l (position #\\ l) (1+(position #\\ l)))))
		  )
	)
)

(defun parse-char(c)
	(print c)
	(cond ((and (atom c)(not (equal c #\"))) c)))
		  
(defun parse-quote(c)
	(cond ((and (atom c)(equal c #\"))) c))
		 
	

(defun parse-value(l)
	l)
			

(defparameter test "\"nome\":\"matteo\"")
(defparameter test1 "{\"cognome\":\"brighi\", \"nome\":\"matteo\"}")
(defparameter test2 "{\"nome\":\"brigh,i\", \"nome\":\"mat,teo\"}")
(defparameter test3 "{\"libri\":[\"sette mondi\", \"1984\"]}")
(defparameter test4 "{\"li,,bri\":[\"se,tt,e mondi\", \"1984\"]}")
(defparameter test5 "{\"lib[ri\":[\"sette mondi\",4, \"1984\"]}")
(defparameter test6 "{\"libr{i\":[\"sette mondi\", \"1984\"]}")
(defparameter lista (map 'list #'identity test1))
