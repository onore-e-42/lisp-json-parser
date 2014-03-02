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

;;json-pair - input: list - output: true or nil
;;if list doesn't contain : that isn't a pair.
;;otherwise launch json-string with a substring that should be contain
;;only the string, and json-value ( : -> character)
(defun json-pair(l)
	(cond 	((null (position #\: l)) nil)	
			((and
				(json-string(subseq l 0 (position #\: l)))
				(json-value(subseq l (1+ (position #\: l)))))
				(
				;fare cose
				))))

;;json-string - input: list - output: true or nil
;;if list doesn't contain " that couldn't be a string.
;;otherwise launch json-space (there's some character before first "?)
;;and launch also json-chars from first " to the end of the list.
(defun json-string(l)
	(cond	((null (position #\" l)) nil)
			((and
				(not(json-space(subseq l 0 (position #\" l))))
				(json-chars(subseq l 
						(1+(position #\" l))
						(position #\" l :start 
							(1+(position #\" l))))))
				(
					;fare qualcosa
				)
			)
	)
)

;;json-space - inptu list - output: true or nil
;;if list contain something else expect #\space, return t
(defun json-space(l)
	(cond ((null l) nil)
		  (t (
				(loop for i in l 
						do (cond ((equal i #\space))
								 (t (return t)))
				)
			 ))
	)				
)

;;json-char - input:list - output: true or nil
;;check there's an escape input inside the string
(defun json-chars(l)
	(cond 
		((null l) t)
		((and 	(equal (car l) #\\ )
				(equal (cdr l) (list)))
			nil)
		((and 	(equal (car l) #\\ )
				(not (equal (cdr l) (list))))
			(json-chars (cddr l)))
		(t (json-chars(cdr l)))
	)
)
			
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
(defparameter test7 "{\"nome\" : \"Arthur\", \"cognome\" : \"Dent\", \"numero civico\" : {\"uno\" : 1, \"due\" : 2, \"tre\": [3, 4, 5232, 42, {\"una\":\"prova\"}], \"quattro\" : {\"nothing to see \\\\\\h\\\\\\\"ere\" : \"42\"}}, \"abi\\\"tan\\\"ti\" : [\"Lui\", \"\\\"lo zio\\\"\", \"il cugino\"]}")