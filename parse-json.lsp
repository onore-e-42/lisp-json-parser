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
					((json-object l) 
						(parse-member(nthcdr 1 (butlast l)) 0)
					)
			)
)

(defun json-object(l)
	(cond 	((and 	(equal (first l) #\{)
					(equal (car (last l)) #\}))
					(json-member (nthcdr 1 (butlast l)) 0))
			((equal (first l) #\space) (json-object (cdr l)))
			((equal (last l) #\space) (json-object (butlast l)))
	)
)

(defun json-member(l p)
	(cond ((null (position #\, l :start p)) (json-pair l 0))
		  ((null l) t)
		  ((json-pair (subseq l 0 (position #\, l :start p)) 0)
				t)
		  (t
			(json-member l (1+(position #\, l :start p))
			 ))
	)
)
	
;;parse-member - input: list, int- output: list
;;if there's no comma, all the list is a pair, otherwise, the list is ;;splitted using comma as separator.
;;check if json-pair is a pair
(defun parse-member(l p)
			(cond 	;;((null (position #\, l :start p)) (parse-pair l))
					((json-pair(subseq l 0 
						(position #\, l :start p)))
							(append 	
								(parse-pair(subseq l 0 
									(position #\, l :start p)) 0)
								(parse-member(subseq l (1+ 
									(position #\, l :start p)))0)
							)
					)
					(t
						(parse-member l (1+ (position #\, l :start p)))
					)
			)
)

;;json-pair - input: list - output: true or nil
;;if list doesn't contain : that isn't a pair.
;;otherwise launch json-string with a substring that should be contain
;;only the string, and json-value ( : -> character)
(defun json-pair(l p)
	(cond 	((null (position #\: l :start p)) nil)	
			((and
				(json-string(subseq l 0 (position #\: l)) 0)
				(json-value(subseq l (1+ (position #\: l)))))
			t)
			((null (position #\: l :start 
				(1+(position #\: l :start p)))) nil)
			(t 
				(json-pair l (position #\: l :start 
									(1+(position #\: l :start p))))
			)
	)
)

;;json-string - input: list - output: true or nil
;;if list doesn't contain " that couldn't be a string.
;;otherwise launch json-space (there's some character before first "?)
;;and launch also json-chars from first " to the end of the list.
(defun json-string(l p)
	(cond	((null (position #\" l)) nil)
			((null (position #\" l :start (1+ (position #\" l :start p)))) nil);; spe
			((and
				(json-space(subseq l 0 
						(position #\" l)))
				(json-chars(subseq l 
						(1+ (position #\" l))
						(1+(position #\" l :start (1+(position #\" l :start p)))))))
						
					(cond ((equal 	(length
									 (subseq l 0 
										(1+(position #\" l :start 
										(1+(position #\" l :start p))))))
									(length l))
							
							t)
						  ((and 
								(json-space(subseq l 
										(1+(position #\" l :start 
										(1+(position #\" l :start p))))))
								((not(equal 	(length
									 (subseq l 0 
										(1+(position #\" l :start 
										(1+(position #\" l :start p))))))
									(length l)))))

							t)
					)
			)
			(t 
				(json-string l (position #\" l 
									:start (1+ (position #\" l :start p)))
											
				)
			)
	)
)

;;json-space - inptu list - output: true or nil
;;if list contain something else expect #\space, return nil
(defun json-space(l)
	(cond ((null l) t)
		  ((not (equal (car l) #\space))
			nil)
		  (t 
			(json-space (cdr l))
		   )
	)
)

;;json-char - input:list - output: true or nil
;;check there's an escape input inside the string
(defun json-chars(l)
	(cond 
		((null l) nil)
		((and 	(equal (car l) #\\ )
				(null (cdr l)) )
			nil)
		((and 	(equal (car l) #\\ )
				(not(null (cdr l))))
			(json-chars(cddr l)))
		((equal (car l) #\")
				t)
		(t (json-chars(cdr l)))
	)
)

	
(defun json-value(l)
	(cond 	((null l) nil)
			((json-string l 0) 
				t)
			((and	(numberp (car l))
					(null (cdr l)))
				t)
			((json-array l) t)
			((json-object l)t)
	)
)

(defun json-array(l)
	(cond ((or (null (position #\[ l))
				(null (position #\] l)))
				nil)
		  ((null(json-space (subseq l 0 (position #\[ l)))) nil)
		  ((null(json-space (subseq l (1+ position #\] l)))) nil)
		  ((json-elements(subseq l
							(1+(position #\[ l))
							(position #\] l)) 0 0) 
			;fare qualcosa
			)
	)
)

(defun json-elements (l p f)
	(cond  	((null(position #\, l :start f)) (json-element l))
			((null(json-element (subseq l p 
								(position #\, l :start f))))
					(json-elements l p (position #\, l 
										:start (1+(position #\, l :start p))))
			)
			((json-element (subseq l f (position #\, l :start f)))
					(json-elements l f f))
			
	)
)

			
	
	
(defun json-element (l)
	(cond	((null l) t)
			((equal #\space (first l)) 
				(json-element (cdr l)))
			((equal #\space (last l))
				(json-element (butlast l)))
			(t (json-value l))))
			
			
		   
	
		
			
	
;;parse-pair - input: lista and int - output: list
;;if there's no : in the 
(defun parse-pair(l p)
			(cond ((null (position #\: l :start p)) nil)
				  ((and (json-string (subseq l 0 
							(position #\: l :start p)))
						(json-value (subseq l (1+ 
							(position #\: l :start p))))
					)
						(append 
							(parse-string (subseq l 0 
								(position #\: l :start p)))
							(parse-value (subseq l (1+ 
								(position #\: l :start p))))))
				  (t
						(parse-pair l (1+ (position #\, l :start p)))
				  )
			)
)


(defun parse-string (l)
	(cond ((json-string(l)) (parse-char-atom l))))
; (defun parse-string(l)
			; (cond ((null (position #\" l)) nil)
				  ; (t (parse-chars-atom(subseq l 
						; (1+ (position #\" l)) 
							; (or
								; (position #\" l :start 
									; (1+ (position #\" lista)))
										; (length l) ))))
			; )
; )
		
		
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
(defparameter error1 "{\"urca\\\"\"aa:\"ciao\",,}")
(defparameter lista (map 'list #'identity test1))
(defparameter errore (map 'list #'identity error1))
(defparameter test7 "{\"nome\" : \"Arthur\", \"cognome\" : \"Dent\", \"numero civico\" : {\"uno\" : 1, \"due\" : 2, \"tre\": [3, 4, 5232, 42, {\"una\":\"prova\"}], \"quattro\" : {\"nothing to see \\\\\\h\\\\\\\"ere\" : \"42\"}}, \"abi\\\"tan\\\"ti\" : [\"Lui\", \"\\\"lo zio\\\"\", \"il cugino\"]}")