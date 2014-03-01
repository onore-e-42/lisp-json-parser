(defun parse-json (string)
		(string-trim " " string)
		(parse-object(map 'list #'identity string))
)				
		
(defun parse-object(l)			
			(cond 	((null  l) nil)
					((and (equal (first l) #\{)(equal (last l) #\})) 
						(parse-member(nthcdr 1 (butlast l)))
					)
			)
)

(defun parse-member(l)
			(cond ((
					

(defparameter test "\"nome\":\"matteo\"")
(defparameter test1 "{\"cognome\":\"brighi\", \"nome\":\"matteo\"}")
(defparameter test2 "{\"nome\":\"brigh,i\", \"nome\":\"mat,teo\"}")
(defparameter test3 "{\"libri\":[\"sette mondi\", \"1984\"]}")
(defparameter test4 "{\"li,,bri\":[\"se,tt,e mondi\", \"1984\"]}")
(defparameter test5 "{\"lib[ri\":[\"sette mondi\",4, \"1984\"]}")
(defparameter test6 "{\"libr{i\":[\"sette mondi\", \"1984\"]}")

