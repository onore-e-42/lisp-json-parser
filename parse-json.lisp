(defun parse-json (string)
	(cond ((equal (subseq string 0 1) "{") 
			(cond ((equal (subseq string (- (length string) 1) (length string)) "}") 
					(json-object string)
				  )
			(NIL)))
	(NIL))
)

;;; passo stringa da dividere in coppie senza{}
;;; separo sulla virgola 
(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        append (list (json-member(string-trim " "(subseq string start finish))))
        until (null finish)
	)
)

(defun json-object (string)
	(comma-split (subseq string 1 (- (length string) 1)))
)


(defun json-member (string)
	(cond 	((null (position #\: string)) (print "error modulo"))
			(T (json-pair (subseq string 0 (position #\: string)) 
			(subseq string (+ 1 (position #\: string)) (length string))))
	)		
)	

(defun json-pair (string val)
	(cond ((json-string-contr string) 
			(cond ((json-val-contr val) (list string val))
			NIL)) 
	NIL)
)

(defun json-string-contr (string)
	;;; cose ;;;
	(string-trim " " string)
)

(defun json-val-contr (val)
	;;; cose ;;;
	(string-trim " " val)
)
	;;;; TESTIIIING
;(json-pair "{ \"sei\", \"uno\", \"scemo\"}")
;(json-pair "{aaaa:bbbb, cccc:dddd, eeee:ffff")
(parse-json "{aaa:aaa, bbb:bbb}")
