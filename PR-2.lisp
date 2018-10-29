(DEFUN prefijo(L N)
	(COND
		((NULL L)		NIL)
		((ZEROP N)		NIL)
		(T			(CONS (CAR L)  (prefijo (CDR L) (- N 1))))
	)
)

(DEFUN sublist(L INI FIN)
	(COND
		((NULL L)		NIL)
		((< FIN INI)		NIL)
		((EQUAL INI 1)		(prefijo L FIN))
		(T			(sublist (CDR L) (- INI 1) (- FIN 1)))
	)
)

(DEFUN rec-reverse(L)
	(COND
		((NULL L)		NIL)
		((ATOM(CAR L))		(APPEND (rec-reverse (CDR L)) (LIST (CAR L))))
		(T			(APPEND (rec-reverse (CDR L)) (LIST(rec-reverse (CAR L)))))
	)
)

(DEFUN elim-n-rec(L N) (elim-aux L N 1))
(DEFUN elim-aux(L N POS)
	(COND
		((NULL L)		NIL)
		((EQUAL N POS)		(elim-aux (CDR L) N (+ POS 1)))
		((ATOM (CAR L))		(CONS (CAR L) (elim-aux (CDR L) N (+ POS 1))))
		(T			(CONS (elim-aux (CAR L) N 1) (elim-aux (CDR L) N (+ POS 1))))
	)
)
