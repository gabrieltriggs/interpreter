-- Program 1 (should terminate with X = 0, Y = 0, Z = 16)
X := 2;
Y := 0;
Z := 2;
WHILE X > Y DO
	Z := Z * Z;
	X := X - 1
END

"X := 2; Y := 0; Z := 2; WHILE X > Y DO Z := Z * Z; X := X - 1 END"

-- AST for Program 1
(Seq (Assign "X" (Const 2))
	 (Seq (Assign "Y" (Const 0))
		  (Seq (Assign "Z" (Const 2))
			   (While (Greater (Var "X") (Var "Y"))
					  (Seq (Assign "Z" (Times (Var "Z") (Var "Z")))
						   (Assign "X" (Minus (Var "X") (Const 1))))))))


interpret (Seq (Assign "X" (Const 2)) (Seq (Assign "Y" (Const 0)) (Seq (Assign "Z" (Const 2)) (While (Greater (Var "X") (Var "Y")) (Seq (Assign "Z" (Times (Var "Z") (Var "Z"))) (Assign "X" (Minus (Var "X") (Const 1)))))))) initial

-- Token list for Program 1
[Ident "X", Ident ":=", Number 2, Ident ";", Ident "Y", Ident ":=", Number 0, Ident ";", Ident "Z", Ident ":=", Number 2, Ident ";", Symbol "WHILE", Ident "X", Ident ">", Ident "Y", Symbol "DO", Ident "Z", Ident ":=", Ident "Z", Ident "*", Ident "Z", Ident ";", Ident "X", Ident ":=", Ident "X", Ident "-", Number 1, Symbol "END"]





-- Program 2 (should terminate with X = 2, Y = 2)
X := 1;
Y := 2;
IF X > Y 
	THEN Z := X
	ELSE X := Y
ENDIF

"X := 1; Y := 2; IF X > Y THEN Z := X ELSE X := Y ENDIF"

-- AST for Program 2
(Seq (Assign "X" (Const 1))
	 (Seq (Assign "Y" (Const 2))
		  (Cond (Greater (Var "X") (Var "Y"))
			    (Assign "Z" (Var "X"))
			    (Assign "X" (Var "Y")))))

 interpret (Seq (Assign "X" (Const 1)) (Seq (Assign "Y" (Const 2)) (Cond (Greater (Var "X") (Var "Y")) (Assign "Z" (Var "X")) (Assign "X" (Var "Y"))))) initial

-- Token list for Program 2
[Ident "X", Ident ":=", Number 1, Ident ";", Ident "Y", Ident ":=", Number 2, Ident ";", Symbol "IF", Ident "X", Ident ">", Ident "Y", Symbol "THEN", Ident "Z", Ident ":=", Ident "X", Symbol "ELSE", Ident "X", Ident ":=", Ident "Y", Symbol "ENDIF"]





-- Program 3 (should terminate with X = 2, Y = 1, Z = 5)
X := 2;
Y := 1;
Z := 3;
IF X > Y
	THEN IF Y > Z
			 THEN Z := 4
			 ELSE Z := 5
		 ENDIF
    ELSE Z := 6
ENDIF

"X := 2; Y := 1; Z := 3; IF X > Y THEN IF Y > Z THEN Z := 4 ELSE Z := 5 ENDIF ELSE Z := 6 ENDIF"

-- AST for Program 3
(Seq (Assign "X" (Const 2))
	 (Seq (Assign "Y" (Const 1))
		  (Seq (Assign "Z" (Const 3))
			   (Cond (Greater (Var "X") (Var "Y"))
				  	 (Cond (Greater (Var "Y") (Var "Z"))
				 	       (Assign "Z" (Const 4))
				 	       (Assign "Z" (Const 5)))
				     (Assign "Z" (Const 6))))))

interpret (Seq (Assign "X" (Const 2)) (Seq (Assign "Y" (Const 1)) (Seq (Assign "Z" (Const 3)) (Cond (Greater (Var "X") (Var "Y")) (Cond (Greater (Var "Y") (Var "Z")) (Assign "Z" (Const 4)) (Assign "Z" (Const 5))) (Assign "Z" (Const 6)))))) initial

-- Token list for Program 3
[Ident "X", Ident ":=", Number 2, Ident ";", Ident "Y", Ident ":=", Number 1, Ident ";", Ident "Z", Ident ":=", Number 3, Ident ";", Symbol "IF", Ident "X", Ident ">", Ident "Y", Symbol "THEN", Symbol "IF", Ident "Y", Ident ">", Ident "Z", Symbol "THEN", Ident "Z", Ident ":=", Number 4, Symbol "ELSE", Ident "Z", Ident ":=", Number 5, Symbol "ENDIF", Symbol "ELSE", Ident "Z", Ident ":=", Number 6, Symbol "ENDIF"]





-- Program 4 (should terminate with X = 1, Y = -2)
X := 1;
Y := (0 - X) * 2

"X := 1; Y := (0 - X) * 2"

-- AST for Program 4
(Seq (Assign "X" (Const 1))
	 (Assign "Y" (Times (Minus (Const 0) (Var "X")) 
	 					(Const 2))))

interpret (Seq (Assign "X" (Const 1)) (Assign "Y" (Times (Minus (Const 0) (Var "X")) (Const 2)))) initial

-- Token list for Program 4
[Ident "X", Ident ":=", Number 1, Ident ";", Ident "Y", Ident ":=", Symbol "(", Number 0, Ident "-", Ident "X", Symbol ")", Ident "*", Number 2]





-- Program 5 (should terminate with X = 0, Y = 0, Z = 100)
X := 10;
WHILE X > 0 DO
	Y := 10;
	WHILE Y > 0 DO
		Z := Z - (0 - 1);
		Y := Y - 1
	END;
	X := X - 1
END

"X := 10; WHILE X > 0 DO Y := 10; WHILE Y > 0 DO Z := Z - (0 - 1); Y := Y - 1 END; X := X - 1 END"

-- AST for Program 5
(Seq (Assign "X" (Const 10))
	 (While (Greater (Var "X") (Const 0))
	 		(Seq (Assign "Y" (Const 10))
	 			 (Seq (While (Greater (Var "Y") (Const 0))
	 			 		(Seq (Assign "Z" (Minus (Var "Z") (Minus (Const 0) (Const 1))))
	 			 			 (Assign "Y" (Minus (Var "Y") (Const 1)))))
	 			 	  (Assign "X" (Minus (Var "X") (Const 1)))))))

interpret (Seq (Assign "X" (Const 10)) (While (Greater (Var "X") (Const 0)) (Seq (Assign "Y" (Const 10)) (Seq (While (Greater (Var "Y") (Const 0)) (Seq (Assign "Z" (Minus (Var "Z") (Minus (Const 0) (Const 1)))) (Assign "Y" (Minus (Var "Y") (Const 1))))) (Assign "X" (Minus (Var "X") (Const 1))))))) initial

-- Token list for Program 5
[Ident "X", Ident ":=", Number 10, Ident ";", Symbol "WHILE", Ident "X", Ident ">", Number 0, Symbol "DO", Ident "Y", Ident ":=", Number 10, Ident ";", Symbol "WHILE", Ident "Y", Ident ">", Number 0, Symbol "DO", Ident "Z", Ident ":=", Ident "Z", Ident "-", Symbol "(", Number 0, Ident "-", Number 1, Symbol ")", Ident ";", Ident "Y", Ident ":=", Ident "Y", Ident "-", Number 1, Symbol "END", Ident ";", Ident "X", Ident ":=", Ident "X", Ident "-", Number 1, Symbol "END"]