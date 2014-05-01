-- Program 1 (should terminate with X = 0, Y = 0, Z = 16)
X := 2;
Y := 0;
Z := 2;
WHILE X > Y DO
	Z := Z * Z;
	X := X - 1
END

-- AST for Program 1
interpret (Seq (Assign "X" (Const 2))
			   (Seq (Assign "Y" (Const 0))
				    (Seq (Assign "Z" (Const 2))
			    	(While (Greater (Var "X") (Var "Y"))
						   (Seq (Assign "Z" (Times (Var "Z") (Var "Z")))
								(Assign "X" (Minus (Var "X") (Const 1))))))))
		  initial

interpret (Seq (Assign "X" (Const 2)) (Seq (Assign "Y" (Const 0)) (Seq (Assign "Z" (Const 2)) (While (Greater (Var "X") (Var "Y")) (Seq (Assign "Z" (Times (Var "Z") (Var "Z"))) (Assign "X" (Minus (Var "X") (Const 1)))))))) initial

-- Program 2 (should terminate with X = 2, Y = 2)
X := 1;
Y := 2;
IF X > Y 
	THEN Z := X
	ELSE X := Y
ENDIF

-- Program 3 (should terminate with X = 2, Y = 1, Z = 5)
X := 2;
Y := 1;
Z := 3;
IF X > Y
	THEN IF Y > Z
			 THEN Z := 4
			 ELSE Z := 5
		 ENDIF;
    ELSE Z := 6
ENDIF

-- Program 4 (should terminate with X = 1, Y = -2)
X := 1;
Y := (0 - X) * 2

-- Program 5 (should terminate with X = 0, Y = 0, Z = 100)
X := 10;
WHILE X > 0
	Y := 10;
	WHILE Y > 0
		Z := Z - (0 - 1)
	END
END