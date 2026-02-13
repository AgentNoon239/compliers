# Lab 1

## Grammar

This is the grammar for the language Cdim (for music fans):

<pre>
Program -> Stmts 'EOF'
Stmts -> Stmt Stmts
	| Stmt
	| epsilon
Stmt -> 'IDENT' '=' Expr ';'
	| '{' Stmts '}'
	| 'WHILE' '(' Expr ')' Stmt
	| 'FOR' '(' 'IDENT' '=' Expr ';' Expr ';' Expr ')' Stmt
	| 'IF' '(' Expr ')' Stmt 
	| 'IF' '(' Expr ')' Stmt 'ELSE' Stmt
	| 'PRINT' '(' Expr ')' ';'
	| ';'
Expr -> Expr '?' LogicExpr ':' LogicExpr
	| LogicExpr
LogicExpr -> LogicExpr Relop CalcExpr
	| CalcExpr
CalcExpr -> CalcExpr '-' Term
	| CalcExpr '+' Term
	| CalcExpr '||' Term
	| Term
Term -> Term '\*' Factor
	| Term '/' Factor
	| Term '&&' Factor
	| Factor
Factor -> '(' Expr ')'
	| 'IDENT' 
	| 'NUMBER'
	| '-' Factor
	| "!" Factor
Relop -> '<' | '<=' | '>' | '>=' | '==' | '!='
</pre>

Comments in the source language start with the characters '//' and end with a line-break (inclusive). There are no multi-line comments.

For clarity above, the one- and two-character tokens from the source are used instead of the tokens, e.g. '<' has token 'LESS'. 

'IDENT' is associated with the regex \[a-zA-Z\]\[a-zA-Z0-9\]\* and 'NUMBER' with 0|\[1-9\]\[0-9\]*

Note that an empty source file is an acceptable program indicated by Stmts -> epsilon, where epsilon is the empty string. "Empty statements" corresponding to ; are also permitted. 

There are also "block" statements such as "{a = 2; b = 3;}" and the interpretation of these block statements is that statements inside should be executed from left to right (or top to bottom if line separated). Empty blocks such as {} are permitted along with {;;}. Blocks can also be nested, i.e. {{}} and {{a = 2;};}

As indicated above, the only type here is the integer type. Negative integers are evaluated through the '-' unary operator.

Variables labelled by 'IDENT' store integer values and are all mutable, and 'IDENT' '=' Expr ';' statements assign them values. If the statement "a = -2;" is executed then a variable called "a" should store the integer value -2. It is not specified what value an 'IDENT' has if it has not been assigned one before an expression containing 'IDENT' is evaluated. For example if the program is "a = b + 2; print(a);" then "b" has not been assigned a value; what should be evaluated and/or executed? This should be determined by your implementation and explained below in the notes for Exercise 3.

The 'PRINT' statement should print the value of the expression (as a string) to the command line followed by '\n' or a new line character. Scala has the built-in println function for this.

Note that there are binary Boolean operators '!', '&&', '||' correponding to NOT, AND and OR respectively, as well as relational operators, but we only have integer types. The interpretation of these operators is not fixed; it will be fixed by your interpreter. In your notes below for Exercise 2, describe your interpretation and justify it. 

The 'IF' statement is such that if the expression is evaluated to (some interpretation of) true, then the statement is executed. If there is an 'ELSE' branch its statement is executed instead. The statement following an 'ELSE' should always be associated with the nearest 'IF' to the left to avoid ambiguity in evaluation.

The 'WHILE' statement dictates that while the expression evaluates to (some interpretation of) true the statement Stmt in the loop is executed.

The 'FOR' statement for concreteness is "for (name = expr1; expr2; expr3) stmt". Thus, the 'FOR' statement is such that name is assigned to the value of expr1. The statement stmt is then executed while name is less than or equal to the value of expr2. Thus the first two expressions must be evaluated before stmt can (or cannot) be executed. At the end of each iteration the value of name is incremented by the value of expr3.

## Exercise 1

-- Use this space to give a clear and concise description of the for loop statement.

## Exercise 2


-- Part a: use this space to give a clear and concise description of your implementation of the conditional expression.

-- Part b: use this space to give a brief summary of what changes you would make to have assignment expressions rather than statements. 

## Exercise 3

-- Use this space to describe clearly and concisely your implementation. You should justify your decisions, e.g. how did you deal with identifiers that are not assigned values and the interpretation of Boolean operators in the integers?

