# Lab 3

## Grammar

This is the grammar for the language Cdim+2:

<pre>
Program -> Decls Stmts 'EOF'
Decls -> Decl Decls
	| epsilon
Decl -> Type Names ';'
Names -> Name ',' Names
Name -> 'IDENT'
	| 'IDENT' Bounds
Bounds -> '[' NUMBER ']'
	| '[' NUMBER ']' Bounds
Type -> int | bool
Stmts -> Stmt Stmts
	| Stmt
	| epsilon
Stmt -> Reference '=' OrTerm ';'
	| '{' Decls Stmts '}'
	| 'WHILE' '(' OrTerm ')' Stmt
	| 'IF' '(' OrTerm ')' Stmt 
	| 'IF' '(' OrTerm ')' Stmt 'ELSE' Stmt
	| 'PRINT' '(' OrTerm ')' ';'
	| ';'
OrTerm -> OrTerm '||' AndTerm
	| AndTerm
AndTerm -> AndTerm '&&' Expr
	| Expr
Expr -> Expr Relop CalcExpr
	| CalcExpr
CalcExpr -> CalcExpr '-' Term
	| CalcExpr '+' Term
	| Term
Term -> Term '\*' Factor
	| Term '/' Factor
	| Factor
Factor -> '(' OrTerm ')'
	| Reference
	| 'NUMBER'
	| 'true'
	| 'false'
	| '-' Factor
	| "!" Factor
Reference -> 'IDENT'
	| 'IDENT' ExprList
ExprList -> '[' CalcExpr ']'
	| '[' CalcExpr ']' ExprList
Relop -> '<' | '<=' | '>' | '>=' | '==' | '!='
</pre>

The description of the language is the same as in Lab 2. 

We should add some implementation choices, especially about order of evaluation of expressions. This will determine the implementation in the code generator, so that Keiko code instructions are in the correct order.

For an assignment, the RHS should be evaluated first. To evaluate the value stored in an array element the expressions in the subscripts should be evaluated first, going from left to right.

Then if the left-hand-side consists of an array element, the expressions inside the subscripts should be evaluated next, from left to right. Then the assignment should take place of the RHS's value being assigned to the memory location dictated by the evaluated values in the subscripts. 

## Exercise 1

## Exercise 2

## Exercise 3

## Exercise 4



