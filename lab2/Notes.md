# Lab 2

## Grammar

This is the grammar for the language Cdim+2 (for music fans):

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
Stmt -> Reference '=' Expr ';'
	| '{' Decls Stmts '}'
	| 'WHILE' '(' Expr ')' Stmt
	| 'IF' '(' Expr ')' Stmt 
	| 'IF' '(' Expr ')' Stmt 'ELSE' Stmt
	| 'PRINT' '(' Expr ')' ';'
	| ';'
Expr -> Expr Relop CalcExpr
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
	| Reference
	| 'NUMBER'
	| 'true'
	| 'false'
	| '-' Factor
	| "!" Factor
Reference -> 'IDENT'
	| 'IDENT' ExprList
ExprList -> '[' Expr ']'
	| '[' Expr ']' ExprList
Relop -> '<' | '<=' | '>' | '>=' | '==' | '!='
</pre>

Comments in the source language start with the characters '//' and end with a line-break (inclusive). There are no multi-line comments.

For clarity above, the one- and two-character tokens from the source are used instead of the tokens, e.g. '<' has token 'LESS'. 

'IDENT' is associated with the regex \[a-zA-Z\]\[a-zA-Z0-9\]\* and 'NUMBER' with 0|\[1-9\]\[0-9\]*

Note that an empty source file is an acceptable program indicated by Decls Stmts =>+ epsilon epsilon, where epsilon is the empty string. "Empty statements" corresponding to ; are also permitted. However, "empty declarations" are not permitted, thus if at the beginning of the program or block, there is ';' it will be parsed as a statement. Any attempted declarations after this will trigger a parser error since it will be expecting a statement. 

For "block" statements notice the grammar is '{' Decls Stmts '}'. Blocks can be nested and declarations can be made at the beginning of a block. The indentifiers declared in the block are within the scope of the block. Shadowing is permitted thus {int i; {bool i;}} is valid. Reminder that empty blocks such as {} are permitted along with {;;}. Again, because empty declarations are not permitted {;int i;} is not valid. 

You are not permitted to declare an identifier more than once within the same scope. As discussed you can declare an identifier in a different scope. 

As indicated by the non-terminal Type above, there are two basic types: int and bool. There are also arrays of int and bool. These are declared by appending square brackets with numbers in after the identifier, e.g. int a\[1\]\[2\], which declares a two-dimensional array, i.e. an array called "a", which is an array with one element which is itself an array of two elements (which are integers in this case). The permitted values of dimensions of arrays are strictly positive, i.e. 0 is not permitted.

Within statements the syntax a\[1\]\[2\] refers to array-based expressions. That is, the expression a\[1\]\[2\] corresponds to a value of a type, which must be a basic type. So for this array a, a\[1\] is not a valid expression and cannot appear in a larger expression. Consequently, for assignments, the type of the expression on the left-hand-side must be a basic type, i.e. we cannot have arrays on the left-hand-side, but single array elements. Consequently, the right-hand-side must also be a basic type. 

The arithmetic operations must have operands that are both integers, the Boolean operations must have operands that are both bools. The relational operators must have types that match, and the type of an expression expr1 Relop expr2 is bool where expr1 and expr2 are expressions. 

For if and while statements the condition expression must be of type bool. The relevant statements are then executed if this expression evaluates to true. 

## Exercise 1

## Exercise 2



