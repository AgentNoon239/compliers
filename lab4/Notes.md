# Lab 4

## Grammar

This is the grammar for the language Cdim7 (for the week of term):

<pre>
Program -> Decls
Decls -> Decl Decls
	| epsilon
Decl -> Var
	| 'FN' Name '(' Params ')' '{' Block '}'
Names -> Name ',' Names
Name -> 'IDENT'
Type -> int | bool
Params -> Param ',' Params
    | Param
    | epsilon
Param -> Type Name
Block -> Vars Stmts
Vars -> Var ; Vars
	| epsilon
Var -> Type Names ';'
Stmts -> Stmt Stmts
	| Stmt
	| epsilon
Stmt -> Reference '=' OrTerm ';'
	| '{' Stmts '}'
	| 'WHILE' '(' OrTerm ')' Stmt
	| 'IF' '(' OrTerm ')' Stmt 
	| 'IF' '(' OrTerm ')' Stmt 'ELSE' Stmt
	| 'PRINT' '(' OrTerm ')' ';'
	| 'RETURN' CalcExpr ';'
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
	| 'IDENT' '(' ArgList ')'
ArgList -> Arg , ArgList
	| Arg
Arg -> OrTerm
Relop -> '<' | '<=' | '>' | '>=' | '==' | '!='
</pre>

This language has the same basic foundation as Cdim from the first practical sheet. Except there are two types: integers and bools. There are no arrays. 

However, we now have functions. A program in Cdim7 consists of a sequence of declarations of either global variables (with the int or bool keyword) or functions using the fn keyword. When a function is declared, it of the form

fn foo(int a, bool b, int c) {
	...
}

where a, b and c are the parameters. The braces denote the body of the function. The body of the function may have declarations of simple variables at the top (it may not have a declaration of a function inside of it). Below the declarations is a list of statements. 

Note that, in the grammar, each parameter must be comma separated with a type in front from every other, just as in C. That is, in the declaration foo(int a, b) is not valid but foo(int a, int b) is.

Functions have a single return type: integer. That is, all functions return integers. All functions must return. Thus an empty function body is not possible, unlike as suggested by the grammar above.

Functions may be called and thus we have function expressions, e.g. foo(2, true, 3). Because functions return integers, we can only do integer arithmetic with these function expressions. 

Functions may have expressions as inputs, as long as they evaluate to values of the correct type, as given by the type of the parameters in the declaration of the function. The argument expressions are evaluated from right to left. 

All global variables and functions must be declared (and the latter initialised) before they can be used. This prevents the language from having mutual recursion. However, it is capable of some recursion.

IMPORTANTLY, every program must have a function called "main" that takes no parameters. This is the entry point for the program. As with all programs, it needs to return an integer, by convention it can return 0 at the end of its body. The main function may have local variables, declared within its body. 

Finally, local variables in functions are lexically scoped to that function. But there is no block scoping otherwise. That is, local variables can be declared within a function, but any further blocks cannot have declarations. This is indicated by the grammar above where Stmt -> '{' Stmts '}' just permits lists of statements and not further declarations.

## Exercise 1


## Exercise 2


## Exercise 3