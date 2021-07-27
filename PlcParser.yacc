%%

%name PlcParser

%pos int

%term VAR
    | BOOL | NIL | INTT | FUN
    | PLUS | MINUS | MULT | DIV
    | TRUE | FALSE | EXCL
    | AND | LESS | LESSEQ | EQ | NOTEQ
    | LBRACK | RBRACK | DBRACK | UNDER
    | LBRACE | RBRACE | LPAR | RPAR 
    | DCOLON | ISE | HD | TL
    | PRINT | SEMIC | ARROW | DARROW | VBAR
    | IF | THEN | ELSE
    | FN | REC | MATCH | WITH | END
    | INTC of int | NAME of string
    | EOF

%nonterm Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr | Const of expr

%right SEMIC DARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NOTEQ
%left LESS LESSEQ
%right DCOLON
%left PLUS MINUS
%left MULT DIV
%nonassoc EXCL HD TL ISE PRINT NAME
%left LBRACK

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
	   | Decl (Decl)
