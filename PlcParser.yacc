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
    | COLON | DCOLON | COMMA
    | ISE | HD | TL | PRINT 
    | SEMIC | ARROW | DARROW | VBAR
    | IF | THEN | ELSE
    | FN | REC | MATCH | WITH | END
    | INTC of int | NAME of string
    | EOF

%nonterm Prog of expr | Decl | Expr of expr | AtomExpr of expr | AppExpr of expr | Const of expr | Comps of expr list | MatchExpr of (expr option * expr) list  | CondExpr of expr list | Args of (plcType * string) list | Params of (plcType * string) list | TypedVar of (plcType * string) | Type of plcType | AtomType of plcType | TypeS of plcType list

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

Decl : VAR NAME EQ Expr ()
     | FUN NAME Args EQ Expr ()
     | FUN REC NAME Args COLON Type EQ Expr ()
    
Expr : AtomExpr ()
     | AppExpr ()
     | IF Expr THEN Expr ELSE Expr ()
     | MATCH Expr WITH MatchExpr ()
     | EXCL Expr ()
     | MINUS Expr ()
     | HD Expr ()
     | TL Expr ()
     | ISE Expr ()
     | PRINT Expr ()
     | Expr AND Expr ()
     | Expr PLUS Expr ()
     | Expr MINUS Expr ()
     | Expr MULT Expr ()
     | Expr DIV Expr ()
     | Expr EQ Expr ()
     | Expr NOTEQ Expr ()
     | Expr LESS Expr ()
     | Expr LESSEQ Expr ()
     | Expr DCOLON Expr ()
     | Expr EXCL Expr ()
     | Expr LBRACK INTC RBRACK ()

AtomExpr : Const ()
         | NAME ()
         | LBRACE Prog RBRACE ()
         | LPAR Expr RPAR ()
         | LPAR Comps RPAR ()
         | FN Args DARROW Expr END ()

AppExpr : AtomExpr AtomExpr ()
        | AppExpr AtomExpr ()

Const : TRUE ()
      | FALSE ()
      | INTC ()
      | LPAR RPAR ()
      | LPAR Type LBRACK RBRACK RPAR ()

Comps : Expr COMMA Expr ()
      | Expr COMMA Comps ()

MatchExpr : END ()
          | VBAR CondExpr ARROW Expr MatchExpr ()

CondExpr : Expr ()
         | UNDER ()

Args : LPAR RPAR ()
     | LPAR Params RPAR ()

Params : TypedVar ()
       | TypedVar COMMA Params ()

TypedVar : Type NAME ()

Type : AtomType ()
     | LPAR TypeS RPAR ()
     | LBRACK Type RBRACK ()
     | Type ARROW Type ()

TypeS : Type COMMA Type ()
      | Type COMMA TypeS ()