%%

%name PlcParser

%pos int

%term VAR
    | BOOL | NIL | INTT | FUN
    | PLUS | MINUS | MULT | DIV
    | TRUE | FALSE | EXCL
    | AND | LESS | LESSEQ | EQ | NOTEQ
    | LBRACK | RBRACK | UNDER
    | LBRACE | RBRACE | LPAR | RPAR 
    | COLON | DCOLON | COMMA
    | ISE | HD | TL | PRINT 
    | SEMIC | ARROW | DARROW | VBAR
    | IF | THEN | ELSE
    | FN | REC | MATCH | WITH | END
    | INTC of int | NAME of string
    | EOF

%nonterm Prog of expr 
        | Decl of expr
        | Expr of expr 
        | AtomExpr of expr 
        | AppExpr of expr 
        | Const of expr 
        | Comps of expr list 
        | MatchExpr of (expr option * expr) list  
        | CondExpr of expr option 
        | Args of (plcType * string) list 
        | Params of (plcType * string) list 
        | TypedVar of (plcType * string) 
        | Type of plcType 
        | AtomType of plcType 
        | TypeS of plcType list

%right SEMIC ARROW
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

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
     | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
     | FUN REC NAME Args COLON Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))
    
Expr : AtomExpr (AtomExpr)
     | AppExpr (AppExpr)
     | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
     | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
     | EXCL Expr (Prim1("!", Expr))
     | MINUS Expr (Prim1("-", Expr))
     | HD Expr (Prim1("hd", Expr))
     | TL Expr (Prim1("tl", Expr))
     | ISE Expr (Prim1("ise", Expr))
     | PRINT Expr (Prim1("print", Expr))
     | Expr AND Expr (Prim2("&&", Expr1, Expr2))
     | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
     | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
     | Expr MULT Expr (Prim2("*", Expr1, Expr2))
     | Expr DIV Expr (Prim2("/", Expr1, Expr2))
     | Expr EQ Expr (Prim2("=", Expr1, Expr2))
     | Expr NOTEQ Expr (Prim2("!=", Expr1, Expr2))
     | Expr LESS Expr (Prim2("<", Expr1, Expr2))
     | Expr LESSEQ Expr (Prim2("<=", Expr1, Expr2))
     | Expr DCOLON Expr (Prim2("::", Expr1, Expr2))
     | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
     | Expr LBRACK INTC RBRACK (Item(INTC, Expr))

AtomExpr : Const (Const)
         | NAME (Var(NAME))
         | LBRACE Prog RBRACE (Prog)
         | LPAR Expr RPAR (Expr)
         | LPAR Comps RPAR (List(Comps))
         | FN Args DARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
        | AppExpr AtomExpr (Call (AppExpr, AtomExpr))

Const : TRUE (ConB(true))
      | FALSE (ConB(false))
      | INTC (ConI(INTC))
      | LPAR RPAR (List [])
      | LPAR Type LBRACK RBRACK RPAR (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
      | Expr COMMA Comps (Expr::Comps)

MatchExpr : END ([])
          | VBAR CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME(Expr))
         | UNDER (NONE)

Args : LPAR RPAR ([])
     | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar::[])
       | TypedVar COMMA Params (TypedVar::Params)

TypedVar : Type NAME ((Type, NAME))

Type : AtomType (AtomType)
     | LPAR TypeS RPAR (ListT(TypeS))
     | LBRACK Type RBRACK (SeqT(Type))
     | Type ARROW Type (FunT(Type1, Type2))

AtomType : NIL (ListT([]))
     | BOOL (BoolT)
     | INTT (IntT)
     | LPAR Type RPAR (Type)
     
TypeS : Type COMMA Type (Type1::Type2::[])
      | Type COMMA TypeS (Type::TypeS)