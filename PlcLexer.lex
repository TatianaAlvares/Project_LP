(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) = 
    case s of
        "var" => VAR (lpos,rpos)
      | "Bool" => BOOL (lpos,rpos)
      | "Nil" => NIL (lpos,rpos)
      | "Int" => INTT (lpos,rpos)
      | "fun" => FUN (lpos,rpos)
      | "true" => TRUE (lpos,rpos)
      | "false" => FALSE (lpos,rpos)
      | "_" => UNDER (lpos,rpos)
      | "ise" => ISE (lpos,rpos)
      | "hd" => HD (lpos,rpos)
      | "tl" => TL (lpos,rpos)
      | "print" => PRINT (lpos,rpos)
      | "if" => IF (lpos,rpos)
      | "then" => THEN (lpos,rpos)
      | "else" => ELSE (lpos,rpos)
      | "fn" => FN (lpos,rpos)
      | "rec" => REC (lpos,rpos)
      | "match" => MATCH (lpos,rpos)
      | "with" => WITH (lpos,rpos)
      | "end" => END (lpos,rpos)
      | _ => NAME (s,lpos,rpos)


(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

fun strToInt s =
  case Int.fromString s of
    SOME i => i
   |NONE => raise Fail ("Could not convert string to int")

(* Initialize the lexer. *)
fun init() = ()
%%

%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9]+;
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;

%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (INTC(strToInt(yytext), yypos, yypos));
{identifier}+ => (keyword(yytext, yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULT(yypos, yypos));
"/" => (DIV(yypos, yypos));
"!" => (EXCL(yypos, yypos));
"&&" => (AND(yypos, yypos));
"<" => (LESS(yypos, yypos));
"<=" => (LESSEQ(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (NOTEQ(yypos, yypos));
"[" => (LBRACK(yypos, yypos));
"]" => (RBRACK(yypos, yypos));
"{" => (LBRACE(yypos, yypos));
"}" => (RBRACE(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
":" => (COLON(yypos, yypos));
"::" => (DCOLON(yypos, yypos));
"," => (COMMA(yypos, yypos));
";" => (SEMIC(yypos, yypos));
"->" => (ARROW(yypos, yypos));
"=>" => (DARROW(yypos, yypos));
"|" => (VBAR(yypos, yypos));
. => (error("\n***Lexer error: bad character***\n"); raise Fail("Lexer error: bad character "^ yytext));