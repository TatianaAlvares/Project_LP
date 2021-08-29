(* PlcInterp *)
use "PlcChecker.sml";
use "Environ.sml";
use "Absyn.sml";

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun getInt (IntV n) = n
	| getInt _ = raise Impossible; 

fun getBool (BoolV b) = b
	| getBool _ = raise Impossible 

fun eval (e:expr) (env:plcVal env) : plcVal =
  case e of
      ConI n => IntV n
    | ConB b => BoolV b
    | ESeq seq => SeqV []
    | Var str => lookup env str
    | Let (name, exp1, exp2) =>
      let
        val value = eval exp1 env
      in
        eval exp2 ((name, value)::env)
      end
    | Letrec (fname, atype, aname, rtype, ex1, ex2) =>
      let 
        val funct = (fname, Clos(fname, aname, ex1, env))
      in
        eval ex2 (funct::env)
      end
    | Prim1 (operator, exp) =>
      let
        val value = eval exp env
      in
        case operator of 
            "!" => BoolV(not(getBool value))
          | "hd" => ( case value of
              SeqV [] => raise HDEmptySeq
            | SeqV seq => hd seq
            | _ => raise Impossible )
          | "tl" => ( case value of
              SeqV [] => raise TLEmptySeq
            | SeqV seq => SeqV (tl seq)
            | _ => raise Impossible )
          | "ise" => ( case value of
              SeqV [] => BoolV true
            | SeqV seq => BoolV false
            | _ => raise Impossible )
          | "print" => 
            let 
              val printing = print(val2string(value) ^ "\n")
            in
              ListV []
            end
          | "-" => IntV(~(getInt value))
          | _ => raise Impossible
      end
    | Prim2 (operator, exp1, exp2) =>
      let 
        val value1 = eval exp1 env
        val value2 = eval exp2 env
      in
        case operator of
            "&&" => BoolV ((getBool value1) andalso (getBool value2))
          | "+" => IntV ((getInt value1) + (getInt value2))
          | "-" => IntV ((getInt value1) - (getInt value2))
          | "*" => IntV ((getInt value1) * (getInt value2))
          | "/" => IntV ((getInt value1) div (getInt value2))
          | "=" => BoolV (value1 = value2)
          | "!=" => BoolV (not (value1 = value2))
          | "<" => BoolV ((getInt value1) < (getInt value2))
          | "<=" => BoolV ((getInt value1) <= (getInt value2))
          | "::" => ( case (value1, value2) of
                      (SeqV a, SeqV b) => SeqV (a @ b)
                    | (_, SeqV b) => SeqV (value1::b)
                    | _ => raise Impossible
            )
          | ";" => value2
          | _ => raise Impossible
      end
    (*| If (cond, iftrue, iffalse) =>
      let
        val tcon = teval cond env
        val ift = teval iftrue env
        val iff = teval iffalse env
      in
        if tcon = BoolT then
          if ift = iff then ift
          else raise DiffBrTypes
        else raise IfCondNotBool
      end
    | Match (name, cases) =>
      let
        val tyname = teval name env
        val tyheadcase = teval (#2(hd(cases))) env
        fun search (opt, wdo) =
          if isSome opt then
            tyname = teval (valOf(opt)) env
          else true
        fun rettype (opt, wdo) = tyheadcase = teval wdo env
      in
        if cases = [] then raise NoMatchResults
        else if List.all search cases then
          if List.all rettype cases then tyheadcase
          else raise MatchResTypeDiff
        else raise MatchCondTypesDiff
      end
    | Call (fname, arg) => 
        let
          val tyname = teval fname env
          val tyarg = teval arg env
        in
          case tyname of 
              FunT(entry, return) =>
                if tyarg = entry then return
                else raise CallTypeMisM
            | _ => raise NotFunc
        end
    | List [] => ListT []
    | List l => 
        if List.length(l) >= 1 then
          let
            fun maplist x = teval x env;
          in
            ListT(map maplist l)
          end
        else raise UnknownType
    | Item (num, lexp)=>
        let
          val tlist = teval lexp env
        in
          case tlist of 
              ListT [] => raise ListOutOfRange
            | ListT l =>
                if num >= 0 andalso num < List.length(l) then List.nth(l, num)
                else raise ListOutOfRange
            | _ => raise OpNonList
        end
    | Anon (ty, str, exp) =>
        let
          val texp = teval exp ((str, ty)::env)
        in
          FunT(ty,texp)
        end*)
  ;