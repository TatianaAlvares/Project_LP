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
    | If (cond, iftrue, iffalse) =>
      let
        val value = eval cond env
      in
        if getBool value then eval iftrue env else eval iffalse env
      end
    | Match (name, cases) =>
      if cases = [] then raise ValueNotFoundInMatch
      else 
        let
          fun search (opt, wtd : expr) =
            if Option.isSome opt andalso name = Option.valOf opt then true
            else if not(Option.isSome opt) then true
            else false
          val opitem = List.find search cases
        in
          if Option.isSome opitem then eval (#2(Option.valOf(opitem))) env
          else raise ValueNotFoundInMatch
        end
    | Call (fname, arg) => 
        let
          val funclos = eval fname env
          val funarg = eval arg env
        in
          case funclos of 
              Clos("", str, exp, cenv) => eval exp ((str, funarg)::cenv)
            | Clos(n, str, exp, cenv) => eval exp ((str, funarg)::(n, funclos)::cenv)
            | _ => raise NotAFunc
        end
    | List [] => ListV []
    | List l => 
        if List.length(l) >= 1 then
          let
            fun maplist x = eval x env;
          in
            ListV(map maplist l)
          end
        else raise UnknownType
    | Item (num, lexp)=>
        let
          val l = eval lexp env
        in
          case l of 
              ListV [] => raise Impossible
            | ListV l =>
                if num > 0 andalso num <= List.length(l) then List.nth(l, num - 1)
                else raise Impossible
            | _ => raise Impossible
        end
    | Anon (ty, str, exp) => Clos("", str, exp, env)
  ;

  