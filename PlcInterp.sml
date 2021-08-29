(* PlcInterp *)
use "PlcChecker.sml";
use "Environ.sml";
use "Absyn.sml";

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

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
            "!" => ( case value of BoolV b => BoolV (not b)
                                 | _ => raise Impossible )
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
          | "-" => ( case value of IntV n => IntV (~ n)
                                 | _ => raise Impossible)
          | _ => raise Impossible
      end
    ;




    (*| Prim2 (operator, exp1, exp2) =>
      let 
        val ty1 = teval exp1 env
        val ty2 = teval exp2 env
      in
        case operator of
            "&&" => if ty1 = BoolT andalso ty2 = BoolT then BoolT
                    else raise UnknownType
          | "+" => if ty1 = IntT andalso ty2 = IntT then IntT
                   else raise UnknownType
          | "-" => if ty1 = IntT andalso ty2 = IntT then IntT
                   else raise UnknownType
          | "*" => if ty1 = IntT andalso ty2 = IntT then IntT
                   else raise UnknownType
          | "/" => if ty1 = IntT andalso ty2 = IntT then IntT
                   else raise UnknownType
          | "=" => if ty1 = ty2 then
                      if IsEqType ty1 then BoolT
                      else raise CallTypeMisM
                   else raise NotEqTypes
          | "!=" => if ty1 = ty2 then
                      if IsEqType ty1 then BoolT
                      else raise CallTypeMisM
                    else raise NotEqTypes
          | "<" => if ty1 = IntT andalso ty2 = IntT then BoolT
                   else raise UnknownType
          | "<=" => if ty1 = IntT andalso ty2 = IntT then BoolT
                    else raise UnknownType
          | "::" => if ty2 = SeqT(ty1) then SeqT(ty1)
                    else raise UnknownType
          | ";" => ty2
          | _ => raise UnknownType
      end
    | If (cond, iftrue, iffalse) =>
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