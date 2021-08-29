(* PlcChecker *)
use "Environ.sml";
use "Absyn.sml";

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList
exception Fuck

fun IsEqType (ty:plcType) : bool =
  case ty of IntT => true
             | BoolT => true
             | FunT (_) => false
             | ListT [] => true
             | ListT tlist => (List.all (IsEqType) tlist)
             | SeqT tseq => (IsEqType tseq);

fun IsSeq (ty:plcType) : bool =
  case ty of SeqT(_) => true
	           | _ => false;

fun teval (ex:expr) (env: plcType env) : plcType =
  case ex of
      ConI n => IntT
    | ConB b => BoolT
    | ESeq seq => 
      if IsSeq(seq) then seq
      else raise EmptySeq (*If necessário?*)
    | Var str => lookup env str
    | Let (name, exp1, exp2) =>
      let
        val ty1 = teval exp1 env
        val ty2 = teval exp2 ((name, ty1)::env)
      in
        ty2
      end 
    | Letrec (fname, atype, aname, rtype, ex1, ex2) =>
      let
        val aenv = (aname, atype)
        val fenv = (fname, FunT(atype, rtype))
        val ty1 = teval ex1 (fenv::aenv::env)
      in
        if (ty1 = rtype) then teval ex1 (fenv::env)
        else raise WrongRetType
      end
    | Prim1 (operator, exp) =>
      let
        val ty = teval exp env
      in
        case operator of 
            "!" => if ty = BoolT then BoolT 
                    else raise UnknownType
          | "hd" => let in case ty of
              SeqT(IntT) => IntT
            | SeqT(BoolT) => BoolT
            | SeqT(FunT(a,b)) => FunT(a,b)
            | SeqT(ListT []) => raise EmptySeq
            | SeqT(ListT l) => ListT l
            | SeqT(SeqT(s)) => SeqT s
            | _ => raise UnknownType
            end
          | "tl" => let in case ty of 
              SeqT(ListT []) => raise EmptySeq
            | SeqT(_) => ty
            | _ => raise UnknownType
            end (*SeqT ou ListT?? Pode SeqT(seq)??*)
          | "ise" => let in case ty of
              SeqT(_) => BoolT
            | _ => raise UnknownType
            end
          | "print" => ListT []
          | "-" => if ty = IntT then IntT
                else raise UnknownType
          | _ => raise UnknownType
      end
    | Prim2 (operator, exp1, exp2) =>
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
        end
    ;

          