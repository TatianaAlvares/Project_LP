open PlcFrontEnd;

fun run (e:expr) = 
  let
    val typeEnv = [] : plcType env
    val valueEnv = [] : plcVal env
  in
    let
      val exprCheck = teval e typeEnv
      val exprInter = eval e valueEnv
    in
      (val2string exprInter) ^ " : " ^ (type2string exprCheck)
    end
  end
  handle
      EmptySeq => "*** Execution Error: Empty input sequence ***"
    | UnknownType => "*** Execution Error: Invalid data type ***"
    | NotEqTypes => "*** Execution Error: Invalid comparison between data types ***"
    | WrongRetType => "*** Execution Error: Return type is not within the function domain ***"
    | DiffBrTypes => "*** Execution Error: Return types of If branches must agree ***"
    | IfCondNotBool => "*** Execution Error: Condition expression is not boolean value ***"
    | NoMatchResults => "*** Execution Error: No Match results found ***"
    | MatchResTypeDiff => "*** Execution Error: Different result types in Match expression ***"
    | MatchCondTypesDiff => "*** Execution Error: Match expression type differs from Match options ***"
    | CallTypeMisM => "*** Execution Error: Argument type is not within the function domain ***"
    | NotFunc => "*** Execution Error: Atempting to call a non functional expression ***"
    | ListOutOfRange => "*** Execution Error: Atempting acess of invalid list position ***"
    | OpNonList => "*** Execution Error: Expression is not a list ***"
    | Impossible => "*** Execution Error: Impossible resolution ***"
    | HDEmptySeq => "*** Execution Error: Atempting to acess head of an empty sequence***"
    | TLEmptySeq => "*** Execution Error: Atempting to acess tail of an empty sequence***"
    | ValueNotFoundInMatch => "*** Execution Error: No matching value found ***"
    | NotAFunc => "*** Execution Error: Expression is not a function ***"
    | SymbolNotFound => "*** Execution Error: Symbol not found ***"