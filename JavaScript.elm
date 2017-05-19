module JavaScript exposing (..)

{-| Int
-}


type IntType
    = IntType


type alias Int =
    { str : String, t : IntType }


int : String -> Int
int str =
    { str = str, t = IntType }


{-| Str
-}
type StrType
    = StrType


type alias Str =
    { str : String, t : StrType }


str : String -> Str
str str =
    { str = str, t = StrType }


{-| Bool
-}
type BoolType
    = BoolType


type alias Bool =
    { str : String, t : BoolType }


bool : String -> Bool
bool str =
    { str = str, t = BoolType }


{-| Date
-}
type DateType
    = DateType


type alias Date =
    { str : String, t : DateType }


date : String -> Date
date str =
    { str = str, t = DateType }


{-| Void
-}
type VoidType
    = VoidType


type alias Void =
    { str : String, t : VoidType }


void : String -> Void
void str =
    { str = str, t = VoidType }


type alias Any a =
    { str : String, t : a }


type alias Stmt =
    { str : String }


stmt : Any a -> Stmt
stmt expr =
    { str = expr.str ++ ";" }


type IfStmtType a
    = IfStmtType a


type alias IfStmt a =
    { str : String, t : IfStmtType a }


if_ : Bool -> Block a -> Block a -> Stmt
if_ cond bodyT bodyF =
    { str =
        "if ("
            ++ cond.str
            ++ ")"
            ++ bodyT.str
            ++ "else"
            ++ bodyF.str
    }



--else_ : IfStmt a -> Block a -> Block a
--else_ body =
--     "else " ++ body.str


type RetType a
    = RetType a


type alias RetStmt a =
    { str : String, t : RetType a }


return : Any a -> RetStmt a
return expr =
    { str =
        "return "
            ++ expr.str
            ++ ";"
    , t = RetType expr.t
    }


type BlockType a
    = BlockType a


type alias Block a =
    { str : String, t : BlockType a }


block : List Stmt -> RetStmt b -> Block b
block stmts retStmt =
    { str =
        "{"
            ++ String.join " " (List.append (List.map .str stmts) [ retStmt.str ])
            ++ "}"
    , t = BlockType ((\(RetType t) -> t) retStmt.t)
    }


chainable : { a | str : String } -> String -> String
chainable { str } methodCall =
    String.concat [ "(function(e) { e", methodCall, "; return e})(", str, ")" ]


var :
    String
    -> Any a
    -> ( Any a, Any a )
var name expr =
    ( { str = name, t = expr.t }
    , { str =
            String.join " " [ "var", name, "=", expr.str ]
                ++ "\n"
      , t = expr.t
      }
    )


type ArgsType a
    = ArgsType a


type alias Args a =
    { str : String, t : ArgsType a }


args : a -> Args a
args tuple =
    { str = toString tuple, t = ArgsType tuple }


type FuncType argsT retT
    = FuncType argsT retT


type alias Func argsT retT =
    { str : String, t : FuncType argsT retT }


func : Args argsT -> Block retT -> Func argsT retT
func args body =
    { str =
        "function"
            ++ args.str
            ++ body.str
    , t =
        FuncType
            ((\(ArgsType t) -> t) args.t)
            ((\(BlockType t) -> t) body.t)
    }
