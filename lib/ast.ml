(* ast.ml implements node types in the AST *)

(* Constant Literals*)
type const =
  | Int of int
  | Char of char
  | String of string

(* Type Definitions *)
type type_def =
  | IntType
  | CharType

(* Binary Operations *)
type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Neq
  | And
  | Or
  | BitAnd
  | BitOr
  | Xor
  | Shl
  | Shr

(* Assignment Operations*)
type assign_op = Assign

(* Unitary ops *)
type un_op =
  | Neg
  | Pos
  | Complement
  | Not

(* Identifiers *)
type ident = Ident of string

(* Expressions *)
type expr =
  | Const of const
  | Var of ident
  | UnOp of un_op * expr
  | BinOp of bin_op * expr * expr
  | TernOp of expr * expr * expr
  | AssignOp of assign_op * ident * expr
  | FunCall of ident * expr list

(* C storage classes *)
type storage_class =
  | Static
  | Volatile
  | Extern
  | Nothing

(* Declarations *)
type declaration =
  { var_type : type_def
  ; var_name : ident
  ; init : expr option
  ; storage_class : storage_class
  }

(* Scoped Blocks *)
type block_item =
  | Statement of statement
  | Decl of declaration

(* Block is a list of block items *)
and block = block_item list

(* Statements *)
and statement =
  | Block of block
  | If of
      { cond : expr
      ; if_body : statement
      ; else_body : statement option
      }
  | Expr of expr option
  | For of
      { init : expr option
      ; cond : expr
      ; post : expr option
      ; body : statement
      }
  | ForDecl of
      { init : declaration
      ; cond : expr
      ; post : expr option
      ; body : statement
      }
  | While of
      { cond : expr
      ; body : statement
      }
  | DoWhile of
      { body : statement
      ; cond : expr
      }
  | ReturnVal of expr
  | Return
  | Break
  | Continue

(* function parameters *)
type fun_param = Param of type_def * ident

(* function declaration *)
type fun_declaration =
  { fun_type : type_def
  ; name : ident
  ; storage_class : storage_class
  ; params : fun_param list
  ; body : block option
  }

(* code *)
type top_level =
  | Function of fun_declaration
  | GlobalVar of declaration

type prog = Prog of top_level list
