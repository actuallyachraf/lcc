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
  | Eq
  | Neq

(* Assignment Operations*)
type assign_op = Assign

(* Identifiers *)
type ident = Ident of string

(* Expressions *)
type expr =
  | Const of const
  | Var of ident
  | BinOp of bin_op * expr * expr
  | AssignOp of assign_op * ident * expr

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
type block_item = Statement of statement

(* Block is a list of block items *)
and block = block_item list

(* Statements *)
and statement =
  | Block of block
  | Expr of expr option
  | ReturnVal of expr
  | Return

(* Function Parameters *)
type fun_param = Param of type_def * ident

(* Function Body *)
type fun_body = Body of statement list

(* Function Declaration *)
type fun_decl = FunDecl of type_def * ident * fun_param list * fun_body

(* Program *)
type program = Prog of fun_decl
