
(* The type of tokens. *)

type token = 
  | TWeakdef
  | TVersion
  | TVNum of (string)
  | TRequires
  | TProvides
  | TOTHER of (string)
  | TIf
  | TIdent of (string)
  | TComma
  | TColon
  | TBang
  | TAlways
  | TAlias
  | TA_Shallow
  | TA_Pure
  | TA_Object_literal
  | TA_Mutator
  | TA_Mutable
  | TA_Const
  | RPARENT
  | LT
  | LPARENT
  | LE
  | GT
  | GE
  | EQ
  | EOL
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val annot: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Primitive.t)
