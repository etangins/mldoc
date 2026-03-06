
(* The type of tokens. *)

type token = Js_token.token

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val standalone_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Javascript.expression)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Lexing.position * (Javascript.statement * Javascript.location)) list)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val standalone_expression: Lexing.position -> (Javascript.expression) MenhirInterpreter.checkpoint
  
  val program: Lexing.position -> ((Lexing.position * (Javascript.statement * Javascript.location)) list) MenhirInterpreter.checkpoint
  
end

(* The parse tables. *)

(* Warning: this submodule is undocumented. In the future,
   its type could change, or it could disappear altogether. *)

module Tables : MenhirLib.TableFormat.TABLES
  with type token = token
