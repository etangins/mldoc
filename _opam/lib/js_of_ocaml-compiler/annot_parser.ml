
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TWeakdef
    | TVersion
    | TVNum of 
# 22 "compiler/lib/annot_parser.mly"
      (string)
# 17 "compiler/lib/annot_parser.ml"
  
    | TRequires
    | TProvides
    | TOTHER of 
# 24 "compiler/lib/annot_parser.mly"
      (string)
# 24 "compiler/lib/annot_parser.ml"
  
    | TIf
    | TIdent of 
# 22 "compiler/lib/annot_parser.mly"
      (string)
# 30 "compiler/lib/annot_parser.ml"
  
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
  
end

include MenhirBasics

type ('s, 'r) _menhir_state = 
  | MenhirState01 : ('s, _menhir_box_annot) _menhir_state
    (** State 01.
        Stack shape : <empty>.
        Start symbol: annot. *)

  | MenhirState07 : ('s, _menhir_box_annot) _menhir_state
    (** State 07.
        Stack shape : <empty>.
        Start symbol: annot. *)

  | MenhirState14 : (('s, _menhir_box_annot) _menhir_cell1_version, _menhir_box_annot) _menhir_state
    (** State 14.
        Stack shape : version.
        Start symbol: annot. *)

  | MenhirState18 : (('s, _menhir_box_annot) _menhir_cell1_separated_nonempty_list_TComma_version_, _menhir_box_annot) _menhir_state
    (** State 18.
        Stack shape : separated_nonempty_list(TComma,version).
        Start symbol: annot. *)

  | MenhirState21 : ('s, _menhir_box_annot) _menhir_state
    (** State 21.
        Stack shape : <empty>.
        Start symbol: annot. *)

  | MenhirState23 : (('s, _menhir_box_annot) _menhir_cell1_TIdent, _menhir_box_annot) _menhir_state
    (** State 23.
        Stack shape : TIdent.
        Start symbol: annot. *)

  | MenhirState25 : (('s, _menhir_box_annot) _menhir_cell1_separated_nonempty_list_TComma_TIdent_, _menhir_box_annot) _menhir_state
    (** State 25.
        Stack shape : separated_nonempty_list(TComma,TIdent).
        Start symbol: annot. *)

  | MenhirState36 : ('s _menhir_cell0_TIdent _menhir_cell0_option_prim_annot_, _menhir_box_annot) _menhir_state
    (** State 36.
        Stack shape : TIdent option(prim_annot).
        Start symbol: annot. *)

  | MenhirState45 : (('s, _menhir_box_annot) _menhir_cell1_arg_annot, _menhir_box_annot) _menhir_state
    (** State 45.
        Stack shape : arg_annot.
        Start symbol: annot. *)

  | MenhirState47 : ('s _menhir_cell0_TIdent _menhir_cell0_option_prim_annot_ _menhir_cell0_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__, _menhir_box_annot) _menhir_state
    (** State 47.
        Stack shape : TIdent option(prim_annot) option(delimited(LPARENT,separated_list(TComma,arg_annot),RPARENT)).
        Start symbol: annot. *)

  | MenhirState51 : ('s _menhir_cell0_TIdent, _menhir_box_annot) _menhir_state
    (** State 51.
        Stack shape : TIdent.
        Start symbol: annot. *)

  | MenhirState54 : ('s _menhir_cell0_TIdent, _menhir_box_annot) _menhir_state
    (** State 54.
        Stack shape : TIdent.
        Start symbol: annot. *)

  | MenhirState56 : ('s, _menhir_box_annot) _menhir_state
    (** State 56.
        Stack shape : <empty>.
        Start symbol: annot. *)

  | MenhirState60 : ('s _menhir_cell0_TIdent, _menhir_box_annot) _menhir_state
    (** State 60.
        Stack shape : TIdent.
        Start symbol: annot. *)


and ('s, 'r) _menhir_cell1_arg_annot = 
  | MenhirCell1_arg_annot of 's * ('s, 'r) _menhir_state * (Primitive.kind_arg)

and 's _menhir_cell0_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ = 
  | MenhirCell0_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ of 's * (Primitive.kind_arg list option)

and 's _menhir_cell0_option_prim_annot_ = 
  | MenhirCell0_option_prim_annot_ of 's * (Primitive.kind option)

and ('s, 'r) _menhir_cell1_separated_nonempty_list_TComma_TIdent_ = 
  | MenhirCell1_separated_nonempty_list_TComma_TIdent_ of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_separated_nonempty_list_TComma_version_ = 
  | MenhirCell1_separated_nonempty_list_TComma_version_ of 's * ('s, 'r) _menhir_state * (((int -> int -> bool) * string) list)

and ('s, 'r) _menhir_cell1_version = 
  | MenhirCell1_version of 's * ('s, 'r) _menhir_state * ((int -> int -> bool) * string)

and ('s, 'r) _menhir_cell1_TIdent = 
  | MenhirCell1_TIdent of 's * ('s, 'r) _menhir_state * 
# 22 "compiler/lib/annot_parser.mly"
      (string)
# 151 "compiler/lib/annot_parser.ml"


and 's _menhir_cell0_TIdent = 
  | MenhirCell0_TIdent of 's * 
# 22 "compiler/lib/annot_parser.mly"
      (string)
# 158 "compiler/lib/annot_parser.ml"


and _menhir_box_annot = 
  | MenhirBox_annot of (Primitive.t) [@@unboxed]

let _menhir_action_01 =
  fun args id opt ->
    (
# 36 "compiler/lib/annot_parser.mly"
    ( `Provides (id,(match opt with None -> `Mutator | Some k -> k),args) )
# 169 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_02 =
  fun l ->
    (
# 38 "compiler/lib/annot_parser.mly"
    ( `Requires (l) )
# 177 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_03 =
  fun l ->
    (
# 40 "compiler/lib/annot_parser.mly"
    ( `Version (l) )
# 185 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_04 =
  fun () ->
    (
# 41 "compiler/lib/annot_parser.mly"
                     ( `Weakdef   )
# 193 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_05 =
  fun () ->
    (
# 42 "compiler/lib/annot_parser.mly"
                    ( `Always   )
# 201 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_06 =
  fun name ->
    (
# 43 "compiler/lib/annot_parser.mly"
                                      ( `Alias (name) )
# 209 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_07 =
  fun name ->
    (
# 44 "compiler/lib/annot_parser.mly"
                                   ( `If (name) )
# 217 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_08 =
  fun name ->
    (
# 45 "compiler/lib/annot_parser.mly"
                                         ( `Ifnot (name) )
# 225 "compiler/lib/annot_parser.ml"
     : (Primitive.t))

let _menhir_action_09 =
  fun () ->
    (
# 53 "compiler/lib/annot_parser.mly"
             ( `Const )
# 233 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg))

let _menhir_action_10 =
  fun () ->
    (
# 54 "compiler/lib/annot_parser.mly"
               ( `Shallow_const)
# 241 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg))

let _menhir_action_11 =
  fun () ->
    (
# 55 "compiler/lib/annot_parser.mly"
                      ( `Object_literal)
# 249 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg))

let _menhir_action_12 =
  fun () ->
    (
# 56 "compiler/lib/annot_parser.mly"
               ( `Mutable)
# 257 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg))

let _menhir_action_13 =
  fun () ->
    (
# 69 "compiler/lib/annot_parser.mly"
        ( () )
# 265 "compiler/lib/annot_parser.ml"
     : (unit))

let _menhir_action_14 =
  fun () ->
    (
# 70 "compiler/lib/annot_parser.mly"
        ( () )
# 273 "compiler/lib/annot_parser.ml"
     : (unit))

let _menhir_action_15 =
  fun _1 ->
    (
# 71 "compiler/lib/annot_parser.mly"
           ( failwith _1  )
# 281 "compiler/lib/annot_parser.ml"
     : (unit))

let _menhir_action_16 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 289 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg list))

let _menhir_action_17 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 297 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg list))

let _menhir_action_18 =
  fun () ->
    (
# 59 "compiler/lib/annot_parser.mly"
       ((<=))
# 305 "compiler/lib/annot_parser.ml"
     : (int -> int -> bool))

let _menhir_action_19 =
  fun () ->
    (
# 60 "compiler/lib/annot_parser.mly"
       ((<))
# 313 "compiler/lib/annot_parser.ml"
     : (int -> int -> bool))

let _menhir_action_20 =
  fun () ->
    (
# 61 "compiler/lib/annot_parser.mly"
       ((>))
# 321 "compiler/lib/annot_parser.ml"
     : (int -> int -> bool))

let _menhir_action_21 =
  fun () ->
    (
# 62 "compiler/lib/annot_parser.mly"
       ((>=))
# 329 "compiler/lib/annot_parser.ml"
     : (int -> int -> bool))

let _menhir_action_22 =
  fun () ->
    (
# 63 "compiler/lib/annot_parser.mly"
       ((=))
# 337 "compiler/lib/annot_parser.ml"
     : (int -> int -> bool))

let _menhir_action_23 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 345 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg list option))

let _menhir_action_24 =
  fun xs ->
    let x =
      let x = 
# 241 "<standard.mly>"
    ( xs )
# 354 "compiler/lib/annot_parser.ml"
       in
      
# 205 "<standard.mly>"
    ( x )
# 359 "compiler/lib/annot_parser.ml"
      
    in
    (
# 114 "<standard.mly>"
    ( Some x )
# 365 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg list option))

let _menhir_action_25 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 373 "compiler/lib/annot_parser.ml"
     : (Primitive.kind option))

let _menhir_action_26 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 381 "compiler/lib/annot_parser.ml"
     : (Primitive.kind option))

let _menhir_action_27 =
  fun () ->
    (
# 47 "compiler/lib/annot_parser.mly"
            (`Pure)
# 389 "compiler/lib/annot_parser.ml"
     : (Primitive.kind))

let _menhir_action_28 =
  fun () ->
    (
# 48 "compiler/lib/annot_parser.mly"
             (`Pure)
# 397 "compiler/lib/annot_parser.ml"
     : (Primitive.kind))

let _menhir_action_29 =
  fun () ->
    (
# 49 "compiler/lib/annot_parser.mly"
               (`Mutable)
# 405 "compiler/lib/annot_parser.ml"
     : (Primitive.kind))

let _menhir_action_30 =
  fun () ->
    (
# 50 "compiler/lib/annot_parser.mly"
               (`Mutator)
# 413 "compiler/lib/annot_parser.ml"
     : (Primitive.kind))

let _menhir_action_31 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 421 "compiler/lib/annot_parser.ml"
     : (string list))

let _menhir_action_32 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 429 "compiler/lib/annot_parser.ml"
     : (string list))

let _menhir_action_33 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 437 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg list))

let _menhir_action_34 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 445 "compiler/lib/annot_parser.ml"
     : (Primitive.kind_arg list))

let _menhir_action_35 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 453 "compiler/lib/annot_parser.ml"
     : (((int -> int -> bool) * string) list))

let _menhir_action_36 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 461 "compiler/lib/annot_parser.ml"
     : (((int -> int -> bool) * string) list))

let _menhir_action_37 =
  fun _1 _2 ->
    (
# 66 "compiler/lib/annot_parser.mly"
             ( _1,_2 )
# 469 "compiler/lib/annot_parser.ml"
     : ((int -> int -> bool) * string))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | TWeakdef ->
        "TWeakdef"
    | TVersion ->
        "TVersion"
    | TVNum _ ->
        "TVNum"
    | TRequires ->
        "TRequires"
    | TProvides ->
        "TProvides"
    | TOTHER _ ->
        "TOTHER"
    | TIf ->
        "TIf"
    | TIdent _ ->
        "TIdent"
    | TComma ->
        "TComma"
    | TColon ->
        "TColon"
    | TBang ->
        "TBang"
    | TAlways ->
        "TAlways"
    | TAlias ->
        "TAlias"
    | TA_Shallow ->
        "TA_Shallow"
    | TA_Pure ->
        "TA_Pure"
    | TA_Object_literal ->
        "TA_Object_literal"
    | TA_Mutator ->
        "TA_Mutator"
    | TA_Mutable ->
        "TA_Mutable"
    | TA_Const ->
        "TA_Const"
    | RPARENT ->
        "RPARENT"
    | LT ->
        "LT"
    | LPARENT ->
        "LPARENT"
    | LE ->
        "LE"
    | GT ->
        "GT"
    | GE ->
        "GE"
    | EQ ->
        "EQ"
    | EOL ->
        "EOL"
    | EOF ->
        "EOF"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_annot : type  ttv_stack. ttv_stack -> _ -> _menhir_box_annot =
    fun _menhir_stack _v ->
      MenhirBox_annot _v
  
  let _menhir_run_05 : type  ttv_stack. ttv_stack -> _menhir_box_annot =
    fun _menhir_stack ->
      let _v = _menhir_action_04 () in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_annot) _menhir_cell1_separated_nonempty_list_TComma_version_ -> _menhir_box_annot =
    fun _menhir_stack ->
      let MenhirCell1_separated_nonempty_list_TComma_version_ (_menhir_stack, _, l) = _menhir_stack in
      let _v = _menhir_action_03 l in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_run_26 : type  ttv_stack. (ttv_stack, _menhir_box_annot) _menhir_cell1_separated_nonempty_list_TComma_TIdent_ -> _menhir_box_annot =
    fun _menhir_stack ->
      let MenhirCell1_separated_nonempty_list_TComma_TIdent_ (_menhir_stack, _, l) = _menhir_stack in
      let _v = _menhir_action_02 l in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_run_48 : type  ttv_stack. ttv_stack _menhir_cell0_TIdent _menhir_cell0_option_prim_annot_ _menhir_cell0_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ -> _menhir_box_annot =
    fun _menhir_stack ->
      let MenhirCell0_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ (_menhir_stack, args) = _menhir_stack in
      let MenhirCell0_option_prim_annot_ (_menhir_stack, opt) = _menhir_stack in
      let MenhirCell0_TIdent (_menhir_stack, id) = _menhir_stack in
      let _v = _menhir_action_01 args id opt in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_run_52 : type  ttv_stack. ttv_stack _menhir_cell0_TIdent -> _menhir_box_annot =
    fun _menhir_stack ->
      let MenhirCell0_TIdent (_menhir_stack, name) = _menhir_stack in
      let _v = _menhir_action_07 name in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_run_55 : type  ttv_stack. ttv_stack _menhir_cell0_TIdent -> _menhir_box_annot =
    fun _menhir_stack ->
      let MenhirCell0_TIdent (_menhir_stack, name) = _menhir_stack in
      let _v = _menhir_action_08 name in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_run_57 : type  ttv_stack. ttv_stack -> _menhir_box_annot =
    fun _menhir_stack ->
      let _v = _menhir_action_05 () in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_run_61 : type  ttv_stack. ttv_stack _menhir_cell0_TIdent -> _menhir_box_annot =
    fun _menhir_stack ->
      let MenhirCell0_TIdent (_menhir_stack, name) = _menhir_stack in
      let _v = _menhir_action_06 name in
      _menhir_goto_annot _menhir_stack _v
  
  let _menhir_goto_endline : type  ttv_stack. ttv_stack -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_s ->
      match _menhir_s with
      | MenhirState01 ->
          _menhir_run_05 _menhir_stack
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack
      | MenhirState47 ->
          _menhir_run_48 _menhir_stack
      | MenhirState51 ->
          _menhir_run_52 _menhir_stack
      | MenhirState54 ->
          _menhir_run_55 _menhir_stack
      | MenhirState56 ->
          _menhir_run_57 _menhir_stack
      | MenhirState60 ->
          _menhir_run_61 _menhir_stack
      | _ ->
          _menhir_fail ()
  
  let _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _v _menhir_s ->
      let _1 = _v in
      let _ = _menhir_action_15 _1 in
      _menhir_goto_endline _menhir_stack _menhir_s
  
  let _menhir_run_03 : type  ttv_stack. ttv_stack -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_s ->
      let _ = _menhir_action_13 () in
      _menhir_goto_endline _menhir_stack _menhir_s
  
  let _menhir_run_04 : type  ttv_stack. ttv_stack -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_s ->
      let _ = _menhir_action_14 () in
      _menhir_goto_endline _menhir_stack _menhir_s
  
  let _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _ -> _menhir_box_annot =
    fun _menhir_stack _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_TComma_version_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TOTHER _v_0 ->
          _menhir_run_02 _menhir_stack _v_0 MenhirState18
      | EOL ->
          _menhir_run_03 _menhir_stack MenhirState18
      | EOF ->
          _menhir_run_04 _menhir_stack MenhirState18
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_goto_separated_nonempty_list_TComma_version_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _ -> _menhir_box_annot =
    fun _menhir_stack _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _v _tok
      | MenhirState07 ->
          _menhir_run_18 _menhir_stack _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_annot) _menhir_cell1_version -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _v _tok ->
      let MenhirCell1_version (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_36 x xs in
      _menhir_goto_separated_nonempty_list_TComma_version_ _menhir_stack _v _menhir_s _tok
  
  let rec _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_19 () in
      _menhir_goto_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_op : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TVNum _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _2) = (_v, _v_0) in
          let _v = _menhir_action_37 _1 _2 in
          (match (_tok : MenhirBasics.token) with
          | TComma ->
              let _menhir_stack = MenhirCell1_version (_menhir_stack, _menhir_s, _v) in
              let _menhir_s = MenhirState14 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LT ->
                  _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LE ->
                  _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | GT ->
                  _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | GE ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EQ ->
                  _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | EOF | EOL | TOTHER _ ->
              let x = _v in
              let _v = _menhir_action_35 x in
              _menhir_goto_separated_nonempty_list_TComma_version_ _menhir_stack _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_18 () in
      _menhir_goto_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_20 () in
      _menhir_goto_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_21 () in
      _menhir_goto_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_12 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_22 () in
      _menhir_goto_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_25 : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _ -> _menhir_box_annot =
    fun _menhir_stack _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_separated_nonempty_list_TComma_TIdent_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TOTHER _v_0 ->
          _menhir_run_02 _menhir_stack _v_0 MenhirState25
      | EOL ->
          _menhir_run_03 _menhir_stack MenhirState25
      | EOF ->
          _menhir_run_04 _menhir_stack MenhirState25
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_goto_separated_nonempty_list_TComma_TIdent_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _ -> _menhir_box_annot =
    fun _menhir_stack _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState23 ->
          _menhir_run_24 _menhir_stack _v _tok
      | MenhirState21 ->
          _menhir_run_25 _menhir_stack _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_24 : type  ttv_stack. (ttv_stack, _menhir_box_annot) _menhir_cell1_TIdent -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _v _tok ->
      let MenhirCell1_TIdent (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_32 x xs in
      _menhir_goto_separated_nonempty_list_TComma_TIdent_ _menhir_stack _v _menhir_s _tok
  
  let rec _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TComma ->
          let _menhir_stack = MenhirCell1_TIdent (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState23 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TIdent _v ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | EOF | EOL | TOTHER _ ->
          let x = _v in
          let _v = _menhir_action_31 x in
          _menhir_goto_separated_nonempty_list_TComma_TIdent_ _menhir_stack _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_goto_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ : type  ttv_stack. ttv_stack _menhir_cell0_TIdent _menhir_cell0_option_prim_annot_ -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _v _tok ->
      let _menhir_stack = MenhirCell0_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | TOTHER _v_0 ->
          _menhir_run_02 _menhir_stack _v_0 MenhirState47
      | EOL ->
          _menhir_run_03 _menhir_stack MenhirState47
      | EOF ->
          _menhir_run_04 _menhir_stack MenhirState47
      | _ ->
          _eRR ()
  
  let _menhir_goto_loption_separated_nonempty_list_TComma_arg_annot__ : type  ttv_stack. ttv_stack _menhir_cell0_TIdent _menhir_cell0_option_prim_annot_ -> _ -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let xs = _v in
      let _v = _menhir_action_24 xs in
      _menhir_goto_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ _menhir_stack _v _tok
  
  let _menhir_run_41 : type  ttv_stack. ttv_stack _menhir_cell0_TIdent _menhir_cell0_option_prim_annot_ -> _ -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_17 x in
      _menhir_goto_loption_separated_nonempty_list_TComma_arg_annot__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  let rec _menhir_goto_separated_nonempty_list_TComma_arg_annot_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState36 ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState45 ->
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_46 : type  ttv_stack. (ttv_stack, _menhir_box_annot) _menhir_cell1_arg_annot -> _ -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_arg_annot (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_34 x xs in
      _menhir_goto_separated_nonempty_list_TComma_arg_annot_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  let rec _menhir_run_37 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_10 () in
      _menhir_goto_arg_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_arg_annot : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TComma ->
          let _menhir_stack = MenhirCell1_arg_annot (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState45 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TA_Shallow ->
              _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TA_Object_literal ->
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TA_Mutable ->
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TA_Const ->
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPARENT ->
          let x = _v in
          let _v = _menhir_action_33 x in
          _menhir_goto_separated_nonempty_list_TComma_arg_annot_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_38 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_11 () in
      _menhir_goto_arg_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_39 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_12 () in
      _menhir_goto_arg_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_40 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_annot) _menhir_state -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_09 () in
      _menhir_goto_arg_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_goto_option_prim_annot_ : type  ttv_stack. ttv_stack _menhir_cell0_TIdent -> _ -> _ -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_option_prim_annot_ (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | LPARENT ->
          let _menhir_s = MenhirState36 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TA_Shallow ->
              _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TA_Object_literal ->
              _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TA_Mutable ->
              _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TA_Const ->
              _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RPARENT ->
              let _v = _menhir_action_16 () in
              _menhir_goto_loption_separated_nonempty_list_TComma_arg_annot__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
          | _ ->
              _eRR ())
      | EOF | EOL | TOTHER _ ->
          let _v = _menhir_action_23 () in
          _menhir_goto_option_delimited_LPARENT_separated_list_TComma_arg_annot__RPARENT__ _menhir_stack _v _tok
      | _ ->
          _eRR ()
  
  let _menhir_goto_prim_annot : type  ttv_stack. ttv_stack _menhir_cell0_TIdent -> _ -> _ -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let x = _v in
      let _v = _menhir_action_26 x in
      _menhir_goto_option_prim_annot_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_annot =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TWeakdef ->
          let _menhir_s = MenhirState01 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TOTHER _v ->
              _menhir_run_02 _menhir_stack _v _menhir_s
          | EOL ->
              _menhir_run_03 _menhir_stack _menhir_s
          | EOF ->
              _menhir_run_04 _menhir_stack _menhir_s
          | _ ->
              _eRR ())
      | TVersion ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TColon ->
              let _menhir_s = MenhirState07 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LT ->
                  _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LE ->
                  _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | GT ->
                  _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | GE ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | EQ ->
                  _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | TRequires ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TColon ->
              let _menhir_s = MenhirState21 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TIdent _v ->
                  _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | TProvides ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TColon ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TIdent _v ->
                  let _menhir_stack = MenhirCell0_TIdent (_menhir_stack, _v) in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TA_Pure ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_27 () in
                      _menhir_goto_prim_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | TA_Mutator ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_30 () in
                      _menhir_goto_prim_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | TA_Mutable ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_29 () in
                      _menhir_goto_prim_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | TA_Const ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_28 () in
                      _menhir_goto_prim_annot _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | EOF | EOL | LPARENT | TOTHER _ ->
                      let _v = _menhir_action_25 () in
                      _menhir_goto_option_prim_annot_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | TIf ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TColon ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TIdent _v ->
                  let _menhir_stack = MenhirCell0_TIdent (_menhir_stack, _v) in
                  let _menhir_s = MenhirState51 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TOTHER _v ->
                      _menhir_run_02 _menhir_stack _v _menhir_s
                  | EOL ->
                      _menhir_run_03 _menhir_stack _menhir_s
                  | EOF ->
                      _menhir_run_04 _menhir_stack _menhir_s
                  | _ ->
                      _eRR ())
              | TBang ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TIdent _v ->
                      let _menhir_stack = MenhirCell0_TIdent (_menhir_stack, _v) in
                      let _menhir_s = MenhirState54 in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | TOTHER _v ->
                          _menhir_run_02 _menhir_stack _v _menhir_s
                      | EOL ->
                          _menhir_run_03 _menhir_stack _menhir_s
                      | EOF ->
                          _menhir_run_04 _menhir_stack _menhir_s
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | TAlways ->
          let _menhir_s = MenhirState56 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TOTHER _v ->
              _menhir_run_02 _menhir_stack _v _menhir_s
          | EOL ->
              _menhir_run_03 _menhir_stack _menhir_s
          | EOF ->
              _menhir_run_04 _menhir_stack _menhir_s
          | _ ->
              _eRR ())
      | TAlias ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TColon ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TIdent _v ->
                  let _menhir_stack = MenhirCell0_TIdent (_menhir_stack, _v) in
                  let _menhir_s = MenhirState60 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TOTHER _v ->
                      _menhir_run_02 _menhir_stack _v _menhir_s
                  | EOL ->
                      _menhir_run_03 _menhir_stack _menhir_s
                  | EOF ->
                      _menhir_run_04 _menhir_stack _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let annot =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_annot v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
