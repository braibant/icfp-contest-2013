type exp = private
  C0
| C1
| Var of ident
| Hole of ident * bool 			(* bool = true -> 3 free variables *)
| If0 of exp * exp * exp * tag
| Fold of exp * exp * exp * tag
| Op1 of op1 list * exp * tag
| Op2 of op2 * exp * exp * tag
| Cst of int64 (* Value *) * exp (* example of realisation *) * tag
and tag
and op1 = | Not | Shl1 | Shr1 | Shr4 | Shr16
and op2 = | And | Or | Xor | Plus
and ident = int

val __op1 : op1 list -> exp -> exp

val get_exp_id : exp -> int

module HC : Hashcons.S with type t = exp

module H : Hashtbl.S with type key = exp

val size : exp -> int

val holes : exp -> int

val subst_holes : exp array -> exp  -> exp

module Constants :
    sig
      val arg : int
      val fold_acc : int
      val fold_arg : int
    end

(* a really uniform random int64 *)
val rnd64 : unit -> int64


module Notations :
    sig
      val mk_arg : exp
      val mk_facc : exp
      val mk_farg : exp
      val c0 : exp
      val c1 : exp
      val cst : int64 -> exp -> exp
      val (&&) : exp -> exp -> exp
      val (||) : exp -> exp -> exp
      val ( ** ) : exp -> exp -> exp (*Xor*)
      val (++) : exp -> exp -> exp
      val (~~) : exp -> exp (* Not *)
      val shl1 : exp -> exp
      val shr1 : exp -> exp
      val shr4 : exp -> exp
      val shr16 : exp -> exp
      val if0 : exp -> exp -> exp -> exp
      val fold : exp -> exp -> exp -> exp
      val op1 : op1 -> exp -> exp
      val op2 : op2 -> exp -> exp -> exp
      val hole : ident -> bool -> exp
    end

type op =
| If0o
| Foldo
| Op1o of op1
| Op2o of op2

val op_of_string : string -> op

val string_of_op1 : op1 -> string
val string_of_op2 : op2 -> string
val string_of_op : op -> string
