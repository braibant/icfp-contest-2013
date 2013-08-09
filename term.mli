type exp = private
  C0
| C1
| Var of ident
| If0 of exp * exp * exp * tag
| Fold of exp * exp * exp * tag
| Op1 of op1 * exp * tag
| Op2 of op2 * exp * exp * tag
and tag
and op1 = | Not | Shl1 | Shr1 | Shr4 | Shr16
and op2 = | And | Or | Xor | Plus
and ident = int

val get_exp_id : exp -> int

module HC : Hashcons.S with type t = exp

val size : exp -> int

module Constants :
    sig
      val arg : int
      val fold_acc : int
      val fold_arg : int
    end

val eval : exp -> int64 -> int64

module Notations :
    sig
      val mk_arg : exp
      val mk_facc : exp
      val mk_farg : exp
      val c0 : exp
      val c1 : exp
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
    end
