open Term

let string_of_id s =
  match s with
  | 0 -> "x"
  | 1 -> "acc"
  | 2 -> "y"
  | _ -> assert false

let string_of_op1 = function
  | Not -> "not"
  | Shl1 -> "shl1"
  | Shr1 -> "shr1"
  | Shr4 -> "shr4"
  | Shr16 -> "shr16"
let string_of_op2 = function
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Plus -> "plus"

type sexp =
| Word of string
| List of sexp list

let rec sexp_exp = function
  | C0 -> Word "0"
  | C1 -> Word "1"
  | Var x -> Word (string_of_id x)
  | If0 (p, a, b, _) ->
    List [Word "if0"; sexp_exp p; sexp_exp a; sexp_exp b]
  | Fold (bytes, init, lam, _) ->
    List [Word "fold"; sexp_exp bytes; sexp_exp init; sexp_lam2 lam]
  | Op1 (op, e, _) -> List [Word (string_of_op1 op); sexp_exp e]
  | Op2 (op, e1, e2, _) -> List [Word (string_of_op2 op); sexp_exp e1; sexp_exp e2]
  | Cst (_, e, _) -> sexp_exp e
and sexp_lam_generic args e =
  List [Word "lambda"; List args; sexp_exp e]
and sexp_lam1 ((* x, *) e) =
  sexp_lam_generic [
    Word (string_of_id Constants.arg);
  ] e
and sexp_lam2 ((* x, y, *) e) =
  sexp_lam_generic [
    Word (string_of_id Constants.fold_arg);
    Word (string_of_id Constants.fold_acc);
  ] e

open PPrint
 
let rec doc_sexp = function
  | Word s -> string s
  | List (Word head :: rest) ->
    group (parens (prefix 2 1 (string head) (separate_map (break 1) doc_sexp rest)))
  | List li ->
    group (parens (separate_map (break 1) doc_sexp li))

let doc_exp exp = doc_sexp (sexp_exp exp)

let print doc =
  ToChannel.pretty 1. 72 stdout doc

let sprint doc : string =
  let b = Buffer.create 42 in 
  ToBuffer.pretty 1. 72 b doc;
  Buffer.contents b

let print_program t =
  sprint (doc_sexp (sexp_lam1 ( t) ))

let print_exp e = print (doc_exp e)
let print_exp_nl e = print (doc_exp e ^^ hardline)

include PPrint
