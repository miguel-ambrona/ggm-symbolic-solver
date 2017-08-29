open Core_kernel.Std
open Expressions
open Abbrevs
open Util
open Equations

let test () =
  let i = ("i",("[n]",NatPar("n"))) in
  let xi = Atom(Matrix("x", Param, [i], scalar_dim, Zp)) in
  let one = Atom(Int(BI.one)) in
  let seventeen = Atom(Int(BI.of_int 17)) in
  let e = Sum(i,[],Div(one, Add(xi, seventeen))) in
  let e' = simplify_expr e in
  F.printf "%a  %a\n" pp_dim (expr_dimension e) pp_expr e;
  F.printf "%a  %a\n" pp_dim (expr_dimension e) pp_expr e'

let test2 () =
  let i = ("i",("[n]",NatPar("n"))) in
  let x = Atom(Matrix("x", Var, [], (NatPar "n", NatInt 1), Zp)) in
  let w = Atom(Matrix("w", Var, [], (NatPar "n", NatInt 1), Zp)) in
  let yi = Atom(Matrix("y", Var, [i], (NatPar "n", NatInt 1), Zp)) in
  let ai = Atom(Matrix("a", Var, [i], (NatPar "n", NatInt 1), Zp)) in
  let e = Add(Atom(Int(BI.opp BI.one)), Sum(i,[],Div(Mul(Transpose(ai), Add(x,w)), Mul(Transpose(yi),w)))) in
  let e' = simplify_expr e in
  F.printf "%a  %a\n" pp_dim (expr_dimension e) pp_expr e;
  F.printf "%a  %a\n" pp_dim (expr_dimension e) pp_expr e'

let test3 () =
  let i = ("i",("[n]",NatPar("n"))) in
  let j = ("j",("[n]",NatPar("n"))) in
  let x = Atom(Matrix("x", Var, [], (NatPar "n", NatInt 1), Zp)) in
  let w = Atom(Matrix("w", Var, [], (NatPar "n", NatInt 1), Zp)) in
  let yi = Atom(Matrix("y", Var, [j], (NatPar "n", NatInt 1), Zp)) in
  let ai = Atom(Matrix("a", Var, [i], (NatPar "n", NatInt 1), Zp)) in
  let e = Add(Atom(Int(BI.opp BI.one)), Sum(i,[],Mul(Mul(Transpose(ai), Add(x,w)), Mul(Transpose(yi),w)))) in
  let eqs = [e] |> L.map ~f:full_norm_simplify in
  let l = polyvars_of_system eqs in
  F.printf "%a  %a\n" pp_dim (expr_dimension e) pp_expr e;
  F.printf "[%a]\n" (pp_list "," pp_expr) l;
  let _gbp = gb_poly_of_norm_expr l (L.hd_exn eqs) in
  let conj = { conj_exists = []; conj_equations = (L.map eqs ~f:(fun e -> { eq_forall = []; eq_is_eq = Eq; eq_expr = e }));
	     conj_substituted = []; conj_replaced = []} in
  let conj' = groebner_basis_conj conj in
  F.printf "%a" pp_conj conj';
  ()
  
let main =
  if Array.length Sys.argv = 1 then test3 ()
  else
    match Sys.argv.(1) with
    | _ -> failwith "Not supported yet"
