(* Expressions and simplification functions *)

open Core_kernel.Std
open Abbrevs
open Util

(* ** Expressions *)

type name = string
type nat =
  | NatInt of int
  | NatPar of name

type set = name * nat
type idx = string * set

type dim = nat * nat
let scalar_dim = (NatInt 1, NatInt 1)

type group_type =
  | Bool
  | Zp

type matrix_type =
  | Param
  | Var

type atom =
  | Matrix of name * matrix_type * idx list * dim * group_type
  | StdBasis of idx
  | Int of BI.t
  | Zero of dim

type expr =
  | Add of expr * expr
  | Mul of expr * expr
  | Opp of expr
  | Div of expr * expr
  | Sum  of idx * idx list * expr
  | Prod of idx * idx list * expr
  | Transpose of expr
  | Diag of expr
  | PWProd of expr * expr (* Pair-wise Product *)
  | Atom of atom

(* ** Pretty printing *)

let string_of_nat = function
  | NatInt (i) -> string_of_int i
  | NatPar (s) -> s

let string_of_set (s,_) = s

let string_of_idx (i,_) = i

let string_of_idx_set (i,s) = i ^ " in " ^ (string_of_set s)

let string_of_dim (m,n) = "(" ^ (string_of_nat m) ^ "," ^ (string_of_nat n) ^ ")"

let string_of_atom = function
  | Matrix(s,_,idxs,_,_) -> if idxs = [] then s else s ^ "_{" ^ (string_of_list "," string_of_idx idxs) ^ "}"
  | Int(n)  -> BI.to_string n
  | Zero(_) -> "0"
  | StdBasis(i) -> "\\vec{e}_" ^ (string_of_idx i)

let rec string_of_expr = function
  | Add(e1,e2) -> (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Mul(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") * (" ^ (string_of_expr e2) ^ ")"
  | Opp(e)     -> "-(" ^ (string_of_expr e) ^ ")"
  | Div(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") / (" ^ (string_of_expr e2) ^ ")"
  | Sum(i,exn,e)  ->
     if exn = [] then "(Sum " ^ (string_of_idx_set i) ^ ": " ^ (string_of_expr e) ^ ")"
     else "(Sum " ^ (string_of_idx_set i) ^ "\\ {" ^ (string_of_list "," string_of_idx exn) ^ "}: " ^ (string_of_expr e) ^ ")"
  | Prod(i,exn,e) ->
     if exn = [] then "(Prod " ^ (string_of_idx_set i) ^ ": " ^ (string_of_expr e) ^ ")"
     else "(Prod " ^ (string_of_idx_set i) ^ "\\ {" ^ (string_of_list "," string_of_idx exn) ^ "}: " ^ (string_of_expr e) ^ ")"
  | Transpose(e)  -> "(" ^ (string_of_expr e) ^ ")^T"
  | Diag(e)    -> "diag(" ^ (string_of_expr e) ^ ")"
  | PWProd(e1,e2) -> "(" ^ (string_of_expr e1) ^ ") . (" ^ (string_of_expr e2) ^ ")"
  | Atom(a)    -> (string_of_atom a)

let pp_nat _fmt nat    = F.printf "%s" (string_of_nat nat)
let pp_set _fmt set    = F.printf "%s" (string_of_set set)
let pp_idx _fmt idx    = F.printf "%s" (string_of_idx idx)
let pp_idx_set _fmt is = F.printf "%s" (string_of_idx_set is)
let pp_dim _fmt dim    = F.printf "%s" (string_of_dim dim)
let pp_atom _fmt atom  = F.printf "%s" (string_of_atom atom)
let pp_expr _fmt expr  = F.printf "%s" (string_of_expr expr)

(* ** Compare expressions *)

(* *** Compare various *)
                                  
let compare_nat n1 n2 =
  match n1,n2 with
  | NatInt i1, NatInt i2 -> compare_int i1 i2
  | NatInt _, _ -> -1
  | _, NatInt _ -> +1
  | NatPar s1, NatPar s2 -> compare_string s1 s2

let compare_set (s1,n1) (s2,n2) =
  let c1 = compare_string s1 s2 in
  if c1 <> 0 then c1
  else compare_nat n1 n2

let compare_idx (i1,s1) (i2,s2) =
  let c1 = compare_string i1 i2 in
  if c1 <> 0 then c1
  else compare_set s1 s2
                                           
let compare_dim (m1,n1) (m2,n2) =
  let c1 = compare_nat m1 m2 in
  if c1 <> 0 then c1
  else compare_nat n1 n2

let compare_group_type t1 t2 =
  match t1,t2 with
  | Bool, Zp -> -1
  | Zp, Bool -> +1
  | _ -> 0

let compare_matrix_type t1 t2 =
  match t1,t2 with
  | Param, Var -> -1
  | Var, Param -> +1
  | _ -> 0           

let compare_tuple (s1,mt1,idxs1,d1,gt1) (s2,mt2,idxs2,d2,gt2) =
  let c1 = compare_matrix_type mt1 mt2 in
  if c1 <> 0 then c1
  else
    let c2 = compare_dim d1 d2 in
    if c2 <> 0 then c2
    else
      let c3 = compare_string s1 s2 in
      if c3 <> 0 then c3
      else
        let c4 = compare_lists ~compare:compare_idx idxs1 idxs2 in
        if c4 <> 0 then c4
        else compare_group_type gt1 gt2
                               
let compare_atom a1 a2 =
  match a1,a2 with
  | Matrix(s1,mt1,idxs1,d1,gt1), Matrix(s2,mt2,idxs2,d2,gt2) ->
     compare_tuple (s1,mt1,idxs1,d1,gt1) (s2,mt2,idxs2,d2,gt2)
  | Matrix _, _ -> -1
  | _, Matrix _ -> +1
  | StdBasis (i1), StdBasis(i2) -> compare_idx i1 i2
  | StdBasis _, _ -> -1
  | _, StdBasis _ -> +1
  | Int i1, Int i2 -> BI.compare i1 i2
  | Int _, _ -> -1
  | _, Int _ -> +1
  | Zero d1, Zero d2 -> compare_dim d1 d2

let equal_nat n1 n2 = compare_nat n1 n2 = 0
let equal_set s1 s2 = compare_set s1 s2 = 0
let equal_idx i1 i2 = compare_idx i1 i2 = 0
let equal_dim d1 d2 = compare_dim d1 d2 = 0
let equal_group_type t1 t2 = compare_group_type t1 t2 = 0
let equal_matrix_type t1 t2 = compare_matrix_type t1 t2 = 0
let equal_atom a1 a2 = compare_atom a1 a2 = 0
                                              

(* *** Indices *)

let index_set (_,s) = s
                                              
let atom_indices = function
  | Matrix(_,_,idxs,_,_) -> idxs
  | StdBasis(idx) -> [idx]
  | _ -> []

let rec get_indices = function
  | Atom (a) -> atom_indices a
  | Sum (i,exn,e) | Prod(i,exn,e) -> L.dedup ((i :: exn) @ (get_indices e))
  | Add(e1,e2) | Mul(e1,e2) | Div(e1,e2) | PWProd(e1,e2) ->
     L.dedup ((get_indices e1) @ (get_indices e2)) ~compare:compare_idx
  | Opp(e) | Transpose(e) | Diag(e) -> get_indices e

let rec free_indices = function
  | Atom (a) -> atom_indices a
  | Sum (i,exn,e) | Prod(i,exn,e) ->
     let free = free_indices e |> L.filter ~f:(fun j -> not(equal_idx i j)) in
     L.dedup (exn @ free) ~compare:compare_idx
  | Add(e1,e2) | Mul(e1,e2) | Div(e1,e2) | PWProd(e1,e2) ->
     L.dedup ((free_indices e1) @ (free_indices e2)) ~compare:compare_idx
  | Opp(e) | Transpose(e) | Diag(e) -> free_indices e

let fresh_index_name names =
  match L.find ["i"; "j"; "k"; "l"] ~f:(fun i -> not (L.mem names i)) with
  | Some i -> i
  | None ->
     let rec aux k =
       let i = "i" ^ (string_of_int k) in
       if L.mem names i then aux (k+1)
       else i
     in
     aux 0

let change_idx i j k =
  match i,j with
  | (_,set_i), (_,set_j) when equal_set set_i set_j -> if equal_idx i k then j else k
  | _ -> failwith ("this index substitution is modifying the index domain: " ^
                     (string_of_idx_set i) ^ " by " ^ (string_of_idx_set j))

let subst_idx_in_atom i j = function
  | Matrix(name,mt,idxs,d,gt) -> Matrix(name,mt, L.map idxs ~f:(change_idx i j),d,gt)
  | StdBasis(idx) -> StdBasis(change_idx i j idx)
  | _ as a -> a

let rec subst_idx i j = function
  | Add(e1,e2) -> Add(subst_idx i j e1, subst_idx i j e2)
  | Mul(e1,e2) -> Mul(subst_idx i j e1, subst_idx i j e2)
  | Opp(e)     -> Opp(subst_idx i j e)
  | Div(e1,e2) -> Div(subst_idx i j e1, subst_idx i j e2)
  | PWProd(e1,e2) -> PWProd(subst_idx i j e1, subst_idx i j e2)
  | Sum(k,exn,e)  ->  Sum(change_idx i j k, L.map exn ~f:(change_idx i j), subst_idx i j e)
  | Prod(k,exn,e) -> Prod(change_idx i j k, L.map exn ~f:(change_idx i j), subst_idx i j e)
  | Transpose(e)  -> Transpose(subst_idx i j e)
  | Diag(e)    -> Diag(subst_idx i j e)
  | Atom(a)    -> Atom(subst_idx_in_atom i j a)


(* *** Compare Expressions *)

let compare_triple ~cmp (i1,exn1,e1) (i2,exn2,e2) =
  let c1 = compare_idx i1 i2 in
  if c1 <> 0 then c1
  else
    let c2 = compare_lists ~compare:compare_idx exn1 exn2 in
    if c2 <> 0 then c2
    else cmp e1 e2
                      
let rec compare_expr e1 e2 =  
  match e1,e2 with
  | Atom(a1), Atom(a2) -> compare_atom a1 a2
  | Atom _, _ -> -1
  | _, Atom _ -> +1
  | Diag(e1), Diag(e2) -> compare_expr e1 e2
  | Diag _, _ -> -1
  | _, Diag _ -> +1
  | Sum(i1,exn1,e1), Sum(i2,exn2,e2) -> compare_triple ~cmp:compare_expr (i1,exn1,e1) (i2,exn2,e2)
  | Sum _, _ -> -1
  | _, Sum _ -> +1
  | Prod(i1,exn1,e1), Prod(i2,exn2,e2) -> compare_triple ~cmp:compare_expr (i1,exn1,e1) (i2,exn2,e2)
  | Prod _, _ -> -1
  | _, Prod _ -> +1
  | Opp(e1), Opp(e2) -> compare_expr e1 e2
  | Opp _, _ -> -1
  | _, Opp _ -> +1
  | Add(e1,e2), Add(e1',e2') -> let c = compare_expr e1 e1' in if c <> 0 then c else compare_expr e2 e2'
  | Add _, _ -> -1
  | _, Add _ -> +1
  | Mul(e1,e2), Mul(e1',e2') -> let c = compare_expr e1 e1' in if c <> 0 then c else compare_expr e2 e2'
  | Mul _, _ -> -1
  | _, Mul _ -> +1
  | Div(e1,e2), Div(e1',e2') -> let c = compare_expr e1 e1' in if c <> 0 then c else compare_expr e2 e2'
  | Div _, _ -> -1
  | _, Div _ -> +1
  | PWProd(e1,e2), PWProd(e1',e2') ->
     let c = compare_expr e1 e1' in if c <> 0 then c else compare_expr e2 e2'
  | PWProd _, _ -> -1
  | _, PWProd _ -> +1
  | Transpose(e1), Transpose(e2) -> compare_expr e1 e2

let equal_expr e1 e2 = compare_expr e1 e2 = 0

let rec is_zero_expr = function
  | Atom(Zero(_)) -> true
  | Atom(Int(a)) -> BI.is_zero a
  | PWProd(e1,e2) | Mul(e1,e2) -> is_zero_expr e1 || is_zero_expr e2
  | Diag(e) | Transpose(e) | Sum(_,_,e) | Prod(_,_,e) | Opp(e) -> is_zero_expr e
  | _ -> false

let is_one_expr = function
  | Atom(Int(a)) -> BI.is_one a
  | _ -> false

           
(* ** Substitute expression *)

let rec subst_expr ~old ~by expr =
  if equal_expr expr old then by
  else
    let f e = if equal_expr e old then by else subst_expr ~old ~by e in
    let indices = get_indices (Add(old,by)) in
    match expr with
    | Add(e1,e2)         -> Add(f e1, f e2)
    | Mul(e1,e2)         -> Mul(f e1, f e2)
    | Div(e1,e2)         -> Div(f e1, f e2)
    | Opp(e)             -> Opp(f e)
    | Sum(i,exn,e)  as s -> if L.mem indices i ~equal:equal_idx then s else Sum(i,exn,f e)
    | Prod(i,exn,e) as p -> if L.mem indices i ~equal:equal_idx then p else Prod(i,exn,f e)
    | Transpose(e)       -> Transpose(f e)
    | Diag(e)            -> Diag(f e)
    | Atom(a)            -> Atom(a)
    | PWProd(e1,e2)      -> PWProd(f e1, f e2)


(* ** Get atoms *)

let get_atoms e =
  let rec aux atoms = function
    | Add(e1,e2) | Mul(e1,e2) | Div(e1,e2) | PWProd(e1,e2) -> (aux atoms e1) @ (aux [] e2)
    | Opp(e) | Sum(_,_,e) | Prod(_,_,e) | Transpose(e) | Diag(e) -> aux atoms e
    | Atom(a) -> a :: atoms
  in
  aux [] e
  |> L.dedup ~compare:compare_atom

let get_vars e   = L.filter (get_atoms e) ~f:(function | Matrix(_,Var,_,_,_) -> true | _ -> false)
let get_params e = L.filter (get_atoms e) ~f:(function | Matrix(_,Param,_,_,_) -> true | _ -> false)

let rec atom_map ~f = function
  | Add(e1,e2)    -> Add(atom_map ~f e1, atom_map ~f e2)
  | Mul(e1,e2)    -> Mul(atom_map ~f e1, atom_map ~f e2)
  | Div(e1,e2)    -> Div(atom_map ~f e1, atom_map ~f e2)
  | Opp(e)        -> Opp(atom_map ~f e)
  | Sum(i,exn,e)  -> Sum(i,exn,atom_map ~f e)
  | Prod(i,exn,e) -> Prod(i,exn,atom_map ~f e)
  | Transpose(e)  -> Transpose(atom_map ~f e)
  | Diag(e)       -> Diag(atom_map ~f e)
  | Atom(a)       -> Atom(f a)
  | PWProd(e1,e2) -> PWProd(atom_map ~f e1, atom_map ~f e2)


(* ** Simplification functions *)

let simplify_expr expr =
  let rec aux = function
    | Mul(Add(e1,e2),e) -> aux (Add(Mul(e1,e), Mul(e2,e)))
    | Mul(e,Add(e1,e2)) -> aux (Add(Mul(e,e1), Mul(e,e2)))
    | Add(z,e) when is_zero_expr z -> aux e | Add(e,z) when is_zero_expr z -> aux e
    | Mul(z,_) when is_zero_expr z -> z     | Mul(_,z) when is_zero_expr z -> z
    | Mul(o,e) when is_one_expr o -> aux e  | Mul(e,o) when is_one_expr o -> aux e
    | Sum(_,_,z) when is_zero_expr z -> z
    | Prod(_,_,z) when is_zero_expr z -> z
    | Prod(_,_,o) when is_one_expr o -> o
    | Opp(Opp(e)) -> aux e
    | Opp(Add(e1,e2)) -> aux (Add(Opp(e1), Opp(e2)))
    | Opp(Div(e1,e2)) -> aux (Div(Opp(e1),e2))
    | Opp(Mul(e1,e2)) -> aux (Mul(Opp(e1),e2))
    | Sum(i,exn,Add(e1,e2)) -> aux (Add(Sum(i,exn,e1), Sum(i,exn,e2)))
    | Add(Div(e1,e1'),Div(e2,e2')) -> aux (Div(Add(Mul(e1,e2'), Mul(e2,e1')), Mul(e1',e2')))
    | Add(Div(e1,e1'),e2) -> aux (Div(Add(e1,Mul(e1',e2)), e1'))
    | Add(e1,Div(e2,e2')) -> aux (Div(Add(Mul(e1,e2'),e2), e2'))
    | Mul(Div(e1,e1'),Div(e2,e2')) -> aux (Div(Mul(e1,e2), Mul(e1',e2')))
    | Mul(Div(e1,e1'),e2) -> aux (Div(Mul(e1,e2), e1'))
    | Mul(e1,Div(e2,e2')) -> aux (Div(Mul(e1,e2), e2'))
    | Sum(i,exn,Div(e1,e2)) as e ->
       let (_,seti) = i in
       let j = fresh_index_name (L.map (get_indices e) ~f:(fun (i,_) -> i)), seti in
       let num = Sum(i,exn, Mul(e1, Prod(j, i :: exn, subst_idx i j e2))) in
       let den = Prod(i,exn,e2) in
       aux (Div(num,den))
    | Div(_,z) when is_zero_expr z -> failwith "Division by zero"
    | Div(Div(e1,e2),e3) -> aux (Div(e1,Mul(e2,e3)))
    | Div(e1,Div(e2,e3)) -> aux (Div(Mul(e1,e3),e2))
    | Div(e, Atom(Int(i))) when BI.is_one i -> aux e
    | Div(e, Atom(Int(i))) when BI.equal (BI.opp BI.one) i -> aux (Opp(e))
    | Div(Atom(Int(i1)), Atom(Int(i2))) ->
       let d = BI.gcd i1 i2 in
       Div(Atom(Int(BI.(i1 /! d))), Atom(Int(BI.(i2 /! d))))
    | Div(e1,e2) -> if equal_expr e1 e2 then Atom(Int(BI.one)) else Div(aux e1, aux e2)
    | Transpose(Transpose(e)) -> aux e
    | Transpose(Add(e1,e2)) -> Add(Transpose(e1), Transpose(e2))
    | Transpose(Mul(e1,e2)) -> Mul(Transpose(e2), Transpose(e1))
    | Transpose(Div(e1,e2)) -> Div(Transpose(e1),e2) (* This rule works because division is only allowed by scalars *)
    | Transpose(Opp(e)) -> Opp(Transpose(e))
    | Transpose(Sum(i,exn,e)) -> Sum(i,exn,Transpose(e))
    | Transpose(Prod(i,exn,e)) -> Prod(i,exn,Transpose(e))
    | Prod(i,exn,e) -> Prod(i,L.dedup exn ~compare:compare_idx, aux e)
    | Atom(Int(i)) when BI.is_zero i -> Atom(Zero(scalar_dim))
    | Mul(Atom(Int(i1)), Atom(Int(i2))) -> Atom(Int(BI.(i1 *! i2)))
    | Mul(Sum(i,exn,e1),e2) | Mul(e2,Sum(i,exn,e1)) ->
       let names = exn @ (get_indices e2) @ (get_indices e1) in
       if L.mem (get_indices e2) i ~equal:equal_idx then
	 let (_,seti) = i in
	 let j = (fresh_index_name (L.map names ~f:(fun (n,_) -> n)), seti) in
	 let e1' = subst_idx i j e1 in
	 aux (Sum(j, L.map exn ~f:(fun j' -> change_idx i j' j), Mul(e1',e2)))
       else aux (Sum(i,exn,Mul(e1,e2)))
    | Opp(Sum(i,exn,e)) -> aux (Sum(i,exn,Opp(e)))
    | Opp(Atom(Int(i))) -> Atom(Int(BI.opp i))
    | Sum(i,exn,e) -> Sum(i,L.dedup exn ~compare:compare_idx, aux e)
    | Add(Atom(Int(i1)), Atom(Int(i2))) -> Atom(Int(BI.(i1 +! i2)))

    | Diag(Add(e1,e2)) -> aux (Add(Diag(e1),Diag(e2)))
    | Mul(Diag(e1),Diag(e2)) -> aux (Diag(PWProd(e1,e2)))
    | Transpose(Diag(e1)) -> aux (Diag(e1))
    | Diag(z) when is_zero_expr z -> Atom(Int(BI.zero))
    | Diag(Transpose(e)) -> Diag(e)

    | PWProd(Atom(Matrix(s,mt,i,d,t)), e) when equal_expr (Atom(Matrix(s,mt,i,d,t))) e && equal_group_type t Bool -> e
    | PWProd(e1,e2) when compare_expr e1 e2 = +1 -> aux (PWProd(e2,e1))
    | PWProd(e1,PWProd(e2,e3)) -> aux (PWProd(PWProd(e1,e2),e3))
    | PWProd(Add(e1,e2),e3) -> aux (Add(PWProd(e1,e3), PWProd(e2,e3)))
    | PWProd(e1,Add(e2,e3)) -> aux (Add(PWProd(e1,e2), PWProd(e1,e3)))
    | Transpose(PWProd(e1,e2)) -> aux (PWProd(Transpose(e1), Transpose(e2)))
    | PWProd(Atom(StdBasis(i)), Atom(StdBasis(j))) ->
       if equal_idx i j then Atom(StdBasis(i)) else Atom(Int(BI.zero))
    | PWProd(z,_) when is_zero_expr z -> Atom(Int(BI.zero))
    | PWProd(_,z) when is_zero_expr z -> Atom(Int(BI.zero))
    | PWProd(e1,e2) -> (* This works because pair-wise is only defined for vectors *)
       let vars2 = get_vars e2 in
       begin match e1 with
       | Atom(Matrix(_,Var,_,_,_)) when vars2 = [] -> aux (Mul(Diag(e2),e1))
       | Transpose(Atom(Matrix(_,Var,_,_,_))) when vars2 = [] -> aux (Mul(e1,Diag(e2)))
       | _ ->
          let vars1 = get_vars e1 in
          begin match e2 with
          | Atom(Matrix(_,Var,_,_,_)) when vars1 = [] -> aux (Mul(Diag(e1),e2))
          | Transpose(Atom(Matrix(_,Var,_,_,_))) when vars1 = [] -> aux (Mul(e2,Diag(e1)))
          | _ -> PWProd(aux e1, aux e2)
          end
       end         
    | Diag(e) -> Diag (aux e)
    | Add(e1,e2) -> Add(aux e1, aux e2)
    | Mul(e1,e2) -> (Mul(aux e1, aux e2))
    | Atom(a) -> Atom(a)
    | Opp(e) -> Mul(Atom(Int(BI.opp BI.one)), aux e)
    | Transpose(e) -> Transpose(aux e)
  in
  aux expr

let full_simplify expr =
  let rec aux last expr =
    if equal_expr last expr then expr
    else aux expr (simplify_expr expr)
  in
  aux expr (simplify_expr expr)

(* ** Compute dimension *)

let atom_dimension = function
  | Matrix(_,_,_,d,_) | Zero d -> d
  | StdBasis ((_,(_,nat))) -> nat, (NatInt(1))
  | Int _ -> scalar_dim

let rec expr_dimension = function
  | Atom (a) -> atom_dimension a
  | Add(e1,e2) ->
     let (m1,n1) = expr_dimension e1 in
     let (m2,n2) = expr_dimension e2 in
     if m1 = m2 && n1 = n2 then (m1,n1)
     else
       if is_zero_expr e1 then (m2,n2)
       else if is_zero_expr e2 then (m1,n1)
       else failwith ("dimensions don't match for addition between "
                    ^ (string_of_expr e1) ^ " and " ^ (string_of_expr e2) ^ ". "
                    ^ (string_of_dim (m1,n1)) ^ " <> " ^ (string_of_dim (m2,n2)))
  | Mul(e1,e2) ->
     let (m1,n1) = expr_dimension e1 in
     let (m2,n2) = expr_dimension e2 in
     if n1 = m2 then (m1,n2)
     else if (m1,n1) = scalar_dim then (m2,n2)
     else if (m2,n2) = scalar_dim then (m1,n1)
     else if is_zero_expr e1 then scalar_dim
     else if is_zero_expr e2 then scalar_dim
     else
       failwith ("dimensions don't match for multiplication between "
                    ^ (string_of_expr e1) ^ " and " ^ (string_of_expr e2) ^ ". "
                    ^ (string_of_dim (m1,n1)) ^ " and " ^ (string_of_dim (m2,n2)))
  | Opp(e) -> expr_dimension e
  | Div(e1,e2) ->
     if (expr_dimension e2) = scalar_dim then expr_dimension e1
     else failwith "division by matrices or vectors is not allowed"
  | Sum(_,_,e)  -> expr_dimension e
  | Prod(_,_,e) -> expr_dimension e
  | Transpose(e) -> let (m,n) = expr_dimension e in (n,m)
  | Diag(e) ->
     let (m,n) = expr_dimension e in
     if equal_nat m (NatInt 1) then (n,n)
     else if equal_nat n (NatInt 1) then (m,m)
     else failwith "Diag operator cannot be applied to matrices, only vectors"
  | PWProd(e1,e2) ->
     let (m1,n1) = expr_dimension e1 in
     let (m2,n2) = expr_dimension e2 in
     if m1 = m2 && n1 = n2 then
       if (equal_nat m1 (NatInt 1)) || (equal_nat n1 (NatInt 1)) then (m1,n1)
       else failwith "pair-wise product is not supported for matrices, only vectors"
     else failwith ("dimensions don't match for pair-wise product between "
                    ^ (string_of_expr e1) ^ " and " ^ (string_of_expr e2) ^ ". "
                    ^ (string_of_dim (m1,n1)) ^ " <> " ^ (string_of_dim (m2,n2)))
