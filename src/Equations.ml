(* Equations and simplification functions for equations *)

open Core_kernel.Std
open Abbrevs
open Util
open Expressions
open Sage

(* ** Types *)

type exists = idx * idx list
type forall = idx * idx list

type is_eq = | Eq | InEq

type equation = {
    eq_forall : forall list;
    eq_is_eq  : is_eq;
    eq_expr   : expr;
}

type conjunction = {
    conj_exists    : exists list;
    conj_equations : equation list;
    conj_substituted : ((forall option) * atom * expr) list;
    conj_replaced  : (forall list * expr * expr) list;
}

type disjunction = conjunction list

(* ** Compute dimensions *)

let eq_dimension eq = expr_dimension eq.eq_expr

let conj_dimensions conj = L.map conj.conj_equations ~f:eq_dimension

(* ** Indices *)

let get_indices_exists (i,exn) = i :: exn
let get_indices_forall = get_indices_exists

let get_indices_eq eq =
  (L.map eq.eq_forall ~f:get_indices_forall |> L.concat) @ (get_indices eq.eq_expr)
  |> L.dedup ~compare:compare_idx

let get_indices_conj conj =
  (L.map conj.conj_exists ~f:get_indices_exists |> L.concat) @
    (L.map conj.conj_equations ~f:get_indices_eq |> L.concat)
  |> L.dedup ~compare:compare_idx

let subst_idx_in_exists i j (k,exn) =
  (change_idx i j k), L.map exn ~f:(change_idx i j)

let subst_idx_in_forall = subst_idx_in_exists

let subst_idx_in_eq i j eq =
  { eq with eq_forall = L.map eq.eq_forall ~f:(subst_idx_in_forall i j);
            eq_expr = subst_idx i j eq.eq_expr }

let subst_idx_in_conj i j conj =
  { conj_exists = L.map conj.conj_exists ~f:(subst_idx_in_exists i j);
    conj_equations = L.map conj.conj_equations ~f:(subst_idx_in_eq i j);
    conj_substituted = L.map conj.conj_substituted
           ~f:(fun (idx_option,a,e) ->
             match idx_option with
             | None -> (None, subst_idx_in_atom i j a, subst_idx i j e)
             | Some idx -> (Some (subst_idx_in_forall i j idx), subst_idx_in_atom i j a, subst_idx i j e)
           );
    conj_replaced = L.map conj.conj_replaced
           ~f:(fun (f,e1,e2) -> (L.map f ~f:(subst_idx_in_forall i j), subst_idx i j e1, subst_idx i j e2) )
  }

(* ** Get atoms *)

let get_atoms_eq eq =
  get_atoms eq.eq_expr

let get_atoms_conj conj =
  L.map conj.conj_equations ~f:get_atoms_eq |> L.concat

let eq_atom_map ~f eq =
  { eq with eq_expr = atom_map ~f eq.eq_expr }

let conj_atom_map ~f conj =
  { conj with conj_equations = L.map conj.conj_equations ~f:(eq_atom_map ~f) }

(* ** Comparison *)

let compare_exists (i1,l1) (i2,l2) =
  if i1 = i2 then compare_lists ~compare:compare_idx l1 l2
  else compare_idx i1 i2

let compare_forall = compare_exists

let compare_is_eq a1 a2 =
  match a1, a2 with
  | Eq, Eq | InEq, InEq -> 0
  | Eq, InEq -> -1
  | InEq, Eq -> +1

let compare_equation eq1 eq2 =
  let c = compare_lists ~compare:compare_forall eq1.eq_forall eq2.eq_forall in
  if c <> 0 then c
  else
    let c = compare_is_eq eq1.eq_is_eq eq2.eq_is_eq in
    if c <> 0 then c else compare_expr eq1.eq_expr eq2.eq_expr

let compare_substituted (idx1,a1,e1) (idx2,a2,e2) =
  let c =
    match idx1,idx2 with
    | Some f1, Some f2 -> compare_forall f1 f2
    | Some _, None -> -1
    | None, Some _ -> +1
    | None, None -> 0
  in
  if c <> 0 then c
  else
    let c = compare_atom a1 a2 in
    if c <> 0 then c
    else compare_expr e1 e2

let compare_conj conj1 conj2 =
  let c = compare_lists ~compare:compare_exists conj1.conj_exists conj2.conj_exists in
  if c <> 0 then c else
    let c = compare_lists ~compare:compare_equation conj1.conj_equations conj2.conj_equations in
    if c <> 0 then c else
      compare_lists ~compare:compare_substituted conj1.conj_substituted conj2.conj_substituted

let equal_exists e1 e2 = compare_exists e1 e2 = 0
let equal_forall f1 f2 = compare_forall f1 f2 = 0
let equal_is_eq a1 a2 = compare_is_eq a1 a2 = 0
let equal_equation eq1 eq2 = compare_equation eq1 eq2 = 0
let equal_conj conj1 conj2 = compare_conj conj1 conj2 = 0

(* ** Pretty Print *)

let string_of_id_exception = function
  | (i,[]) -> (string_of_idx_set i)
  | (i,exn) -> (string_of_idx_set i) ^ " not in {" ^ (string_of_list "," (string_of_idx) exn) ^ "}"

let string_of_is_eq = function
  | Eq -> "="
  | InEq -> "<>"

let string_of_eq eq =
  let forall =
    match eq.eq_forall with
    | [] -> ""
    | f -> "forall " ^ (string_of_list ", " string_of_id_exception) f ^ ". "
  in
  forall ^ (string_of_expr eq.eq_expr) ^ " " ^ (string_of_is_eq eq.eq_is_eq) ^ " 0"

let string_of_conj conj =
  let exists =
    match conj.conj_exists with
    | [] -> ""
    | f -> "exists " ^ (string_of_list ", " string_of_id_exception) f ^ ":\n"
  in
  exists ^ (string_of_list " /\\ \n" string_of_eq) conj.conj_equations

let pp_is_eq _fmt is_eq = F.printf "%s" (string_of_is_eq is_eq)
let pp_eq _fmt eq       = F.printf "%s" (string_of_eq eq)
let pp_conj _fmt conj   = F.printf "%s" (string_of_conj conj)


(* ** Simplify *)

(* *** Separate indices *)

let fresh_index (_,set) indices =
  fresh_index_name (L.map indices ~f:(fun (i,_) -> i)), set

let rec separate_idx i = function
  | Add(e1,e2)   -> Add(separate_idx i e1, separate_idx i e2)
  | Mul(e1,e2)   -> Mul(separate_idx i e1, separate_idx i e2)
  | Div(e1,e2)   -> Div(separate_idx i e1, separate_idx i e2)
  | Opp(e)       -> Opp(separate_idx i e)
  | Transpose(e) -> Transpose(separate_idx i e)
  | Diag(e)      -> Diag(separate_idx i e)
  | Atom(a)      -> Atom(a)
  | PWProd(e1,e2) -> PWProd(separate_idx i e1, separate_idx i e2)
  | Sum(k,exn,e)  ->
     if (L.mem exn i ~equal:equal_idx ) || not(equal_set (index_set i) (index_set k)) then
       let e' = separate_idx i e in
       Sum(k,exn,e')
     else if equal_idx k i then
       let j = fresh_index k (i :: (exn @ (get_indices e))) in
       let e' = subst_idx k j e in
       Add(Sum(j,i :: exn,e'), subst_idx j i e')
     else
       let e' = separate_idx i e in
       Add(Sum(k,i :: exn,e'), subst_idx k i e')
  | Prod(k,exn,e) ->
     if (L.mem exn i ~equal:equal_idx ) || not(equal_set (index_set i) (index_set k)) then
       let e' = separate_idx i e in
       Prod(k,exn,e')
     else if equal_idx k i then
       let j = fresh_index k (i :: (exn @ (get_indices e))) in
       let e' = subst_idx k j e in
       Mul(Prod(j,i :: exn,e'), subst_idx j i e')
     else
       let e' = separate_idx i e in
       Mul(Prod(k,i :: exn,e'), subst_idx k i e')

let rec separate_idx_eq i eq =
  match eq.eq_forall with
  | [] -> [{ eq with eq_expr = separate_idx i eq.eq_expr; }]
  | (k,exn) :: rest ->
     let equations = separate_idx_eq i { eq with eq_forall = rest; } in
     if (L.mem exn i ~equal:equal_idx ) || not(equal_set (index_set i) (index_set k)) then
       L.map equations ~f:(fun eq -> { eq with eq_forall = (k,exn) :: eq.eq_forall; })
     else L.map equations ~f:(fun eq' ->
                  if equal_idx k i then
                    let j = fresh_index k (get_indices_eq eq') in
                    let eq'' = subst_idx_in_eq k j eq' in
                    [ { eq'' with eq_forall = (j, i :: exn) :: eq''.eq_forall; }; subst_idx_in_eq j i eq'' ]
                  else [ { eq' with eq_forall = (k, i :: exn) :: eq'.eq_forall; }; subst_idx_in_eq k i eq' ])
          |> L.concat

let separate_idx_substituted i subst =
  L.map subst ~f:(fun (idx,t,e) ->
     let eqs_t = { eq_forall = begin match idx with | None -> [] | Some a -> [a] end; eq_is_eq = Eq; eq_expr = Atom(t) } |> separate_idx_eq i in
     let eqs_e = { eq_forall = begin match idx with | None -> [] | Some a -> [a] end; eq_is_eq = Eq; eq_expr = e } |> separate_idx_eq i in
     L.map (L.zip_exn eqs_t eqs_e)
        ~f:(fun (eq_t, eq_e) ->
          let idx_t = match eq_t.eq_forall with | [] -> None | a :: [] -> Some a | _ -> assert false in
          let idx_e = match eq_e.eq_forall with | [] -> None | a :: [] -> Some a | _ -> assert false in
          assert (equal_option ~equal:equal_forall idx_t idx_e);
          match eq_t.eq_expr with
          | Atom(t) -> (idx_t, t, eq_e.eq_expr)
          | _ -> assert false
        )
      )
  |> L.concat

let rec separate_idx_in_conj i conj =
  match conj.conj_exists with
  | [] -> [{ conj with conj_equations = L.map conj.conj_equations ~f:(separate_idx_eq i) |> L.concat;
                       conj_substituted = separate_idx_substituted i conj.conj_substituted;
          }]
  | (k,exn) :: rest ->
     let rest = L.filter rest ~f:(fun (k',exn') -> (k <> k') || not(equal_lists exn exn' ~compare:compare_idx )) in
     let disj = separate_idx_in_conj i { conj with conj_exists = rest; } in
     if not (equal_set (index_set i) (index_set k)) || equal_idx i k || L.mem exn i || L.exists rest ~f:(fun (j,_) -> i = j) then
       L.map disj ~f:(fun conj -> { conj with conj_exists = (k,exn) :: conj.conj_exists })
     else
       L.map disj ~f:(fun conj' ->
               let conj'' = subst_idx_in_conj k i conj' in
               [ { conj' with conj_exists = (k, i :: exn) :: conj'.conj_exists; };
                 { conj'' with conj_exists = (i, L.map exn ~f:(change_idx k i)) :: conj''.conj_exists; } ])
       |> L.concat

let rec normalize_indices_expr = function
  | Add(e1,e2)   -> Add(normalize_indices_expr e1, normalize_indices_expr e2)
  | Mul(e1,e2)   -> Mul(normalize_indices_expr e1, normalize_indices_expr e2)
  | Div(e1,e2)   -> Div(normalize_indices_expr e1, normalize_indices_expr e2)
  | Opp(e)       -> Opp(normalize_indices_expr e)
  | Transpose(e) -> Transpose(normalize_indices_expr e)
  | Diag(e)      -> Diag(normalize_indices_expr e)
  | Atom(a)      -> Atom(a)
  | PWProd(e1,e2) -> PWProd(normalize_indices_expr e1, normalize_indices_expr e2)
  | Sum(k,exn,e)  ->
     let e = normalize_indices_expr e in
     Sum(k,exn, separate_idx k e)
  | Prod(k,exn,e) ->
     let e = normalize_indices_expr e in
     Prod(k,exn, separate_idx k e)

let normalize_indices_eq eq =
  let eq = { eq with eq_expr = normalize_indices_expr eq.eq_expr } in
  L.fold_left (L.map eq.eq_forall ~f:(fun (i,_) -> i))
       ~init:[eq]
       ~f:(fun eqs i ->
         L.map eqs ~f:(fun eq ->
                 if L.exists eq.eq_forall ~f:(fun (j,_) -> equal_idx i j) then
                   let seti = index_set i in
                   let (j,_) = L.find_exn eq.eq_forall ~f:(fun (j,_) -> equal_set seti (index_set j)) in
                   if equal_idx i j then [{eq with eq_expr = separate_idx i eq.eq_expr}]
                   else separate_idx_eq i eq
                 else [eq]
               )
         |> L.concat
       )

let normalize_indices conj =
  let conj = { conj with conj_equations = (L.map conj.conj_equations ~f:normalize_indices_eq |> L.concat) } in
  L.fold_left (L.map conj.conj_exists ~f:(fun (i,_) -> i))
       ~init:[conj]
       ~f:(fun conjs i ->
         L.map conjs ~f:(fun c ->
                 if L.exists c.conj_exists ~f:(fun (j,_) -> equal_idx i j) then
                   let seti = index_set i in
                   let (j,_) = L.find_exn c.conj_exists ~f:(fun (j,_) -> equal_set seti (index_set j)) in
                   if equal_idx i j then
                     let new_substituted =
                       L.map c.conj_substituted
                             ~f:(fun (idx,t,e) ->
                               let f = begin match idx with | None -> [] | Some a -> [a] end in
                               let eqs_t = { eq_forall = f; eq_is_eq = Eq; eq_expr = Atom(t) } |> (separate_idx_eq i) in
                               let eqs_e = { eq_forall = f; eq_is_eq = Eq; eq_expr = e } |> (separate_idx_eq i) in
                               L.map (L.zip_exn eqs_t eqs_e) ~f:(fun (eq_t,eq_e) ->
                                       if list_is_contained_in_list (L.map f ~f:(fun (k,_) -> k)) (atom_indices t) ~equal:equal_idx ||
                                            equal_lists ~compare:compare_forall eq_t.eq_forall eq_e.eq_forall then
                                         match eq_t.eq_expr, eq_t.eq_forall with
                                         | Atom(t), [] -> (None, t, eq_e.eq_expr)
                                         | Atom(t), (i,iexn) :: [] -> (Some (i,iexn), t, eq_e.eq_expr)
                                         | _ -> assert false
                                       else assert false
                                     )
                             )
                       |> L.concat
                     in
                     let new_replaced =
                       L.map c.conj_replaced
                             ~f:(fun (f,e1,e2) ->
                               let eqs1 = { eq_forall = f; eq_is_eq = Eq; eq_expr = e1 } |> (separate_idx_eq i) in
                               let eqs2 = { eq_forall = f; eq_is_eq = Eq; eq_expr = e2 } |> (separate_idx_eq i) in
                               L.map (L.zip_exn eqs1 eqs2) ~f:(fun (eq1,eq2) ->
                                       if list_is_contained_in_list (L.map f ~f:(fun (k,_) -> k)) (get_indices e1) ~equal:equal_idx ||
                                            equal_lists ~compare:compare_forall eq1.eq_forall eq2.eq_forall then
                                         (eq1.eq_forall, eq1.eq_expr, eq2.eq_expr)
                                       else assert false
                                     )
                             )
                       |> L.concat
                     in
                     [{c with conj_equations = L.map c.conj_equations ~f:(separate_idx_eq i) |> L.concat;
                              conj_substituted = new_substituted;
                              conj_replaced = new_replaced;
                      }]
                   else separate_idx_in_conj i c
                 else [c]
               )
         |> L.concat
       )

let normalize_indices_disj disj =
  L.map disj ~f:normalize_indices |> L.concat


(* *** Check index coherence *)

let check_index_coherence_quantification idxs quantification =
  let _ = L.fold_left quantification
     ~init:idxs
     ~f:(fun indices ((i,seti),exn) ->
       if L.exists exn ~f:(fun (j,setj) -> i = j || not(equal_set seti setj) || not(L.mem indices (j,setj) ~equal:equal_idx )) then
         failwith ("Indices are not coherent in quantification: " ^ (string_of_list ", " string_of_id_exception quantification))
       else indices @ [(i,seti)]
     )
  in ()

let rec check_index_coherence_expr indices = function
  | Add(e1,e2) | Mul(e1,e2) | Div(e1,e2) | PWProd(e1,e2) ->
     check_index_coherence_expr indices e1; check_index_coherence_expr indices e2
  | Opp(e) | Transpose(e) | Diag(e) -> check_index_coherence_expr indices e
  | Atom(a) ->
     if L.exists (atom_indices a) ~f:(fun i -> not(L.mem indices i ~equal:equal_idx )) then
       failwith ("atom " ^ (string_of_atom a) ^ " contains unbound indices")
     else ()
  | Sum(k,exn,e) | Prod(k,exn,e) ->
     check_index_coherence_quantification indices [(k,exn)];
     check_index_coherence_expr (indices @ [k]) e

let check_index_coherence_eq indices eq =
  check_index_coherence_quantification indices eq.eq_forall;
  check_index_coherence_expr (indices @ (L.map eq.eq_forall ~f:(fun (i,_) -> i))) eq.eq_expr

let check_index_coherence_conj conj =
  check_index_coherence_quantification [] conj.conj_exists;
  L.iter conj.conj_equations ~f:(check_index_coherence_eq (L.map conj.conj_exists ~f:(fun (i,_) -> i)) )


(* *** Simplify normalized expressions *)

(* **** Description of normalized expression *)

(* A normalized expression does not contain divisions,
   it consists of an addition of nested summations of products.
   All summations include all possible index exceptions
 *)


let rec contains_summations = function
  | Mul(e1,e2) | Div(e1,e2) | PWProd(e1,e2) -> (contains_summations e1) || (contains_summations e2)
  | Opp(e) | Transpose(e) | Diag (e) -> contains_summations e
  | Add(_) | Sum(_) -> true
  | Atom(_) -> false
  | Prod(_,_,_) -> false (* If additions are inside products, these are treated as blocks *)

let rec is_normalized = function
  | Add(e1,e2) -> (is_normalized e1) && (is_normalized e2)
  | Mul(e1,e2) -> not (contains_summations e1) && not (contains_summations e2) &&
                    (is_normalized e1) && (is_normalized e2)
  | Diag(e) -> not (contains_summations e) && (is_normalized e)
  | PWProd(e1,e2) -> not (contains_summations e1) && not (contains_summations e2) &&
                             (is_normalized e1) && (is_normalized e2)
  | Opp(Transpose(Atom(_))) | Opp(Atom(_)) | Transpose(Atom(_)) -> true
  | Opp(_) | Transpose(_) | Div(_) -> false
  | Prod(i,_,e) -> (* If additions are inside products, these are treated as blocks *)
     begin match e with
     | Sum(j,exn,_) | Prod(j,exn,_) ->
        (is_normalized e) && (not (equal_set (index_set i) (index_set j)) || (L.mem exn i))
     | e' -> is_normalized e'
     end
  | Sum(i,_,e) ->
     begin match e with
     | Sum(j,exn,_) | Prod(j,exn,_) ->
        (is_normalized e) && (not (equal_set (index_set i) (index_set j)) || (L.mem exn i))
     | e' -> not (contains_summations e') && (is_normalized e')
     end
  | Atom(_) -> true

let summations_list_of_norm_expr expr =
  (if not (is_normalized expr) then assert false else ());
  let rec aux output = function
    | Add(e1,e2) -> (aux output e1) @ (aux [] e2)
    | _ as e -> output @ [e]
  in
  aux [] expr

let norm_expr_of_summations_list = function
  | [] -> Atom(Zero(scalar_dim))
  | a :: rest -> L.fold_left rest ~init:a ~f:(fun e b -> Add(e,b))
                 |> (fun e -> if is_normalized e then e else assert false)

let expr_of_summations_list = function
  | [] -> Atom(Zero(scalar_dim))
  | a :: rest -> L.fold_left rest ~init:a ~f:(fun e b -> Add(e,b))

let rec split_summation = function
  | Sum(i,exn,e) ->
     let sum_indices, product = split_summation e in
     (i,exn) :: sum_indices, product
  | _ as p -> [], p

let summation_of_split (indices,p) =
  L.fold_left (L.rev indices) ~init:p ~f:(fun e (i,exn) -> Sum(i,exn,e))

let mul_of_list list =
  let list =
    let first,tail = match list with | [] -> Atom(Int(BI.one)), [] | a :: tail -> a, tail in
    L.fold_left tail
       ~init:[first]
       ~f:(fun l a ->
         match l with
         | (Diag(e)) :: rest ->
            begin match a with
            | Diag(e') -> Diag(PWProd(e,e')) :: rest
            | _ -> a :: l
            end
         | _ -> a :: l
       )
    |> L.rev
  in
  match list with
  | [] -> assert false
  | a :: rest -> L.fold_left rest ~init:a ~f:(fun e b -> Mul(e,b))

let rec split_product = function
  | Mul(e1,e2) ->
     let coeff1, scalars1, terms1 = split_product e1 in
     let coeff2, scalars2, terms2 = split_product e2 in
     let prod = Mul(mul_of_list terms1, mul_of_list terms2) in
     if terms1 @ terms2 <> [] && equal_dim (expr_dimension prod) scalar_dim then
       BI.(coeff1 *! coeff2), scalars1 @ scalars2 @ [terms1 @ terms2], []
     else
       BI.(coeff1 *! coeff2), scalars1 @ scalars2, terms1 @ terms2
  | PWProd(e1,e2) ->
     let coeff1, scalars1, terms1 = split_product e1 in
     let coeff2, scalars2, terms2 = split_product e2 in
     let prod = PWProd(mul_of_list terms1, mul_of_list terms2) in
     BI.(coeff1 *! coeff2), scalars1 @ scalars2, [prod]
  | Prod(i,exn,e) ->
     (if equal_dim (expr_dimension e) scalar_dim then () else assert false);
     BI.one, [[Prod(i,exn,e)]], []
  | Opp(e) -> let coeff, scalars, terms = split_product e in BI.opp coeff, scalars, terms
  | Transpose(e) ->
     let coeff, scalars, terms = split_product e in
     coeff, scalars, (L.rev (L.map terms ~f:(fun t -> Transpose(t))))
  | Diag(e) ->
     let coeff, scalars, terms = split_product e in
     coeff, scalars, [Diag(mul_of_list terms)]
  | Atom(Int(i)) -> i, [], []
  | Atom(Zero(_)) -> BI.zero, [], []
  | Atom(x) -> if equal_dim (atom_dimension x) scalar_dim then BI.one, [[Atom(x)]], []
               else BI.one, [], [Atom(x)]
  | Add(_,_) -> assert false
  | Sum(_) -> failwith "Summations are not allowed inside Prods()"
  | Div(_) -> assert false

let full_split_summation term =
  let indices, product = split_summation term in
  let coeff, scalars, terms = split_product product in
  indices, coeff, scalars, terms

let summation_of_full_split (indices, coeff, scalars, terms) =
  let scalars =
    L.map scalars ~f:(fun l ->
        let l' = L.rev (L.map l ~f:(fun a -> full_simplify (Transpose(a)) )) in
        if compare_lists ~compare:compare_expr l l' > 0 then l' else l
          )
  in
  let p = mul_of_list ([Atom(Int(coeff))] @ (L.map scalars ~f:mul_of_list ) @ terms) in
  summation_of_split (indices, p)

let check_all_common_exceptions idxs =
  let qindices = L.map idxs ~f:(fun (i,_) -> i) in
  L.iter idxs
    ~f:(fun (_,exn) ->
      L.iter (L.filter exn ~f:(fun i -> not(L.mem qindices i)))
        ~f:(fun j ->
          if L.exists idxs ~f:(fun (_,exn) -> not (L.mem exn j)) then
            failwith "Expression is non index-normalized"
          else ()
        )
    )

let rename_term indices1 indices2 ss ts =
  L.fold_left (L.zip_exn indices1 indices2)
      ~init:(ss,ts)
      ~f:(fun (ss',ts') (i,j) ->
        let () = if not (equal_set (index_set i) (index_set j)) then assert false else () in
        L.map ss' ~f:(L.map ~f:(subst_idx i j)),
        L.map ts' ~f:(subst_idx i j)
      )

let norm_transpose l =
  let l' = L.rev (L.map l ~f:(function | Transpose(e) -> e | e -> Transpose(e))) in
  if compare_lists ~compare:compare_expr l l' > 0 then l
  else l'

let equivalent_terms (idxs1,_c1,ss1,ts1) (idxs2,_c2,ss2,ts2) =
  if
    (try
       check_all_common_exceptions idxs1;
       check_all_common_exceptions idxs2;
       false
     with | _ -> true
    )
  then false
  else
    if (L.length idxs1 <> L.length idxs2) || (L.length ss1 <> L.length ss2) then false
    else
      let indices =
        (L.map idxs1 ~f:(fun (i,exn) -> i :: exn) |> L.concat) @
          (L.map ss1 ~f:(fun l -> L.map l ~f:get_indices |> L.concat) |> L.concat) @
            (L.map ts1 ~f:get_indices |> L.concat) @
              (L.map idxs2 ~f:(fun (i,exn) -> i :: exn) |> L.concat) @
                (L.map ss2 ~f:(fun l -> L.map l ~f:get_indices |> L.concat) |> L.concat) @
                  (L.map ts2 ~f:get_indices |> L.concat)
      in
      let fresh_indices =
        L.fold_left idxs1
                    ~init:[]
                    ~f:(fun fresh (i,_) -> let j = fresh_index i (indices @ fresh) in fresh @ [j])
      in
    let ss1', ts1' = rename_term (L.map idxs1 ~f:(fun (i,_) -> i)) fresh_indices ss1 ts1 in
    L.exists (perms fresh_indices)
             ~f:(fun perm ->
               let indices1 = L.map idxs1 ~f:(fun (i,_) -> i) in
               if L.exists (L.zip_exn indices1 perm)
                           ~f:(fun (i1,i2) -> not (equal_set (index_set i1) (index_set i2)))
               then false
               else
                 let ss2', ts2' = rename_term (L.map idxs1 ~f:(fun (i,_) -> i)) perm ss2 ts2 in
                 if not (equal_expr (mul_of_list ts1') (mul_of_list ts2')) then false
                 else equal_lists ~compare:compare_expr
                                (L.map (L.map ss1' ~f:norm_transpose ) ~f:mul_of_list )
                                (L.map (L.map ss2' ~f:norm_transpose ) ~f:mul_of_list )
             )

let simplify_normalized_expr expr =
  L.fold_left (summations_list_of_norm_expr expr |> L.map ~f:full_split_summation )
    ~init:[]
    ~f:(fun l (idxs, c, ss, ts) ->
      match list_find_and_remove ~f:(equivalent_terms (idxs,c,ss,ts)) l with
      | None, _ -> l @ [(idxs,c,ss,ts)]
      | Some (_,c',_,_), l' -> l' @ [(idxs, BI.(c +! c'), ss, ts)]
    )
  |> L.map ~f:summation_of_full_split
  |> norm_expr_of_summations_list

let step_simplify expr =
  if is_normalized expr then simplify_expr (simplify_normalized_expr expr)
  else simplify_expr expr

let full_norm_simplify expr =
  let rec aux last expr =
(*    F.printf "%a\n\n" pp_expr expr; F.print_flush();*)
    if equal_expr last expr then expr
    else aux expr (step_simplify expr)
  in
  aux expr (step_simplify expr)


(* *** Substitute expr improved *)

let subst_expr_improved ~old ~by expr =
  if not (is_normalized old) || not(is_normalized by) || not(is_normalized expr) then
    subst_expr ~old ~by expr
  else
    let expr = subst_expr ~old ~by expr in
    match summations_list_of_norm_expr old, summations_list_of_norm_expr by with
    | s_old :: [], s_by :: [] ->
       let indices_old, coeff_old, scalars_old', terms_old = full_split_summation s_old in
       let indices_by, coeff_by, scalars_by', terms_by = full_split_summation s_by in
       let scalars_old = L.map scalars_old' ~f:(fun sc -> mul_of_list sc |> full_simplify) in
       let scalars_by  = L.map scalars_by' ~f:(fun sc -> mul_of_list sc |> full_simplify) in
       if indices_old <> [] || indices_by <> [] then subst_expr ~old ~by expr
       else
         L.map (summations_list_of_norm_expr expr)
          ~f:(fun s ->
            let indices, coeff, scalars', terms = full_split_summation s in
            let scalars = L.map scalars' ~f:(fun sc -> mul_of_list sc |> full_simplify) in
            if (list_is_sublist_of_list terms_old terms ~equal:equal_expr ) &&
                 (list_is_contained_in_list scalars_old scalars
                    ~equal:(fun e1 e2 -> equal_expr e1 e2 || equal_expr (full_simplify (Transpose(e1))) e2))
            then
              let new_scalars =
                (L.fold_left scalars_old
                   ~init:scalars
                   ~f:(fun scalars' sc ->
                     let sct = full_simplify (Transpose sc) in
                     let equal sc' = equal_expr sc sc' || equal_expr sct sc' in
                     remove_first scalars' ~equal
                   )
                ) @ scalars_by
              in
              let new_terms = replace_sublist_of_list_by_list ~equal:equal_expr terms_old terms_by terms in
              Div(
                  summation_of_full_split (indices, BI.((coeff *! coeff_by)), [], new_scalars @ new_terms),
                  Atom(Int(coeff_old))
                )
            else
              (* This one may occur frequently due to normalization *)
              let split_scalars scalars =
                L.map (L.concat scalars)
                      ~f:(fun s -> let (c,l,t) = split_product s in
                                   if not (BI.is_one c) then assert false
                                   else (L.concat l) @ t
                                 )
                |> L.concat
              in
              let scalars_old'' = split_scalars scalars_old' in
              let scalars_by'' = split_scalars scalars_by' in
              let scalars'' = split_scalars scalars' in
              if (list_is_sublist_of_list (scalars_old'' @ terms_old) (scalars'' @ terms) ~equal:equal_expr ) then
                let new_terms = replace_sublist_of_list_by_list
                                  ~equal:equal_expr
                                  (scalars_old'' @ terms_old) (scalars_by'' @ terms_by) (scalars'' @ terms)
                in
                Div(
                  summation_of_full_split (indices, BI.((coeff *! coeff_by)), [], new_terms),
                  Atom(Int(coeff_old))
                )
              else s
          )
         |> expr_of_summations_list
    | _, _ -> subst_expr ~old ~by expr


(* *** Simplify equations *)

let clean_quantifier = L.map ~f:(fun (i,exn) -> (i,L.dedup exn ~compare:compare_idx ))
let dedup_quantifier = remove_duplicates ~equal:equal_exists

let simplify_equation ?(full = false) eq =
  let eq =
    if full then { eq with eq_forall = clean_quantifier eq.eq_forall; eq_expr = full_norm_simplify eq.eq_expr; }
    else { eq with eq_forall = clean_quantifier eq.eq_forall; eq_expr = step_simplify eq.eq_expr; }
  in
  let free = free_indices eq.eq_expr in
  let new_forall = L.filter eq.eq_forall ~f:(fun (i,_) -> L.mem free i ~equal:equal_idx ) in
  let useless = L.filter eq.eq_forall ~f:(fun (i,_) -> not(L.mem free i ~equal:equal_idx ))
                |> L.map ~f:(fun (i,_) -> i)
  in
  let new_forall = L.map new_forall
       ~f:(fun (i,exn) -> (i, L.filter exn ~f:(fun j -> not(L.mem useless j ~equal:equal_idx ))))
  in
  let eq = { eq with eq_forall = new_forall; } in
  let forall_dep i = L.exists eq.eq_forall ~f:(fun (j,_) -> equal_idx i j) in
  match eq.eq_expr, eq.eq_is_eq with
  | Div(e1,e2), Eq -> [ (None, { eq_forall = eq.eq_forall; eq_is_eq = Eq; eq_expr = e1 });
                        (None, { eq_forall = eq.eq_forall; eq_is_eq = InEq; eq_expr = e2 }) ]
  | Atom(Int(i)), Eq when not (BI.is_zero i) -> [(None, { eq with eq_expr = Atom(Int(BI.one)); })]  (* Reference to prime bound here *)
  | Atom(Int(i)), InEq when not (BI.is_zero i) -> []
  | Mul(Atom(Int(i)),e), _ | Mul(e,Atom(Int(i))), _ when not (BI.is_zero i) ->
     [(None, {eq with eq_expr = e})] (* Reference to prime bound here *)
  | Mul(e1,e2), InEq when (not(L.exists (get_indices e1) ~f:forall_dep ) || not(L.exists (get_indices e1) ~f:forall_dep )) &&
                            (expr_dimension e1 = scalar_dim || expr_dimension e2 = scalar_dim) ->
     [ (None, { eq with eq_expr = e1; }); (None, { eq with eq_expr = e2; }) ]
  | Prod(i,exn,e), InEq -> [ (None, { eq with eq_forall = eq.eq_forall @ [(i,exn)]; eq_expr = e; }) ]
  | Prod(i,exn,e), Eq ->
     let j = fresh_index i (get_indices_eq eq) in
     [ (Some (j,exn), { eq with eq_expr = subst_idx i j e; }) ]
  | Diag(e), _ -> [(None, { eq with eq_expr = e; })]
  | _ -> [(None,eq)]

let contradictory_equations eq1 eq2 =
  (equal_expr eq1.eq_expr eq2.eq_expr) &&
    not (equal_is_eq eq1.eq_is_eq eq2.eq_is_eq) &&
      (equal_lists eq1.eq_forall eq2.eq_forall ~compare:compare_forall )

let find_contradictions conj =
  L.fold_left conj.conj_equations
    ~init:{ conj with conj_equations = []; }
    ~f:(fun conj' eq ->
      let new_eq =
        if L.exists conj'.conj_equations ~f:(contradictory_equations eq) then
          { eq with eq_is_eq = InEq; eq_expr = Atom(Zero(scalar_dim)) }
        else eq
      in
      { conj' with conj_equations = conj'.conj_equations @ [new_eq] }
    )

let trivial_eq eq =
  if is_zero_expr eq.eq_expr && eq.eq_is_eq = Eq then true
  else false

let remove_trivial conj =
  { conj with conj_equations = L.filter conj.conj_equations ~f:(fun eq -> not(trivial_eq eq)); }

let rec subst_quantified (i,iexn) t e expr =
  let f = subst_quantified (i,iexn) t e in
  match expr with
  | Add(e1,e2)    -> Add(f e1, f e2)
  | Mul(e1,e2)    -> Mul(f e1, f e2)
  | Div(e1,e2)    -> Div(f e1, f e2)
  | Opp(e)        -> Opp(f e)
  | Transpose(e)  -> Transpose(f e)
  | Diag(e)       -> Diag(f e)
  | Atom(a)       -> Atom(a)
  | Sum(j,jexn,expr)  ->
     if (list_is_contained_in_list iexn jexn ~equal:equal_idx ) && (equal_set (index_set i) (index_set j)) then
       Sum(j,jexn, f (subst_expr_improved ~old:(subst_idx i j t) ~by:(subst_idx i j e) expr))
     else Sum(j,jexn,f expr)
  | Prod(j,jexn,expr)  ->
     if (list_is_contained_in_list iexn jexn ~equal:equal_idx ) && (equal_set (index_set i) (index_set j)) then
       Prod(j,jexn, f (subst_expr_improved ~old:(subst_idx i j t) ~by:(subst_idx i j e) expr))
     else Prod(j,jexn,f expr)
  | PWProd(e1,e2) -> PWProd(f e1, f e2)

let get_linear_term substituted eq =
  if not (is_normalized eq.eq_expr) then None
  else
    let linear s =
      let iexn, p = split_summation s in
      match iexn with
      | [] ->
         let coeff, scalars, terms = split_product p in
         begin match scalars, terms with
         | [], a :: [] | (a :: []) :: [], [] when not(BI.is_zero coeff) &&
                begin match a with | Atom(Matrix(_,Param,_,_,_)) | Transpose(Atom(Matrix(_,Param,_,_,_))) -> true | _ -> false end
           -> Some(coeff,a)
         | _ -> None
         end
      | _ -> None
    in
    let terms =
      L.map (summations_list_of_norm_expr eq.eq_expr) ~f:linear
      |> L.filter ~f:(function | None -> false | Some _ -> true)
    in
    let set_term = function
      | None -> None
      | Some (coeff,t) ->
         let a,is_transposed =
           begin match t with
           | Atom(a) -> a, false
           | Transpose(Atom(a)) -> a, true
           | _ -> assert false
           end
         in
         if (L.exists substituted
               ~f:(fun (idx_opt, a', _) ->
                 begin match a,a' with
                 | Matrix(s1,Param,idxs1,_,_), Matrix(s2,Param,idxs2,_,_) ->
                    if s1 <> s2 then false
                    else
                      begin match L.zip idxs1 idxs2 with
                      | None -> false
                      | Some l ->
                         L.fold_left l
                            ~init:true
                            ~f:(fun b (i,i') -> b &&
                                ( equal_idx i i' ||
                                    ((begin match idx_opt with | None -> false | Some (j,_) -> i' = j end) &&
                                       (L.exists eq.eq_forall ~f:(fun (j,_) -> i = j)) &&
                                    (equal_set (index_set i) (index_set i')))
                                )
                               )
                      end
                 | _ -> false
                 end
               )
            ) || (get_vars eq.eq_expr) <> []
         then None
         else
           let by = full_norm_simplify (Add(Opp(eq.eq_expr), Mul(Atom(Int(coeff)), t))) in
           let by = if is_transposed then full_norm_simplify (Transpose(by)) else by in
           let by = Div(by, Atom(Int(coeff))) in
           if L.exists (get_atoms by)
                       ~f:(fun atom ->
                         L.exists (a :: (L.map substituted ~f:(fun (_,a',_) -> a')))
                           ~f:(fun a' ->
                             begin match atom, a' with
                             | Matrix(s,Param,idxs,d,t), Matrix(s',Param,idxs',d',t') ->
                                s = s' && (L.length idxs = L.length idxs') &&
                                  equal_dim d d' && equal_group_type t t'
                             | _, Matrix(_,Param,_,_,_) -> false
                             | _, _ -> assert false
                             end
                           )
                       ) then None
           else
             begin match eq.eq_forall with
             | [] -> Some (None, a, by)
             | (i,iexn) :: [] when L.mem (atom_indices a) i ~equal:equal_idx -> Some (Some (i,iexn), a, by)
             | _ -> None
             end
    in
    match L.find (L.map terms ~f:set_term ) ~f:(function | None -> false | Some _ -> true) with
    | None -> None
    | Some c -> c

let apply_linear_eq (idx, t, e) eq =
  match idx with
  | None -> { eq with eq_expr = subst_expr_improved ~old:(Atom(t)) ~by:e eq.eq_expr }
  | Some (i,iexn) ->
     let expr =
       L.fold_left eq.eq_forall
         ~init:eq.eq_expr
         ~f:(fun expr (j,jexn)->
           if equal_set (index_set i) (index_set j) && list_is_contained_in_list iexn jexn ~equal:(=) then
             subst_expr_improved ~old:(Atom(subst_idx_in_atom i j t)) ~by:(subst_idx i j e) expr
           else expr
         )
     in
     { eq with eq_expr = subst_quantified (i,iexn) (Atom(t)) e expr }

let simplify_linear_equations conj =
  L.fold_left conj.conj_equations
     ~init:conj
     ~f:(fun conj eq ->
       if eq.eq_is_eq = InEq then conj
       else
         match get_linear_term conj.conj_substituted eq with
         | None -> conj
         | Some (idx,t,e) ->
            { conj with conj_equations = L.map conj.conj_equations ~f:(apply_linear_eq (idx,t,e));
                        conj_substituted =
                          (L.map conj.conj_substituted
                              ~f:(fun (idx',t',e') ->
                                let eq = { eq_forall = begin match idx' with | None -> [] | Some a -> [a] end;
                                           eq_is_eq = Eq; (* This does not matter *)
                                           eq_expr = e'
                                         }
                                         |> apply_linear_eq (idx,t,e)
                                in
                                (idx', t', eq.eq_expr)
                              )
                          ) @ [(idx,t,e)]
            }
     )

let apply_substitutions_from_conj conj =
  let new_equations =
    L.fold_left conj.conj_substituted
       ~init:conj.conj_equations
       ~f:(fun eqs (idx,t,e) -> L.map eqs ~f:(apply_linear_eq (idx,t,e)))
  in
  { conj with conj_equations = new_equations }

let apply_replacement_eq (f,e1,e2) eq =
  match f with
  | [] ->
     if equal_expr eq.eq_expr (full_simplify (Add(e1,Opp(e2)))) then eq
     else { eq with eq_expr = full_simplify (subst_expr_improved ~old:e1 ~by:e2 eq.eq_expr) }
  | (i,iexn) :: [] ->
     let eq = { eq with eq_expr = subst_quantified (i,iexn) e1 e2 eq.eq_expr; } in
     L.fold_left eq.eq_forall
        ~init:eq
        ~f:(fun eq (j,jexn) ->
          if list_is_contained_in_list iexn jexn ~equal:equal_idx then
            if not (equal_set (index_set i) (index_set j)) || equal_expr eq.eq_expr (full_simplify (Add(e1,Opp(e2)))) then eq
            else
              let new_expr = full_simplify (subst_expr_improved ~old:(subst_idx i j e1) ~by:(subst_idx i j e2) eq.eq_expr) in
              { eq with eq_expr = new_expr }
          else eq
        )
  | _ -> eq (* Not supported yet *)

let apply_replacements_from_conj conj =
  let new_equations =
    L.map conj.conj_replaced ~f:(fun r -> L.map conj.conj_equations ~f:(apply_replacement_eq r))
    |> L.concat
  in
  (*
    L.fold_left conj.conj_replaced
       ~init:conj.conj_equations
       ~f:(fun eqs r -> L.map eqs ~f:(apply_replacement_eq r))
  in
   *)
  { conj with conj_equations = conj.conj_equations @ new_equations }


let subst_zero_expr (idx,zt) expr =
  if not (is_normalized expr) then expr
  else
    L.map (summations_list_of_norm_expr expr)
       ~f:(fun s ->
         let indices, _, scalars, terms = full_split_summation s in
         match idx with
         | None ->
            if (list_is_sublist_of_list zt ((L.concat scalars) @ terms) ~equal:equal_expr ) ||
                 (list_is_sublist_of_list (L.rev (L.map zt ~f:(fun a -> Transpose(a)))) ((L.concat scalars) @ terms) ~equal:equal_expr ) then
              summation_of_full_split (indices, BI.zero, [], [])
            else s
         | Some (i,iexn) ->
            let indices_with_free_exns =
              L.fold_left indices
                  ~init:[]
                  ~f:(fun idxs' (j,exn) ->
                    idxs' @ [(j, L.filter exn ~f:(fun k -> not(L.exists idxs' ~f:(fun (k',_) -> equal_idx k k'))))]
                  )
            in
            if L.exists indices_with_free_exns ~f:(fun (j,jexn) ->
               if (equal_set (index_set j) (index_set j)) && list_is_contained_in_list iexn jexn ~equal:equal_idx then
                 let zt' = L.map zt ~f:(subst_idx i j) in
                 (list_is_sublist_of_list zt' ((L.concat scalars) @ terms) ~equal:equal_expr ) ||
                   (list_is_sublist_of_list (L.rev (L.map zt' ~f:(fun a -> Transpose(a)))) ((L.concat scalars) @ terms) ~equal:equal_expr )
               else false
              )
            then summation_of_full_split (indices, BI.zero, [], [])
            else s
       )
    |> norm_expr_of_summations_list

let substitute_zero_eq (idx, zt) eq =
  match idx with
  | None -> { eq with eq_expr = subst_zero_expr (None,zt) eq.eq_expr }
  | Some (i,iexn) ->
     let expr =
       L.fold_left eq.eq_forall
         ~init:eq.eq_expr
         ~f:(fun expr (j,jexn)->
           if (equal_set (index_set i) (index_set j)) && list_is_contained_in_list iexn jexn ~equal:equal_idx then
             subst_zero_expr (None, (L.map zt ~f:(subst_idx i j))) expr
           else expr
         )
     in
     let expr = subst_zero_expr (Some (i,iexn), zt) expr in
     { eq with eq_expr = subst_zero_expr (Some (i,iexn), zt) expr }

let substitute_zero_terms conj =
  L.fold_left conj.conj_equations
     ~init:conj
     ~f:(fun conj eq ->
	 if eq.eq_is_eq = InEq || not (is_normalized eq.eq_expr) ||
	      (get_vars eq.eq_expr) <> [] then conj
       else
         match summations_list_of_norm_expr eq.eq_expr with
         | Atom(Int(_)) :: [] -> conj
         | s :: [] ->
            let indices, product = split_summation s in
            if indices <> [] then conj
            else
              let (coeff, scalars, terms) = split_product product in
              if BI.is_zero coeff then conj
              else
                let zt = (L.concat scalars) @ terms in
                let z_expr = mul_of_list zt in
                let eq = { eq with eq_expr = z_expr } in
                begin match eq.eq_forall with
                | [] -> { conj with conj_equations = (L.map conj.conj_equations ~f:(substitute_zero_eq (None, zt))) @ [eq];
                                    conj_replaced = conj.conj_replaced @ [([],z_expr,Atom(Zero(expr_dimension z_expr)))]
                        }
                | (i,iexn) :: [] -> { conj with conj_equations = (L.map conj.conj_equations ~f:(substitute_zero_eq (Some (i,iexn), zt))) @ [eq];
                                                conj_replaced = conj.conj_replaced @ [(eq.eq_forall,z_expr,Atom(Zero(expr_dimension z_expr)))]
                                    }
                | _ -> conj
                end
(*         | s1 :: s2 :: [] when (get_vars s1) = [] && (get_vars s2) = [] ->
            let (indices1, coeff1, scalars1, terms1) = full_split_summation s1 in
            let (indices2, coeff2, scalars2, terms2) = full_split_summation s2 in
            if (BI.is_zero coeff1) || (BI.is_zero coeff2) || (indices1 <> []) || (indices2 <> []) then conj
            else
              if compare_expr s1 s2 > 0 then (* Prime bound here *)
                let old = summation_of_full_split (indices1, BI.one, scalars1, terms1) in
                let by  = Div(s2, Atom(Int(BI.opp coeff1))) |> full_simplify in
                { conj with conj_replaced = conj.conj_replaced @ [(eq.eq_forall, old, by)] }
              else
                let old = summation_of_full_split (indices2, BI.one, scalars2, terms2) in
                let by  = Div(s1, Atom(Int(BI.opp coeff2))) |> full_simplify in
                { conj with conj_replaced = conj.conj_replaced @ [(eq.eq_forall, old, by)] }
 *)
         | _ -> conj
     )

let replaced_to_equation (f,e1,e2) =
  { eq_forall = f; eq_is_eq = Eq; eq_expr = full_simplify (Add(e1,Opp(e2))) }

let undo_replacements conj =
  { conj with
    conj_equations = conj.conj_equations @ (L.map conj.conj_replaced ~f:replaced_to_equation );
    conj_substituted = L.map conj.conj_substituted ~f:(fun (idx,t,e) -> (idx,t,full_norm_simplify e));
    conj_replaced = [];
  }

let compare_replaced (f,e1,e2) (f',e1',e2') =
  let c = compare_lists f f' ~compare:compare_forall in
  if c <> 0 then c
  else
    let c = compare_expr e1 e1' in
    if c <> 0 then c
    else compare_expr e2 e2'

let dedup_conj conj =
  { conj with conj_equations = L.dedup conj.conj_equations ~compare:compare_equation; }

let simplify_inside_conj ~full conj =
  L.fold_left (L.map (L.dedup conj.conj_equations ~compare:compare_equation ) ~f:(simplify_equation ~full))
     ~init:{ conj with conj_exists = dedup_quantifier conj.conj_exists; conj_equations = []; }
     ~f:(fun conj eqs ->
       let new_exists, new_eqs =
         L.fold_left eqs
            ~init:([],[])
            ~f:(fun (exists,eqs) (a,eq) ->
              match a with
              | None -> (exists, eqs @ [eq])
              | Some (i,exn) ->
                 let j = fresh_index i (get_indices_conj conj) in
                 (exists @ [(j,exn)], eqs @ [subst_idx_in_eq i j eq])
            )
       in
       { conj_exists = conj.conj_exists @ new_exists;
         conj_equations = conj.conj_equations @ new_eqs;
         conj_substituted = L.map conj.conj_substituted ~f:(fun (idx,t,e) -> (idx,t,full_norm_simplify e));
         conj_replaced = L.map conj.conj_replaced ~f:(fun (f,e1,e2) -> (f,full_norm_simplify e1,full_norm_simplify e2))
                         |> L.dedup ~compare:compare_replaced
       }
     )

(* *** Groebner basis simplification *)

type gb_poly = (BI.t * BI.t list) list

let polyvars_of_expr expr =
  L.map (summations_list_of_norm_expr expr)
      ~f:(fun term ->
        let indices, _coeff, scalars, terms = full_split_summation term in
        if (terms <> []) then assert false
        else
          let f s = L.exists (get_indices s)
                       ~f:(fun i -> L.mem (L.map indices ~f:(fun (j,_) -> j)) i ~equal:equal_idx )
          in
          let g ss = L.exists ss ~f in
          let l1 = L.filter scalars ~f:(fun ss -> not(g ss)) in
          let l2 = L.filter scalars ~f:g in
          if (indices <> []) then
            (L.map (L.filter l1 ~f:(fun a -> a <> [])) ~f:mul_of_list ) @
              [summation_of_full_split (indices, BI.one, l2, terms)]
          else L.map (L.filter l1 ~f:(fun a -> a <> [])) ~f:mul_of_list
      )
  |> L.concat
  |> L.dedup ~compare:compare_expr

let polyvars_of_system system =
  L.map system ~f:polyvars_of_expr |> L.concat |> L.dedup ~compare:compare_expr

let gb_poly_of_norm_expr polyvars expr =
  L.map (summations_list_of_norm_expr expr)
      ~f:(fun term ->
        let indices, coeff, scalars, terms = full_split_summation term in
        if (terms <> []) then assert false
        else
          let f s = L.exists (get_indices s)
                       ~f:(fun i -> L.mem (L.map indices ~f:(fun (j,_) -> j)) i ~equal:equal_idx )
          in
          let g ss = L.exists ss ~f in
          let l1 = L.filter scalars ~f:(fun ss -> not(g ss)) in
          let l2 = L.filter scalars ~f:g in
          let monom_list =
            if (indices <> []) then
              (L.map (L.filter l1 ~f:(fun a -> a <> [])) ~f:mul_of_list ) @
                [summation_of_full_split (indices, BI.one, l2, terms)]
            else L.map (L.filter l1 ~f:(fun a -> a <> [])) ~f:mul_of_list
          in
          (coeff, L.map monom_list
             ~f:(fun m -> L.map polyvars ~f:(fun v ->
                    if (equal_expr m v) || (equal_expr (full_simplify (Transpose(m))) v) then 1 else 0))
                  |> L.fold_left
                       ~init:(mk_list 0 (L.length polyvars))
                       ~f:(fun degrees d ->
                         assert(list_sum ~add:(+) ~zero:0 d = 1);
                         L.map2_exn degrees d ~f:(+)
                       )
          )
      )

let string_of_gb_term (c,degrees) =
  "(" ^ (BI.to_string c) ^ ",[" ^ (string_of_list "," string_of_int degrees) ^ "])"

let string_of_gb_poly gbpoly =
  "[" ^ (string_of_list "," string_of_gb_term gbpoly) ^ "]"

let gb_cmd gbpolys =
  "{'cmd': 'GroebnerBasis', 'system': [" ^
    (string_of_list "," string_of_gb_poly gbpolys) ^ "]}\n"

let gb_term_of_string string =
  let l = S.split ~on:',' string in
  let coeff = BI.of_int (int_of_string (L.hd_exn l)) in
  let monoms = L.map (L.tl_exn l) ~f:int_of_string in
  (coeff, monoms)

let gb_poly_of_string string =
  L.map (S.split ~on:'t' string) ~f:gb_term_of_string

let gb_system_of_string string =
  L.map (S.split ~on:'p' string) ~f:gb_poly_of_string

let expr_of_gb_poly polyvars gb_poly =
  L.fold_left gb_poly
     ~init:(Atom(Zero(scalar_dim)))
     ~f:(fun s (c,degrees) ->
       let m =
         L.fold_left (L.zip_exn degrees polyvars)
            ~init:(Atom(Int(c)))
            ~f:(fun p (d,v) ->
              L.fold_left (mk_list v d)
                  ~init:p
                  ~f:(fun p' t -> Mul(p',t))
            )
       in
       Add(s,m)
     )

let groebner_basis_conj (conj : conjunction) =
  let filter eq = eq.eq_is_eq = Eq && eq.eq_forall = [] &&
                    (expr_dimension eq.eq_expr = scalar_dim)
  in
  let eqs = L.filter conj.conj_equations ~f:filter in
  let rest_eqs = L.filter conj.conj_equations ~f:(fun eq -> not (filter eq)) in
  let expressions = L.map eqs ~f:(fun eq -> eq.eq_expr) in
  let exprs = expressions |> L.map ~f:full_norm_simplify in
  let polyvars = polyvars_of_system exprs in
  let gb_ps = L.map exprs ~f:(gb_poly_of_norm_expr polyvars) in
  let answer = call_Sage (gb_cmd gb_ps) in
  if answer = "NoVar" then
    (*let () = F.printf "here!!\n%a\n\n" pp_conj conj in
    F.print_flush();*)
    simplify_inside_conj ~full:true conj
  else
    try
      let exprs = L.map (gb_system_of_string answer) ~f:(expr_of_gb_poly polyvars)
                  |> (L.map ~f:full_norm_simplify)
                  |> (fun exprs -> L.iter exprs ~f:(fun e -> if not(is_normalized e) then assert false else ()); exprs)
      in
      let new_eqs =
        L.map exprs ~f:(fun e -> { eq_forall = []; eq_is_eq = Eq; eq_expr = e; })
      in
      let conj = { conj with conj_equations = new_eqs @ rest_eqs; } in
      (*F.printf "gb\n%a\n\n" pp_conj conj;*)
      conj
    with
    | _ -> conj

let simplify_conj ?(full = false) conj =
  simplify_inside_conj ~full conj
  (*  |> apply_replacements_from_conj *)
  |> find_contradictions
  |> simplify_linear_equations
  |> substitute_zero_terms
  |> apply_substitutions_from_conj
  (*  |> apply_replacements_from_conj *)
  |> simplify_inside_conj ~full
  |> remove_trivial
  |> dedup_conj

let rec contains_division_expr = function
  | Add(e1,e2) | Mul(e1,e2) | PWProd(e1,e2) -> (contains_division_expr e1) || (contains_division_expr e2)
  | Opp(e) | Sum(_,_,e) | Prod(_,_,e) | Transpose(e) | Diag(e) -> contains_division_expr e
  | Div(_,_) -> true
  | Atom(_) -> false

let contains_division_eq eq =
  contains_division_expr eq.eq_expr

let contains_division_conj conj =
  L.exists conj.conj_equations ~f:contains_division_eq

let remove_divisions_conj conj =
  let rec aux conj =
    if contains_division_conj conj then aux (simplify_conj conj)
    else conj
  in
  aux conj

let full_simplify_conj ?(full = false) conj =
  let rec aux last conjs =
    match conjs with
    | [] -> assert false
    | c :: rest ->
       if not(full) || equal_conj last c then [c] @ rest
       else aux c ((normalize_indices (simplify_conj ~full c)) @ rest)
  in
  aux conj (normalize_indices (simplify_conj ~full conj))


(* ** Rules *)

let subst_var_in_eq ~var ~by eq =
  match var, eq.eq_is_eq with
  | _, InEq -> failwith "substitution by variables is not allowed in inequalities"
  | Matrix(_,Var,_,_,_), Eq -> { eq with eq_expr = subst_expr ~old:(Atom(var)) ~by eq.eq_expr }
  | _ -> failwith ((string_of_atom var) ^ " is not a variable, it cannot be substituted")

let subst_var_in_conj ~var ~by n conj =
  (L.iter (atom_indices var)
     ~f:(fun i ->
       if L.exists conj.conj_exists ~f:(fun (j,_) -> i = j) then ()
       else failwith ((string_of_atom var) ^ " is not an existentially quantified variable")
     )
  );
  let new_eq = subst_var_in_eq ~var ~by (L.nth_exn conj.conj_equations n) in
  { conj with conj_equations = conj.conj_equations @ [ { new_eq with eq_expr = full_simplify new_eq.eq_expr } ]; }

let is_contradiction_eq eq =
  match eq.eq_expr with
  | Atom(Zero(_)) -> eq.eq_is_eq = InEq
  | Atom(Int(i)) ->
     if BI.is_zero i then eq.eq_is_eq = InEq
     else eq.eq_is_eq = Eq
  | _ -> false

let exists_contradiction conj =
  L.exists conj.conj_equations ~f:is_contradiction_eq

let rec contains_summations_msg = function
  | Mul(e1,e2) | Div(e1,e2) | PWProd(e1,e2) -> (contains_summations_msg e1) || (contains_summations_msg e2)
  | Opp(e) | Transpose(e) | Diag (e) -> contains_summations_msg e
  | Add(_) | Sum(_) -> failwith "hola"
  | Atom(_) -> false
  | Prod(_,_,_) -> false (* If additions are inside products, these are treated as blocks *)

let rec extract_coeff ~var ~degree expr =
  if not (is_normalized expr) then
    let expr = full_norm_simplify expr in
    if not (is_normalized expr) then [expr]
    else extract_coeff ~var ~degree expr
  else
    try
      L.fold_left (summations_list_of_norm_expr expr)
       ~init:([],[])
       ~f:(fun (withvar, withoutvar) s ->
         let indices, coeff, scalars, terms = full_split_summation s in
         let list = terms :: scalars in
         let equal e =
           match e with
           | Prod(_,_,e') when L.mem (get_atoms e') var ~equal:equal_atom ->
              failwith ("Variable " ^ (string_of_atom var) ^ " cannot be extracted, it appears inside a Prod()")
           | _ -> (equal_expr (Atom(var)) e) || (equal_expr (Transpose(Atom(var))) e)
         in
         let equal_but_transposed e = (equal e) && (equal_expr (Transpose(Atom(var))) e) in
         match var with
         | Matrix(_,Var,_,d,_) ->
            if equal_dim scalar_dim d then
              if (L.count (L.concat scalars) ~f:equal ) = degree then
                let new_s =
                  let sum_terms = L.map list ~f:(fun l -> L.filter l ~f:(fun a -> not (equal a)) |> mul_of_list ) in
                  summation_of_split (indices, mul_of_list (Atom(Int(coeff)) :: sum_terms)) |> full_norm_simplify
                in
                (withvar @ [new_s], withoutvar)
              else (withvar, withoutvar @ [s])
            else
              let left l = if L.length l = 0 then false else equal (L.hd_exn l) in
              let right l = if L.length l <= 1 then false else equal (L.hd_exn (L.rev l)) in
              let center l = if L.length l <= 2 then false else L.exists (L.tl_exn (L.rev (L.tl_exn l))) ~f:equal in
              let l = L.count list ~f:left in
              let r = L.count list ~f:right in
              let c = L.count list ~f:center in
              if degree = 1 then
                if (l+r+c) = 1 then
                  let new_s =
                    let sum_terms = L.map list ~f:(fun l -> L.filter l ~f:(fun a -> not (equal a)) |> mul_of_list ) in
                    summation_of_split (indices, mul_of_list (Atom(Int(coeff)) :: sum_terms)) |> full_norm_simplify
                  in
                  if c = 1 then failwith ("Impossible to extract coeff on " ^ (string_of_atom var))
                  else if L.exists list
                            ~f:(fun l -> ((L.length l > 0) && equal_but_transposed (L.hd_exn l)) ||
                                           ((L.length l > 1) && equal_but_transposed (L.hd_exn (L.rev l))))
                  then (withvar @ [full_norm_simplify (Transpose(new_s))], withoutvar)
                  else (withvar @ [new_s], withoutvar)
                else (withvar, withoutvar @ [s])
              else if degree = 2 then
                failwith "extraction for degree 2 for vectors is not supported yet"
                         (*
                let new_s =
                  let sum_terms = L.map list ~f:(fun l -> L.filter l ~f:(fun a -> not (equal a)) |> mul_of_list ) in
                  summation_of_split (indices, mul_of_list (Atom(Int(coeff)) :: sum_terms)) |> full_norm_simplify
                in
                if c > 0 then failwith ("Impossible to extract coeff on " ^ (string_of_atom var))
                else
                  if l = 1 then (withvar @ [new_s], withoutvar)
                  else
                    (* One needs one x and one x^T *)
                    let list = (* There is always one to be modified in scalars *)
                      if l = 0 then
                        let scalars = modify_first scalars
                             ~f:(L.exists ~f:(fun a -> equal a && not(equal_but_transposed a)))
                             ~modify:(fun l -> L.rev (L.map l ~f:(fun a -> Transpose(a))))
                        in
                        terms :: scalars
                      else
                        let scalars = modify_last scalars
                             ~f:(L.exists ~f:(fun a -> equal_but_transposed a))
                             ~modify:(fun l -> L.rev (L.map l ~f:(fun a -> Transpose(a) |> full_norm_simplify )))
                        in
                        scalars @ [terms]
                    in
                    let new_s =
                      let sum_terms = L.map list ~f:(fun l -> L.filter l ~f:(fun a -> not (equal a)) |> mul_of_list ) in
                      summation_of_split (indices, mul_of_list (Atom(Int(coeff)) :: sum_terms)) |> full_norm_simplify
                    in
                    (withvar @ [new_s], withoutvar)
                          *)
              else failwith "Extracting coefficient of vectors/matrices for degree > 2 is not supported"
         | _ -> assert false
       )
      |> (fun (withvar, withoutvar) -> [withvar; withoutvar])
      |> L.map ~f:(fun l -> norm_expr_of_summations_list l)
    with
    | _ -> failwith "coeff was not extracted"

let extract_coeff_in_eq ~quant ~var ~degree exist_idxs eq =
  (if eq.eq_is_eq = InEq then failwith "Impossible to extract coeff from innequalities" else ());
  match quant,var with
  | [], Matrix(_,Var,_,_,_) ->
     let exprs = extract_coeff ~var ~degree eq.eq_expr in
     L.map exprs ~f:(fun e -> { eq with eq_expr = e })

  | (i,exni) :: [], Matrix(_,Var,_,_,_) ->
     let eqs = separate_idx_eq i eq in
     let eqs1 =
       L.map eqs ~f:(fun eq ->
             let exprs = extract_coeff ~var ~degree eq.eq_expr in
             L.map exprs ~f:(fun e -> { eq with eq_forall = (i,exist_idxs) :: eq.eq_forall; eq_expr = e })
             )
       |> L.concat
     in
     let eqs2 = L.map exist_idxs
         ~f:(fun j ->
           if not (equal_set (index_set i) (index_set j)) || L.mem exni j ~equal:equal_idx then []
           else
             let eqs = separate_idx_eq j eq in
             L.map eqs ~f:(fun eq ->
                let exprs = extract_coeff ~var:(subst_idx_in_atom i j var) ~degree eq.eq_expr in
                L.map exprs ~f:(fun e -> {eq with eq_expr = e })
             ) |> L.concat
         )
         |> L.concat
     in
     eqs1 @ eqs2

  | _, Matrix(_,Var,_,_,_) ->
     failwith "coefficient extraction under more than one quantified variable is not supported yet"
  | _ -> failwith ((string_of_atom var) ^ " is not a variable, coefficient cannot be extracted")

let extract_coeff_in_conj ~quant ~var ~degree n conj =
  let idxs = atom_indices var in
  let new_idxs =
    L.fold_left idxs
       ~init:[]
       ~f:(fun new_idxs i ->
         if L.exists (quant @ conj.conj_exists) ~f:(fun (j,_) -> i = j) then
           if (L.exists quant ~f:(fun (j,_) -> i = j)) && (L.exists conj.conj_exists ~f:(fun (j,_) -> i = j)) then
             let j = fresh_index i (new_idxs @ (L.map conj.conj_exists ~f:get_indices_exists |> L.concat)) in
             new_idxs @ [j]
           else new_idxs @ [i]
         else failwith ((string_of_atom var) ^ " contains unquantified indices")
       )
  in
  let quant,var =
    L.fold_left (L.zip_exn idxs new_idxs)
       ~init:(quant,var)
       ~f:(fun (quant,var) (i,j) ->
         if equal_idx i j then (quant,var)
         else L.map quant ~f:(subst_idx_in_forall i j), subst_idx_in_atom i j var
       )
  in
  let eq, rest = list_nth_and_remove_exn conj.conj_equations n in
  let exists_idxs = L.map conj.conj_exists ~f:(fun (i,_) -> i) in
  { conj with conj_equations = rest @ (extract_coeff_in_eq ~quant ~var ~degree exists_idxs eq); }
