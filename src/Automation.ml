(* Function for automatic solver *)

open Abbrevs
open Util
open Expressions
open Equations

let get_degrees_of_var ~var expr =
  if not (is_normalized expr) then []
  else
    L.map (summations_list_of_norm_expr expr)
      ~f:(fun s ->
        let _,_,scalars,terms = full_split_summation s in
        let equal e =
          match e with
          | Prod(_,_,e') when L.mem (get_atoms e') var ~equal:equal_atom ->
             failwith ("Variable " ^ (string_of_atom var) ^ " appears inside a Prod(), unknown degree")
          | _ -> (equal_expr (Atom(var)) e) || (equal_expr (Transpose(Atom(var))) e)
        in
        L.count (terms :: scalars |> L.concat) ~f:equal         
      )
    |> L.dedup
      
let rec extract_all_coeffs conj =
  let rec aux conj' v d eq_number =
    if exists_contradiction conj' && false then conj'
    else
      let conj' =
        match (full_simplify_conj ~full:true conj') with
        | c :: [] -> c
        | _ -> assert false
      in
      if eq_number >= L.length conj'.conj_equations then conj'
      else
        let eq = L.nth_exn conj'.conj_equations eq_number in
        let vars = get_vars eq.eq_expr in
        try
          if v >= L.length vars then aux conj' 0 0 (eq_number+1)
          else
            let var = L.nth_exn vars v in
            begin match L.find (get_degrees_of_var ~var eq.eq_expr) ~f:(fun d' -> d' > d) with
            | None -> aux conj' (v+1) 0 eq_number
            | Some degree ->
               let conj' = extract_coeff_in_conj ~quant:[] ~var ~degree eq_number conj' in
               match atom_indices var with
               | [] ->
                  begin try extract_all_coeffs conj'
                        with | _ -> aux conj' v (d+1) eq_number
                  end
               | j :: [] ->
                  let conj'' = extract_coeff_in_conj ~quant:[(j,[])] ~var ~degree eq_number conj' in
                  aux conj'' v (d+1) eq_number

               | _ -> aux conj' v (d+1) eq_number
            end
        with
        | _ -> aux conj' (v+1) 0 eq_number
  in
  aux conj 0 0 0

let substitution_heuristic conj =
  let params =
    L.filter (get_atoms_conj conj) ~f:(function | Matrix(_,Param,_,_,_) -> true | _ -> false)
    |> L.map ~f:(fun p -> [Atom(p); Opp(Atom(p))])
    |> L.concat
  in
  let candidates_to_substitute_by = (Atom(Int(BI.zero))) :: params in
  let n_eq = L.length conj.conj_equations in
  let rec aux conj' k =
    if k >= n_eq then conj'
    else
      let eq = L.nth_exn conj'.conj_equations k in
      let vars = get_vars eq.eq_expr in
      let conj'' =
        L.fold_left vars
           ~init:conj'
           ~f:(fun conj' var ->
             L.fold_left candidates_to_substitute_by
                  ~init:conj'
                  ~f:(fun conj'' e ->
                    try subst_var_in_conj ~var ~by:e k conj'' |> simplify_conj ~full:true
                    with | _ -> conj''
                  )
           )
      in
      aux conj'' (k+1)
  in
  aux conj 0

let multiply_by_all_atoms conj =
  let atoms = get_atoms_conj conj |> L.filter ~f:(function | Matrix(_,Param,_,_,_) -> true | _ -> false) in
  let new_equations =
    L.map atoms ~f:(fun a ->
            let (a1,a2) = atom_dimension a in
            L.map conj.conj_equations
                  ~f:(fun eq ->
                    if eq.eq_is_eq = InEq then []
                    else
                      let (d1,d2) = expr_dimension eq.eq_expr in
                      let eq1 = if equal_nat a2 d1 then [{ eq with eq_expr = Mul(Atom(a),eq.eq_expr); }] else [] in
                      let eq2 = if equal_nat a1 d1 then [{ eq with eq_expr = Mul(Transpose(Atom(a)),eq.eq_expr); }] else [] in
                      let eq3 = if equal_nat a1 d2 then [{ eq with eq_expr = Mul(eq.eq_expr, Atom(a)); }] else [] in
                      let eq4 = if equal_nat a2 d2 then [{ eq with eq_expr = Mul(eq.eq_expr, Transpose(Atom(a))); }] else [] in
                      eq1 @ eq2 @ eq3 @ eq4
                  )
            |> L.concat
          )
    |> L.concat
  in
  { conj with conj_equations = conj.conj_equations @ new_equations; }
      
let go conj =
  extract_all_coeffs conj
  |> substitution_heuristic
  |> extract_all_coeffs
  |> (fun conj ->
    if exists_contradiction conj then conj
    else groebner_basis_conj conj
  )
  (*  |> multiply_by_all_atoms This heuristic is not ready yet*)
