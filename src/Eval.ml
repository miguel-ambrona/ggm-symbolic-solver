(* Helper functions for Parser *)

open Abbrevs
open Util
open Expressions
open Equations

let f s i = (i,(s, NatInt(0)))
       
let mk_sum  (i,s,exn) e = Sum(f s i, L.map exn ~f:(f s),e)
let mk_prod (i,s,exn) e = Prod(f s i, L.map exn ~f:(f s),e)

let mk_equation forall e1 is_eq e2 =
  { eq_forall = L.map (optional ~d:[] forall) ~f:(fun (i,s,exn) -> (f s i, L.map exn ~f:(f s)) );
    eq_is_eq  = is_eq;
    eq_expr   = if is_zero_expr e1 then e2
                else if is_zero_expr e2 then e1
                else Add(e1,Opp(e2))
  }

let mk_conjunction exists eqs =
  { conj_exists    = L.map (optional ~d:[] exists) ~f:(fun (i,s,exn) -> (f s i, L.map exn ~f:(f s)) );
    conj_equations = eqs;
    conj_substituted = [];
    conj_replaced = [];
  }

type instr =
  | EvalSets of set list
  | EvalAtom of ((atom list * dim option * group_type) list) * ((string * string) list option) * bool
  | EvalConj of conjunction
  | Goal of int
  | Substitute of atom * expr * int
  | Simplify
  | FullSimplify
  | Contradiction
  | ExtractCoeff of atom * int * int * ((string * string) option)
  | Go
  | AddEquation of equation

let update_index sets (i,(sname,_)) =
  match L.find sets ~f:(fun (name,_) -> sname = name) with
  | None -> failwith ("Undefined set " ^ sname)
  | Some (_,n) -> (i,(sname,n))
                    
let update_indices_quantification sets quant =
  L.map quant ~f:(fun (i,exn) -> (update_index sets i, L.map exn ~f:(update_index sets)))

let update_indices_atom ?(onlycheck = false) sup_idxs sets atom =
  let f (i,_) =
    match L.find (L.rev sup_idxs) ~f:(fun (j,_) -> i = j) with (* Searching in the reverse order is important *)
    | None -> failwith ("unbound index " ^ i ^ " in " ^ (string_of_atom atom))
    | Some (_,(s,n)) -> update_index sets (i,(s,n))
  in
  match atom with
  | Matrix(s,mt,idxs,d,gt) ->
     (if onlycheck then
        match L.find (L.zip_exn idxs (L.map idxs ~f)) ~f:(fun (i,j) -> compare_idx i j <> 0) with
        | None -> ()
        | Some (i,j) ->
           failwith ("index " ^ (string_of_idx i) ^ " in atom " ^ (string_of_atom atom) ^
                       " belongs to " ^ (string_of_set (index_set j)) ^ " and it should belong to " ^
                         (string_of_set (index_set i)) ^ " by definition")
      else ()
     );
     Matrix(s,mt,L.map idxs ~f, d, gt)
  | StdBasis(i) -> StdBasis(f i)
  | a -> a
        
let rec up_idxs sup_idxs sets = function
  | Add(e1,e2)    -> Add(up_idxs sup_idxs sets e1, up_idxs sup_idxs sets e2)
  | Mul(e1,e2)    -> Mul(up_idxs sup_idxs sets e1, up_idxs sup_idxs sets e2)
  | Div(e1,e2)    -> Div(up_idxs sup_idxs sets e1, up_idxs sup_idxs sets e2)
  | Opp(e)        -> Opp(up_idxs sup_idxs sets e)
  | Transpose(e)  -> Transpose(up_idxs sup_idxs sets e)
  | Diag(e)       -> Diag(up_idxs sup_idxs sets e)                       
  | PWProd(e1,e2) -> PWProd(up_idxs sup_idxs sets e1, up_idxs sup_idxs sets e2)
  | Atom(a)       -> Atom(update_indices_atom ~onlycheck:true sup_idxs sets a)
  | Sum(k,exn,e)  -> Sum(update_index sets k, L.map exn ~f:(update_index sets), up_idxs (sup_idxs @ [k]) sets e)
  | Prod(k,exn,e) -> Prod(update_index sets k, L.map exn ~f:(update_index sets), up_idxs (sup_idxs @ [k]) sets e)
        
let update_indices_eq sup_idxs sets eq =
  { eq with eq_forall = update_indices_quantification sets eq.eq_forall;
            eq_expr = up_idxs (sup_idxs @ (L.map eq.eq_forall ~f:(fun (i,_) -> i))) sets eq.eq_expr;
  }
        
let update_indices sets conj =
  { conj with conj_exists = update_indices_quantification sets conj.conj_exists;
              conj_equations = L.map conj.conj_equations ~f:(update_indices_eq (L.map conj.conj_exists ~f:(fun (i,_) -> i))sets);
  }
                     
let rec check_expr expr =
  let _ = expr_dimension expr in (* This checks that dimensions are consistent *)
  match expr with
  | Prod(_,_,e) ->
     if expr_dimension e <> scalar_dim then failwith "Products can only be defined on scalars, non matrices"
     else ()
  | Add(e1,e2) | Mul(e1,e2) | Div(e1,e2) | PWProd(e1,e2) -> check_expr e1; check_expr e2
  | Opp(e) | Transpose(e) | Sum(_,_,e) | Diag(e) -> check_expr e
  | Atom(_) -> ()
              
let equal_generic_atoms a1 a2 =
  match a1,a2 with
  | Matrix(s1,_,idxs1,_,_), Matrix(s2,_,idxs2,_,_) -> (s1 = s2) && (L.length idxs1 = L.length idxs2)
  | _ -> assert false

let change_dimension d = function
  | Matrix(s,mt,idxs,_,gt) -> Matrix(s,mt,idxs,d,gt)
  | Int(_) | Zero(_) | StdBasis(_) -> failwith "integers cannot be defined as parameters/variables"

let change_type gt = function
  | Matrix(s,mt,idxs,d,_) -> Matrix(s,mt,idxs,d,gt)
  | _ -> assert false
                                           
let convert_to_var = function
  | Matrix(s,Param,idxs,d,gt) -> Matrix(s,Var,idxs,d,gt)
  | _ -> assert false

let set_expression atoms expr =
  L.fold_left
    (L.filter (get_atoms expr) ~f:(function | Int(_) | Zero(_) -> false | _ -> true ))
    ~init:expr
    ~f:(fun e a ->
      let indices = atom_indices a in
      match L.find atoms ~f:(equal_generic_atoms a) with
      | None -> failwith ("atom " ^ (string_of_atom a) ^ " was not defined")
      | Some a' ->
         let replacing atom =
           if equal_atom a atom then
             begin match a' with
             | Matrix(s,mt,indices',d,gt) ->
                let new_indices = L.map (L.zip_exn indices indices') ~f:(fun ((i,_), (_,(s,n))) -> (i,(s,n)) ) in
                Matrix(s,mt,new_indices,d,gt)
             | _ -> assert false
             end
           else atom
         in
         atom_map ~f:replacing e
    )
  |> (fun expr -> check_expr expr; expr)
    
let eval_instr conjs sets atoms goal instr =
  match instr with
  | EvalSets(l) ->
     (L.iter l ~f:(fun (s,_) ->
          if L.exists sets ~f:(fun (s',_) -> s' = s) then failwith ("set " ^ s ^ " is already defined")
          else ())
     );
     conjs, sets @ l, atoms, goal
  | EvalAtom(l,q,variables) ->
     let indices =
       begin match q with
       | None -> []
       | Some quant -> L.map quant ~f:(fun (i,s) -> update_index sets (i,(s,NatInt(1))) )
       end
     in
     let new_atoms =
       L.map l ~f:(fun (l2,d,t) -> L.map l2
                   ~f:(fun a -> change_dimension (optional ~d:scalar_dim d) (change_type t a)
                                |> update_indices_atom indices sets
                  ))
       |> L.concat
     in
     L.iter new_atoms ~f:(fun a ->
              if L.mem atoms a ~equal:equal_generic_atoms then
                failwith ("atom " ^ (string_of_atom a) ^ " is defined twice")
              else ());
     if variables then conjs, sets, atoms @ (L.map new_atoms ~f:convert_to_var ), goal
     else conjs, sets, atoms @ new_atoms, goal

  | EvalConj(c) ->
     let set_equation eq = { eq with eq_expr = set_expression atoms eq.eq_expr } in
     let c = { c with conj_equations = L.map c.conj_equations ~f:set_equation } in
     let c = update_indices sets c in
     let _ = conj_dimensions c in (* This checks that dimensions are consistent *)
     check_index_coherence_conj c;
     c :: conjs, sets, atoms, goal
                   
  | Goal(n) -> conjs, sets, atoms, n-1

  | Substitute(a,e,n) ->
     let c = L.nth_exn conjs goal in
     let exist_idx = L.map c.conj_exists ~f:(fun (i,_) -> i) in
     let e = set_expression atoms e in
     let a = update_indices_atom exist_idx sets a in
     (if L.exists (get_indices e) ~f:(fun i -> not(L.mem exist_idx i)) then
        failwith ("expression " ^ (string_of_expr e) ^ " contains unquantified indices")
      else ()
     );
     begin match a with
     | Matrix(s,Param,idxs,_,gt) ->
        begin match L.find atoms ~f:(equal_generic_atoms a) with
        | None -> failwith ("atom " ^ (string_of_atom a) ^ " was not defined")
        | Some (Matrix(_,Var,_,d,_)) ->
           let a = Matrix(s,Var,idxs,d,gt) in 
           (apply_f_to_list_nth ~f:(subst_var_in_conj ~var:a ~by:e (n-1)) ~pos:goal conjs)
           |> L.map ~f:(fun c -> let _ = conj_dimensions c in c),
           sets, atoms, goal
        | _ -> failwith ((string_of_atom a) ^ " is not a variable, it cannot be substituted")
        end
     | _ -> assert false
     end

  | Simplify ->
     (apply_flist_to_list_nth ~f:full_simplify_conj ~pos:goal conjs), sets, atoms, goal
                                                                               
  | FullSimplify ->
     (apply_flist_to_list_nth ~f:(full_simplify_conj ~full:true) ~pos:goal conjs), sets, atoms, goal
                                                                                            
  | Contradiction ->
     if exists_contradiction (L.nth_exn conjs goal) then
       (apply_flist_to_list_nth ~f:(fun _ -> []) ~pos:goal conjs), sets, atoms, goal
     else failwith "Contradiction not found"

  | ExtractCoeff(a,deg,n,idx) ->
     let quant_idx, quant =
       begin match idx with       | None -> [], []
       | Some (i,s) ->
          let quant_idx = update_index sets (i,(s,NatInt(0))) in
          let quant = [quant_idx, []] in
          [quant_idx], quant
       end
     in
     let c = L.nth_exn conjs goal in
     let a = update_indices_atom ((L.map c.conj_exists ~f:(fun (i,_) -> i)) @ quant_idx) sets a in
     begin match a with
     | Matrix(s,Param,idxs,_,gt) ->
        begin match L.find atoms ~f:(equal_generic_atoms a) with
        | None -> failwith ("atom " ^ (string_of_atom a) ^ " was not defined")
        | Some (Matrix(_,Var,_,d,_)) ->
           let a = Matrix(s,Var,idxs,d,gt) in 
           (apply_f_to_list_nth ~f:(extract_coeff_in_conj ~quant ~var:a ~degree:deg (n-1)) ~pos:goal conjs)
           |> L.map ~f:(fun c -> let _ = conj_dimensions c in c),
           sets, atoms, goal
        | _ -> failwith ((string_of_atom a) ^ " is not a variable, it cannot be substituted")
        end
     | _ -> assert false
     end

  | Go ->
     (apply_flist_to_list_nth ~f:(fun c -> [Automation.go c]) ~pos:goal conjs), sets, atoms, goal

  | AddEquation(eq) ->
     (apply_flist_to_list_nth
        ~f:(fun c -> [{ c with conj_equations = c.conj_equations @ [{ eq with eq_expr = set_expression atoms eq.eq_expr}]}])
        ~pos:goal conjs),
     sets, atoms, goal
                   
let eval_instrs instrs =
  L.fold_left instrs
    ~init:([],[],[],0)
    ~f:(fun (conjs,sets,atoms,goal) instr -> eval_instr conjs sets atoms goal instr )
