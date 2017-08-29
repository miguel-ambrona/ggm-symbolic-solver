(* Functions to pretty print in LaTeX Format *)

open Core_kernel.Std
open Abbrevs
open Util
open Expressions
open Equations

(* ** Pretty printing in LaTeX *)

let string_of_nat_latex = function
  | NatInt (i) -> string_of_int i
  | NatPar (s) -> s

let string_of_dim_latex (m,n) =
  match n with
  | NatInt(i) when i = 1 ->
     begin match m with
     | NatInt(j) when j = 1 -> ""
     | _ -> string_of_nat_latex m
     end
  | _ -> (string_of_nat_latex m) ^ "\\times " ^ (string_of_nat_latex n)

let var_color = "#dd83f9"
let param_color = "#ccee77"

let string_of_set_latex (name, n) = "|" ^ name ^ "| = " ^ (string_of_nat_latex n)
                    
let string_of_idx_set_latex (i,s) = i ^ " \\in " ^ (string_of_set s)

let f s =
  match split_by_number s with
  | None -> s
  | Some (a,b) -> a ^ "_{" ^ b ^ "}"
                                                     
let string_of_atom_latex = function
  | Matrix(s,Param,[],_,_) -> "\\color{" ^ param_color ^ "}{" ^ (f s) ^ "}"
  | Matrix(s,Var,[],_,_)   -> "\\color{" ^ var_color ^ "}{" ^ (f s) ^ "}"
  | Matrix(s,Param,idxs,_,_) -> "\\color{" ^ param_color ^ "}{" ^ (f s) ^ "}_{" ^ (string_of_list "," string_of_idx idxs) ^ "}"
  | Matrix(s,Var,idxs,_,_)   -> "\\color{" ^ var_color ^ "}{" ^ (f s) ^ "}_{" ^ (string_of_list "," string_of_idx idxs) ^ "}"
  | StdBasis(i) -> "\\vec{e}_{" ^ (string_of_idx i) ^ "}"
  | Zero((n1,n2)) ->
     if equal_dim (n1,n2) scalar_dim then "0"
     else if equal_nat n1 (NatInt(1)) then "\\vec{0}^{\\top}"
     else if equal_nat n2 (NatInt(1)) then "\\vec{0}"
     else "\\textbf{0}"
  | Int(n)  -> " " ^ (BI.to_string n) ^ " "
                                          
let zp = "\\mathbb{Z}_p"
let zo = "\\{0,1\\}"

let string_of_atom_dim = function
  | Matrix(s,mt,[],d,gt) ->
     let color = begin match mt with | Param -> param_color | Var -> var_color end in
     let set = begin match gt with | Zp -> zp | Bool -> zo end in
     "\\color{" ^ color ^ "}{" ^ s ^ "}" ^ "\\in " ^ set ^ "^{" ^ (string_of_dim_latex d) ^ "}"
  | Matrix(s,mt,idxs,d,gt) ->
     let color = begin match mt with | Param -> param_color | Var -> var_color end in
     let set = begin match gt with | Zp -> zp | Bool -> zo end in
     "\\color{" ^ color ^ "}{" ^ s ^ "}_{" ^ (string_of_list "," string_of_idx idxs) ^ "}" ^ "\\in " ^ set ^ "^{" ^ (string_of_dim_latex d) ^ "}"
  | _ -> assert false
           
let rec string_of_expr_latex = function
  | Add(e1,e2) -> (string_of_expr_latex e1) ^ " + " ^ (string_of_expr_latex e2)
  | Mul(e1,e2) ->
     let aux e =
       begin match e with
       | Add(_,_) | Opp(_) | Div(_,_) -> "\\left(" ^ (string_of_expr_latex e) ^ "\\right)"
       | Atom(Int(i)) when BI.compare BI.zero i > 0 -> "\\left(" ^ (string_of_expr_latex e) ^ "\\right)"
       | Atom(Int(_)) -> (string_of_expr_latex e) ^ " \\ " 
       | _ -> string_of_expr_latex e
       end
     in
     (aux e1) ^ (aux e2)
  | Opp(e)     ->
     begin match e with
     | Add(_,_) -> "-\\left(" ^ (string_of_expr_latex e) ^ "\\right)"
     | _ -> "-" ^ (string_of_expr_latex e)
     end
  | Div(e1,e2) -> "\\frac{" ^ (string_of_expr_latex e1) ^ "}{" ^ (string_of_expr_latex e2) ^ "}"
  | Sum(i,set,e)  ->
     if set = [] then "\\left(\\sum_{" ^ (string_of_idx_set_latex i) ^ "}" ^ (string_of_expr_latex e) ^ "\\right)"
     else "\\left(\\sum_{" ^ (string_of_idx_set_latex i) ^ "\\setminus \\{" ^ (string_of_list "," string_of_idx set) ^ "\\}}" ^ (string_of_expr_latex e) ^ "\\right)"
  | Prod(i,set,e) ->
     if set = [] then "\\left(\\prod_{" ^ (string_of_idx_set_latex i) ^ "}" ^ (string_of_expr_latex e) ^ "\\right)"
     else "\\left(\\prod_{" ^ (string_of_idx_set_latex i) ^ "\\setminus \\{" ^ (string_of_list "," string_of_idx set) ^ "\\}}" ^ (string_of_expr_latex e) ^ "\\right)"
  | Transpose(e)  ->
     begin match e with
     | Atom(_) -> (string_of_expr_latex e) ^ "^{\\top}"
     | _ -> "\\left(" ^ (string_of_expr_latex e) ^ "\\right)^{\\top}"
     end
  | Diag(e)  -> "\\textsf{diag}\\left(" ^ (string_of_expr_latex e) ^ "\\right)"
  | PWProd(e1,e2) ->
     let aux e =
       begin match e with
       | Atom(_) -> string_of_expr_latex e
       | _ -> "\\left(" ^ (string_of_expr_latex e) ^ "\\right)"
       end
     in
     (aux e1) ^ "\\circ " ^ (aux e2)                                                       
  | Atom(a) -> (string_of_atom_latex a)

let cut_to_last_space string =
  let rec aux k =
    if k = 0 then failwith "Not found"
    else if string.[k] = ' ' then S.slice string 0 k
    else aux (k-1)
  in
  aux ((S.length string) - 1)
                 
let string_of_expr_latex expr =
  let s = string_of_expr_latex expr in
  if S.length s > 10000 then
    if is_normalized expr then
      let _terms = summations_list_of_norm_expr expr in
      let rec _aux output = function
        | [] -> output
        | s :: rest ->
           if S.length output > 10000 then output ^ "\\dots"
           else
             let new_s = string_of_expr_latex s in
             let new_s = cut_to_last_space new_s in
             let new_s = string_replace ~exp:"\\right" ~by:" " new_s in
             let new_s = string_replace ~exp:"\\left" ~by:" " new_s in
             let b = S.count new_s ~f:(fun a -> a = '{') in
             let a = S.count new_s ~f:(fun a -> a = '}') in
             let new_s = new_s ^ (S.make (b-a) '}') in
             _aux (output ^ new_s ^ " + \\\\ ") rest
      in
      "\\text{Too long}" (* aux "" terms *)
    else "\\text{Too long}"
  else s
                 
let string_of_id_exception_latex = function
  | (i,[]) -> (string_of_idx_set_latex i)
  | (i,set) -> (string_of_idx_set_latex i) ^ " \\setminus \\{" ^ (string_of_list "," string_of_idx set) ^ "\\}"

let string_of_is_eq_latex = function
  | Eq -> "="
  | InEq -> "\\neq"

let string_of_eq_latex eq =
  let d = expr_dimension eq.eq_expr in
  let forall =
    match eq.eq_forall with
    | [] -> ""
    | f -> "\\forall " ^ (string_of_list ", " string_of_id_exception_latex) f ^ ". \\ "
  in
  " & " ^ forall ^ (string_of_expr_latex eq.eq_expr) ^
    (string_of_is_eq_latex eq.eq_is_eq) ^ (string_of_atom_latex (Zero(d)))

let string_of_conj_latex conj =
  let exists =
    match conj.conj_exists with
    | [] -> ""
    | f -> "\\exists " ^ (string_of_list ", " string_of_id_exception_latex) f ^ ":\\\\"
  in
  let eqs_to_show =
    if L.length conj.conj_equations > 50 then L.slice conj.conj_equations 0 50
    else conj.conj_equations
  in
  let eqs_string, _ =
    L.fold_left eqs_to_show
        ~init:("",1)
        ~f:(fun (s,k) eq ->
          if k = 1 then s ^ "(" ^ (string_of_int k) ^ ") \\ " ^ (string_of_eq_latex eq), k+1
          else s ^ "& & \\land \\\\ (" ^ (string_of_int k) ^ ") \\ " ^ (string_of_eq_latex eq), k+1
        )
  in
  let eqs_string =
    if L.length conj.conj_equations > 50 then eqs_string ^ "& & \\land \\\\ " ^
                                                (string_of_int (L.length conj.conj_equations)) ^
                                                   " \text{ more equations not shown}"
    else eqs_string
  in
  let sep1,sep1' = if S.length eqs_string > 1 then " & & \\\\ ", "" else "", "\\\\ " in
  let substituted =
    L.fold_left conj.conj_substituted
        ~init:""
        ~f:(fun s (idx_option, a, e) ->
          let string_e =
            if is_zero_expr e then (string_of_atom_latex (Zero(atom_dimension a)))
            else string_of_expr_latex e
          in
          match idx_option with
          | None -> s ^ sep1 ^ " & " ^ (string_of_atom_latex a) ^ "\\mapsto " ^ string_e ^ sep1'
          | Some f ->
             s ^ sep1 ^ " & \\forall " ^ (string_of_id_exception_latex f) ^ ", \\ " ^ (string_of_atom_latex a) ^
               "\\mapsto " ^ string_e ^ sep1'
        )
  in
  let replaced =
    L.fold_left conj.conj_replaced
        ~init:""
        ~f:(fun s (f, e1, e2) ->
          let string_e2 =
            if is_zero_expr e2 then (string_of_atom_latex (Zero(expr_dimension e1)))
            else string_of_expr_latex e2
          in
          match f with
          | [] -> s ^ sep1 ^ " & " ^ (string_of_expr_latex e1) ^ "\\mapsto " ^ string_e2 ^ sep1'
          | _ ->
             s ^ sep1 ^ " & \\forall " ^ (string_of_list ", \\ " string_of_id_exception_latex f) ^
               ", \\ " ^ (string_of_expr_latex e1) ^ "\\mapsto " ^ string_e2 ^ sep1'
        )
  in
  let sep2 = if S.length eqs_string > 1 then " \\\\ " else "" in
  "\\begin{aligned}" ^ exists ^ eqs_string ^ sep2 ^ substituted ^ sep2 ^ replaced ^ "\\end{aligned}"
    
let pp_nat_latex _fmt nat     = F.printf "%s" (string_of_nat_latex nat)
let pp_dim_latex _fmt dim     = F.printf "%s" (string_of_dim_latex dim)
let pp_atom_latex _fmt atom   = F.printf "%s" (string_of_atom_latex atom)
let pp_atom_dim _fmt atom     = F.printf "%s" (string_of_atom_dim atom)
let pp_expr_latex _fmt expr   = F.printf "%s" (string_of_expr_latex expr)
let pp_is_eq_latex _fmt is_eq = F.printf "%s" (string_of_is_eq_latex is_eq)
let pp_eq_latex _fmt eq       = F.printf "%s" (string_of_eq_latex eq)
let pp_conj_latex _fmt conj   = F.printf "%s" (string_of_conj_latex conj)


let pp_group_of_atoms _fmt atoms =
  let groups =
    L.fold_left atoms
        ~init:[]
        ~f:(fun groups a ->
          let d = expr_dimension (Atom(a)) in
          let t = match a with | Matrix(_,_,_,_,t) -> t | StdBasis(_) -> Bool | _ -> Zp in
          match L.find groups ~f:(fun (d',t',_) -> equal_dim d d' && equal_group_type t t') with
          | None -> groups @ [(d,t,[a])]
          | Some (_,_,l) ->
             (L.filter groups ~f:(fun (d',t',_) -> not(equal_dim d d' && equal_group_type t t'))) @ [(d,t,l @ [a])]
        )
  in
  let pp_group _fmt (d,t,l) =
    match t with
    | Bool -> F.printf "%a \\in %s" (pp_list ", " pp_atom_latex) l (zo ^ "^{" ^ (string_of_dim_latex d) ^ "}");
    | Zp   -> F.printf "%a \\in %s" (pp_list ", " pp_atom_latex) l (zp ^ "^{" ^ (string_of_dim_latex d) ^ "}");
  in
  F.printf "%a" (pp_list ", \\ " pp_group) groups
