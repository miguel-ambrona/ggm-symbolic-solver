open Core_kernel.Std
open Abbrevs
open Big_int

(* ======================================================================= *)
(* Big int helper functions *)

module BI = struct
  type t = big_int
  let one = unit_big_int
  let zero = zero_big_int
  let is_one = eq_big_int one
  let is_zero = eq_big_int zero
  let opp = minus_big_int
  let ( *! ) a b = mult_big_int a b
  let ( +! ) a b = add_big_int a b
  let ( -! ) a b = a +! (opp b)
  let ( /! ) a b = div_big_int a b
  let gcd = gcd_big_int
  let of_int = big_int_of_int
  let to_int = int_of_big_int
  let to_string = string_of_big_int
  let t_of_sexp se = big_int_of_string (string_of_sexp se)
  let sexp_of_t bi = sexp_of_string (string_of_big_int bi)
  let compare = compare_big_int
  let equal a b = compare a b = 0
  let max = max_big_int
  let min = min_big_int
  let abs = abs_big_int
  let sign = sign_big_int
end

let mk_list el n =
  let rec aux output n =
    if n <= 0 then output
    else aux (el :: output) (n-1)
  in
  aux [] n

let string_of_list sep string_of_a list =
  let rec aux output = function
    | []        -> output
    | a :: []   -> output ^ (string_of_a a)
    | a :: rest -> aux (output ^ (string_of_a a) ^ sep) rest
  in
  aux "" list

let pp_string fmt s = F.fprintf fmt "%s" s

let pp_int fmt i = F.fprintf fmt "%i" i

let rec pp_list sep pp_elt f l =
  match l with
  | [] -> ()
  | [e] -> pp_elt f e
  | e::l -> F.fprintf f "%a%(%)%a" pp_elt e sep (pp_list sep pp_elt) l


let compare_lists ~compare l1 l2 =
  let n1 = L.length l1 in
  let n2 = L.length l2 in
  if n1 < n2 then -1
  else if n1 > n2 then +1
  else
    let rec aux l1 l2 =
      match l1,l2 with
      | [], [] -> 0
      | a :: resta, b :: restb ->
         let c = compare a b in
         if c <> 0 then c else aux resta restb
      | _ -> assert false
    in
    aux (L.sort l1 ~cmp:compare) (L.sort l2 ~cmp:compare)

let equal_lists ~compare l1 l2 = compare_lists ~compare l1 l2 = 0

let equal_option ~equal o1 o2 =
  match o1,o2 with
  | None, None -> true
  | Some a1, Some a2 -> equal a1 a2
  | _ -> false
        
let trim_newlines str =
  let rec aux filtered k =
    if k = S.length str then filtered
    else
      let a = str.[k] in
      match a with
      | '\n' -> aux (filtered ^ "@") (k+1)
      | _    -> aux (filtered ^ (S.of_char a)) (k+1)
  in
  aux "" 0

let remove_duplicates ~equal list =
  let rec aux output = function
    | [] -> output
    | a :: rest ->
       if L.exists rest ~f:(fun b -> equal a b) then aux output rest
       else aux (output @ [a]) rest
  in
  aux [] list

let apply_f_to_list_nth ~f ~pos list =
  let rec aux output k = function
    | [] -> output
    | a :: rest ->
       if k = pos then aux (output @ [f a]) (k+1) rest
       else aux (output @ [a]) (k+1) rest
  in
  aux [] 0 list
      
let apply_flist_to_list_nth ~f ~pos list =
  let rec aux output k = function
    | [] -> output
    | a :: rest ->
       if k = pos then aux (output @ (f a)) (k+1) rest
       else aux (output @ [a]) (k+1) rest
  in
  aux [] 0 list

let list_find_and_remove ~f list =
  let rec aux previous = function
    | [] -> None, previous
    | a :: rest ->
       if f a then Some a, previous @ rest
       else aux (previous @ [a]) rest
  in
  aux [] list

let list_nth_and_remove_exn list n =
  let rec aux previous k = function
    | [] -> failwith "Not found"
    | a :: rest ->
       if k = n then a, previous @ rest
       else aux (previous @ [a]) (k+1) rest
  in
  aux [] 0 list

(* insert x at all positions into a list *)
let rec insert x list =
  match list with
  | [] -> [[x]]
  | hd::tl -> (x::list) :: (L.map ~f:(fun l -> hd::l) (insert x tl))
      
(* list of all permutations of a list *)
let rec perms = function
  | [] -> [[]]
  | hd::tl -> L.concat (L.map ~f:(insert hd) (perms tl))

let list_is_contained_in_list ~equal list1 list2 =
  not (L.exists list1 ~f:(fun a -> not(L.mem list2 a ~equal)) )

let list_is_sublist_of_list ~equal list1 list2 =
  let n1 = L.length list1 in
  let n2 = L.length list2 in
  let rec aux k =
    if (n2 - k) < n1 then false
    else
      let sublist = L.slice list2 k (k+n1) in
      if not (L.exists (L.zip_exn list1 sublist) ~f:(fun (a,b) -> not (equal a b))) then true
      else aux (k+1)
  in
  if n1 = 0 then false
  else aux 0

let replace_sublist_of_list_by_list ~equal list_old list_by list =
  let n1 = L.length list_old in
  let n2 = L.length list in
  let rec aux k =
    if (n2 - k) < n1 then failwith "Not found"
    else
      let sublist = L.slice list k (k+n1) in
      if not (L.exists (L.zip_exn list_old sublist) ~f:(fun (a,b) -> not (equal a b))) then
        if k = 0 then list_by @ (L.slice list (k+n1) n2)
        else if (k+n1) = n2 then (L.slice list 0 k) @ list_by
        else (L.slice list 0 k) @ list_by @ (L.slice list (k+n1) n2)
      else aux (k+1)
  in
  if n1 = 0 then failwith "Initial list is empty"
  else aux 0           

let remove_first ~equal list =
  let rec aux previous = function
    | [] -> failwith "Not found"
    | a :: rest -> if equal a then previous @ rest else aux (previous @ [a]) rest
  in
  aux [] list
           
let modify_first ~f ~modify list =
  let rec aux previous = function
    | [] -> failwith "Not found"
    | a :: rest -> if f a then previous @ [modify a] @ rest else aux (previous @ [a]) rest
  in
  aux [] list
                                                         
let modify_last ~f ~modify list =
  modify_first ~f ~modify (L.rev list) |> L.rev

let split_by_number s =
  let rec aux i =
    if i >= S.length s then None
    else if Char.is_digit s.[i] then Some (S.sub ~pos:0 ~len:i s, S.sub ~pos:i ~len:((S.length s)-i) s)
    else aux (i+1)
  in
  aux 0

let string_replace ~exp ~by string =
  let n = S.length exp in
  let rec aux output k =
    if k < 0 then output
    else if exp = S.slice output k (k+n) then
      if k = 0 then aux (by ^ (S.slice output (k+n) (S.length output))) (k-1)
      else if k+n = S.length output then aux ((S.slice output 0 k) ^ by) (k-1)
      else aux ((S.slice output 0 k) ^ by ^ (S.slice output (k+n) (S.length output))) (k-1)
    else aux output (k-1)
  in
  aux string ((S.length string)-n)

let list_sum ~add ~zero l =
  L.fold_left l ~init:zero ~f:add
