open Core_kernel.Std
module YS = Yojson.Safe

(* Set the env variable GGM_PATH *)
let sage_script = Format.sprintf "sage -python %s/src/groebner_basis.py" (Sys.getenv "GGM_PATH")


type st_sage = {
  mutable sts_closed : bool;
  sts_cin    : in_channel;
  sts_cout   : out_channel;
}

let persistent_sage = ref None
 
let start_sage () =
  let (c_in, c_out) = Unix.open_process sage_script in
  { sts_cin = c_in; sts_cout = c_out; sts_closed = false }

let eval_sage sts cmd =
  if sts.sts_closed then failwith "sage process already closed";
  let (c_in, c_out) = sts.sts_cin, sts.sts_cout in
  output_string c_out cmd;
  flush c_out;
  let res = input_line c_in in
  res
  |> String.filter ~f:(fun c -> c <> '\n')
  |> String.filter ~f:(fun c -> c <> ' ')
  |> String.filter ~f:(fun c -> c <> '"')

let stop_sage sts =
  if sts.sts_closed then failwith "sage process already closed";
  let (c_in, c_out) = sts.sts_cin, sts.sts_cout in
  let cmd = YS.to_string (`Assoc [ ("cmd",`String"exit") ])^"\n" in
  output_string c_out cmd;
  flush c_out;
  let o = input_line c_in in
  if o <> "end" then failwith "close: end expected";
  ignore (Unix.close_process (c_in,c_out))

let call_Sage cmd =
  match !persistent_sage with
  | None ->
    let sts = start_sage () in
    persistent_sage := Some sts;
    eval_sage sts cmd
  | Some sts ->
    eval_sage sts cmd
