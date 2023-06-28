open Effect
open Effect.Deep

type _ Effect.t += Conversion_failure : string -> int Effect.t

let int_of_string l =
  try int_of_string l with Failure _ -> perform (Conversion_failure l)

let rec sum_up acc =
  let l = input_line stdin in
  acc := !acc + int_of_string l;
  sum_up acc

let effect_handler (type a) (e : a Effect.t) : _ =
  match e with
  | Conversion_failure s ->
      Some
        (fun (k : (a, _) continuation) ->
          Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s;
          continue k 0)
  | _ -> None

let effect_handler_2 (type a) (e : a Effect.t) : _ =
  match e with
  | Conversion_failure s ->
      Some
        (fun (k : (a, _) continuation) ->
          Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s;
          continue k 0)
  | _ -> None

let _ =
  Printf.printf "Starting up. Please input:\n%!";
  let r = ref 0 in
  match_with sum_up r
    {
      effc = effect_handler_2;
      exnc =
        (function
        | End_of_file -> Printf.printf "Sum is %d\n" !r | e -> raise e);
      (* Shouldn't reach here, means sum_up returned a value *)
      retc = (fun _ -> failwith "Impossible?");
    }

type t = E : 'a. 'a * ('a -> 'a) * ('a -> string) -> t

let ints = E (0, (fun x -> x + 1), string_of_int)
let floats = E (0., (fun x -> x +. 1.), string_of_float)
