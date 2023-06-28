open Effect
open Effect.Deep

type _ t += Exn : exn -> _ t

let raise (e : exn) : 'a = perform (Exn e)

let try_with (f : unit -> 'a) (h : exn -> 'a) : 'a =
  try_with f ()
    {
      effc =
        (fun (type c) (e : c t) ->
          match e with
          | Exn exn -> Some (fun (k : (c, _) continuation) -> h exn)
          | _ -> None);
    }

exception Invalid_argument

(** [sqrt f] returns the square root of [f].
    @raise Invalid_argument if f < 0. *)
let sqrt f = if f < 0.0 then raise Invalid_argument else sqrt f

let _ =
  try_with
    (fun () ->
      let r = sqrt 42.42 in
      Printf.printf "%f\n%!" r;
      let r = sqrt (-1.0) in
      Printf.printf "%f\n" r)
    (function
      | Invalid_argument -> Printf.printf "Invalid_argument to sqrt\n" | _ -> ())

(* Prints:
   6.513064
   Invalid_argument to sqrt *)
