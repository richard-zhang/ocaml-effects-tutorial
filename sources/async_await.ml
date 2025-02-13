open Printf

module type Scheduler = sig
  type 'a promise

  (* Type of promises *)
  val async : (unit -> 'a) -> 'a promise

  (* [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a

  (* [await p] returns the result of the promise. *)
  val yield : unit -> unit

  (* yields control to another task *)
  val run : (unit -> 'a) -> unit
  (* Runs the scheduler *)
end

module Scheduler : Scheduler = struct
  open Effect
  open Effect.Deep

  type 'a _promise = Waiting of ('a, unit) continuation list | Done of 'a
  type 'a promise = 'a _promise ref

  type _ Effect.t +=
    | Async : (unit -> 'a) -> 'a promise Effect.t
    | Yield : unit Effect.t
    | Await : 'a promise -> 'a Effect.t

  let async f = perform (Async f)
  let yield () = perform Yield
  let await p = perform (Await p)
  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () = if Queue.is_empty q then () else Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
     fun pr main ->
      match_with main ()
        {
          retc =
            (fun v ->
              match !pr with
              | Waiting deps ->
                  List.iter (fun k -> enqueue (fun () -> continue k v)) deps;
                  pr := Done v;
                  dequeue ()
              | Done v -> failwith "impossible");
          exnc = raise;
          effc =
            (fun (type b) (eff : b Effect.t) ->
              match eff with
              | Async f ->
                  Some
                    (fun (k : (b, _) continuation) ->
                      let newPr = ref (Waiting []) in
                      enqueue (fun () -> fork newPr f);
                      continue k newPr)
              | Yield ->
                  Some
                    (fun k ->
                      enqueue (continue k);
                      dequeue ())
              | Await p ->
                  Some
                    (fun (k : (b, _) continuation) ->
                      match !p with
                      | Done v -> continue k v
                      | Waiting l ->
                          p := Waiting (k :: l);
                          dequeue ())
              | _ -> None);
        }
    in
    fork (ref (Waiting [])) main
end

open Scheduler

let main () =
  let task name () =
    Printf.printf "starting %s\n%!" name;
    let v = Random.int 100 in
    Printf.printf "yielding %s\n%!" name;
    yield ();
    Printf.printf "ending %s with %d\n%!" name v;
    v
  in
  let pa = async (task "a") in
  let pb = async (task "b") in
  let pc =
    async (fun () ->
        Printf.printf "Starting c\n%!";
        let val_a = await pa in
        let val_b = await pb in
        let v = val_a + val_b in
        Printf.printf "ending %s with %d\n%!" "c" v;
        v)
  in
  let pd = async (task "d") in
  let pe = async (task "e") in
  let pf =
    async (fun () ->
        Printf.printf "starting f\n%!";
        let x = await pa in
        Printf.printf "yielding f\n%!";
        yield ();
        let v = x + await pe + await pc - await pd in
        Printf.printf "ending %s with %d\n%!" "f" v;
        v)
  in
  Printf.printf "Before waiting on anything\n%!";
  Printf.printf "pf is %d\n" (await pf);
  Printf.printf "Sum is %d\n" (await pc);
  assert (await pa + await pb = await pc)

let _ = run main
