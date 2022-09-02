open Errors

type ('a, 'b) scope = ('a, 'b) Hashtbl.t list

let _report_empty_scope_error = raise @@ Error "empty scope"

let rec lookup scope k =
  match scope with
  | [] -> _report_empty_scope_error
  | head :: rest -> (
      match Hashtbl.find_opt head k with Some v -> v | None -> lookup rest k)

let insert scope k v =
  match scope with
  | [] -> _report_empty_scope_error
  | head :: _ -> Hashtbl.add head k v

let entry scope = Hashtbl.create 8 :: scope

let leave scope =
  match scope with [] -> _report_empty_scope_error | _ :: rest -> rest
