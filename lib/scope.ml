type ('a, 'b) scope = ('a, 'b) Hashtbl.t list

exception EmptyScope
exception NotFound

let rec lookup_opt scope k =
  match scope with
  | [] -> None
  | head :: rest -> (
      match Hashtbl.find_opt head k with
      | Some _ as v -> v
      | None -> lookup_opt rest k)

let rec lookup scope k =
  match scope with
  | [] -> raise NotFound
  | head :: rest -> (
      match Hashtbl.find_opt head k with Some v -> v | None -> lookup rest k)

let insert scope k v =
  match scope with [] -> raise EmptyScope | head :: _ -> Hashtbl.add head k v

let entry scope = Hashtbl.create 16 :: scope
let leave scope = match scope with [] -> raise EmptyScope | _ :: rest -> rest
let empty = []