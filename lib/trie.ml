type ('a, 'b) trie = Node of 'b option * ('a * ('a, 'b) trie) list

let rec find_opt trie ks =
  match trie with
  | Node (v, edges) -> (
      match ks with
      | [] -> v
      | h :: t -> (
          match List.assq_opt h edges with
          | Some child -> find_opt child t
          | None -> None))

let rec find trie ks =
  match trie with
  | Node (v, edges) -> (
      match ks with
      | [] -> if Option.is_none v then raise Not_found else Option.get v
      | h :: t -> find (List.assq h edges) t)

let rec mem trie ks =
  try
    match trie with
    | Node (v, edges) -> (
        match ks with
        | [] -> Option.is_some v
        | h :: t -> mem (List.assq h edges) t)
  with Not_found -> false

let empty v = Node (v, [])

let rec insert trie ks v =
  match trie with
  | Node (old, edges) -> (
      match ks with
      | [] -> Node (Some v, edges)
      | h :: t -> (
          match List.assq_opt h edges with
          | Some child ->
              Node (old, (h, insert child t v) :: List.remove_assq h edges)
          | None -> Node (old, (h, insert (empty None) t v) :: edges)))
