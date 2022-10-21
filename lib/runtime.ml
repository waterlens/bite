open Llvm

let string m = type_by_name m "struct.string" |> Option.get
let array m = type_by_name m "struct.array" |> Option.get
let closure m = type_by_name m "struct.closure" |> Option.get
let tuple m = type_by_name m "struct.tuple" |> Option.get
let tagged m = type_by_name m "struct.tagged" |> Option.get
let i64 m = type_by_name m "struct.i64" |> Option.get
let obj m = type_by_name m "struct.object" |> Option.get
