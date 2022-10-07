open Llvm

let string ctx = named_struct_type ctx "struct.string"
let array ctx = named_struct_type ctx "struct.array"
let closure ctx = named_struct_type ctx "struct.closure"
let tuple ctx = named_struct_type ctx "struct.tuple"
let tagged ctx = named_struct_type ctx "struct.tagged"

