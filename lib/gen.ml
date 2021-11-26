(* x64 Assembly Code Generation *)

let print_asm chan asm = Printf.fprintf chan "%s\n" asm
let emit_label label = Printf.sprintf "%s: \n" label

let emit_compare chan set_inst =
  print_asm chan "    cmp %rcx, %rax\n";
  print_asm chan "    movq $0, %rax\n";
  Printf.fprintf chan "   %s, %%al" set_inst
;;

let emit_function_epilogue chan =
  print_asm chan "movq %rbp, %rsp";
  print_asm chan "popq %rbp";
  print_asm chan "ret"
;;

let emit_bin_op chan op =
  match op with
  | Ast.Add -> print_asm chan "add %rcx, %rax"
  | Ast.Sub -> print_asm chan "sub %rcx, %rax"
  | Ast.Mul -> print_asm chan "imul %rcx, %rax"
  | Ast.Xor -> print_asm chan "xor %rcx, %rax"
  | _ -> failwith "not implemented"
;;

let codegen program filename =
  let f = filename ^ ".asm" in
  let chan = open_out f in
  match program with
  | Ast.Prog tl_list -> close_out chan
;;
