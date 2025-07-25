(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare

let verbose = ref false

include Cfg_intf.S
module DLL = Oxcaml_utils.Doubly_linked_list

type basic_instruction_list = basic instruction DLL.t

type basic_block =
  { mutable start : Label.t;
    body : basic_instruction_list;
    mutable terminator : terminator instruction;
    mutable predecessors : Label.Set.t;
    mutable stack_offset : int;
    mutable exn : Label.t option;
    mutable can_raise : bool;
    mutable is_trap_handler : bool;
    mutable cold : bool
  }

type codegen_option =
  | Reduce_code_size
  | No_CSE
  | Assume_zero_alloc of
      { strict : bool;
        never_returns_normally : bool;
        never_raises : bool;
        loc : Location.t
      }
  | Check_zero_alloc of
      { strict : bool;
        loc : Location.t;
        custom_error_msg : string option
      }

let rec of_cmm_codegen_option : Cmm.codegen_option list -> codegen_option list =
 fun cmm_options ->
  match cmm_options with
  | [] -> []
  | hd :: tl -> (
    match hd with
    | No_CSE -> No_CSE :: of_cmm_codegen_option tl
    | Reduce_code_size -> Reduce_code_size :: of_cmm_codegen_option tl
    | Assume_zero_alloc { strict; never_returns_normally; never_raises; loc } ->
      Assume_zero_alloc { strict; never_returns_normally; never_raises; loc }
      :: of_cmm_codegen_option tl
    | Check_zero_alloc { strict; loc; custom_error_msg } ->
      Check_zero_alloc { strict; loc; custom_error_msg }
      :: of_cmm_codegen_option tl
    | Use_linscan_regalloc -> of_cmm_codegen_option tl)

type t =
  { blocks : basic_block Label.Tbl.t;
    fun_name : string;
    fun_args : Reg.t array;
    fun_codegen_options : codegen_option list;
    fun_dbg : Debuginfo.t;
    entry_label : Label.t;
    fun_contains_calls : bool;
    (* CR-someday gyorsh: compute locally. *)
    fun_num_stack_slots : int Stack_class.Tbl.t;
    fun_poll : Lambda.poll_attribute;
    next_instruction_id : InstructionId.sequence
  }

let create ~fun_name ~fun_args ~fun_codegen_options ~fun_dbg ~fun_contains_calls
    ~fun_num_stack_slots ~fun_poll ~next_instruction_id =
  { fun_name;
    fun_args;
    fun_codegen_options;
    fun_dbg;
    entry_label = Label.entry_label;
    (* CR gyorsh: We should use [Cmm.new_label ()] here, but validator tests
       currently rely on it to be initialized as above. *)
    blocks = Label.Tbl.create 31;
    fun_contains_calls;
    fun_num_stack_slots;
    fun_poll;
    next_instruction_id
  }

let mem_block t label = Label.Tbl.mem t.blocks label

let successor_labels_normal ti =
  match ti.desc with
  | Tailcall_self { destination } -> Label.Set.singleton destination
  | Switch labels -> Array.to_seq labels |> Label.Set.of_seq
  | Return | Raise _ | Tailcall_func _ -> Label.Set.empty
  | Call_no_return _ -> Label.Set.empty
  | Never -> Label.Set.empty
  | Always l -> Label.Set.singleton l
  | Parity_test { ifso; ifnot } | Truth_test { ifso; ifnot } ->
    Label.Set.singleton ifso |> Label.Set.add ifnot
  | Float_test { width = _; lt; gt; eq; uo } ->
    Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
    |> Label.Set.add uo
  | Int_test { lt; gt; eq; imm = _; is_signed = _ } ->
    Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
  | Call { op = _; label_after } | Prim { op = _; label_after } ->
    Label.Set.singleton label_after

let successor_labels ~normal ~exn block =
  match normal, exn with
  | false, false -> Label.Set.empty
  | true, false -> successor_labels_normal block.terminator
  | false, true -> (
    match block.exn with
    | None -> Label.Set.empty
    | Some label -> Label.Set.singleton label)
  | true, true -> (
    match block.exn with
    | None -> successor_labels_normal block.terminator
    | Some label ->
      Label.Set.add label (successor_labels_normal block.terminator))

let predecessor_labels block = Label.Set.elements block.predecessors

let replace_successor_labels t ~normal ~exn block ~f =
  (* Check that the new labels are in [t] *)
  let f src =
    let dst = f src in
    if not (mem_block t dst)
    then
      Misc.fatal_errorf
        "Cfg.replace_successor_labels: \nnew successor %a not found in the cfg"
        Label.format dst;
    dst
  in
  if exn then block.exn <- Option.map f block.exn;
  if normal
  then
    let desc =
      match block.terminator.desc with
      | Never -> Never
      | Always l -> Always (f l)
      | Parity_test { ifso; ifnot } ->
        Parity_test { ifso = f ifso; ifnot = f ifnot }
      | Truth_test { ifso; ifnot } ->
        Truth_test { ifso = f ifso; ifnot = f ifnot }
      | Int_test { lt; eq; gt; is_signed; imm } ->
        Int_test { lt = f lt; eq = f eq; gt = f gt; is_signed; imm }
      | Float_test { width; lt; eq; gt; uo } ->
        Float_test { width; lt = f lt; eq = f eq; gt = f gt; uo = f uo }
      | Switch labels -> Switch (Array.map f labels)
      | Tailcall_self { destination } ->
        Tailcall_self { destination = f destination }
      | Tailcall_func Indirect
      | Tailcall_func (Direct _)
      | Return | Raise _ | Call_no_return _ ->
        block.terminator.desc
      | Call { op; label_after } -> Call { op; label_after = f label_after }
      | Prim { op; label_after } -> Prim { op; label_after = f label_after }
    in
    block.terminator <- { block.terminator with desc }

let add_block_exn t block =
  if Label.Tbl.mem t.blocks block.start
  then
    Misc.fatal_errorf "Cfg.add_block_exn: block %a is already present"
      Label.format block.start;
  Label.Tbl.add t.blocks block.start block

let get_block t label = Label.Tbl.find_opt t.blocks label

let get_block_exn t label =
  match Label.Tbl.find t.blocks label with
  | exception Not_found ->
    Misc.fatal_errorf "Cfg.get_block_exn: block %a not found" Label.format label
  | block -> block

let can_raise_interproc block = block.can_raise && Option.is_none block.exn

type 'a instr_mapper = { f : 'b. 'b instruction -> 'a } [@@unboxed]

let map_first_instruction (block : basic_block) (t : 'a instr_mapper) =
  match DLL.hd block.body with
  | None -> t.f block.terminator
  | Some first_instr -> t.f first_instr

let first_instruction_id (block : basic_block) : InstructionId.t =
  map_first_instruction block { f = (fun instr -> instr.id) }

let first_instruction_stack_offset (block : basic_block) : int =
  map_first_instruction block { f = (fun instr -> instr.stack_offset) }

let fun_name t = t.fun_name

let entry_label t = t.entry_label

let iter_blocks t ~f = Label.Tbl.iter f t.blocks

let iter_blocks_dfs : t -> f:(Label.t -> basic_block -> unit) -> unit =
 fun cfg ~f ->
  let marked = ref Label.Set.empty in
  let rec iter (label : Label.t) : unit =
    if not (Label.Set.mem label !marked)
    then (
      marked := Label.Set.add label !marked;
      let block = get_block_exn cfg label in
      f label block;
      Label.Set.iter
        (fun succ_label -> iter succ_label)
        (successor_labels ~normal:true ~exn:true block))
  in
  iter cfg.entry_label;
  (* note: some block may not have been seen since we currently cannot remove
     all non-reachable blocks. *)
  if Label.Set.cardinal !marked <> Label.Tbl.length cfg.blocks
  then
    iter_blocks cfg ~f:(fun label block ->
        if not (Label.Set.mem label !marked) then f label block)

let fold_blocks t ~f ~init = Label.Tbl.fold f t.blocks init

let fold_body_instructions t ~f ~init =
  let helper _ block acc = DLL.fold_left block.body ~f ~init:acc in
  fold_blocks t ~f:helper ~init

let register_predecessors_for_all_blocks (t : t) =
  Label.Tbl.iter
    (fun label block ->
      let targets = successor_labels ~normal:true ~exn:true block in
      Label.Set.iter
        (fun target ->
          let target_block =
            match Label.Tbl.find t.blocks target with
            | target_block -> target_block
            | exception Not_found ->
              Misc.fatal_errorf
                "Cfg.register_predecessors_for_all_blocks: block %a not found"
                Label.format target
          in
          target_block.predecessors
            <- Label.Set.add label target_block.predecessors)
        targets)
    t.blocks

(* Printing for debug *)

let dump_basic ppf (basic : basic) =
  let open Format in
  match basic with
  | Op op -> Operation.dump ppf op
  | Reloadretaddr -> fprintf ppf "Reloadretaddr"
  | Pushtrap { lbl_handler } ->
    fprintf ppf "Pushtrap handler=%a" Label.format lbl_handler
  | Poptrap { lbl_handler } ->
    fprintf ppf "Poptrap handler=%a" Label.format lbl_handler
  | Prologue -> fprintf ppf "Prologue"
  | Stack_check { max_frame_size_bytes } ->
    fprintf ppf "Stack_check size=%d" max_frame_size_bytes

let dump_terminator' ?(print_reg = Printreg.reg) ?(res = [||]) ?(args = [||])
    ?(sep = "\n") ppf (terminator : terminator) =
  let first_arg =
    if Array.length args >= 1
    then Format.fprintf Format.str_formatter " %a" print_reg args.(0);
    Format.flush_str_formatter ()
  in
  let second_arg =
    if Array.length args >= 2
    then Format.fprintf Format.str_formatter " %a" print_reg args.(1);
    Format.flush_str_formatter ()
  in
  let print_args ppf args =
    if Array.length args = 0
    then ()
    else Format.fprintf ppf " %a" (Printreg.regs' ~print_reg) args
  in
  let print_res ppf =
    if Array.length res > 0
    then Format.fprintf ppf "%a := " (Printreg.regs' ~print_reg) res
  in
  let dump_linear_call_op ppf op =
    Printlinear.call_operation ~print_reg ppf op args
  in
  let open Format in
  match terminator with
  | Never -> fprintf ppf "deadend"
  | Always l -> fprintf ppf "goto %a" Label.format l
  | Parity_test { ifso; ifnot } ->
    fprintf ppf "if even%s goto %a%selse goto %a" first_arg Label.format ifso
      sep Label.format ifnot
  | Truth_test { ifso; ifnot } ->
    fprintf ppf "if true%s goto %a%selse goto %a" first_arg Label.format ifso
      sep Label.format ifnot
  | Float_test { width = _; lt; eq; gt; uo } ->
    fprintf ppf "if%s <%s goto %a%s" first_arg second_arg Label.format lt sep;
    fprintf ppf "if%s =%s goto %a%s" first_arg second_arg Label.format eq sep;
    fprintf ppf "if%s >%s goto %a%s" first_arg second_arg Label.format gt sep;
    fprintf ppf "else goto %a" Label.format uo
  | Int_test { lt; eq; gt; is_signed; imm } ->
    let cmp =
      Printf.sprintf " %s%s"
        (if is_signed then "s" else "u")
        (match imm with None -> second_arg | Some i -> " " ^ Int.to_string i)
    in
    fprintf ppf "if%s <%s goto %a%s" first_arg cmp Label.format lt sep;
    fprintf ppf "if%s =%s goto %a%s" first_arg cmp Label.format eq sep;
    fprintf ppf "if%s >%s goto %a" first_arg cmp Label.format gt
  | Switch labels ->
    fprintf ppf "switch%s%s" first_arg sep;
    let label_count = Array.length labels in
    if label_count >= 1
    then (
      for i = 0 to label_count - 2 do
        fprintf ppf "case %d: goto %a%s" i Label.format labels.(i) sep
      done;
      let i = label_count - 1 in
      fprintf ppf "case %d: goto %a" i Label.format labels.(i))
  | Call_no_return { func_symbol; _ } ->
    fprintf ppf "Call_no_return %s%a" func_symbol print_args args
  | Return -> fprintf ppf "Return%a" print_args args
  | Raise _ -> fprintf ppf "Raise%a" print_args args
  | Tailcall_self { destination } ->
    dump_linear_call_op ppf
      (Linear.Ltailcall_imm
         { func =
             { sym_name =
                 Printf.sprintf "self(%s)" (Label.to_string destination);
               sym_global = Local
             }
         })
  | Tailcall_func call ->
    dump_linear_call_op ppf
      (match call with
      | Indirect -> Linear.Ltailcall_ind
      | Direct func -> Linear.Ltailcall_imm { func })
  | Call { op = call; label_after } ->
    Format.fprintf ppf "%t%a" print_res dump_linear_call_op
      (match call with
      | Indirect -> Linear.Lcall_ind
      | Direct func -> Linear.Lcall_imm { func });
    Format.fprintf ppf "%sgoto %a" sep Label.format label_after
  | Prim { op = prim; label_after } ->
    Format.fprintf ppf "%t%a" print_res dump_linear_call_op
      (match prim with
      | External
          { func_symbol = func;
            ty_res;
            ty_args;
            alloc;
            stack_ofs;
            stack_align;
            effects = _
          } ->
        Linear.Lextcall
          { func;
            ty_res;
            ty_args;
            returns = true;
            alloc;
            stack_ofs;
            stack_align
          }
      | Probe { name; handler_code_sym; enabled_at_init } ->
        Linear.Lprobe { name; handler_code_sym; enabled_at_init });
    Format.fprintf ppf "%sgoto %a" sep Label.format label_after

let dump_terminator ?sep ppf terminator = dump_terminator' ?sep ppf terminator

let print_basic' ?print_reg ppf (instruction : basic instruction) =
  let desc = Cfg_to_linear_desc.from_basic instruction.desc in
  let instruction =
    { Linear.desc;
      next = Linear.end_instr;
      arg = instruction.arg;
      res = instruction.res;
      dbg = Debuginfo.none;
      fdo = None;
      live = Reg.Set.empty;
      available_before = None;
      available_across = None
    }
  in
  Printlinear.instr' ?print_reg ppf instruction

let print_basic ppf i = print_basic' ppf i

let print_terminator' ?print_reg ppf (ti : terminator instruction) =
  dump_terminator' ?print_reg ~res:ti.res ~args:ti.arg ~sep:"\n" ppf ti.desc

let print_terminator ppf ti = print_terminator' ppf ti

let print_instruction' ?print_reg ppf i =
  match i with
  | `Basic i -> print_basic' ?print_reg ppf i
  | `Terminator i -> print_terminator' ?print_reg ppf i

let print_instruction ppf i = print_instruction' ppf i

let can_raise_terminator (i : terminator) =
  match i with
  | Call_no_return { func_symbol; _ } ->
    not (String.equal func_symbol Cmm.caml_flambda2_invalid)
  | Raise _ | Tailcall_func _ | Call _ | Prim { op = Probe _; label_after = _ }
    ->
    true
  | Prim { op = External { alloc; effects; _ }; label_after = _ } -> (
    if not alloc
    then false
    else
      (* Even if going via [caml_c_call], if there are no effects, the function
         cannot raise an exception. (Example: [caml_obj_dup].) *)
      match effects with No_effects -> false | Arbitrary_effects -> true)
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Tailcall_self _ ->
    false

(* CR gyorsh: [is_pure_terminator] is not the same as [can_raise_terminator]
   because of [Tailcal Self] which is not pure but marked as cannot raise at the
   moment, which we might want to reconsider later. *)
(* CR mshinwell/gyorsh: maybe this function could be made more precise e.g.
   taking into account [effects] on extcalls *)
let is_pure_terminator desc =
  match (desc : terminator) with
  | Return | Raise _ | Call_no_return _ | Tailcall_func _ | Tailcall_self _
  | Call _ | Prim _ ->
    false
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ ->
    (* CR gyorsh: fix for memory operands *)
    true

let is_never_terminator desc =
  match (desc : terminator) with
  | Never -> true
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _ | Call _ | Prim _ ->
    false

let is_return_terminator desc =
  match (desc : terminator) with
  | Return -> true
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Raise _ | Tailcall_self _ | Tailcall_func _ | Call_no_return _
  | Call _ | Prim _ ->
    false

let is_pure_basic : basic -> bool = function
  | Op op -> Operation.is_pure op
  | Reloadretaddr ->
    (* This is a no-op on supported backends but on some others like "power" it
       wouldn't be. Saying it's not pure doesn't decrease the generated code
       quality and is future-proof.*)
    false
  | Pushtrap _ | Poptrap _ ->
    (* Those instructions modify the trap stack which actually modifies the
       stack pointer. *)
    false
  | Prologue ->
    (* [Prologue] grows the stack when entering a function and therefore
       modifies the stack pointer. [Prologue] can be considered pure if it's
       ensured that it wouldn't modify the stack pointer (e.g. there are no used
       local stack slots nor calls). *)
    false
  | Stack_check _ ->
    (* May reallocate the stack. *)
    false

let same_location (r1 : Reg.t) (r2 : Reg.t) =
  Reg.same_loc_fatal_on_unknown
    ~fatal_message:"Cfg got unknown register location." r1 r2

let is_noop_move instr =
  match instr.desc with
  | Op (Move | Spill | Reload) -> same_location instr.arg.(0) instr.res.(0)
  | Op (Csel _) -> (
    match instr.res.(0).loc with
    | Unknown -> false
    | Reg _ | Stack _ ->
      let len = Array.length instr.arg in
      let ifso = instr.arg.(len - 2) in
      let ifnot = instr.arg.(len - 1) in
      Reg.same_loc instr.res.(0) ifso && Reg.same_loc instr.res.(0) ifnot)
  | Op
      ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
      | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Stackoffset _
      | Load _ | Store _ | Intop _ | Intop_imm _ | Intop_atomic _ | Floatop _
      | Opaque | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Specific _ | Name_for_debugger _ | Begin_region | End_region | Dls_get
      | Poll | Alloc _ | Pause )
  | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Stack_check _ ->
    false

let set_stack_offset (instr : _ instruction) stack_offset =
  if stack_offset < 0
  then
    Misc.fatal_errorf "Cfg.set_stack_offset: expected non-negative got %d"
      stack_offset;
  instr.stack_offset <- stack_offset

let set_stack_offset_for_block (block : basic_block) stack_offset =
  if stack_offset < 0
  then
    Misc.fatal_errorf
      "Cfg.set_stack_offset_for_block: expected non-negative got %d"
      stack_offset;
  block.stack_offset <- stack_offset

let set_live (instr : _ instruction) live = instr.live <- live

let string_of_irc_work_list = function
  | Unknown_list -> "unknown_list"
  | Coalesced -> "coalesced"
  | Constrained -> "constrained"
  | Frozen -> "frozen"
  | Work_list -> "work_list"
  | Active -> "active"

let make_instruction ~desc ?(arg = [||]) ?(res = [||]) ?(dbg = Debuginfo.none)
    ?(fdo = Fdo_info.none) ?(live = Reg.Set.empty) ~stack_offset ~id
    ?(irc_work_list = Unknown_list) ?(ls_order = 0) ?(available_before = None)
    ?(available_across = None) () =
  { desc;
    arg;
    res;
    dbg;
    fdo;
    live;
    stack_offset;
    id;
    irc_work_list;
    ls_order;
    available_before;
    available_across
  }

let invalid_stack_offset = -1

let make_empty_block ?label terminator : basic_block =
  let start =
    match label with None -> Cmm.new_label () | Some label -> label
  in
  { start;
    body = DLL.make_empty ();
    terminator;
    predecessors = Label.Set.empty;
    stack_offset = invalid_stack_offset;
    exn = None;
    can_raise = false;
    is_trap_handler = false;
    cold = false
  }

let is_poll (instr : basic instruction) =
  match instr.desc with
  | Op Poll -> true
  | Reloadretaddr | Prologue | Pushtrap _ | Poptrap _ | Stack_check _
  | Op
      ( Alloc _ | Move | Spill | Reload | Opaque | Pause | Begin_region
      | End_region | Dls_get | Const_int _ | Const_float32 _ | Const_float _
      | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
      | Stackoffset _ | Load _
      | Store (_, _, _)
      | Intop _
      | Intop_imm (_, _)
      | Intop_atomic _
      | Floatop (_, _)
      | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Specific _ | Name_for_debugger _ ) ->
    false

let is_alloc (instr : basic instruction) =
  match instr.desc with
  | Op (Alloc _) -> true
  | Reloadretaddr | Prologue | Pushtrap _ | Poptrap _ | Stack_check _
  | Op
      ( Poll | Move | Spill | Reload | Opaque | Begin_region | End_region
      | Dls_get | Pause | Const_int _ | Const_float32 _ | Const_float _
      | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
      | Stackoffset _ | Load _
      | Store (_, _, _)
      | Intop _
      | Intop_imm (_, _)
      | Intop_atomic _
      | Floatop (_, _)
      | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Specific _ | Name_for_debugger _ ) ->
    false

let is_end_region (b : basic) =
  match b with
  | Op End_region -> true
  | Reloadretaddr | Prologue | Pushtrap _ | Poptrap _ | Stack_check _
  | Op
      ( Alloc _ | Poll | Move | Spill | Reload | Opaque | Begin_region | Dls_get
      | Pause | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
      | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Stackoffset _
      | Load _
      | Store (_, _, _)
      | Intop _
      | Intop_imm (_, _)
      | Intop_atomic _
      | Floatop (_, _)
      | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Specific _ | Name_for_debugger _ ) ->
    false

let is_alloc_or_poll instr = is_alloc instr || is_poll instr

let basic_block_contains_calls block =
  block.is_trap_handler
  || (match block.terminator.desc with
     | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
     | Int_test _ | Switch _ | Return ->
       false
     | Raise raise_kind -> (
       match raise_kind with
       | Lambda.Raise_notrace -> false
       | Lambda.Raise_regular | Lambda.Raise_reraise ->
         (* PR#6239 *)
         (* caml_stash_backtrace; we #mark_call rather than #mark_c_tailcall to
            get a good stack backtrace *)
         true)
     | Tailcall_self _ -> false
     | Tailcall_func _ -> false
     | Call_no_return _ -> true
     | Call _ -> true
     | Prim { op = External _; _ } -> true
     | Prim { op = Probe _; _ } -> true)
  || DLL.exists block.body ~f:is_alloc_or_poll

let equal_irc_work_list left right =
  match left, right with
  | Unknown_list, Unknown_list
  | Coalesced, Coalesced
  | Constrained, Constrained
  | Frozen, Frozen
  | Work_list, Work_list
  | Active, Active ->
    true
  | (Unknown_list | Coalesced | Constrained | Frozen | Work_list | Active), _ ->
    false

let remove_trap_instructions t removed_trap_handlers =
  (* Remove Lpushtrap and Lpoptrap instructions that refer to dead labels and
     update stack_offsets of affected instructions and blocks. [stack_offset] is
     in bytes throughout this function. *)
  let visited = ref Label.Set.empty in
  let update_instruction (i : _ instruction) ~stack_offset =
    if not (Int.equal i.stack_offset stack_offset)
    then set_stack_offset i stack_offset
  in
  let rec update_basic cursor ~stack_offset =
    let update_basic_next r ~stack_offset =
      match r with
      | Error `End_of_list -> stack_offset
      | Ok () -> update_basic cursor ~stack_offset
    in
    let basic = DLL.Cursor.value cursor in
    update_instruction basic ~stack_offset;
    match basic.desc with
    | Pushtrap { lbl_handler } ->
      if Label.Set.mem lbl_handler removed_trap_handlers
      then update_basic_next (DLL.Cursor.delete_and_next cursor) ~stack_offset
      else (
        update_block lbl_handler ~stack_offset;
        update_basic_next (DLL.Cursor.next cursor)
          ~stack_offset:(stack_offset + Proc.trap_size_in_bytes))
    | Poptrap { lbl_handler } ->
      if Label.Set.mem lbl_handler removed_trap_handlers
      then update_basic_next (DLL.Cursor.delete_and_next cursor) ~stack_offset
      else
        update_basic_next (DLL.Cursor.next cursor)
          ~stack_offset:(stack_offset - Proc.trap_size_in_bytes)
    | Op (Stackoffset n) ->
      update_basic_next (DLL.Cursor.next cursor) ~stack_offset:(stack_offset + n)
    | Op
        ( Move | Spill | Reload | Const_int _ | Const_float _ | Const_float32 _
        | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
        | Load _ | Store _ | Intop _ | Intop_imm _ | Intop_atomic _ | Floatop _
        | Csel _ | Static_cast _ | Reinterpret_cast _ | Probe_is_enabled _
        | Opaque | Begin_region | End_region | Specific _ | Name_for_debugger _
        | Dls_get | Poll | Alloc _ | Pause )
    | Reloadretaddr | Prologue | Stack_check _ ->
      update_basic_next (DLL.Cursor.next cursor) ~stack_offset
  and update_body r ~stack_offset =
    match r with
    | Error `Empty -> stack_offset
    | Ok cursor -> update_basic cursor ~stack_offset
  and update_terminator terminator ~stack_offset =
    (match terminator.desc with
    | Tailcall_self _ -> assert (Int.equal stack_offset 0)
    | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ | Tailcall_func _
    | Call_no_return _ | Call _ | Prim _ ->
      ());
    update_instruction terminator ~stack_offset
  and update_block label ~stack_offset =
    let block = get_block_exn t label in
    if Label.Set.mem label !visited
    then assert (block.stack_offset = stack_offset)
    else (
      visited := Label.Set.add label !visited;
      if not (Int.equal block.stack_offset stack_offset)
      then set_stack_offset_for_block block stack_offset;
      let stack_offset =
        update_body (DLL.create_hd_cursor block.body) ~stack_offset
      in
      update_terminator block.terminator ~stack_offset;
      Label.Set.iter
        (update_block ~stack_offset)
        (successor_labels ~normal:true ~exn:false block)
      (* Stack offset is not propagated across exceptional edges, because an
         exception that is raised "folds" everything on the stack up to the top
         of the currnt trap stack, and the amount can be different in different
         blocks. *))
  in
  if not (Label.Set.is_empty removed_trap_handlers)
  then
    (* CR-someday gyorsh: avoid iterating over all the instructions to just
       remove a few pushtrap/poptraps. *)
    (* update all blocks reachable from entry *)
    update_block t.entry_label ~stack_offset:0

let remove_blocks t labels_to_remove =
  let removed_labels = ref Label.Set.empty in
  let removed_trap_handlers = ref Label.Set.empty in
  Label.Tbl.filter_map_inplace
    (fun l b ->
      if Label.Set.mem l labels_to_remove
      then (
        assert (Label.Set.is_empty b.predecessors);
        assert (Label.Set.is_empty (successor_labels b ~normal:true ~exn:false));
        if b.is_trap_handler
        then removed_trap_handlers := Label.Set.add l !removed_trap_handlers;
        removed_labels := Label.Set.add l !removed_labels;
        None)
      else Some b)
    t.blocks;
  let labels_not_found = Label.Set.diff labels_to_remove !removed_labels in
  if not (Label.Set.is_empty labels_not_found)
  then
    Misc.fatal_errorf "Cfg.remove_blocks: not found blocks %a" Label.Set.print
      labels_not_found;
  remove_trap_instructions t !removed_trap_handlers
