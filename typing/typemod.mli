(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Type-checking of the module language and typed ast hooks

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Types

module Signature_names : sig
  type t

  val simplify: Env.t -> t -> signature -> signature
end

val type_module:
        Env.t -> Parsetree.module_expr -> Typedtree.module_expr * Shape.t
val type_structure:
  Env.t -> Parsetree.structure ->
  Typedtree.structure * Types.signature * Signature_names.t * Shape.t *
  Env.t
val type_toplevel_phrase:
  Env.t -> Types.signature -> Parsetree.structure ->
  Typedtree.structure * Types.signature * Signature_names.t * Shape.t *
  Env.t
val type_implementation:
  Unit_info.t -> Compilation_unit.t -> Env.t ->
  Parsetree.structure -> Typedtree.implementation
val type_interface:
  sourcefile:string -> Compilation_unit.t -> Env.t ->
  Parsetree.signature -> Typedtree.signature
val check_nongen_signature:
        Env.t -> Types.signature -> unit
        (*
val type_open_:
        ?used_slot:bool ref -> ?toplevel:bool ->
        Asttypes.override_flag ->
        Env.t -> Location.t -> Longident.t Asttypes.loc -> Path.t * Env.t
        *)
val modtype_of_package:
        Env.t -> Location.t ->
        Path.t -> (Longident.t * type_expr) list -> module_type

val path_of_module : Typedtree.module_expr -> Path.t option

val save_signature:
  Unit_info.t -> Compilation_unit.t -> Typedtree.signature ->
  Env.t -> Cmi_format.cmi_infos_lazy -> unit

val package_units:
  Env.t -> string list -> Unit_info.Artifact.t -> Compilation_unit.t
  -> Typedtree.module_coercion

(* Should be in Envaux, but it breaks the build of the debugger *)
val initial_env:
  loc:Location.t ->
  initially_opened_module:string option ->
  open_implicit_modules:string list -> Env.t

module Sig_component_kind : sig
  type t =
    | Value
    | Type
    | Constructor
    | Label
    | Unboxed_label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type

  val to_string : t -> string
end

type hiding_error =
  | Illegal_shadowing of {
      shadowed_item_id: Ident.t;
      shadowed_item_kind: Sig_component_kind.t;
      shadowed_item_loc: Location.t;
      shadower_id: Ident.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }
  | Appears_in_signature of {
      opened_item_id: Ident.t;
      opened_item_kind: Sig_component_kind.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }

type functor_dependency_error =
    Functor_applied
  | Functor_included

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.explanation
  | Not_included_functor of Includemod.explanation
  | Cannot_eliminate_dependency of functor_dependency_error * module_type
  | Signature_expected
  | Structure_expected of module_type
  | Functor_expected of module_type
  | Signature_parameter_expected of module_type
  | Signature_result_expected of module_type
  | Recursive_include_functor
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.explanation
  | With_makes_applicative_functor_ill_typed of
      Longident.t * Path.t * Includemod.explanation
  | With_changes_module_alias of Longident.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | With_package_manifest of Longident.t * type_expr
  | Repeated_name of Sig_component_kind.t * string
  | Non_generalizable of { vars : type_expr list; expression : type_expr }
  | Non_generalizable_module of
      { vars : type_expr list; item : value_description; mty : module_type }
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_includable_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t
  | Cannot_scrape_package_type of Path.t
  | Badly_formed_signature of string * Typedecl.error
  | Cannot_hide_id of hiding_error
  | Invalid_type_subst_rhs
  | Non_packable_local_modtype_subst of Path.t
  | With_cannot_remove_packed_modtype of Path.t * module_type
  | Toplevel_nonvalue of string * Jkind.sort
  | Toplevel_unnamed_nonvalue of Jkind.sort
  | Strengthening_mismatch of Longident.t * Includemod.explanation
  | Cannot_pack_parameter
  | Compiling_as_parameterised_parameter
  | Cannot_compile_implementation_as_parameter
  | Cannot_implement_parameter of Compilation_unit.Name.t * Misc.filepath
  | Argument_for_non_parameter of Global_module.Name.t * Misc.filepath
  | Cannot_find_argument_type of Global_module.Parameter_name.t
  | Inconsistent_argument_types of {
      new_arg_type: Global_module.Parameter_name.t option;
      old_arg_type: Global_module.Parameter_name.t option;
      old_source_file: Misc.filepath;
    }
  | Duplicate_parameter_name of Global_module.Parameter_name.t
  | Submode_failed of Mode.Value.error
  | Modal_module_not_supported

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error: Env.t -> loc:Location.t -> error -> Location.error

(** Clear several bits of global state that may retain large amounts of memory
    after typechecking is finished. *)
val reset : preserve_persistent_env:bool -> unit
