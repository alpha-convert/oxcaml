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

(* Environment handling *)

open Types
open Misc

type value_unbound_reason =
  | Val_unbound_instance_variable
  | Val_unbound_self
  | Val_unbound_ancestor
  | Val_unbound_ghost_recursive of Location.t

type module_unbound_reason =
  | Mod_unbound_illegal_recursion of
      { container : string option; unbound: string }

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description * Mode.Value.l
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_presence * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t
  (** The string set argument of [Env_open] represents a list of module names
      to skip, i.e. that won't be imported in the toplevel namespace. *)
  | Env_functor_arg of summary * Ident.t
  | Env_constraints of summary * type_declaration Path.Map.t
  | Env_copy_types of summary
  | Env_persistent of summary * Ident.t
  | Env_value_unbound of summary * string * value_unbound_reason
  | Env_module_unbound of summary * string * module_unbound_reason
  (* CR zqian: track [add_lock] as well *)

type address = Persistent_env.address =
  | Aunit of Compilation_unit.t
  | Alocal of Ident.t
  | Adot of address * int

type t

val empty: t

(* This environment is lazy so that it may depend on the enabled extensions,
   typically adjusted via command line flags.  If extensions are changed after
   theis environment is forced, they may be inaccurate.  This could happen, for
   example, if extensions are adjusted via the compiler-libs. *)
val initial: t Lazy.t

val diff: t -> t -> Ident.t list

(* approximation to the preimage equivalence class of [find_type] *)
val same_type_declarations: t -> t -> bool

type type_descr_kind =
  (label_description, unboxed_label_description, constructor_description) type_kind

  (* alias for compatibility *)
type type_descriptions = type_descr_kind

(* For short-paths *)
type iter_cont
val iter_types:
    (Path.t -> Path.t * type_declaration -> unit) ->
    t -> iter_cont
val run_iter_cont: iter_cont list -> (Path.t * iter_cont) list
val same_types: t -> t -> bool
val used_persistent: unit -> Compilation_unit.Name.Set.t
val find_shadowed_types: Path.t -> t -> Path.t list
val without_cmis: ('a -> 'b) -> 'a -> 'b
(* [without_cmis f arg] applies [f] to [arg], but does not
   allow opening cmis during its execution *)

(* Lookup by paths *)

val find_value_no_locks_exn: Ident.t -> t ->
  Subst.Lazy.value_description * Mode.Value.l
(** Find a value by an [Ident.t]. Raises if encounters any locks. *)

val find_value: Path.t -> t -> Subst.Lazy.value_description
val find_type: Path.t -> t -> type_declaration
val find_type_descrs: Path.t -> t -> type_descriptions
val find_module_lazy: Path.t -> t -> Subst.Lazy.module_declaration
val find_module: Path.t -> t -> module_declaration
val find_modtype_lazy: Path.t -> t -> Subst.Lazy.modtype_declaration
val find_modtype: Path.t -> t -> modtype_declaration
val find_class: Path.t -> t -> class_declaration
val find_cltype: Path.t -> t -> class_type_declaration

val find_ident_constructor: Ident.t -> t -> constructor_description
val find_ident_label: 'rcd record_form -> Ident.t -> t -> 'rcd gen_label_description

val find_type_expansion:
    Path.t -> t -> type_expr list * type_expr * int
val find_type_expansion_opt:
    Path.t -> t -> type_expr list * type_expr * int
(* Find the manifest type information associated to a type for the sake
   of the compiler's type-based optimisations. *)
val find_modtype_expansion: Path.t -> t -> module_type
val find_modtype_expansion_lazy: Path.t -> t -> Subst.Lazy.module_type

val find_hash_type: Path.t -> t -> type_declaration
(* Find the "#t" type given the path for "t" *)

val find_value_address: Path.t -> t -> address
val find_module_address: Path.t -> t -> address
val find_class_address: Path.t -> t -> address
val find_constructor_address: Path.t -> t -> address

val shape_of_path:
  namespace:Shape.Sig_component_kind.t -> t -> Path.t -> Shape.t

val add_functor_arg: Ident.t -> t -> t
val is_functor_arg: Path.t -> t -> bool

val normalize_module_path: Location.t option -> t -> Path.t -> Path.t
(* Normalize the path to a concrete module.
   If the option is None, allow returning dangling paths.
   Otherwise raise a Missing_module error, and may add forgotten
   head as required global. *)

val normalize_type_path: Location.t option -> t -> Path.t -> Path.t
(* Normalize the prefix part of the type path *)

val normalize_value_path: Location.t option -> t -> Path.t -> Path.t
(* Normalize the prefix part of the value path *)

val normalize_modtype_path: t -> Path.t -> Path.t
(* Normalize a module type path *)

val normalize_instance_names_in_module_path: Path.t -> Path.t
(* Normalize the instance names appearing in a module path by removing
   excess arguments arising from transparent aliases *)

val reset_required_globals: unit -> unit
val get_required_globals: unit -> Compilation_unit.t list
val add_required_global: Path.t -> t -> unit

val reset_probes: unit -> unit
val add_probe: string -> unit
val has_probe: string -> bool

val has_local_constraints: t -> bool

(* Mark definitions as used *)
val mark_value_used: Uid.t -> unit
val mark_module_used: Uid.t -> unit
val mark_type_used: Uid.t -> unit

type constructor_usage = Positive | Pattern | Exported_private | Exported
val mark_constructor_used:
    constructor_usage -> constructor_declaration -> unit
val mark_extension_used:
    constructor_usage -> extension_constructor -> unit

type label_usage =
    Projection | Mutation | Construct | Exported_private | Exported
val mark_label_used:
    label_usage -> label_declaration -> unit

(* Lookup by long identifiers *)

(* Lookup errors *)

type unbound_value_hint =
  | No_hint
  | Missing_rec of Location.t

type locality_context =
  | Tailcall_function
  | Tailcall_argument
  | Partial_application
  | Return
  | Lazy

type closure_context =
  | Function of locality_context option
  | Functor
  | Lazy

type escaping_context =
  | Letop
  | Probe
  | Class

type shared_context =
  | For_loop
  | While_loop
  | Letop
  | Comprehension
  | Class
  | Probe

type locks

type held_locks = locks * Longident.t * Location.t
(** Sometimes we get the locks for something, but either want to walk them later, or
walk them for something else. The [Longident.t] and [Location.t] are only for error
messages, and point to the variable for which we actually want to walk the locks. *)

val locks_empty : locks

val locks_is_empty : locks -> bool

(** Items whose accesses are affected by locks *)
type lock_item =
  | Value
  | Module
  | Class

type structure_components_reason =
  | Project
  | Open

type lookup_error =
  | Unbound_value of Longident.t * unbound_value_hint
  | Unbound_type of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t * record_form_packed * label_usage
  | Unbound_module of Longident.t
  | Unbound_class of Longident.t
  | Unbound_modtype of Longident.t
  | Unbound_cltype of Longident.t
  | Unbound_settable_variable of string
  | Not_a_settable_variable of string
  | Masked_instance_variable of Longident.t
  | Masked_self_variable of Longident.t
  | Masked_ancestor_variable of Longident.t
  | Structure_used_as_functor of Longident.t
  | Abstract_used_as_functor of Longident.t * Path.t
  | Functor_used_as_structure of Longident.t * structure_components_reason
  | Abstract_used_as_structure of Longident.t * Path.t * structure_components_reason
  | Generative_used_as_applicative of Longident.t
  | Illegal_reference_to_recursive_module of
      { container : string option; unbound : string }
  | Illegal_reference_to_recursive_class_type of
      { container : string option;
        unbound : string;
        unbound_class_type : Longident.t;
        container_class_type : string
      }
  | Cannot_scrape_alias of Longident.t * Path.t
  | Local_value_escaping of lock_item * Longident.t * escaping_context
  | Once_value_used_in of lock_item * Longident.t * shared_context
  | Value_used_in_closure of lock_item * Longident.t * Mode.Value.Comonadic.error * closure_context
  | Local_value_used_in_exclave of lock_item * Longident.t
  | Non_value_used_in_object of Longident.t * type_expr * Jkind.Violation.t
  | No_unboxed_version of Longident.t * type_declaration
  | Error_from_persistent_env of Persistent_env.error
  | Mutable_value_used_in_closure of
      [`Escape of escaping_context | `Shared of shared_context | `Closure]


val lookup_error: Location.t -> t -> lookup_error -> 'a

(* The [lookup_foo] functions will emit proper error messages (by
   raising [Error]) if the identifier cannot be found, whereas the
   [find_foo_by_name] functions will raise [Not_found] instead.

   The [~use] parameters of the [lookup_foo] functions control
   whether this lookup should be counted as a use for usage
   warnings and alerts.

   [Longident.t]s in the program source should be looked up using
   [lookup_foo ~use:true] exactly one time -- otherwise warnings may be
   emitted the wrong number of times. *)

type actual_mode = {
  mode : Mode.Value.l;
  context : shared_context option
  (** Explains why [mode] is high. *)
}

(** Takes the [mode] and [ty] of a value at definition site, walks through the list of
    locks and constrains [mode] and [ty]. Return the access mode of the value allowed by
    the locks. [ty] is optional as the function works on modules and classes as well, for
    which [ty] should be [None]. *)
val walk_locks : env:t -> item:lock_item -> Mode.Value.l -> type_expr option ->
  held_locks -> actual_mode

val lookup_value:
  ?use:bool -> loc:Location.t -> Longident.t -> t ->
  Path.t * value_description * Mode.Value.l * locks
val lookup_type:
  ?use:bool -> loc:Location.t -> Longident.t -> t ->
  Path.t * type_declaration
val lookup_module:
  ?use:bool -> loc:Location.t -> Longident.t -> t ->
  Path.t * module_declaration * locks
val lookup_modtype:
  ?use:bool -> loc:Location.t -> Longident.t -> t ->
  Path.t * modtype_declaration
val lookup_class:
  ?use:bool -> loc:Location.t -> Longident.t -> t ->
  Path.t * class_declaration * Mode.Value.l
val lookup_cltype:
  ?use:bool -> loc:Location.t -> Longident.t -> t ->
  Path.t * class_type_declaration

(* When locks are returned instead of walked for modules, the mode remains as
  defined (always legacy), and thus not returned. *)
val lookup_module_path:
  ?use:bool -> loc:Location.t -> load:bool -> Longident.t -> t ->
    Path.t * locks
val lookup_modtype_path:
  ?use:bool -> loc:Location.t -> Longident.t -> t -> Path.t
val lookup_module_instance_path:
  ?use:bool -> loc:Location.t -> load:bool -> Global_module.Name.t -> t ->
    Path.t * locks

val lookup_constructor:
  ?use:bool -> loc:Location.t -> constructor_usage -> Longident.t -> t ->
  constructor_description
val lookup_all_constructors:
  ?use:bool -> loc:Location.t -> constructor_usage -> Longident.t -> t ->
  ((constructor_description * (unit -> unit)) list,
   Location.t * t * lookup_error) result
val lookup_all_constructors_from_type:
  ?use:bool -> loc:Location.t -> constructor_usage -> Path.t -> t ->
  (constructor_description * (unit -> unit)) list

val lookup_label:
  ?use:bool -> record_form:'rcd record_form -> loc:Location.t -> label_usage -> Longident.t -> t ->
  'rcd gen_label_description
val lookup_all_labels:
  ?use:bool -> record_form:'rcd record_form -> loc:Location.t -> label_usage -> Longident.t -> t ->
  (('rcd gen_label_description * (unit -> unit)) list,
   Location.t * t * lookup_error) result
val lookup_all_labels_from_type:
  ?use:bool -> record_form:'rcd record_form -> loc:Location.t -> label_usage -> Path.t -> t ->
  ('rcd gen_label_description * (unit -> unit)) list

type settable_variable =
  | Instance_variable of Path.t * Asttypes.mutable_flag * string * type_expr
  | Mutable_variable of Ident.t * Mode.Value.r * type_expr * Jkind.Sort.t

val lookup_settable_variable:
  ?use:bool -> loc:Location.t -> string -> t -> settable_variable

val find_value_by_name:
  Longident.t -> t -> Path.t * value_description
val find_type_by_name:
  Longident.t -> t -> Path.t * type_declaration
val find_module_by_name:
  Longident.t -> t -> Path.t * module_declaration
val find_modtype_by_name:
  Longident.t -> t -> Path.t * modtype_declaration
val find_class_by_name:
  Longident.t -> t -> Path.t * class_declaration
val find_cltype_by_name:
  Longident.t -> t -> Path.t * class_type_declaration

val find_constructor_by_name:
  Longident.t -> t -> constructor_description
val find_label_by_name:
  'rep record_form -> Longident.t -> t -> 'rep gen_label_description

(** The [find_*_index] functions computes a "namespaced" De Bruijn index
    of an identifier in a given environment. In other words, it returns how many
    times an identifier has been shadowed by a more recent identifiers with the
    same name in a given environment.
    Those functions return [None] when the identifier is not bound in the
    environment. This behavior is there to facilitate the detection of
    inconsistent printing environment, but should disappear in the long term.
*)
val find_value_index:   Ident.t -> t -> int option
val find_type_index:    Ident.t -> t -> int option
val find_module_index:  Ident.t -> t -> int option
val find_modtype_index: Ident.t -> t -> int option
val find_class_index:   Ident.t -> t -> int option
val find_cltype_index:  Ident.t -> t -> int option

(* Check if a name is bound *)

val bound_value: string -> t -> bool
val bound_module: string -> t -> bool
val bound_type: string -> t -> bool
val bound_modtype: string -> t -> bool
val bound_class: string -> t -> bool
val bound_cltype: string -> t -> bool

val make_copy_of_types: t -> (t -> t)

(* Resolution of globals *)

(* [global_of_instance_compilation_unit cu] checks that a compilation unit is a
   complete instantiation - that is, that all of its parameters are filled by
   arguments, and all of those arguments' parameters are filled, and so on -
   and converts it into a global. *)
val global_of_instance_compilation_unit : Compilation_unit.t -> Global_module.t

(* Insertion by identifier *)

val add_value_lazy:
    ?check:(string -> Warnings.t) -> mode:(Mode.allowed * 'r) Mode.Value.t ->
    Ident.t -> Subst.Lazy.value_description -> t -> t
val add_value:
    ?check:(string -> Warnings.t) -> mode:(Mode.allowed * 'r) Mode.Value.t ->
    Ident.t -> Types.value_description -> t -> t
val add_type:
    check:bool -> ?shape:Shape.t -> Ident.t -> type_declaration -> t -> t
val add_extension:
  check:bool -> ?shape:Shape.t -> rebind:bool -> Ident.t ->
  extension_constructor -> t -> t
val add_module: ?arg:bool -> ?shape:Shape.t ->
  Ident.t -> module_presence -> module_type -> t -> t
val add_module_lazy: update_summary:bool ->
  Ident.t -> module_presence -> Subst.Lazy.module_type -> t -> t
val add_module_declaration: ?arg:bool -> ?shape:Shape.t -> check:bool ->
  Ident.t -> module_presence -> module_declaration -> ?locks:locks -> t -> t
val add_module_declaration_lazy: ?arg:bool -> update_summary:bool ->
  Ident.t -> module_presence -> Subst.Lazy.module_declaration -> t -> t
val add_modtype: Ident.t -> modtype_declaration -> t -> t
val add_modtype_lazy: update_summary:bool ->
   Ident.t -> Subst.Lazy.modtype_declaration -> t -> t
val add_class: Ident.t -> class_declaration -> t -> t
val add_cltype: Ident.t -> class_type_declaration -> t -> t
val add_local_constraint: Path.t -> type_declaration -> t -> t

(* Insertion of persistent signatures *)

(* [add_persistent_structure id env] is an environment such that
   module [id] points to the persistent structure contained in the
   external compilation unit with the same name.

   The compilation unit itself is looked up in the load path when the
   contents of the module is accessed. *)
val add_persistent_structure : Ident.t -> t -> t

 (* Returns the set of persistent structures found in the given
   directory. *)
val persistent_structures_of_dir : Load_path.Dir.t -> Misc.Stdlib.String.Set.t

(* [filter_non_loaded_persistent f env] removes all the persistent
   structures that are not yet loaded and for which [f] returns
   [false]. *)
val filter_non_loaded_persistent : (Ident.t -> bool) -> t -> t

(* Insertion of all fields of a signature. *)

val add_signature: signature -> t -> t
val add_signature_lazy: Subst.Lazy.signature_item list -> t -> t

(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. Returns None if the path refers to a functor,
   not a structure. *)
val open_signature:
    used_slot:bool ref ->
    loc:Location.t -> toplevel:bool ->
    Asttypes.override_flag -> Longident.t Location.loc ->
    t -> Path.t * t

(* CR zqian: locks beyond the open are not tracked. Fix that. *)
val open_signature_by_path: Path.t -> t -> t

val open_pers_signature: string -> t -> Path.t * t

val remove_last_open: Path.t -> t -> t option

(* Insertion by name *)

val enter_value:
    ?check:(string -> Warnings.t) -> mode:(Mode.allowed * 'r) Mode.Value.t ->
    string -> value_description -> t -> Ident.t * t
val enter_type: scope:int -> string -> type_declaration -> t -> Ident.t * t
val enter_extension:
  scope:int -> rebind:bool -> string ->
  extension_constructor -> t -> Ident.t * t
val enter_module:
  scope:int -> ?arg:bool -> string -> module_presence ->
  module_type -> t -> Ident.t * t
val enter_module_declaration:
  scope:int -> ?arg:bool -> ?shape:Shape.t -> string -> module_presence ->
  module_declaration -> ?locks:locks -> t -> Ident.t * t
val enter_modtype:
  scope:int -> string -> modtype_declaration -> t -> Ident.t * t
val enter_class: scope:int -> string -> class_declaration -> t -> Ident.t * t
val enter_cltype:
  scope:int -> string -> class_type_declaration -> t -> Ident.t * t

(* Same as [add_signature] but refreshes (new stamp) and rescopes bound idents
   in the process. *)
val enter_signature: ?mod_shape:Shape.t -> scope:int -> signature -> t ->
  signature * t

(* Same as [enter_signature] but also extends the shape map ([parent_shape])
   with all the the items from the signature, their shape being a projection
   from the given shape. *)
val enter_signature_and_shape: scope:int -> parent_shape:Shape.Map.t ->
  Shape.t -> signature -> t -> signature * Shape.Map.t * t

val enter_unbound_value : string -> value_unbound_reason -> t -> t

val enter_unbound_module : string -> module_unbound_reason -> t -> t

(* Lock the environment *)

val add_escape_lock : escaping_context -> t -> t

(** `once` variables beyond the share lock cannot be accessed. Moreover,
    `unique` variables beyond the lock can still be accessed, but will be
    relaxed to `shared` *)
val add_share_lock : shared_context -> t -> t
val add_closure_lock : closure_context
  -> ('l * Mode.allowed) Mode.Value.Comonadic.t -> t -> t
val add_region_lock : t -> t
val add_exclave_lock : t -> t
val add_unboxed_lock : t -> t

(* Initialize the cache of in-core module interfaces. *)
val reset_cache: preserve_persistent_env:bool -> unit

(* To be called before each toplevel phrase. *)
val reset_cache_toplevel: unit -> unit

(* Remember the name of the current compilation unit. *)
val set_unit_name: Unit_info.t option -> unit
val get_unit_name: unit -> Unit_info.t option

(* Read, save a signature to/from a file. *)
val read_signature:
  Global_module.Name.t -> Unit_info.Artifact.t
  -> signature
        (* Arguments: module name, file name, [add_binding] flag.
           Results: signature. If [add_binding] is true, creates an entry for
           the module in the environment. *)
val save_signature:
  alerts:alerts -> Types.signature -> Compilation_unit.Name.t -> Cmi_format.kind
  -> Unit_info.Artifact.t -> Cmi_format.cmi_infos_lazy
        (* Arguments: signature, module name, module kind, file name. *)
val save_signature_with_imports:
  alerts:alerts -> signature -> Compilation_unit.Name.t -> Cmi_format.kind
  -> Unit_info.Artifact.t -> Import_info.t array -> Cmi_format.cmi_infos_lazy
        (* Arguments: signature, module name, module kind,
           file name, imported units with their CRCs. *)

(* Register a module as a parameter to this unit. *)
val register_parameter: Global_module.Parameter_name.t -> unit

(* Return the CRC of the interface of the given compilation unit *)
val crc_of_unit: Compilation_unit.Name.t -> Digest.t

(* Return the set of compilation units imported, with their CRC *)
val imports: unit -> Import_info.t list

(* may raise Persistent_env.Consistbl.Inconsistency *)
val import_crcs: source:string -> Import_info.t array -> unit

(* Return the set of imports represented as runtime parameters (see
   [Persistent_env.runtime_parameter_bindings] for details) *)
val runtime_parameter_bindings: unit -> (Global_module.t * Ident.t) list

(* Return whether an ident appears in [runtime_parameter_bindings] *)
val is_bound_to_runtime_parameter: Ident.t -> bool

(* Return the list of parameters specified for the current unit, in
   alphabetical order *)
val parameters: unit -> Global_module.Parameter_name.t list

(* [is_imported_opaque md] returns true if [md] is an opaque imported module *)
val is_imported_opaque: Compilation_unit.Name.t -> bool

(* [register_import_as_opaque md] registers [md] as an opaque imported module *)
val register_import_as_opaque: Compilation_unit.Name.t -> unit

(* [is_parameter_unit md] returns true if [md] was compiled with
   -as-parameter *)
val is_parameter_unit: Global_module.Name.t -> bool

(* [implemented_parameter md] is the argument given to -as-argument-for when
   [md] was compiled *)
val implemented_parameter:
  Global_module.Name.t -> Global_module.Parameter_name.t option

(* [is_imported_parameter md] is true if [md] has been imported and is a
   parameter to this module *)
val is_imported_parameter: Global_module.Name.t -> bool

(* Summaries -- compact representation of an environment, to be
   exported in debugging information. *)

val summary: t -> summary

(* Return an equivalent environment where all fields have been reset,
   except the summary. The initial environment can be rebuilt from the
   summary, using Envaux.env_of_only_summary. *)

val keep_only_summary : t -> t
val env_of_only_summary : (summary -> Subst.t -> t) -> t -> t

(* Error report *)

type error =
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string
  | Lookup_error of Location.t * t * lookup_error
  | Incomplete_instantiation of { unset_param : Global_module.Parameter_name.t; }

exception Error of error

open Format

val report_error: formatter -> error -> unit

val report_lookup_error: Location.t -> t -> formatter -> lookup_error -> unit

val in_signature: bool -> t -> t

val is_in_signature: t -> bool

val set_value_used_callback:
    Subst.Lazy.value_description -> (unit -> unit) -> unit
val set_type_used_callback:
    type_declaration -> ((unit -> unit) -> unit) -> unit

(* Forward declaration to break mutual recursion with Includemod. *)
val check_functor_application:
  (errors:bool -> loc:Location.t ->
   lid_whole_app:Longident.t ->
   f0_path:Path.t -> args:(Path.t * Types.module_type) list ->
   arg_path:Path.t -> arg_mty:Types.module_type ->
   param_mty:Types.module_type ->
   t -> unit) ref
(* Forward declaration to break mutual recursion with Typemod. *)
val check_well_formed_module:
    (t -> Location.t -> string -> module_type -> unit) ref
(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref
(* Forward declaration to break mutual recursion with Mtype. *)
val scrape_alias:
    (t -> Subst.Lazy.module_type -> Subst.Lazy.module_type) ref
(* Forward declaration to break mutual recursion with Ctype. *)
val same_constr: (t -> type_expr -> type_expr -> bool) ref
(* Forward declaration to break mutual recursion with Ctype. *)
val constrain_type_jkind:
  (t -> type_expr -> jkind_r -> (unit, Jkind.Violation.t) result) ref
(* Forward declaration to break mutual recursion with Printtyp. *)
val print_longident: (Format.formatter -> Longident.t -> unit) ref
(* Forward declaration to break mutual recursion with Printtyp. *)
val print_path: (Format.formatter -> Path.t -> unit) ref
(* Forward declaration to break mutual recursion with Printtyp. *)
val print_type_expr: (Format.formatter -> Types.type_expr -> unit) ref


(** Folds *)

val fold_values:
  (string -> Path.t -> Subst.Lazy.value_description -> Mode.Value.l -> 'a -> 'a)
  -> Longident.t option -> t -> 'a -> 'a
val fold_types:
  (string -> Path.t -> type_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_constructors:
  (constructor_description -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_labels:
  'rcd record_form -> ('rcd gen_label_description -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a

(** Persistent structures are only traversed if they are already loaded. *)
val fold_modules:
  (string -> Path.t -> module_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a

val fold_modtypes:
  (string -> Path.t -> modtype_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_classes:
  (string -> Path.t -> class_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_cltypes:
  (string -> Path.t -> class_type_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a


(** Utilities *)
val check_value_name: string -> Location.t -> unit

val print_address : Format.formatter -> address -> unit

type address_head =
  | AHunit of Compilation_unit.t
  | AHlocal of Ident.t

val address_head : address -> address_head

val sharedness_hint : Format.formatter -> shared_context -> unit
