(*							-*-tuareg-*- *)
open Core.Std

(*
 * Reexport some types from the compiler libs to enable sexp on them.
 *)

module Location = struct
    type t =
      Location.t = {
          loc_start: Source_code_position.t;
          loc_end: Source_code_position.t;
          loc_ghost: bool;
        } with sexp
  end

type lexer_error =
  Lexer.error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
#if ocaml_version >= (4, 02)
  | Unterminated_string_in_comment of Location.t * Location.t
#else
  | Unterminated_string_in_comment of Location.t
#endif
  | Keyword_as_label of string
  | Literal_overflow of string
  with sexp

type syntaxerr_error =
  Syntaxerr.error =
  | Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
#if ocaml_version >= (4, 02)
  | Not_expecting of Location.t * string
#endif
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
#if ocaml_version >= (4, 02)
  | Ill_formed_ast of Location.t * string
#endif
  with sexp

module Ident = struct
    type t = Ident.t =
               { stamp: int; name: string; mutable flags: int } with sexp
end

module Longident = struct
    type t =
      Longident.t =
        Lident of string
      | Ldot of t * string
      | Lapply of t * t
      with sexp
  end

module Path = struct
    type t =
      Path.t =
      | Pident of Ident.t
      | Pdot of t * string * int
      | Papply of t * t
      with sexp
  end

module Asttypes = struct
    type constant =
      Asttypes.constant =
        Const_int of int
      | Const_char of char
#if ocaml_version >= (4, 02)
      | Const_string of string * string option
#else
      | Const_string of string
#endif
      | Const_float of string
      | Const_int32 of int32
      | Const_int64 of int64
      | Const_nativeint of nativeint
      with sexp

    type rec_flag =
      Asttypes.rec_flag =
      | Nonrecursive
      | Recursive
#if ocaml_version < (4, 02)
      | Default
#endif
      with sexp

    type direction_flag = Asttypes.direction_flag = Upto | Downto with sexp

    type private_flag = Asttypes.private_flag = Private | Public with sexp

    type mutable_flag = Asttypes.mutable_flag = Immutable | Mutable with sexp

    type virtual_flag = Asttypes.virtual_flag = Virtual | Concrete with sexp

    type override_flag = Asttypes.override_flag = Override | Fresh with sexp

    type closed_flag = Asttypes.closed_flag = Closed | Open with sexp

    type label = string with sexp

    type 'a loc = 'a Asttypes.loc = {
          txt : 'a;
          loc : Location.t;
        } with sexp

#if ocaml_version >= (4, 02)
    type variance =
      Asttypes.variance =
      | Covariant
      | Contravariant
      | Invariant
      with sexp
#endif
  end

module Parsetree = struct
    open Asttypes

#if ocaml_version >= (4, 02)
    type attribute = string loc * payload
     and extension = string loc * payload
     and attributes = attribute list
     and payload = Parsetree.payload =
       | PStr of structure
       | PTyp of core_type
       | PPat of pattern * expression option
     and core_type =
#else
     type core_type =
#endif
       Parsetree.core_type =
         {
           ptyp_desc: core_type_desc;
           ptyp_loc: Location.t;
#if ocaml_version >= (4, 02)
           ptyp_attributes: attributes;
#endif
         }
     and core_type_desc =
       Parsetree.core_type_desc =
       | Ptyp_any
       | Ptyp_var of string
       | Ptyp_arrow of label * core_type * core_type
       | Ptyp_tuple of core_type list
       | Ptyp_constr of Longident.t loc * core_type list
#if ocaml_version >= (4, 02)
       | Ptyp_object of (string * attributes * core_type) list * closed_flag
       | Ptyp_class of Longident.t loc * core_type list
#else
       | Ptyp_object of core_field_type list
       | Ptyp_class of Longident.t loc * core_type list * label list
#endif
       | Ptyp_alias of core_type * string
#if ocaml_version >= (4, 02)
       | Ptyp_variant of row_field list * closed_flag * label list option
#else
       | Ptyp_variant of row_field list * bool * label list option
#endif
       | Ptyp_poly of string list * core_type
       | Ptyp_package of package_type
#if ocaml_version >= (4, 02)
       | Ptyp_extension of extension
#endif
     and package_type =
       Longident.t loc * (Longident.t loc * core_type) list
#if ocaml_version < (4, 02)
     and core_field_type =
       Parsetree.core_field_type =
       { pfield_desc: core_field_desc;
         pfield_loc: Location.t }
     and core_field_desc =
       Parsetree.core_field_desc =
       | Pfield of string * core_type
       | Pfield_var
#endif
     and row_field =
       Parsetree.row_field =
#if ocaml_version >= (4, 02)
       | Rtag of label * attributes * bool * core_type list
#else
       | Rtag of label * bool * core_type list
#endif
       | Rinherit of core_type
     and pattern =
       Parsetree.pattern =
         {
           ppat_desc: pattern_desc;
           ppat_loc: Location.t;
#if ocaml_version >= (4, 02)
           ppat_attributes: attributes; (* ... [@id1] [@id2] *)
#endif
         }
     and pattern_desc =
       Parsetree.pattern_desc =
       | Ppat_any
       | Ppat_var of string loc
       | Ppat_alias of pattern * string loc
       | Ppat_constant of constant
#if ocaml_version >= (4, 02)
       | Ppat_interval of constant * constant
#endif
       | Ppat_tuple of pattern list
#if ocaml_version >= (4, 02)
       | Ppat_construct of Longident.t loc * pattern option
#else
       | Ppat_construct of Longident.t loc * pattern option * bool
#endif
       | Ppat_variant of label * pattern option
       | Ppat_record of (Longident.t loc * pattern) list * closed_flag
       | Ppat_array of pattern list
       | Ppat_or of pattern * pattern
       | Ppat_constraint of pattern * core_type
       | Ppat_type of Longident.t loc
       | Ppat_lazy of pattern
       | Ppat_unpack of string loc
#if ocaml_version >= (4, 02)
       | Ppat_exception of pattern
       | Ppat_extension of extension
#endif
     (* Value expressions *)
     and expression =
       Parsetree.expression =
         {
           pexp_desc: expression_desc;
           pexp_loc: Location.t;
#if ocaml_version >= (4, 02)
           pexp_attributes: attributes; (* ... [@id1] [@id2] *)
#endif
         }
     and expression_desc =
       Parsetree.expression_desc =
       | Pexp_ident of Longident.t loc
       | Pexp_constant of constant
#if ocaml_version >= (4, 02)
       | Pexp_let of rec_flag * value_binding list * expression
       | Pexp_function of case list
       | Pexp_fun of label * expression option * pattern * expression
#else
       | Pexp_let of rec_flag * (pattern * expression) list * expression
       | Pexp_function of label * expression option * (pattern * expression) list
#endif
       | Pexp_apply of expression * (label * expression) list
#if ocaml_version >= (4, 02)
       | Pexp_match of expression * case list
       | Pexp_try of expression * case list
#else
       | Pexp_match of expression * (pattern * expression) list
       | Pexp_try of expression * (pattern * expression) list
#endif
       | Pexp_tuple of expression list
#if ocaml_version >= (4, 02)
       | Pexp_construct of Longident.t loc * expression option
#else
       | Pexp_construct of Longident.t loc * expression option * bool
#endif
       | Pexp_variant of label * expression option
       | Pexp_record of (Longident.t loc * expression) list * expression option
       | Pexp_field of expression * Longident.t loc
       | Pexp_setfield of expression * Longident.t loc * expression
       | Pexp_array of expression list
       | Pexp_ifthenelse of expression * expression * expression option
       | Pexp_sequence of expression * expression
       | Pexp_while of expression * expression
#if ocaml_version >= (4, 02)
       | Pexp_for of
           pattern *  expression * expression * direction_flag * expression
       | Pexp_constraint of expression * core_type
       | Pexp_coerce of expression * core_type option * core_type
#else
       | Pexp_for of
           string loc * expression * expression * direction_flag * expression
       | Pexp_constraint of expression * core_type option * core_type option
       | Pexp_when of expression * expression
#endif
       | Pexp_send of expression * string
       | Pexp_new of Longident.t loc
       | Pexp_setinstvar of string loc * expression
       | Pexp_override of (string loc * expression) list
       | Pexp_letmodule of string loc * module_expr * expression
       | Pexp_assert of expression
#if ocaml_version < (4, 02)
       | Pexp_assertfalse
#endif
       | Pexp_lazy of expression
       | Pexp_poly of expression * core_type option
       | Pexp_object of class_structure
       | Pexp_newtype of string * expression
       | Pexp_pack of module_expr
       | Pexp_open of override_flag * Longident.t loc * expression
#if ocaml_version >= (4, 02)
       | Pexp_extension of extension
     and case =
       Parsetree.case =
         {
           pc_lhs: pattern;
           pc_guard: expression option;
           pc_rhs: expression;
         }
#endif
     (* Value descriptions *)
     and value_description =
       Parsetree.value_description =
         {
#if ocaml_version >= (4, 02)
           pval_name: string loc;
#endif
           pval_type: core_type;
           pval_prim: string list;
#if ocaml_version >= (4, 02)
           pval_attributes: attributes;
#endif
           pval_loc: Location.t;
         }
     (* Type declarations *)
     and type_declaration =
       Parsetree.type_declaration =
         {
#if ocaml_version >= (4, 02)
           ptype_name: string loc;
           ptype_params: (core_type * variance) list;
#else
           ptype_params: string loc option list;
#endif
           ptype_cstrs: (core_type * core_type * Location.t) list;
           ptype_kind: type_kind;
           ptype_private: private_flag;
           ptype_manifest: core_type option;
#if ocaml_version >= (4, 02)
           ptype_attributes: attributes;
#else
           ptype_variance: (bool * bool) list;
#endif
           ptype_loc: Location.t;
         }
#if ocaml_version >= (4, 02)
     and type_kind =
       Parsetree.type_kind =
       | Ptype_abstract
       | Ptype_variant of constructor_declaration list
       | Ptype_record of label_declaration list
       | Ptype_open
     and label_declaration =
       Parsetree.label_declaration =
         {
           pld_name: string loc;
           pld_mutable: mutable_flag;
           pld_type: core_type;
           pld_loc: Location.t;
           pld_attributes: attributes; (* l [@id1] [@id2] : T *)
         }
     and constructor_declaration =
       Parsetree.constructor_declaration =
         {
           pcd_name: string loc;
           pcd_args: core_type list;
           pcd_res: core_type option;
           pcd_loc: Location.t;
           pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
         }
     and type_extension =
       Parsetree.type_extension =
       {
         ptyext_path: Longident.t loc;
         ptyext_params: (core_type * variance) list;
         ptyext_constructors: extension_constructor list;
         ptyext_private: private_flag;
         ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
       }
     and extension_constructor =
       Parsetree.extension_constructor =
         {
           pext_name: string loc;
           pext_kind : extension_constructor_kind;
           pext_loc : Location.t;
           pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
         }
     and extension_constructor_kind =
       Parsetree.extension_constructor_kind =
         Pext_decl of core_type list * core_type option
       | Pext_rebind of Longident.t loc
#else
     and type_kind =
       Parsetree.type_kind =
         Ptype_abstract
       | Ptype_variant of
           (string loc * core_type list * core_type option * Location.t) list
       | Ptype_record of
           (string loc * mutable_flag * core_type * Location.t) list
     and exception_declaration = core_type list
#endif
     (** {2 Class language} *)
     and class_type =
       Parsetree.class_type =
         {
           pcty_desc: class_type_desc;
           pcty_loc: Location.t;
#if ocaml_version >= (4, 02)
           pcty_attributes: attributes; (* ... [@id1] [@id2] *)
#endif
         }
     and class_type_desc =
       Parsetree.class_type_desc =
       | Pcty_constr of Longident.t loc * core_type list
       | Pcty_signature of class_signature
#if ocaml_version >= (4, 02)
       | Pcty_arrow of label * core_type * class_type
       | Pcty_extension of extension
#else
       | Pcty_fun of label * core_type * class_type
#endif
     and class_signature =
       Parsetree.class_signature =
         {
           pcsig_self: core_type;
           pcsig_fields: class_type_field list;
#if ocaml_version < (4, 02)
           pcsig_loc: Location.t;
#endif
         }
     and class_type_field =
       Parsetree.class_type_field =
         {
           pctf_desc: class_type_field_desc;
           pctf_loc: Location.t;
#if ocaml_version >= (4, 02)
           pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
#endif
         }
     and class_type_field_desc =
       Parsetree.class_type_field_desc =
#if ocaml_version >= (4, 02)
       | Pctf_inherit of class_type
       | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
       | Pctf_method  of (string * private_flag * virtual_flag * core_type)
       | Pctf_constraint  of (core_type * core_type)
       | Pctf_attribute of attribute
       | Pctf_extension of extension
#else
       | Pctf_inher of class_type
       | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
       | Pctf_virt  of (string * private_flag * core_type)
       | Pctf_meth  of (string * private_flag * core_type)
       | Pctf_cstr  of (core_type * core_type)
#endif
     and 'a class_infos =
       'a Parsetree.class_infos =
         {
           pci_virt: virtual_flag;
#if ocaml_version >= (4, 02)
           pci_params: (core_type * variance) list;
#else
           pci_params: string loc list * Location.t;
#endif
           pci_name: string loc;
           pci_expr: 'a;
#if ocaml_version < (4, 02)
           pci_variance: (bool * bool) list;
#endif
           pci_loc: Location.t;
#if ocaml_version >= (4, 02)
           pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
#endif
         }
     and class_description = class_type class_infos
     and class_type_declaration = class_type class_infos
     and class_expr =
       Parsetree.class_expr =
         {
           pcl_desc: class_expr_desc;
           pcl_loc: Location.t;
#if ocaml_version >= (4, 02)
           pcl_attributes: attributes; (* ... [@id1] [@id2] *)
#endif
         }
     and class_expr_desc =
       Parsetree.class_expr_desc =
       | Pcl_constr of Longident.t loc * core_type list
       | Pcl_structure of class_structure
       | Pcl_fun of label * expression option * pattern * class_expr
       | Pcl_apply of class_expr * (label * expression) list
#if ocaml_version >= (4, 02)
       | Pcl_let of rec_flag * value_binding list * class_expr
#else
       | Pcl_let of rec_flag * (pattern * expression) list * class_expr
#endif
       | Pcl_constraint of class_expr * class_type
#if ocaml_version >= (4, 02)
       | Pcl_extension of extension
#endif
     and class_structure =
       Parsetree.class_structure =
         {
#if ocaml_version >= (4, 02)
           pcstr_self: pattern;
#else
           pcstr_pat: pattern;
#endif
           pcstr_fields: class_field list;
         }
     and class_field =
       Parsetree.class_field =
       {
         pcf_desc: class_field_desc;
         pcf_loc: Location.t;
#if ocaml_version >= (4, 02)
         pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
#endif
       }
#if ocaml_version >= (4, 02)
     and class_field_desc =
       Parsetree.class_field_desc =
       | Pcf_inherit of override_flag * class_expr * string option
       | Pcf_val of (string loc * mutable_flag * class_field_kind)
       | Pcf_method of (string loc * private_flag * class_field_kind)
       | Pcf_constraint of (core_type * core_type)
       | Pcf_initializer of expression
       | Pcf_attribute of attribute
       | Pcf_extension of extension
     and class_field_kind =
       Parsetree.class_field_kind =
       | Cfk_virtual of core_type
       | Cfk_concrete of override_flag * expression
#else
     and class_field_desc =
       Parsetree.class_field_desc =
         Pcf_inher of override_flag * class_expr * string option
       | Pcf_valvirt of (string loc * mutable_flag * core_type)
       | Pcf_val of (string loc * mutable_flag * override_flag * expression)
       | Pcf_virt of (string loc * private_flag * core_type)
       | Pcf_meth of (string loc * private_flag * override_flag * expression)
       | Pcf_constr of (core_type * core_type)
       | Pcf_init of expression
#endif
     and class_declaration = class_expr class_infos
     (** {2 Module language} *)
     and module_type =
       Parsetree.module_type =
         {
           pmty_desc: module_type_desc;
           pmty_loc: Location.t;
#if ocaml_version >= (4, 02)
           pmty_attributes: attributes; (* ... [@id1] [@id2] *)
#endif
         }
     and module_type_desc =
       Parsetree.module_type_desc =
       | Pmty_ident of Longident.t loc
       | Pmty_signature of signature
#if ocaml_version >= (4, 02)
       | Pmty_functor of string loc * module_type option * module_type
       | Pmty_with of module_type * with_constraint list
#else
       | Pmty_functor of string loc * module_type * module_type
       | Pmty_with of module_type * (Longident.t loc * with_constraint) list
#endif
       | Pmty_typeof of module_expr
#if ocaml_version >= (4, 02)
       | Pmty_extension of extension
       | Pmty_alias of Longident.t loc
#endif
     and signature = signature_item list
     and signature_item =
       Parsetree.signature_item =
         {
           psig_desc: signature_item_desc;
           psig_loc: Location.t;
         }
#if ocaml_version >= (4, 02)
     and signature_item_desc =
       Parsetree.signature_item_desc =
       | Psig_value of value_description
       | Psig_type of type_declaration list
       | Psig_typext of type_extension
       | Psig_exception of extension_constructor
       | Psig_module of module_declaration
       | Psig_recmodule of module_declaration list
       | Psig_modtype of module_type_declaration
       | Psig_open of open_description
       | Psig_include of include_description
       | Psig_class of class_description list
       | Psig_class_type of class_type_declaration list
       | Psig_attribute of attribute
       | Psig_extension of extension * attributes
     and module_declaration =
       Parsetree.module_declaration =
         {
           pmd_name: string loc;
           pmd_type: module_type;
           pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
           pmd_loc: Location.t;
         }
     and module_type_declaration =
       Parsetree.module_type_declaration =
         {
           pmtd_name: string loc;
           pmtd_type: module_type option;
           pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
           pmtd_loc: Location.t;
         }
     and open_description =
       Parsetree.open_description =
         {
           popen_lid: Longident.t loc;
           popen_override: override_flag;
           popen_loc: Location.t;
           popen_attributes: attributes;
         }
     and 'a include_infos =
       'a Parsetree.include_infos =
         {
           pincl_mod: 'a;
           pincl_loc: Location.t;
           pincl_attributes: attributes;
         }
     and include_description = module_type include_infos
     and include_declaration = module_expr include_infos
     and with_constraint =
       Parsetree.with_constraint =
       | Pwith_type of Longident.t loc * type_declaration
       | Pwith_module of Longident.t loc * Longident.t loc
       | Pwith_typesubst of type_declaration
       | Pwith_modsubst of string loc * Longident.t loc
#else
     and signature_item_desc =
       Parsetree.signature_item_desc =
         Psig_value of string loc * value_description
       | Psig_type of (string loc * type_declaration) list
       | Psig_exception of string loc * exception_declaration
       | Psig_module of string loc * module_type
       | Psig_recmodule of (string loc * module_type) list
       | Psig_modtype of string loc * modtype_declaration
       | Psig_open of override_flag * Longident.t loc
       | Psig_include of module_type
       | Psig_class of class_description list
       | Psig_class_type of class_type_declaration list
     and modtype_declaration =
       Parsetree.modtype_declaration =
         Pmodtype_abstract
       | Pmodtype_manifest of module_type
     and with_constraint =
       Parsetree.with_constraint =
         Pwith_type of type_declaration
       | Pwith_module of Longident.t loc
       | Pwith_typesubst of type_declaration
       | Pwith_modsubst of Longident.t loc
#endif
     (* Value expressions for the module language *)
     and module_expr =
       Parsetree.module_expr =
         {
           pmod_desc: module_expr_desc;
           pmod_loc: Location.t;
#if ocaml_version >= (4, 02)
           pmod_attributes: attributes; (* ... [@id1] [@id2] *)
#endif
         }
     and module_expr_desc =
       Parsetree.module_expr_desc =
       | Pmod_ident of Longident.t loc
       | Pmod_structure of structure
#if ocaml_version >= (4, 02)
       | Pmod_functor of string loc * module_type option * module_expr
#else
       | Pmod_functor of string loc * module_type * module_expr
#endif
       | Pmod_apply of module_expr * module_expr
       | Pmod_constraint of module_expr * module_type
       | Pmod_unpack of expression
#if ocaml_version >= (4, 02)
       | Pmod_extension of extension
#endif
     and structure = structure_item list
     and structure_item =
       Parsetree.structure_item =
         {
           pstr_desc: structure_item_desc;
           pstr_loc: Location.t;
         }
#if ocaml_version >= (4, 02)
     and structure_item_desc =
       Parsetree.structure_item_desc =
       | Pstr_eval of expression * attributes
       | Pstr_value of rec_flag * value_binding list
       | Pstr_primitive of value_description
       | Pstr_type of type_declaration list
       | Pstr_typext of type_extension
       | Pstr_exception of extension_constructor
       | Pstr_module of module_binding
       | Pstr_recmodule of module_binding list
       | Pstr_modtype of module_type_declaration
       | Pstr_open of open_description
       | Pstr_class of class_declaration list
       | Pstr_class_type of class_type_declaration list
       | Pstr_include of include_declaration
       | Pstr_attribute of attribute
       | Pstr_extension of extension * attributes
     and value_binding =
       Parsetree.value_binding =
         {
           pvb_pat: pattern;
           pvb_expr: expression;
           pvb_attributes: attributes;
           pvb_loc: Location.t;
         }
     and module_binding =
       Parsetree.module_binding =
       {
         pmb_name: string loc;
         pmb_expr: module_expr;
         pmb_attributes: attributes;
         pmb_loc: Location.t;
       }
#else
     and structure_item_desc =
       Parsetree.structure_item_desc =
         Pstr_eval of expression
       | Pstr_value of rec_flag * (pattern * expression) list
       | Pstr_primitive of string loc * value_description
       | Pstr_type of (string loc * type_declaration) list
       | Pstr_exception of string loc * exception_declaration
       | Pstr_exn_rebind of string loc * Longident.t loc
       | Pstr_module of string loc * module_expr
       | Pstr_recmodule of (string loc * module_type * module_expr) list
       | Pstr_modtype of string loc * module_type
       | Pstr_open of override_flag * Longident.t loc
       | Pstr_class of class_declaration list
       | Pstr_class_type of class_type_declaration list
       | Pstr_include of module_expr
#endif
     with sexp
  end

module Types = struct
    open Asttypes

    type type_expr =
      Types.type_expr =
      { mutable desc: type_desc;
        mutable level: int;
        mutable id: int }

     and type_desc =
       Types.type_desc =
         Tvar of string option
       | Tarrow of string * type_expr * type_expr * commutable
       | Ttuple of type_expr list
       | Tconstr of Path.t * type_expr list * abbrev_memo ref
       | Tobject of type_expr * (Path.t * type_expr list) option ref
       | Tfield of string * field_kind * type_expr * type_expr
       | Tnil
       | Tlink of type_expr
       | Tsubst of type_expr         (* for copying *)
       | Tvariant of row_desc
       | Tunivar of string option
       | Tpoly of type_expr * type_expr list
       | Tpackage of Path.t * Longident.t list * type_expr list

     and row_desc =
       Types.row_desc =
         { row_fields: (string * row_field) list;
           row_more: type_expr;
           row_bound: unit; (* kept for compatibility *)
           row_closed: bool;
           row_fixed: bool;
           row_name: (Path.t * type_expr list) option }

     and row_field =
       Types.row_field =
       | Rpresent of type_expr option
       | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
       | Rabsent

     and abbrev_memo =
       Types.abbrev_memo =
         Mnil
       | Mcons of private_flag * Path.t
                  * type_expr * type_expr * abbrev_memo
       | Mlink of abbrev_memo ref

     and field_kind =
       Types.field_kind =
         Fvar of field_kind option ref
       | Fpresent
       | Fabsent

     and commutable =
       Types.commutable =
         Cok
       | Cunknown
       | Clink of commutable ref
     with sexp

    type label_description =
      Types.label_description =
        { lbl_name: string;
          lbl_res: type_expr;
          lbl_arg: type_expr;
          lbl_mut: mutable_flag;
          lbl_pos: int;
          lbl_all: label_description array;
          lbl_repres: record_representation;
          lbl_private: private_flag;
#if ocaml_version >= (4, 02)
          lbl_loc: Location.t;
          lbl_attributes: Parsetree.attributes;
#endif
        }
     and record_representation =
       Types.record_representation =
         Record_regular
       | Record_float
       with sexp

    module Variance = struct
        type t = Types.Variance.t

        (* FIXME: Variance.t is abstract and does not offer enough
           functions to deconstruct and reconstruct it.  We use the
           fact that we know its internal representation is an [int]. *)
        let t_of_sexp : Sexplib.Sexp.t -> t = fun sexp ->
          (Obj.magic (int_of_sexp sexp) : t)
        let sexp_of_t : t -> Sexplib.Sexp.t = fun t ->
          sexp_of_int(Obj.magic t)
        ;;
      end

    type type_declaration =
      Types.type_declaration =
      { type_params: type_expr list;
        type_arity: int;
        type_kind: type_kind;
        type_private: private_flag;
        type_manifest: type_expr option;
        type_variance: Variance.t list;
        (* covariant, contravariant, weakly contravariant, injective *)
        type_newtype_level: (int * int) option;
        (* definition level * expansion level *)
        type_loc: Location.t;
#if ocaml_version >= (4, 02)
        type_attributes: Parsetree.attributes;
#endif
      }

#if ocaml_version >= (4, 02)
      and type_kind =
        Types.type_kind =
          Type_abstract
        | Type_record of label_declaration list  * record_representation
        | Type_variant of constructor_declaration list
        | Type_open
      and label_declaration =
        Types.label_declaration =
        {
          ld_id: Ident.t;
          ld_mutable: mutable_flag;
          ld_type: type_expr;
          ld_loc: Location.t;
          ld_attributes: Parsetree.attributes;
        }
      and constructor_declaration =
        Types.constructor_declaration =
        {
          cd_id: Ident.t;
          cd_args: type_expr list;
          cd_res: type_expr option;
          cd_loc: Location.t;
          cd_attributes: Parsetree.attributes;
        }
      and extension_constructor =
        Types.extension_constructor =
          {
            ext_type_path: Path.t;
            ext_type_params: type_expr list;
            ext_args: type_expr list;
            ext_ret_type: type_expr option;
            ext_private: private_flag;
            ext_loc: Location.t;
            ext_attributes: Parsetree.attributes;
          }
#else
      and type_kind =
        Types.type_kind =
          Type_abstract
        | Type_record of
            (Ident.t * mutable_flag * type_expr) list * record_representation
        | Type_variant of (Ident.t * type_expr list * type_expr option) list
#endif
      and type_transparence =
        Types.type_transparence =
          Type_public      (* unrestricted expansion *)
        | Type_new         (* "new" type *)
        | Type_private     (* private type *)
      with sexp
  end

module Includecore = struct
    type type_mismatch =
      Includecore.type_mismatch =
        Arity
      | Privacy
      | Kind
      | Constraint
      | Manifest
      | Variance
      | Field_type of Ident.t
      | Field_mutable of Ident.t
      | Field_arity of Ident.t
      | Field_names of int * Ident.t * Ident.t
      | Field_missing of bool * Ident.t
      | Record_representation of bool
      with sexp
  end

module Env = struct
    type t = Env.t
    (* The compiler type is abstract and may contain closures.  We
       replace environments by empty ones. *)

    type empty = Empty with sexp
    let empty_sexp = sexp_of_empty Empty

    let t_of_sexp _ = Env.empty
    let sexp_of_t _ = empty_sexp
  end

module Typedecl = struct
    open Types

    type error =
      Typedecl.error =
      | Repeated_parameter
      | Duplicate_constructor of string
      | Too_many_constructors
      | Duplicate_label of string
      | Recursive_abbrev of string
#if ocaml_version >= (4, 02)
      | Cycle_in_def of string * type_expr
#endif
      | Definition_mismatch of type_expr * Includecore.type_mismatch list
      | Constraint_failed of type_expr * type_expr
      | Inconsistent_constraint of Env.t * (type_expr * type_expr) list
      | Type_clash of Env.t * (type_expr * type_expr) list
      | Parameters_differ of Path.t * type_expr * type_expr
      | Null_arity_external
      | Missing_native_external
      | Unbound_type_var of type_expr * type_declaration
#if ocaml_version >= (4, 02)
      | Not_open_type of Path.t
      | Not_extensible_type of Path.t
      | Extension_mismatch of Path.t * Includecore.type_mismatch list
      | Rebind_wrong_type of Longident.t * Env.t * (type_expr * type_expr) list
      | Rebind_mismatch of Longident.t * Path.t * Path.t
      | Rebind_private of Longident.t
#else
      | Unbound_exception of Longident.t
      | Not_an_exception of Longident.t
#endif
      | Bad_variance of int * (bool*bool*bool) * (bool*bool*bool)
      | Unavailable_type_constructor of Path.t
      | Bad_fixed_type of string
#if ocaml_version >= (4, 02)
      | Unbound_type_var_ext of type_expr * extension_constructor
#else
      | Unbound_type_var_exc of type_expr * type_expr
#endif
      | Varying_anonymous
      with sexp
  end

module Typetexp = struct
    open Types

    type error =
      Typetexp.error =
        Unbound_type_variable of string
      | Unbound_type_constructor of Longident.t
      | Unbound_type_constructor_2 of Path.t
      | Type_arity_mismatch of Longident.t * int * int
      | Bound_type_variable of string
      | Recursive_type
      | Unbound_row_variable of Longident.t
      | Type_mismatch of (type_expr * type_expr) list
      | Alias_type_mismatch of (type_expr * type_expr) list
      | Present_has_conjunction of string
      | Present_has_no_type of string
      | Constructor_mismatch of type_expr * type_expr
      | Not_a_variant of type_expr
      | Variant_tags of string * string
      | Invalid_variable_name of string
      | Cannot_quantify of string * type_expr
      | Multiple_constraints_on_type of Longident.t
      | Repeated_method_label of string
      | Unbound_value of Longident.t
      | Unbound_constructor of Longident.t
      | Unbound_label of Longident.t
      | Unbound_module of Longident.t
      | Unbound_class of Longident.t
      | Unbound_modtype of Longident.t
      | Unbound_cltype of Longident.t
      | Ill_typed_functor_application of Longident.t
      | Illegal_reference_to_recursive_module
#if ocaml_version >= (4, 02)
      | Access_functor_as_structure of Longident.t
#endif
      with sexp
  end

module Typecore = struct
    open Asttypes
    open Types

    type error =
      Typecore.error =
        Polymorphic_label of Longident.t
      | Constructor_arity_mismatch of Longident.t * int * int
      | Label_mismatch of Longident.t * (type_expr * type_expr) list
      | Pattern_type_clash of (type_expr * type_expr) list
#if ocaml_version >= (4, 02)
      | Or_pattern_type_clash of Ident.t * (type_expr * type_expr) list
#endif
      | Multiply_bound_variable of string
      | Orpat_vars of Ident.t
      | Expr_type_clash of (type_expr * type_expr) list
      | Apply_non_function of type_expr
      | Apply_wrong_label of label * type_expr
      | Label_multiply_defined of string
      | Label_missing of Ident.t list
      | Label_not_mutable of Longident.t
#if ocaml_version >= (4, 02)
      | Wrong_name of string * type_expr * string * Path.t * Longident.t
#else
      | Wrong_name of string * Path.t * Longident.t
#endif
      | Name_type_mismatch of
          string * Longident.t * (Path.t * Path.t) * (Path.t * Path.t) list
#if ocaml_version >= (4, 02)
      | Invalid_format of string
#else
      | Incomplete_format of string
      | Bad_conversion of string * int * char
#endif
      | Undefined_method of type_expr * string
      | Undefined_inherited_method of string
      | Virtual_class of Longident.t
      | Private_type of type_expr
      | Private_label of Longident.t * type_expr
      | Unbound_instance_variable of string
      | Instance_variable_not_mutable of bool * string
      | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
      | Outside_class
      | Value_multiply_overridden of string
      | Coercion_failure of
          type_expr * type_expr * (type_expr * type_expr) list * bool
      | Too_many_arguments of bool * type_expr
      | Abstract_wrong_label of label * type_expr
      | Scoping_let_module of string * type_expr
      | Masked_instance_variable of Longident.t
      | Not_a_variant_type of Longident.t
      | Incoherent_label_order
      | Less_general of string * (type_expr * type_expr) list
      | Modules_not_allowed
      | Cannot_infer_signature
      | Not_a_packed_module of type_expr
      | Recursive_local_constraint of (type_expr * type_expr) list
      | Unexpected_existential
      | Unqualified_gadt_pattern of Path.t * string
#if ocaml_version >= (4, 02)
      | Invalid_interval
      | Invalid_for_loop_index
      | No_value_clauses
      | Exception_pattern_below_toplevel
#endif
      with sexp
  end

module Symtable = struct
    type error =
      Symtable.error =
        Undefined_global of string
      | Unavailable_primitive of string
      | Wrong_vm of string
      | Uninitialized_global of string
      with sexp
  end



(*** Suppress values beginning with _.  Lifted straight from uTop:
 * uTop_main.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 **)

let rec map_items (unwrap: 'a -> Outcometree.out_sig_item * 'b)
                  (wrap: Outcometree.out_sig_item -> 'b -> 'a)
                  (items: 'a list) : 'a list =
  match items with
  | [] ->
     []
  | item :: items ->
     let sig_item, _ = unwrap item in
     let name, _rec_status =
       match sig_item with
       | Outcometree.Osig_class (_, name, _, _, rs)
       | Outcometree.Osig_class_type (_, name, _, _, rs)
       | Outcometree.Osig_module (name, _, rs)
#if ocaml_version >= (4, 02)
       | Outcometree.Osig_type ({ Outcometree.otype_name = name }, rs) ->
#else
       | Outcometree.Osig_type ((name, _, _, _, _), rs) ->
#endif
          (name, rs)
#if ocaml_version >= (4, 02)
       | Outcometree.Osig_typext ({ Outcometree.oext_name = name}, _)
#else
       | Outcometree.Osig_exception (name, _)
#endif
       | Outcometree.Osig_modtype (name, _)
       | Outcometree.Osig_value (name, _, _) ->
          (name, Outcometree.Orec_not)
#if ocaml_version >= (4, 03)
       | Outcometree.Osig_ellipsis -> ("", Outcometree.Orec_not)
#endif
     in
     let keep = name = "" || name.[0] <> '_' in
     if keep then
       item :: map_items unwrap wrap items
     else
       (* Replace the [Orec_next] at the head of items by [Orec_first] *)
       let items =
         match items with
         | [] ->
            []
         | item :: items' ->
            let sig_item, extra = unwrap item in
            match sig_item with
            | Outcometree.Osig_class (a, name, b, c, rs) ->
               if rs = Outcometree.Orec_next then
                 wrap (Outcometree.Osig_class
                         (a, name, b, c, Outcometree.Orec_first)) extra
                 :: items'
               else
                 items
            | Outcometree.Osig_class_type (a, name, b, c, rs) ->
               if rs = Outcometree.Orec_next then
                 wrap (Outcometree.Osig_class_type
                         (a, name, b, c, Outcometree.Orec_first)) extra
                 :: items'
               else
                 items
            | Outcometree.Osig_module (name, a, rs) ->
               if rs = Outcometree.Orec_next then
                 wrap (Outcometree.Osig_module
                         (name, a, Outcometree.Orec_first)) extra :: items'
               else
                 items
            | Outcometree.Osig_type (oty, rs) ->
               if rs = Outcometree.Orec_next then
                 wrap (Outcometree.Osig_type
                         (oty, Outcometree.Orec_first)) extra :: items'
               else
                 items
#if ocaml_version >= (4, 02)
            | Outcometree.Osig_typext _
#else
            | Outcometree.Osig_exception _
#endif
#if ocaml_version >= (4, 03)
            | Outcometree.Osig_ellipsis
#endif
            | Outcometree.Osig_modtype _
            | Outcometree.Osig_value _ ->
               items
       in
       map_items unwrap wrap items


let signatures_remove_underscore_names
      (items : Outcometree.out_sig_item list) =
  map_items (fun x -> (x, ())) (fun x () -> x) items

let phrase_remove_underscore_names (phrase:Outcometree.out_phrase) =
  match phrase with
  | Outcometree.Ophr_eval _
  | Outcometree.Ophr_exception _ -> phrase
  | Outcometree.Ophr_signature items ->
     Outcometree.Ophr_signature (map_items (fun x -> x) (fun x y -> (x, y)) items)

(* End of uTop code *)
