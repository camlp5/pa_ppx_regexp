(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_migrate *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

[%%typedecls
  [%%import: MLast.expr
    [@add [%%import: MLast.loc]]
    [@add [%%import: MLast.type_var]]
    [@with Ploc.vala := vala]
  ]
  [%%import: 'a Ploc.vala]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = MLast
        ; dstmod = Ploc
        ; types = [
            vala
          ]
        }
      ; {
          srcmod = MLast
        ; dstmod = MLast
        ; types = [
            class_infos
          ; longid
          ; ctyp
          ; poly_variant
          ; patt
          ; expr
          ; case_branch
          ; module_type
          ; functor_parameter
          ; sig_item
          ; with_constr
          ; module_expr
          ; str_item
          ; type_decl
          ; generic_constructor
          ; extension_constructor
          ; type_extension
          ; class_type
          ; class_sig_item
          ; class_expr
          ; class_str_item
          ; longid_lident
          ; payload
          ; attribute_body
          ; attribute
          ; attributes_no_anti
          ; attributes
          ; type_var
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ x -> Option.map (subrw __dt__) x)
        }
      ; migrate_loc = {
          srctype = [%typ: loc]
        ; dsttype = [%typ: MLast.loc]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let parse_expr (loc : Ploc.t) s =
    Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string s)
        

let parse_antiquot_expr (loc : Ploc.t) s =
  try 
    Ploc.call_with Plexer.force_antiquot_loc true
      (Grammar.Entry.parse Pcaml.expr_eoi) (Stream.of_string s)
    with Ploc.Exc(subloc, exn) ->
          let bp = Ploc.first_pos subloc in
          let ep = Ploc.last_pos subloc in
          let newsubloc = Ploc.(sub loc bp (ep - bp)) in
          raise (Ploc.Exc(newsubloc, exn))

module Options = struct

type t =
  Multi
| Single
| Global
| Insensitive
| Expr
| Raw
| Strings
| StringGroups of (int *bool) list
| Pred
| Exception
| RePerl
| Pcre2
| Dynamic
| Static
[@@deriving show]

let pp_hum pps = function
  Multi -> Fmt.(pf pps "m")
| Single -> Fmt.(pf pps "s")
| Global -> Fmt.(pf pps "g")
| Insensitive -> Fmt.(pf pps "i")
| Expr -> Fmt.(pf pps "e")
| Raw -> Fmt.(pf pps "raw")
| Strings -> Fmt.(pf pps "strings")
| StringGroups l ->
   let cgnum pps = function
       (n,true) -> Fmt.(pf pps "!%d" n)
     | (n,false) -> Fmt.(pf pps "%d" n) in
   Fmt.(pf pps "strings (%a)" (list ~sep:(const string ",") cgnum) l)
| Pred -> Fmt.(pf pps "pred")
| Exception -> Fmt.(pf pps "exc")
| RePerl -> Fmt.(pf pps "re_perl")
| Pcre2 -> Fmt.(pf pps "pcre2")
| Dynamic -> Fmt.(pf pps "dynamic")
| Static -> Fmt.(pf pps "static")

let fixed_only l =
  Std.filter (function StringGroups _ -> false | _ -> true) l

let default_string_groups ngroups =
  (Std.interval 0 (ngroups-1))
  |> List.map (fun i -> if i = 0 then (0,true) else (i,false))

let convert e =
  let rec conv l =
    let badarg ?(msg="") e = Fmt.(raise_failwithf (MLast.loc_of_expr e) "extract_options: malformed option%s" msg) in
    match l with
      <:expr< m >>::l -> Multi::(conv l)
    | <:expr< s >>::l -> Single::(conv l)
    | <:expr< i >>::l -> Insensitive::(conv l)
    | <:expr< e >>::l -> Expr::(conv l)
    | <:expr< g >>::l -> Global::(conv l)
    | <:expr< raw >>::l -> Raw::(conv l)
    | <:expr< strings >>::<:expr< ( $list:gl$ ) >>::l ->
       let gl = gl |> List.map (function
                            <:expr< $int:n$ >> -> (int_of_string n,false)
                          | <:expr< ! $int:n$ >> -> (int_of_string n,true)
                          | e -> badarg e) in
       Strings::(StringGroups gl)::(conv l)

    | (<:expr< ( $list:_$ ) >> as e)::l ->
       badarg ~msg:" (maybe this is the problem) group-list must be immediately preceded by 'strings'" e

    | <:expr< strings >>::<:expr< $int:n$ >>::l ->
       Strings::(StringGroups [(int_of_string n, false)])::(conv l)
    | <:expr< strings >>::<:expr< ! $int:n$ >>::l ->
       Strings::(StringGroups [(int_of_string n, true)])::(conv l)

    | <:expr< strings >>::l -> Strings::(conv l)
    | <:expr< pred >>::l -> Pred::(conv l)
    | <:expr< exc >>::l -> Exception::(conv l)
    | <:expr< re_perl >>::l -> RePerl::(conv l)
    | <:expr< pcre2 >>::l -> Pcre2::(conv l)
    | <:expr< dynamic >>::l -> Dynamic::(conv l)
    | <:expr< static >>::l -> Static::(conv l)
    | [] -> []
    | (e::_) -> badarg e in
  let (f,l) = Expr.unapplist e in
  let l = Std.uniquize (conv (f::l)) in
  if not (List.mem RePerl l || List.mem Pcre2 l) then
    RePerl::l
  else l

let string_groups loc options ngroups =
  if not (List.mem Strings  options) && not(List.mem Raw options) then
    default_string_groups ngroups
  else
  match List.find_map (function StringGroups l -> Some l | _ -> None) options with
    Some l -> l
  | None ->
     if List.mem Strings options then
       default_string_groups ngroups
     else
       Fmt.(raise_failwithf loc "Options.string_groups: internal error: <<%a>>" (list pp) options)

let check_oneof ~l options =
  List.length (Std.intersect options l) <= 1

let forbidden_options ~l options =
  let options = fixed_only options in
  Std.subtract options l

end

let compile_opts loc options =
  let open Options in
  let case_insensitive = List.mem Insensitive options in
  let dotall = List.mem Single options in
  let multiline = List.mem Multi options in
  if List.mem Pcre2 options then
    let opts = [] in
    let opts = if case_insensitive then <:expr< `CASELESS >>::opts else opts in
    let opts = if dotall then <:expr< `DOTALL >>::opts else opts in
    let opts = if multiline then <:expr< `MULTILINE >>::opts else opts in
    convert_up_list_expr loc opts
  else if List.mem RePerl options then
    let opts = [] in
    let opts = if case_insensitive then <:expr< `Caseless >>::opts else opts in
    let opts = if dotall then <:expr< `Dotall >>::opts else opts in
    let opts = if multiline then <:expr< `Multiline >>::opts else opts in
    convert_up_list_expr loc opts
  else assert false


let wrap_loc loc f arg =
  try f arg
  with e ->
        raise (Ploc.Exc(loc, e))


module Pattern = struct

(* String parts are:

   Either:

   * "$$"

   * "$" <digit>+

   * "$" "{" <digit>+ "}"

   * "$" "{" <expr> "}"
 *)

let string_parts_pattern = Re.Perl.compile_pat {|\$\$|\$([0-9]+)|\$\{([0-9]+)\}|\$\{([^}]+)\}|}

let add_loc_to_parts loc parts =
  let rec addrec loc = function
      [] -> []
    | (`Text s as p)::t ->
       let slen = String.length s in
       let loclen = Ploc.((last_pos loc) - (first_pos loc)) in
       let subloc = Ploc.(sub loc 0 slen) in
       let restloc = Ploc.sub loc slen (loclen - slen) in
       (subloc, p)::(addrec restloc t)
    | (`Delim g as p)::t ->
       let s = match Re.Group.get_opt g 0 with
           None -> raise_failwithf loc "Pattern.add_loc_to_parts: Internal error: group 0 was None"
         | Some s -> s
       in
       let slen = String.length s in
       let loclen = Ploc.((last_pos loc) - (first_pos loc)) in
       let subloc = Ploc.(sub loc 0 slen) in
       let restloc = Ploc.sub loc slen (loclen - slen) in
       (subloc, p)::(addrec restloc t)
  in
  addrec loc parts


let extract_parts loc patstr =
  let open Options in
  let parts = Re.split_full string_parts_pattern patstr in
  let parts = parts
              |> List.filter_map (function
                       `Text "" -> None
                     | x -> Some x) in
  let loc_parts = add_loc_to_parts loc parts in
  loc_parts |> List.map (function
                     (loc, `Text s) -> (loc, `Text s)
                   | (loc, `Delim g) ->
                      match (Re.Group.get_opt g 0, Re.Group.get_opt g 1, Re.Group.get_opt g 2, Re.Group.get_opt g 3) with
                        (Some "$$", _, _, _) -> (loc, `Text "$")
                      | (_, Some nstr, _, _)
                      | (_, _, Some nstr, _) ->
                         (loc, `CGroup (int_of_string nstr))
                      | (_, _, _, Some exps) ->
                         (loc, `Expr (parse_expr loc exps))
                      | _ -> Fmt.(raise_failwithf loc "pa_ppx_regexp: unrecognized pattern: <<%a>>" Dump.string patstr)
                 )

let build_string loc ~cgroups ~options patstr =
  let open Options in
  let has_cgroups = ref (match cgroups with None -> false | Some _ -> true) in
  let ngroups = (match cgroups with None -> 0 | Some n -> n) in
  let cgroup_extract_expr n =
    let nstr = string_of_int n in
    if List.mem RePerl options then
      <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>
    else if List.mem Pcre2 options then
      <:expr< match Pcre2.get_substring __g__ $int:nstr$ with exception Not_found -> "" | s -> s >>
    else Fmt.(raise_failwithf loc "Pattern.build_string: neither <<re>> nor <<pcre2>> were found in options: %a\n"
            (list ~sep:(const string " ") Options.pp_hum) options) in
  let loc_parts = extract_parts loc patstr in
  let parts_exps =
    loc_parts |> List.map (function
                       (loc, `Text s) ->
                        let s = String.escaped s in
                        <:expr< $str:s$ >>
                     | (loc, `CGroup n) ->
                        if ngroups < 0 then
                          Fmt.(raise_failwithf loc "Pattern(string): capture-groups not allowed")
                        else if ngroups > 0 && n >= ngroups then
                          Fmt.(raise_failwithf loc "Pattern(string): capture-group reference %d not in range [0..%d)" n ngroups) ;
                        has_cgroups := true ;
                        cgroup_extract_expr n
                     | (loc, `Expr e) -> e
                     | _ -> Fmt.(raise_failwithf loc "pa_ppx_regexp: unrecognized pattern: <<%a>>" Dump.string patstr)
                   ) in
  let listexpr = convert_up_list_expr loc parts_exps in
  if !has_cgroups then
    <:expr< fun __g__ -> String.concat "" $exp:listexpr$ >>
  else
    <:expr< String.concat "" $exp:listexpr$ >>

let build_expr loc ~cgroups ~options (patloc, patstr) =
  let open Options in
  let has_cgroups = ref (match cgroups with None -> false | Some _ -> true) in
  let ngroups = (match cgroups with None -> 0 | Some n -> n) in
  let cgroup_extract_expr nstr =
    if List.mem RePerl options then
      <:expr< match Re.Group.get_opt __g__ $int:nstr$ with None -> "" | Some s -> s >>
    else if List.mem Pcre2 options then
      <:expr< match Pcre2.get_substring __g__ $int:nstr$ with exception Not_found -> "" | s -> s >>
    else Fmt.(raise_failwithf loc "Pattern.build_expr: neither <<re>> nor <<pcre2>> were found in options: %a\n"
            (list ~sep:(const string " ") Options.pp_hum) options) in
  let e = parse_antiquot_expr patloc patstr in
  let dt = make_dt () in
  let old_migrate_expr = dt.migrate_expr in
  let migrate_expr dt = function
      ExXtr(loc, antiquot, _) ->
       let (nstr,_) = Std.sep_last (String.split_on_char ':' antiquot) in
       if ngroups < 0 then
         Fmt.(raise_failwithf loc "Pattern(string): capture-groups not allowed" nstr ngroups)
       else if ngroups > 0 && int_of_string nstr >= ngroups then
         Fmt.(raise_failwithf loc "Pattern(expr): capture-group reference %s not in range [0..%d)" nstr ngroups) ;
       has_cgroups := true ;
       cgroup_extract_expr nstr
    | e -> old_migrate_expr dt e in
  let dt = { dt with migrate_expr = migrate_expr } in
  let e = dt.migrate_expr dt e in
  if !has_cgroups then
    <:expr< fun __g__ -> $exp:e$ >>
  else
    e

let validate_options modn loc options =
  let open Options in
  let fl = forbidden_options  ~l:[Expr; RePerl; Pcre2] options in
  if fl <> [] then
    Fmt.(raise_failwithf loc "%s extension: forbidden option: %a" modn (list ~sep:(const string " ") Options.pp_hum) fl) ;
  ()

let build_pattern loc ~cgroups ~options (patloc, patstr) =
  let open Options in
  validate_options "pattern" loc options ;
  let patstr = Scanf.unescaped patstr in
  if List.mem Expr options then
    build_expr patloc ~cgroups ~options (patloc, patstr)
  else
    build_string patloc ~cgroups ~options patstr

let build_dynamic_regexp_string loc ~options (patloc, patstr) =
  let open Options in
  validate_options "pattern" loc options ;
  let unesc_patstr = Scanf.unescaped patstr in
  let loc_parts = extract_parts loc unesc_patstr in
  match loc_parts with
    [_, `Text _] -> (<:expr< $str:patstr$ >>, unesc_patstr)
  | _ ->
     let quote_expr =
       if List.mem Pcre2 options then
         <:expr< Pcre2.quote >>
       else if List.mem RePerl options then
         Fmt.(raise_failwithf loc "Dynamic Regexp: cannot do it with re_perl (no support for quoting)")
       else assert false in
     
     let parts_exps =
       loc_parts |> List.map (function
                          (loc, `Text s) ->
                           let s = String.escaped s in
                           <:expr< $str:s$ >>
                        | (loc, `CGroup n) ->
                           Fmt.(raise_failwithf loc "Dynamic Regexp: capture-groups not allowed")

                        | (loc, `Expr e) -> <:expr< $quote_expr$ $e$ >>
                      ) in
     let simple_version =
       loc_parts
       |> List.map (function
                (loc, `Text s) -> s
              | (loc, `CGroup n) -> assert false
              | (loc, `Expr e) -> "x"
            )
       |> String.concat "" in
     let listexpr = convert_up_list_expr loc parts_exps in
     (<:expr< String.concat "" $exp:listexpr$ >>, simple_version)

end

module RE = struct

let group_count loc options (reloc, unesc_restr) =
  let open Options in
  if List.mem Pcre2 options then
    1 + Pcre2.capturecount (wrap_loc reloc Pcre2.regexp unesc_restr)
  else if List.mem RePerl options then
    let re = wrap_loc loc Re.Perl.compile_pat unesc_restr in
    Re.group_count re
  else assert false


open Options
let _build loc ~options (reloc, restrexp) =
  let use_dynamic = List.mem Dynamic options in
  if List.mem Pcre2 options then
    let compile_opt_expr = compile_opts loc options in
    let regexp_expr = <:expr< Pcre2.regexp ~flags:$exp:compile_opt_expr$ $restrexp$ >> in
    if not use_dynamic then <:expr< [%static $exp:regexp_expr$ ] >> else regexp_expr

  else if List.mem RePerl options then
    let compile_opt_expr = compile_opts loc options in
    let regexp_expr = <:expr< Re.Perl.compile_pat ~opts:$exp:compile_opt_expr$ $restrexp$ >> in
    if not use_dynamic then <:expr< [%static $exp:regexp_expr$ ] >> else regexp_expr
  else Fmt.(raise_failwithf loc "pa_ppx_regexp: neither <<re>> nor <<pcre2>> were found in options: %a\n"
              (list ~sep:(const string " ") Options.pp_hum) options)

let build loc ~options (reloc, restr) =
  let unesc_restr = Scanf.unescaped restr in
  if List.length (Pattern.extract_parts loc unesc_restr) > 1 && [] = Std.intersect [Dynamic;Static] options then
    Fmt.(raise_failwithf loc "Must specify one of dynamic, static for a regexp that appears to have dynamic syntax") ;
  let use_dynamic = List.mem Dynamic options in
  let (restrexp, unesc_restr) =
    if not use_dynamic then (<:expr< $str:restr$ >>, unesc_restr)
    else
      let options = Std.intersect options [Pcre2; RePerl] in
      Pattern.build_dynamic_regexp_string loc ~options (reloc, restr) in
  let ngroups = group_count loc options (reloc, unesc_restr) in
  (ngroups,
   _build loc ~options (reloc, restrexp))

end

module Match = struct

module ReBuild = struct

let _string_converter loc ~options ngroups =
  let open Options in
  let string_groups = Options.string_groups loc options ngroups in
  let group_exp (n,required) =
    if n >= ngroups then
      Fmt.(raise_failwithf loc "Match(Re): group %d exceeds capture groups of [0..%d)" n ngroups) ;
    if required then
      <:expr< Re.Group.get __g__ $int:string_of_int n$ >>
    else
      <:expr< Re.Group.get_opt __g__ $int:string_of_int n$ >> in
  let group_exps = List.map group_exp string_groups in
  let group_tuple = Expr.tuple loc group_exps in
  <:expr< (fun __g__ -> $exp:group_tuple$ ) >>

let rec _result loc ~options ngroups use_exception =
  let open Options in
  if List.mem Raw options then
     if use_exception then
       <:expr< Re.exec __re__ __subj__ >>
     else
       <:expr< Re.exec_opt __re__ __subj__ >>
  else if List.mem Pred options then
    <:expr< Re.execp __re__ __subj__ >>
  else
    let convf = _string_converter loc ~options ngroups in
     if use_exception then
       let res = _result loc ~options:[Raw] ngroups true in
       <:expr< $exp:convf$ $exp:res$ >>
     else
       let res = _result loc ~options:[Raw] ngroups false in
       <:expr< match Option.map $exp:convf$ $exp:res$ with
                 exception Not_found -> None
               | rv -> rv
                 >>
end

module Pcre2Build = struct
let _string_converter loc ~options ngroups =
  let open Options in
  let string_groups = Options.string_groups loc options ngroups in
  let group_exp (n,required) =
    if n >= ngroups then
      Fmt.(raise_failwithf loc "Match(Pcre2): group %d exceeds capture groups of [0..%d)" n ngroups) ;
    if required then
      <:expr< Pcre2.get_substring __g__ $int:string_of_int n$ >>
    else
      <:expr< try Some(Pcre2.get_substring __g__ $int:string_of_int n$) with Not_found -> None >> in
  let group_exps = List.map group_exp string_groups in
  let group_tuple = Expr.tuple loc group_exps in
  <:expr< (fun __g__ -> $exp:group_tuple$ ) >>

let rec _result loc ~options ngroups use_exception =
  let open Options in
  if List.mem Raw options then
     if use_exception then
       <:expr< Pcre2.exec ~rex:__re__ __subj__ >>
     else
       <:expr< try Some (Pcre2.exec ~rex:__re__ __subj__) with Not_found -> None >>
  else if List.mem Pred options then
    <:expr< Pcre2.pmatch ~rex:__re__ __subj__ >>
  else
    let convf = _string_converter loc ~options ngroups in
     if use_exception then
       let res = _result loc ~options:[Raw] ngroups true in
       <:expr< $exp:convf$ $exp:res$ >>
     else
       let res = _result loc ~options:[Raw] ngroups false in
       <:expr< match Option.map $exp:convf$ $exp:res$ with
                 exception Not_found -> None
               | rv -> rv
                 >>
end


let validate_options modn loc options =
  let open Options in
  if not (check_oneof ~l:[RePerl; Pcre2] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<re>>, <<pcre2>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  if not (check_oneof ~l:[Pred; Exception] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<pred>>, <<exc>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  if not (check_oneof ~l:[Strings; Raw; Pred] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<strings>>, <<raw>>, <<pred>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  if not (check_oneof ~l:[Multi;Single] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<s>>, <<m>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  let fl = forbidden_options  ~l:[Insensitive; Single; Multi; Exception; Raw; Strings; Pred; RePerl; Pcre2; Dynamic; Static] options in
  if fl <> [] then
    Fmt.(raise_failwithf loc "%s extension: forbidden option: %a" modn (list ~sep:(const string " ") Options.pp_hum) fl) ;
  ()

let build_regexp loc ~options (reloc, restr) =
  let open Options in
  validate_options "match" loc options ;
  let (ngroups, regexp_expr) = RE.build loc ~options (reloc, restr) in
  let use_exception = List.mem Exception options in

  if List.mem Pcre2 options then
    let result = Pcre2Build._result loc ~options ngroups use_exception in
    <:expr< let __re__ = $exp:regexp_expr$ in
            fun __subj__->
            $exp:result$ >>
  else if List.mem RePerl options then
    let result = ReBuild._result loc ~options ngroups use_exception in
    <:expr< let __re__ = $exp:regexp_expr$ in
            fun __subj__->
            $exp:result$ >>
  else Fmt.(raise_failwithf loc "match extension: neither <<re>> nor <<pcre2>> were found in options: %a\n"
            (list ~sep:(const string " ") Options.pp_hum) options)
end

module Split = struct

module ReBuild = struct

let _result loc ~options ngroups =
  let open Options in
  if List.mem Raw options then
    <:expr< Re.split_full __re__ __subj__ >>
  else if List.mem Strings options then
    let converter_fun_exp =
      let convf = Match.ReBuild._string_converter loc ~options ngroups in
      <:expr< function `Text s -> `Text s
                       | `Delim __g__ -> `Delim ($exp:convf$ __g__) >> in
    <:expr< List.map $exp:converter_fun_exp$ (Re.split_full __re__ __subj__) >>
  else
    <:expr< Re.split __re__ __subj__ >>
end

module Pcre2Build = struct
let _result loc ~options ngroups =
  let open Options in
  if List.mem Strings options then
    let converter_fun_exp =
      let convf = Match.Pcre2Build._string_converter loc ~options ngroups in
      <:expr< function `Text s -> `Text s
                       | `Delim __g__ -> `Delim ($exp:convf$ __g__) >> in
    <:expr< List.map $exp:converter_fun_exp$ (Pa_ppx_regexp_runtime.pcre2_full_split __re__ __subj__) >>

  else if List.mem Raw options then
    <:expr< Pcre2.full_split ~rex:__re__ __subj__ >>
  else
    <:expr< Pcre2.split ~rex:__re__ __subj__ >>
end


let validate_options modn loc options =
  let open Options in
  Match.validate_options modn loc options ;
  if List.mem Pred options then
    Fmt.(raise_failwithf loc "%s extension: forbidden option: pred" modn)

let build_regexp loc ~options (reloc, restr) =
  let open Options in
  validate_options "split" loc options ;
  let (ngroups, regexp_expr) = RE.build loc ~options (reloc, restr) in
  if List.mem RePerl options then
    if ngroups > 1 && not (List.mem Strings options || List.mem Raw options) then
      Fmt.(raise_failwithf loc "split extension: must specify one of <<strings>>, <<raw>> for regexp with capture groups: %a"
             (list Options.pp) options)
    else
      let result = ReBuild._result loc ~options ngroups in
      <:expr< let __re__ = $exp:regexp_expr$ in
              fun __subj__->
              $exp:result$ >>
  else if List.mem Pcre2 options then
    if ngroups > 1 && not (List.mem Strings options || List.mem Raw options) then
      Fmt.(raise_failwithf loc "split extension: must specify one of <<strings>>, <<raw>> for regexp with capture groups: %a"
             (list Options.pp) options)
    else
      let result = Pcre2Build._result loc ~options ngroups in
      <:expr< let __re__ = $exp:regexp_expr$ in
              fun __subj__->
              $exp:result$ >>
  else Fmt.(raise_failwithf loc "split extension: neither <<re>> nor <<pcre2>> were found in options: %a\n"
              (list ~sep:(const string " ") Options.pp_hum) options)
end

module Subst = struct

let validate_options modn loc options =
  let open Options in
  if not (check_oneof ~l:[RePerl; Pcre2] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<re>>, <<pcre2>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  if not (check_oneof ~l:[Multi;Single] options) then
    Fmt.(raise_failwithf loc "%s extension: can specify at most one of <<s>>, <<m>>: %a"
           modn (list ~sep:(const string " ") Options.pp_hum) options) ;
  let fl = forbidden_options  ~l:[Global; Multi; Single; Insensitive; Expr; RePerl; Pcre2; Dynamic; Static] options in
  if fl <> [] then
    Fmt.(raise_failwithf loc "%s extension: forbidden option: %a" modn (list ~sep:(const string " ") Options.pp_hum) fl) ;
  ()

  let build_subst loc ~options (reloc, restr) (patloc, patstr) =
  let open Options in
  validate_options "subst" loc options ;
  let (ngroups, regexp_expr) = RE.build loc ~options (reloc, restr) in
  if List.mem RePerl options then
    let _ = wrap_loc reloc Re.Perl.compile_pat (Scanf.unescaped restr) in
    let global = List.mem Global options in
    let global = if global then <:expr< true >> else <:expr< false >> in
    let patexpr = Pattern.build_pattern loc ~cgroups:(Some ngroups) ~options:(Std.intersect [Expr;RePerl] options) (patloc, patstr) in
    <:expr< Re.replace ~all:$exp:global$ $exp:regexp_expr$ ~f:$exp:patexpr$ >>
  else if List.mem Pcre2 options then
    let _ = wrap_loc reloc Pcre2.regexp (Scanf.unescaped restr) in
    let global = List.mem Global options in
    let replacef = if global then <:expr< Pcre2.substitute_substrings >> else <:expr< Pcre2.substitute_substrings_first >> in
    let patexpr = Pattern.build_pattern loc ~cgroups:(Some ngroups) ~options:(Std.intersect [Expr;Pcre2] options) (patloc, patstr) in
    <:expr< $exp:replacef$ ~rex:$exp:regexp_expr$ ~subst:$exp:patexpr$ >>
  else Fmt.(raise_failwithf loc "subst extension: neither <<re>> nor <<pcre2>> were found in options: %a\n"
              (list ~sep:(const string " ") Options.pp_hum) options)
end

let rewrite_match arg = function
  <:expr:< [%match $locstr:(reloc, Ploc.VaVal s)$ ] >> -> Match.build_regexp loc ~options:[Options.RePerl] (reloc, s)
| <:expr:< [%match $locstr:(reloc, Ploc.VaVal s)$ / $exp:optexpr$ ] >> ->
   let options = Options.convert optexpr in
   Match.build_regexp loc ~options (reloc, s)
| _ -> assert false

let rewrite_split arg = function
  <:expr:< [%split $locstr:(reloc, Ploc.VaVal s)$ ] >> -> Split.build_regexp loc ~options:[Options.RePerl] (reloc, s)
| <:expr:< [%split $locstr:(reloc, Ploc.VaVal s)$ / $exp:optexpr$ ] >> ->
   let options = Options.convert optexpr in
   Split.build_regexp loc ~options (reloc, s)
| _ -> assert false

let rewrite_pattern arg = function
  <:expr:< [%pattern $locstr:(patloc, Ploc.VaVal s)$ / $exp:optexpr$ ] >> ->
   let options = Options.convert optexpr in
   Pattern.build_pattern loc ~cgroups:None ~options (patloc, s)
| <:expr:< [%pattern $locstr:(patloc, Ploc.VaVal s)$ ] >> -> Pattern.build_pattern loc ~cgroups:None ~options:[] (patloc, s)
| e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "pa_regexp.rewrite_pattern: unsupported extension <<%a>>"
            Pp_MLast.pp_expr e)

let rewrite_subst arg = function
  <:expr:< [%subst $locstr:(reloc, Ploc.VaVal restr)$ / $locstr:(patloc, Ploc.VaVal patstr)$ / $exp:optexpr$ ] >> ->
   let options = Options.convert optexpr in
   Subst.build_subst loc ~options (reloc, restr) (patloc, patstr)
| <:expr:< [%subst $locstr:(reloc, Ploc.VaVal restr)$ / $locstr:(patloc, Ploc.VaVal patstr)$ ] >> -> Subst.build_subst loc ~options:[Options.RePerl] (reloc, restr) (patloc, patstr)
| e -> Fmt.(raise_failwithf (MLast.loc_of_expr e) "pa_regexp.rewrite_subst: unsupported extension <<%a>>"
            Pp_MLast.pp_expr e)

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%match $exp:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_match arg z)
  | <:expr:< [%split $exp:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_split arg z)
  | <:expr:< [%pattern $exp:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_pattern arg z)
  | <:expr:< [%subst $exp:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_subst arg z)
  ] } in
  Pa_passthru.(install { name = "pa_regexp"; ef =  ef ; pass = None ; before = ["pa_static"] ; after = [] })
;;

install();;
