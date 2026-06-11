#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|
# Specifications for the "pa_ppx_regexp" preprocessor:
requires = "camlp5,fmt,re,pcre2,pa_ppx.base,pa_ppx_static,pa_ppx_regexp_runtime,camlp5.parser_quotations,camlp5.printers.full"
version = "%s"
description = "pa_ppx_regexp: pa_ppx_regexp rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,re,pcre2,pa_ppx.base.link,pa_ppx_static,camlp5.parser_quotations.link,camlp5.printers.full"
archive(byte) = "pa_ppx_regexp.cmo"
archive(native) = "pa_ppx_regexp.cmx"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_regexp.cmo"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,re,pcre2,pa_ppx.base,pa_ppx_static,camlp5.parser_quotations,camlp5.printers.full"
  archive(syntax,preprocessor,-native) = "pa_ppx_regexp.cmo"
  archive(syntax,preprocessor,native) = "pa_ppx_regexp.cmx"

|} version)
