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
# Specifications for the "pa_ppx_regexp_runtime" package:
requires = "fmt,re,pcre,pcre2,pa_ppx_static.runtime"
version = "%s"
description = "pa_ppx_regexp runtime support"

# For linking
archive(byte) = "pa_ppx_regexp_runtime.cmo"
archive(native) = "pa_ppx_regexp_runtime.cmx"

# For the toploop:
archive(byte,toploop) = "pa_ppx_regexp_runtime.cmo"

|} version)
