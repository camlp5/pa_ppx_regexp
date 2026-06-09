WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGES := $(PACKAGES),pa_ppx_regexp,ounit2,pa_ppx.utils

ML= \
	pa_ppx_regexp_test.ml ya_wrap_ocamlfind.ml join_meta.ml syntax_test.ml standalone_test.ml

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)

TESTS=pa_ppx_regexp_test.exe syntax_test.exe ya-wrap-ocamlfind join_meta standalone_test.exe

all: $(TESTS)

test:: all
	mkdir -p _build && ./pa_ppx_regexp_test.exe
	mkdir -p _build && ./syntax_test.exe
	mkdir -p _build && ./standalone_test.exe

pa_ppx_regexp_test.exe: pa_ppx_regexp_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $<

standalone_test.exe: standalone_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package re,pcre2 -linkpkg -linkall -o $@ $<

syntax_test.exe: syntax_test.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pa_op.link,camlp5.pr_o.link,str,pa_ppx.base.link,pa_ppx_regexp.link,pa_ppx.testutils -linkpkg -linkall -o $@ $<

ya-wrap-ocamlfind: ya_wrap_ocamlfind.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg -package $(PACKAGES) $< -o $@

join_meta: join_meta.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc -linkall -linkpkg -package $(PACKAGES) $< -o $@

clean::
	rm -rf *.cm* _build *.exe ya-wrap-ocamlfind join_meta *.NEW
