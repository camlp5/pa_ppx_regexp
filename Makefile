# Makefile,v
# Copyright (c) INRIA 2007-2017

TOP=.
include $(TOP)/config/Makefile.top

WD=$(shell pwd)
DESTDIR=

SYSDIRS= runtime pa_regexp

TESTDIRS= tests tests-mdx

all: sys
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) all; cd ..; done

sys:
	set -e; for i in $(SYSDIRS); do cd $$i; $(MAKE) all; cd ..; done

test: all
	set -e; for i in $(TESTDIRS); do cd $$i; $(MAKE) test; cd ..; done

META: all
	$(JOINMETA) -rewrite pa_ppx_regexp_runtime:pa_ppx_regexp.runtime \
			-direct-include pa_regexp \
			-wrap-subdir runtime:runtime > META

install: META
	$(OCAMLFIND) remove pa_ppx_regexp || true
	$(OCAMLFIND) install pa_ppx_regexp META local-install/lib/*/*.*

uninstall:
	$(OCAMLFIND) remove pa_ppx_regexp || true

clean::
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) clean; cd ..; done
	rm -rf docs local-install $(BATCHTOP) META *.corrected

depend:
	set -e; for i in $(SYSDIRS) $(TESTDIRS); do cd $$i; $(MAKE) depend; cd ..; done
