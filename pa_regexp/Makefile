# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),fmt,re,pcre,pcre2,pa_ppx.base,pa_ppx.deriving_plugins.std,pa_ppx_static
TARGET_PACKAGE = pa_ppx_regexp

ML = pa_ppx_regexp.ml
TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

all: $(CMO) $(CMX) camlp5.pa_ppx_regexp camlp5.pa_ppx_regexp.opt META
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)

camlp5.pa_ppx_regexp: $(CMO)
	$(MKCAMLP5) -package fmt,re,pcre,pcre2,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(CMO) -o $@

camlp5.pa_ppx_regexp.opt: $(CMX)
	$(MKCAMLP5OPT) -package fmt,re,pcre,pcre2,camlp5.pa_r,camlp5.pr_r,pa_ppx.base $(CMX) -o $@

TARGET_INSTALL_FILES=$(FILES_TARGET_INSTALL_FILES)
install:: default_install

clean::
	rm -rf META camlp5.pa_ppx_regexp* mk_meta mk_meta.ml
