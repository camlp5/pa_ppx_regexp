WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub
include $(TOP)/config/Makefile.mdx

all test:: mdx-test

mdx-test:: error-handling.asciidoc.TEST ../README.asciidoc.TEST
