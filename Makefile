## -*- Mode: Makefile -*-

##======================================================================
## Vars

##--------------------------------------------------------------
## Vars: config

srcdir ?= wikidata
#srcfiles ?= $(wildcard $(srcdir)/*.csv)
include $(srcdir)/config.mak
srcfiles = $(addprefix $(srcdir)/,$(_srcfiles))

base ?= $(notdir $(srcdir:.d=))

##--------------------------------------------------------------
## Vars: Targets

TARGETS ?= groups.enum.bin

##--------------------------------------------------------------
## Vars: cleanup
CLEAN_FILES ?=
REALCLEAN_FILES ?=

##======================================================================
## Rules: Defaults

all: $(TARGETS)

.SECONDARY:

config:
	@echo "srcdir=$(srcdir)"
	@echo "srcfiles=$(srcfiles)"
	@echo "base=$(base)"

##======================================================================
## Rules: Groups



##======================================================================
## Rules: Cleanup

clean:
	test -z "$(CLEAN_FILES)" || rm -f $(CLEAN_FILES)

realclean: clean
	test -z "$(REALCLEAN_FILES)" || rm -f $(REALCLEAN_FILES)


