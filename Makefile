MAKEFLAGS += --no-builtin-rules --no-builtin-variables --warn-undefined-variables

BUILD ?= build

-include $(BUILD)/config.mk

VERBOSE ?= 0
TRACE ?= # 0 1 2 i unit[1|2] repl[1|2|i] topl[1|2]
SRC ?= $(CURDIR)
ONLY ?= 0123i
DESTDIR ?=
NAME ?= extol
PREFIX ?= $(BUILD)/local
BINDIR ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share
DOCDIR ?= $(PREFIX)/share/doc/$(NAME)
PLC ?= gplc
PLC_FLAGS ?= --global-size 128000 --local-size 32000 -C -O2 --fixed-sizes --no-top-level --strip

CONFIG_VARIABLES = VERBOSE ONLY DESTDIR NAME PREFIX BINDIR DATADIR DOCDIR PLC PLC_FLAGS

ifneq ($(VERBOSE),1)
MAKEFLAGS += --silent
endif

/ := $(SRC)/
! := $(BUILD)/
./ :=

ifeq ($/,$(CURDIR)/)
/ :=
endif
ifeq ($!,./)
! :=
./ = ./
endif

all_sources = $(shell find $/src -type f -iname '*.xtl')

.PHONY: default
default: 2
	@true

.PHONY: everything
everything: check extras

.PHONY: extras
extras: docker

.PHONY: check1 check2 check
check0: # TODO: eunit0
check1: unit1 test1 # TODO: eunit1
check2: unit2 test2 # TODO: eunit2
eunit: eunit0 eunit1 # TODO: eunit2
unit: unit1 unit2
check: check0 check1 check2 diff23 testi
	@echo [--] ALL TESTS PASSED

test-%: test1-% test2-%
	@true

eunit-%: eunit0-% eunit1-% eunit2-%
	@true

check0-%: eunit0-%
	@true

check1-%: unit1-% test1-% eunit1-%
	@true

check2-%: unit2-% test2-% eunit2-%
	@true

.PHONY: testi
testi: install-if-needed
	echo [I ] INTEGRATION TESTS
	STAGE=I EXTOL=$(BINDIR)/$(NAME) $(SHELL) $/test/run

testi-%: install-if-needed
	echo [I ] INTEGRATION TESTS $*
	STAGE=I EXTOL=$(BINDIR)/$(NAME) $(SHELL) $/test/run "$*"

trace = $(foreach x,$(1) all,$(if $(findstring  $(x) , $(TRACE) ),EXTOL_TRACE=1))

define make_stage

ifneq (,$$(findstring $(1),$$(ONLY)))
STAGE$(1)_PL := $!stage$(1).pl
STAGE$(1) := $!stage$(1)
else
STAGE$(1)_PL :=
STAGE$(1) :=
endif

.PHONY: test$(1)
test$(1): $$(STAGE$(1))
	echo [$(1) ] INTEGRATION TESTS
	$$(call trace,test1 test 1) STAGE=$(1) EXTOL=$$!stage$(1) $$(SHELL) $/test/run

test$(1)-%: $$(STAGE$(1))
	echo [$(1) ] INTEGRATION TESTS $$*
	$$(call trace,$$@ test $(1)) STAGE=$(1) EXTOL=$$!stage$(1) $$(SHELL) $/test/run "$$*"

.PHONY: diff$(1)$(2)
diff$(1)$(2): $!stage$(2).pl $$(STAGE$(1)_PL)
	@echo [$(2) ] DIFF
	diff --unified=2 --brief $$^ || echo [$(2)] DIFF FAILED

.PHONY: unit$(2)
unit$(2): $!stage$(2)
	@echo [$(2) ] UNIT $$<
	$$(call trace,$$@ unit $(2)) $(./)$$< test

unit$(2)-%: $!stage$(2)
	@echo [$(2) ] UNIT $$< $$*
	$$(call trace,unit$(2) unit $(2)) $(./)$$< test $$*

.PHONY: unit$(2)
eunit$(1): $!stage$(1)
	@echo [$(1)E] EVAL UNIT $$<
	$$(call trace,$$@ enuit $(1)) $(./)$$< eval-tests $/src/main.xtl

eunit$(1)-%: $!stage$(1)
	@echo [$(1)E] UNIT $$< $$*
	$$(call trace,eunit$(1) eunit $(1)) $(./)$$< eval-tests $/src/main.xtl $$*

$!stage$(2).pl: $$(STAGE$(1)) $(all_sources) $$!embedded-prelude.pl
	@echo [$(2) ] TOPL $$@
	@rm -f $$@
	$$(call trace,topl$(1) topl $(1)) $(./)$!stage$(1) extoltoprolog $/src/main.xtl $$@ --inject-prolog $$!embedded-prelude.pl

$!stage$(1): $!stage$(1).pl
	@echo [$(1) ] PLC $$@
	$(PLC) $(PLC_FLAGS) $$< -o $$@

.PHONY: $(1)
$(1): $!stage$(1)

.PHONY: repl$(1)
repl$(1): $(1)
	@echo [$(1) ] REPL
	$$(call trace,$$@ repl $(1)) $(./)$!stage$(1) repl

endef

%/.:
	@echo [--] MKDIR $@
	mkdir -p $@

configure: $!Makefile
	@echo [--] CONFIG $!config.mk
	@echo $$'$(foreach var,$(CONFIG_VARIABLES),\n$(var) := $($(var))\n)' > $!config.mk

$!Makefile: | $!.
	@echo [--] CREATE $@
	echo $$'BUILD=.\nSRC=$(realpath $(SRC))\ninclude $$(SRC)/Makefile' > $@

$!stage0.pl: $/bootstrap/stage0.pl | $!.
	@echo [0 ] COPY $@
	cp $< $@

$(eval $(call make_stage,0,1))
$(eval $(call make_stage,1,2))
$(eval $(call make_stage,2,3))

.PHONY: reboot
reboot: 2
	@echo [--] BOOT $/bootstrap/stage0.pl
	$(call trace,reboot topl 2) $(./)$!stage2 extoltoprolog $/src/main.xtl $!stage0.pl --slim --inject-prolog $!embedded-prelude.pl
	cp $!stage0.pl $/bootstrap/stage0.pl
	@echo [--] REBOOT COMPLETE

.PHONY: clean
clean:
	@echo [--] CLEAN
	rm -f $!stage0.pl $!stage1.pl $!stage2.pl $!stage3.pl $!stage0 $!stage1 $!stage2 $!stage3

.PHONY: repl
repl: repl2

.PHONY: repli
repli: install-if-needed
	@echo [I ] REPL
	$(call trace,repli repl i) $(BINDIR)/$(NAME) repl

unit-%: unit1-% unit2-%
	@echo [--] UNIT TEST "'$*'" PASSED

.PHONY: todo
todo:
	cd $/. && git grep -En 'TOD[O]|- \[ \]'

.PHONY: install
install: $!stage2
	@echo [I ] INSTALL $(DESTDIR)$(PREFIX)
	set -o pipefail; ( \
	  install -Cvm 755 $!stage2 -DT $(DESTDIR)$(BINDIR)/$(NAME) ; \
	  install -Cvm 644 $/README.md $/LICENSE.md $/NOTICE -Dt $(DESTDIR)$(DOCDIR) ; \
	  install -Cvm 644 $/integrations/emacs/extol.el -DT $(DESTDIR)$(DATADIR)/emacs/site-lisp/$(NAME).el ; \
	) | sed 's/^/[I ] + /'

.PHONY: install-if-needed
ifneq (,$(findstring i,$(ONLY)))
install-if-needed: install
	@true
else
install-if-needed:
	@true
endif

.PHONY: docker
docker:
	docker build $/. -t extol

.PHONY: docker-repl
docker-repl: docker
	docker run --rm --interactive --tty extol

$!generate-embedded-prelude.pl: $/src/generate-embedded-prelude.xtl $!stage0
	@echo '[  ]' TOPL $@
	$(call trace,topl 0 topl0) $!stage0 extoltoprolog --slim $< $@

$!generate-embedded-prelude: $!generate-embedded-prelude.pl
	@echo '[  ]' PLC $@
	$(PLC) $(PLC_FLAGS) $< -o $@

$!embedded-prelude.pl: $!generate-embedded-prelude
	@echo '[  ]' GEN $@
	unset EXTOL_TRACE; ./$< > $@.out
	mv $@.out $@
