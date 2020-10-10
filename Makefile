MAKEFLAGS += --no-builtin-rules --no-builtin-variables --warn-undefined-variables

BUILD ?= build

-include $(BUILD)/config.mk

VERBOSE ?= 0
SRC ?= $(CURDIR)
ONLY ?= 0123i
DESTDIR ?=
NAME ?= extol
PREFIX ?= $(SRC)/local
BINDIR ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share
DOCDIR ?= $(PREFIX)/share/doc/$(NAME)
PLC ?= gplc
PLC_FLAGS ?= --global-size 128000 --local-size 32000

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

all_sources = $(shell find $/src -type f)

.PHONY: default
default: 2
	@true

.PHONY: check
check: unit1 test1 unit2 test2 diff23 testi
	@echo [-] ALL TESTS PASSED

.PHONY: testi
testi: install-if-needed
	echo [I] INTEGRATION TESTS
	STAGE=I EXTOL=$(BINDIR)/$(NAME) $(SHELL) $/test/run

testi-%: install-if-needed
	echo [I] INTEGRATION TESTS $*
	STAGE=I EXTOL=$(BINDIR)/$(NAME) $(SHELL) $/test/run "$*"

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
	echo [$(1)] INTEGRATION TESTS
	STAGE=$(1) EXTOL=$$!stage$(1) $$(SHELL) $/test/run

test$(1)-%: $$(STAGE$(1))
	echo [2] INTEGRATION TESTS $$*
	STAGE=$(1) EXTOL=$$!stage$(1) $$(SHELL) $/test/run "$$*"

.PHONY: diff$(1)$(2)
diff$(1)$(2): $!stage$(2).pl $$(STAGE$(1)_PL)
	@echo [$(2)] DIFF
	diff --unified=2 --brief $$^ || echo [$(2)] DIFF FAILED

.PHONY: unit$(2)
unit$(2): $!stage$(2)
	@echo [$(2)] UNIT $$<
	$(./)$$< test

unit$(2)-%: $!stage$(2)
	@echo [$(2)] UNIT $$< $$*
	$(./)$$< test $$*

$!stage$(2).pl: $$(STAGE$(1)) $(all_sources)
	@echo [$(2)] TOPL $$@
	@rm -f $$@
	$(./)$!stage$(1) extoltoprolog src/main.xtl $$@

$!stage$(1): $!stage$(1).pl
	@echo [$(1)] GPLC $$@
	$(PLC) $(PLC_FLAGS) $$< -o $$@

.PHONY: $(1)
$(1): $!stage$(1)

.PHONY: repl$(1)
repl$(1): $(1)
	@echo [$(1)] REPL
	$(./)$!stage$(1) repl

endef

%/.:
	@echo [-] MKDIR $@
	mkdir -p $@

configure: $!Makefile
	@echo [-] CONFIG $!config.mk
	@echo $$'$(foreach var,$(CONFIG_VARIABLES),\n$(var) := $($(var))\n)' > $!config.mk

$!Makefile: | $!.
	@echo [-] CREATE $@
	echo $$'BUILD=.\nSRC=$(realpath $(SRC))\ninclude $$(SRC)/Makefile' > $@

$!stage0.pl: $/bootstrap/stage0.pl | $!.
	@echo [0] COPY $@
	cp $< $@

$(eval $(call make_stage,0,1))
$(eval $(call make_stage,1,2))
$(eval $(call make_stage,2,3))

.PHONY: reboot
reboot: 2
	@echo [-] BOOT $/bootstrap/stage0.pl
	$(./)$!stage2 extoltoprolog $/src/main.xtl $!stage0.pl # --slim
	cp $!stage0.pl $/bootstrap/stage0.pl
	@echo [-] REBOOT COMPLETE

.PHONY: clean
clean:
	@echo [-] CLEAN
	rm -f $!stage0.pl $!stage1.pl $!stage2.pl $!stage3.pl $!stage0 $!stage1 $!stage2 $!stage3

.PHONY: repl
repl: repl2

.PHONY: repli
repli: install-if-needed
	@echo [I] REPL
	$(BINDIR)/$(NAME) repl

unit-%: unit1-% unit2-%
	@echo [-] UNIT TEST "'$*'" PASSED

.PHONY: todo
todo:
	cd $/. && git grep -En 'TOD[O]|- \[ \]'

.PHONY: install
install: $!stage2
	@echo [I] INSTALL $(DESTDIR)$(PREFIX)
	set -o pipefail; ( \
	  install -Cvm 755 $!stage2 -DT $(DESTDIR)$(BINDIR)/$(NAME) ; \
	  install -Cvm 644 $/README.md $/LICENSE.md $/NOTICE -Dt $(DESTDIR)$(DOCDIR) ; \
	  install -Cvm 644 $/integrations/emacs/extol.el -DT $(DESTDIR)$(DATADIR)/emacs/site-lisp/$(NAME).el ; \
	) | sed 's/^/[I] + /'

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
