MAKEFLAGS += --no-builtin-rules --no-builtin-variables --warn-undefined-variables

BUILD ?= build

-include $(BUILD)/config.mk

VERBOSE ?= 0
SRC ?= $(CURDIR)
DESTDIR ?=
NAME ?= extol
PREFIX ?= $(SRC)/local
BINDIR ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share
DOCDIR ?= $(PREFIX)/share/doc/$(NAME)
PLC ?= gplc
PLC_FLAGS ?= --global-size 1280001 --local-size 1280002

CONFIG_VARIABLES = VERBOSE DESTDIR NAME PREFIX BINDIR DATADIR DOCDIR PLC PLC_FLAGS

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

.PHONY: test
test: test1 test2 diff23
	@echo [-] ALL TESTS PASSED

.PHONY: check
check: test
	@true

define make_stage

.PHONY: diff$(1)$(2)
diff$(1)$(2): $!stage$(2).pl $!stage$(1).pl
	@echo [$(2)] DIFF
	diff --unified=2 --report-identical-files $$^

.PHONY: test$(2)
test$(2): $!stage$(2)
	@echo [$(2)] TEST $$<
	$(./)$$< test

test$(2)-%: $!stage$(2)
	@echo [$(2)] TEST $$< $$*
	$(./)$$< test $$*

$!stage$(2).pl: $!stage$(1) $(all_sources)
	@echo [$(2)] TOPL $$@
	@rm -f $$@
	$(./)$$< extoltoprolog src/main.xtl $$@

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
$(eval $(call make_stage,3,4))

.PHONY: reboot
reboot: 2
	@echo [-] BOOT $/bootstrap/stage0.pl
	$(./)$!stage2 extoltoprolog $/src/main.xtl $!stage0.pl --slim
	cp $!stage0.pl $/bootstrap/stage0.pl

.PHONY: clean
clean:
	@echo [-] CLEAN
	rm -f $!stage0.pl $!stage1.pl $!stage2.pl $!stage3.pl $!stage0 $!stage1 $!stage2 $!stage3

.PHONY: repl
repl: repl2

test-%: test1-% test2-%
	@echo [-] TEST "'$*'" PASSED

.PHONY: todo
todo:
	cd $/. && git grep -En 'TOD[O]|- \[ \]'

.PHONY: install
install: $!stage2
	@echo [2] INSTALL $(DESTDIR)$(PREFIX)
	install -Cvm 755 $!stage2 -DT $(DESTDIR)$(BINDIR)/$(NAME)
	install -Cvm 644 $/README.md $/LICENSE.md $/NOTICE -Dt $(DESTDIR)$(DOCDIR)
	install -Cvm 644 $/integrations/emacs/extol.el -DT $(DESTDIR)$(DATADIR)/emacs/site-lisp/$(NAME).el
