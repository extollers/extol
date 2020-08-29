MAKEFLAGS += --no-builtin-rules --no-builtin-variables --warn-undefined-variables

ifneq ($(V),1)
MAKEFLAGS += --silent
endif

# LOCALSZ=102400
# export LOCALSZ
GLOBALSZ=128000
export GLOBALSZ

BUILD ?= ./build
/ := $(BUILD)/

fulltest: test1 test2 diff23
	@echo [-] ALL TESTS PASSED

define make_stage

diff$(1)$(2): $/stage$(1).pl $/stage$(2).pl
	@echo [$(2)] DIFF
	diff --unified=2 --report-identical-files $$^

test$(2): $/stage$(2)
	@echo [$(2)] TEST $$<
	$$< test

test$(2)-%: $/stage$(2)
	@echo [$(2)] TEST $$< $$*
	$$< test $$*

$/stage$(2).pl: $/stage$(1) main.xtl lib/*.xtl
	@echo [$(2)] TOPL $$@
	@rm -f $$@
	$$< extoltoprolog main.xtl $$@

$/stage$(1): $/stage$(1).pl
	@echo [$(1)] GPLC $$@
	gplc $$< -o $$@

$(1): $/stage$(1)

endef

$/stage0.pl: bootstrap/stage0.pl
	@echo [0] COPY $@
	mkdir -p $/
	cp $< $@

$(eval $(call make_stage,0,1))
$(eval $(call make_stage,1,2))
$(eval $(call make_stage,2,3))
$(eval $(call make_stage,3,4))

reboot: 2
	@echo [3] BOOT bootstrap/stage0.pl
	$/stage2 extoltoprolog main.xtl $/stage0.pl --slim
	cp $/stage0.pl bootstrap/stage0.pl

clean:
	@echo [-] CLEAN
	rm -f $/stage0.pl $/stage1.pl $/stage2.pl $/stage3.pl $/stage0 $/stage1 $/stage2 $/stage3

repl: 2
	@echo [2] REPL
	$/stage2 repl

test-%: test1-% test2-%
	@true
