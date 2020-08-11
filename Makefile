MAKEFLAGS += --no-builtin-rules --no-builtin-variables --warn-undefined-variables

LOCALSZ=102400
export LOCALSZ
GLOBALSZ=1048576
export GLOBALSZ

fulltest: test1 test2 diff23

define make_stage

diff$(2)$(1): stage$(2).pl stage$(1).pl
	diff -U2 $$^

test$(1): stage$(1)
	./$$< test

test$(1)-%: stage$(1)
	./$$< test $$*

stage$(1).pl: stage$(2) main.xtl
	rm -f $$@
	./$$< extoltoprolog main.xtl $$@

stage$(2): stage$(2).pl
	gplc $$< -o $$@

$(1): stage$(1).pl

endef

$(eval $(call make_stage,1,0))
$(eval $(call make_stage,2,1))
$(eval $(call make_stage,3,2))

reboot: stage3.pl
	cp stage3.pl stage0.pl

clean:
	rm -f stage1.pl stage2.pl stage3.pl

