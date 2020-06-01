# pl = echo -n | LOCALSZ=102400 GLOBALSZ=1048576 gprolog --consult-file
pl = LOCALSZ=102400 GLOBALSZ=1048576 gprolog --consult-file

fulltest: test1 test2 diff23 test3

define make_stage

diff$(2)$(1): stage$(2).pl stage$(1).pl
	diff -U2 $$^

test$(1): stage$(1).pl
	$(pl) stage$(1).pl test

test$(1)-%: stage$(1).pl
	$(pl) stage$(1).pl test $$*

stage$(1).pl: stage$(2).pl main.xtl
	rm -f $$@
	$(pl) $$< extoltoprolog main.xtl $$@
endef

$(eval $(call make_stage,1,0))
$(eval $(call make_stage,2,1))
$(eval $(call make_stage,3,2))

reboot: stage3.pl
	cp stage3.pl stage0.pl

clean:
	rm -f stage1.pl stage2.pl stage3.pl
