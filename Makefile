pl = echo -n | GLOBALSZ=1048576 gprolog --consult-file

.PHONY: test fasttest fulltest difftest clean

fulltest: test2 difftest test3

diff12: stage2.pl
	diff -U2  stage1.pl stage2.pl

diff23: stage2.pl stage3.pl
	diff -U2  stage2.pl stage3.pl

diff13: stage3.pl
	diff -U2  stage1.pl stage3.pl

test3: stage3.pl
	$(pl) stage3.pl test

test3-%: stage3.pl
	$(pl) stage3.pl test $*

test2: stage2.pl
	$(pl) stage2.pl test

test2-%: stage2.pl
	$(pl) stage2.pl test $*

test1:
	$(pl) stage1.pl test

test1-%: stage1.pl
	$(pl) stage1.pl test $*

stage3.pl: stage2.pl main.xtl
	rm -f stage3.pl
	$(pl) stage2.pl extoltoprolog main.xtl stage3.pl

stage2.pl: main.xtl
	rm -f stage2.pl
	$(pl) stage1.pl extoltoprolog main.xtl stage2.pl

stage1.pl: stage3.pl
	cp stage3.pl stage1.pl

clean:
	rm -f stage2.pl stage3.pl
