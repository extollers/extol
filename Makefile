pl = echo -n | GLOBALSZ=1048576 gprolog --consult-file

.PHONY: test fasttest fulltest difftest clean

fulltest: fasttest difftest test

difftest: main.pl trampoline.pl
	diff -U2  trampoline.pl main.pl

test: main.pl
	$(pl) main.pl test

test-%: main.pl
	$(pl) main.pl test $*

fasttest: trampoline.pl
	$(pl) trampoline.pl test

fasttest-%: trampoline.pl
	$(pl) trampoline.pl test $*

main.pl: trampoline.pl main.xtl
	rm -f main.pl
	$(pl) trampoline.pl extoltoprolog main.xtl main.pl

trampoline.pl: main.xtl
	rm -f trampoline.pl
	$(pl) boot.pl extoltoprolog main.xtl trampoline.pl

boot.pl: main.pl
	rm -f newboot.pl
	$(pl) main.pl extoltoprolog main.xtl newboot.pl
	mv newboot.pl boot.pl

clean:
	rm -f main.pl trampoline.pl
