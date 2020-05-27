pl = echo -n | GLOBALSZ=1048576 gprolog --consult-file

.PHONY: test clean

test: main.pl trampoline.pl
	diff main.pl trampoline.pl
	$(pl) main.pl test < /dev/null

main.pl: trampoline.pl main.xtl
	rm -f main.pl
	$(pl) trampoline.pl extoltoprolog main.xtl main.pl

trampoline.pl: main.xtl
	rm -f trampoline.pl
	$(pl) boot.pl extoltoprolog main.xtl trampoline.pl

boot.pl: main.xtl
	rm -f newboot.pl
	$(pl) boot.pl extoltoprolog main.xtl newboot.pl
	mv newboot.pl boot.pl

clean:
	rm -f main.pl trampoline.pl
