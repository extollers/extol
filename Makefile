pl = echo -n | GLOBALSZ=$$((1024*1024)) gprolog --consult-file

test: main.pl trampoline.pl
	diff main.pl trampoline.pl
	$(pl) main.pl test < /dev/null

main.pl: trampoline.pl main.xtl
	$(pl) trampoline.pl extol2prolog main.xtl main.pl

trampoline.pl: main.xtl
	$(pl) boot.pl extol2prolog main.xtl trampoline.pl

boot.pl: main.xtl
	$(pl) boot.pl extol2prolog main.xtl newboot.pl
	mv newboot.pl boot.pl
