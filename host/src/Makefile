
PAS_READLINE=../pas-readline
PAS_TCL=../pas-tcl
PAS_LIBUSB=../pas-libusb

FPC=fpc
FPC_OPT = -Fu$(PAS_READLINE)/src -Fu$(PAS_TCL)/src -Fu$(PAS_LIBUSB)/src

all: eztool man/man1/eztool.1

eztool: eztool.pas
	$(FPC) $(FPC_OPT) -o$@ $<

man/man1/eztool.1: eztool.pas
	# Strange, if just written as script, make starts it using /bin/sh,
	# but this shell does not provide the required features. Therefore
	# we directly start it with bash. :-)
	bash $(PAS_TCL)/bin/genman.sh $<

clean:
	rm -f eztool *.ppu *.o *.compiled *.res *~
