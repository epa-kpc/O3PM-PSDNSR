FLGS = -convert big_endian -extend_source
#FLGS = -C -byteswapio -Bstatic

LIBS = 


TARGT = ptsmrg

OBJCTS = \
ptsmrg.o

ptsmrg: $(OBJCTS)
	ifort -o $(TARGT) $(FLGS) $(OBJCTS) $(LIBS)
.f.o	:
	ifort -c -o $@ $(FLGS) $<


