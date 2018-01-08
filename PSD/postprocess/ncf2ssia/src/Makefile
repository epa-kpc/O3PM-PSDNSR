.SUFFIXES:
.SUFFIXES: .f .F .o .x


###### FOR portland group fortran compiler on hpcc #######
FC=ifort
FFLAGS= -extend_source -convert big_endian
LFLAGS=
LIBDIR=-L/home/wdx/lib_sol/x86_64/ifc-13.1/ioapi_3.1/Linux2_x86_64ifort  -L/home/wdx/lib_sol/x86_64/ifc-13.1/netcdf/lib
LLIBS=-lioapi -lnetcdf -lnetcdff


INCLUDES = /home/wdx/lib/src/ioapi_3/ioapi/fixed_src 
SRC = .
BIN = .

SRCNAME = ncf2ssia

OBJS = $(SRCNAME).o

all: $(OBJS) $(SRCNAME).x

.F.o:
	$(FC) $(FFLAGS) -I$(INCLUDES) -c $(SRC)/$<

.f.o:
	$(FC) $(FFLAGS) -I$(INCLUDES) -c $(SRC)/$<

.o.x:
	$(FC) $(LFLAGS) -o $(BIN)/$@ $< $(LIBDIR) $(LLIBS)


clean:
	- rm -f *.o
	- rm $(BIN)/*.x
