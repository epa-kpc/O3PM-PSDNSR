######################################################################
#
#   Makefile for UAM2NCF
#
#   DIRECTORIES:
#
#     IOAPI_INC  - I/O-API include file directory
#     IOAPI_LIB  - I/O-API library file directory
#     NETCDF_LIB - NetCDF library file directory
#
######################################################################

IOAPI_INC  =  /home/wdx/lib_sol/src/ioapi_3.1/ioapi/fixed_src
IOAPI_LIB  = /home/wdx/lib_sol/x86_64/ifc/ioapi_3.1/Linux2_x86_64ifort
NETCDF_LIB = /home/wdx/lib_sol/x86_64/ifort/netcdf/lib

######################################################################

FC  = ifort
#OPT = -O2 -pc 64 -Mdalign -Mnoframe -byteswapio -Mlfs -Wl,-Bstatic
OPT = -convert big_endian
LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf -lnetcdff
INC = -I$(IOAPI_INC)
EXE = uam2ncf


.PHONY: all clean

all : $(EXE)


$(EXE) : $(EXE).o
	$(FC) $(OPT) -o $@ $^ $(LIB)

.f.o :
	$(FC) $(INC) $(OPT) -c $<

clean :
	rm -f *.o


