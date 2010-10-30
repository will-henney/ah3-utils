F90C=gfortran
F90FLAGS=-O3
LFITS=/usr/local/lib/libcfitsio.a
-include local.mk # optionally override compiler and flags with this file

EXECS= \
     ah3tofits ah3doctor ah3test ah3rebin

all: $(EXECS)

clean: 
	rm -fr $(EXECS) *.o *.mod

.SUFFIXES: .f90
.f90.o:  
	$(F90C) $(F90FLAGS) -c $<

ah3test.o: ah3mod.o
ah3doctor.o: ah3mod.o
ah3rebin.o: ah3mod.o
ah3tofits.o: ah3mod.o wfitsutils.o

ah3tofits: ah3mod.o ah3tofits.o wfitsutils.o
	$(F90C) $(F90FLAGS) -o $@ $^ $(LFITS)

ah3uniform: ah3mod.o ah3uniform.o
	$(F90C) $(F90FLAGS) -o $@ $^

ah3doctor: ah3mod.o ah3doctor.o
	$(F90C) $(F90FLAGS) -o $@ $^

ah3rebin: ah3mod.o ah3rebin.o
	$(F90C) $(F90FLAGS) -o $@ $^

ah3test: ah3mod.o ah3test.o
	$(F90C) $(F90FLAGS) -o $@ $^

