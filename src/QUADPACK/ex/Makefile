FC = gfortran-8
FFLAGS = -g -Og -std=f2008 -fcheck=all -ffpe-trap=invalid,zero,overflow

LDLIBS = -lquadpack
LDFLAGS = -L..

TARGET = $(patsubst %.f90,%,$(wildcard ex_*.f90))


ex : $(TARGET)
	

ex_% : ex_%.f90 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)
	./$@ | tee -a testlog

.PHONY: clean

clean : 
	$(RM) $(TARGET)
	$(RM) -r *.dSYM
	$(RM) testlog
