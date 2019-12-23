FC = gfortran-8
FFLAGS = -O2 -Wall -Wextra -std=f2008 -pedantic
LDLIBS = -LQUADPACK -lquadpack
OBJS = $(patsubst %.f90, %.o, kinds.f90 $(wildcard *_interface.f90))

.PHONY : all quadpack tests clean deepclean

all : quadpack $(OBJS) tests

quadpack : 
	$(MAKE) -C QUADPACK

%.o : %.f90
	$(FC) -c $(FFLAGS) -o $@ $<

tests :
	$(MAKE) -C tests

clean :
	$(RM) $(OBJS)
	$(RM) *.mod
	$(MAKE) -C tests clean

deepclean : 
	$(MAKE) clean
	$(MAKE) -C QUADPACK clean
