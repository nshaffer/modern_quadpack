FC = gfortran-8
FFLAGS = -O2 -Wall -Wextra -std=f2008 -pedantic -I..
LDLIBS = -L../QUADPACK -lquadpack
OBJS = $(wildcard ../*.o) quadpack_test_cases.o test_quadpack.o
TARGET = test_quadpack

.PHONY : all target

all : $(TARGET)

$(TARGET) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDLIBS)

%.o : %.f90
	$(FC) -c $(FFLAGS) -o $@ $<

clean :
	$(RM) $(TARGET)
	$(RM) $(OBJS)
	$(RM) *.mod
