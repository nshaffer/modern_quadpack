FC = gfortran
FFLAGS =-O2 -std=legacy
ARFLAGS = -rcs

OBJ = $(patsubst %.f,%.o,$(wildcard *.f))
TARGET = libquadpack.a

.PHONY : all ex clean

all : $(TARGET) ex

$(TARGET) : $(OBJ)
	$(AR) $(ARFLAGS) $@ $^

ex : 
	$(MAKE) -C ex

%.o : %.f
	$(FC) $(FFLAGS) -c $^


clean : 
	$(RM) $(TARGET)
	$(RM) $(OBJ)
	$(MAKE) -C ex clean
