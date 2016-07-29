FC=g95 -ftrace=frame -ftrace=full

all: main_force.exe

OBJ= nrtype.o nrutil.o gaussj.o module.o number_of_line.o get_data_number.o \
     main_force.o get_data_matrix.o fitting.o fit_polynomial.o

main_force.exe: $(OBJ)
	$(FC) $^ -o $@

%.o: %.f90
	$(FC) -c $<

clean:
	del *.o *.mod

