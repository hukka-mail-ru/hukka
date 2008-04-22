
PROG = tree
SRC = /home/sadovnikov/template

EXTRA_INCLUDES += \
		-I$(SRC)/include \




BLD = $(SRC)/bld

$(PROG) : $(PROG).o
	g++ $(DLL) -o $(BLD)/bin/$(PROG) $(BLD)/obj/$(PROG).o -L$(BLD)/bin -lcolor

$(PROG).o : $(PROG).cpp 
	g++ -g -Wall -Wno-long-long -fPIC -DLINUX -Wall -Wno-invalid-offsetof $(EXTRA_INCLUDES) -c $(PROG).cpp -o $(BLD)/obj/$(PROG).o 

clean: 
	rm $(BLD)/obj/$(PROG).o $(BLD)/bin/$(PROG).so
