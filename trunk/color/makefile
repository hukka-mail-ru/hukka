
PROG = color
SRC = /home/sadovnikov/template
LIB = -shared -fPIC  # set DLL = -shared -fPIC if you want to build a DLL

EXTRA_INCLUDES += \
		-I$(SRC)/include \




BLD = $(SRC)/bld

$(PROG) : $(PROG).o
	g++ $(LIB) -o $(BLD)/bin/lib$(PROG).so $(BLD)/obj/$(PROG).o

$(PROG).o : $(PROG).cpp 
	g++ -g -Wall -Wno-long-long -fPIC -DLINUX -Wall -Wno-invalid-offsetof $(EXTRA_INCLUDES) -c $(PROG).cpp -o $(BLD)/obj/$(PROG).o 

clean: 
	rm $(BLD)/obj/$(PROG).o $(BLD)/bin/lib$(PROG).so
