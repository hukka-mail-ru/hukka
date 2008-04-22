
PROG = speed
SRC = /home/sadovnikov/$(PROG)
LIB = # -shared -fPIC  # set DLL = -shared -fPIC if you want to build a DLL

EXTRA_INCLUDES += -I$(SRC)/include \



BLD = $(SRC)/bld

$(PROG) : $(PROG).o ping.o
	g++ $(LIB) -o $(BLD)/bin/$(PROG) $(BLD)/obj/$(PROG).o $(BLD)/obj/ping.o


$(PROG).o : $(PROG).cpp 
	g++ -g -Wall -Wno-long-long -fPIC -DLINUX -Wall -Wno-invalid-offsetof $(EXTRA_INCLUDES) -c $(PROG).cpp -o $(BLD)/obj/$(PROG).o

ping.o : ping.cpp
	g++ -g -Wall -Wno-long-long -fPIC -DLINUX -Wall -Wno-invalid-offsetof $(EXTRA_INCLUDES) -c ping.cpp -o $(BLD)/obj/ping.o


clean: 
	rm $(BLD)/obj/$(PROG).o $(BLD)/obj/ping.o $(BLD)/bin/$(PROG)
