
PROG = tree
SRC = ~/hukka_project

EXTRA_INCLUDES += \
		-I$(SRC)/include \




BLD = $(SRC)/bld/linux-2-6-x86-threaded-gcc

$(PROG) : $(PROG).o
	g++ $(DLL) -o $(BLD)/bin/$(PROG) $(BLD)/obj/$(PROG).o 

$(PROG).o : $(PROG).cpp 
	g++ -g -Wall -Wno-long-long -fPIC -DLINUX -Wall -Wno-invalid-offsetof $(EXTRA_INCLUDES) -c $(PROG).cpp -o $(BLD)/obj/$(PROG).o 

clean: 
	rm $(BLD)/obj/$(PROG).o $(BLD)/bin/$(PROG).so
