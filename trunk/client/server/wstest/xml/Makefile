CXX= g++
CXXFLAGS = -Wall

ifeq ($(ver),debug)
override CXXFLAGS := -O0 -g3
endif

ifeq ($(ver),release)
override CXXFLAGS := -O3
endif

all: xml

xml: main.o setting.o mysyslog.o
	$(CXX) -L/usr/local/lib -lnxml main.o setting.o mysyslog.o -o xml

main.o: main.cpp
	$(CXX) -c -MD -I/usr/local/include $(CXXFLAGS) main.cpp

setting.o: setting.cpp
	$(CXX) -c -MD -I/usr/local/include $(CXXFLAGS) setting.cpp

mysyslog.o: mysyslog.cpp
	$(CXX) -c -MD $(CXXFLAGS) mysyslog.cpp

.PHONY: clean

clean:
	rm -f xml *.o *.d