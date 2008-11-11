

#CXXFLAGS =	-O2 -g -Wall -fmessage-length=0 
CXXFLAGS =-Wall -I../src


OBJS =		test.o

TARGET =	test

$(TARGET):	$(OBJS)
	$(CXX) -o  $(TARGET) $(OBJS)

all:	$(TARGET)

clean:
	rm -f $(OBJS) $(TARGET)
