
DIR = speed
EXE = speed
SRCS = 	speed.cpp \
	ping.cpp \
	ipconfig.cpp

EXTRA_INCLUDES 	+= -Iinclude \
			-I$(SRCROOT)/debug/include \
			-I$(SRCROOT)/color/include \
			-I$(SRCROOT)/third_party/boost

LIBPATHS	+= $(BLDROOT)/$(MTARGET)/lib 

LIBS += debug color


include $(MAKE_HOME)/makefile.incl
include $(MAKE_HOME)/makefile.exec

