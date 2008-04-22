
DIR = speed
EXE = speed
SRCS = 	speed.cpp \
	ping.cpp 

EXTRA_INCLUDES 	+= -Iinclude \
			-I$(SRCROOT)/debug/include \
			-I$(SRCROOT)/color/include

EXTRALIBPATHS	+= $(BLDROOT)/$(MTARGET)/lib 

EXTRALIBS += debug color


include $(MAKE_HOME)/makefile.incl
include $(MAKE_HOME)/makefile.exec

