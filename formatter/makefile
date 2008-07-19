DIR = parser
EXE = parser
SRCS = 	parser.cpp \
	formatter.cpp

EXTRA_INCLUDES 	+= -Iinclude \
	           -I$(SRCROOT)/third_party/boost \
	           -I$(SRCROOT)/common/include \
		   -I \

LIBPATHS	+= $(BLDROOT)/$(MTARGET)/lib 		

EXTRALIBS += debug color

REQUIRE_PCRE = 1

include $(MAKE_HOME)/makefile.incl
include $(MAKE_HOME)/makefile.exec

