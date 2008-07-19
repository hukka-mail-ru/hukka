DIR = test_formatter
EXE = test_formatter
SRCS = 	test_formatter.cpp \
	../parser.cpp

EXTRA_INCLUDES 	+= -I$(SRCROOT)/common/include \
                   -Iinclude \
	           -I$(SRCROOT)/third_party/boost \
                   -I$(SRCROOT)/third_party/cppunit/include \
                   -I$(SRCROOT)/formatter


LIBPATHS	+= $(BLDROOT)/$(MTARGET)/lib 

EXTRALIBS += cppunit dl	color debug	

REQUIRE_PCRE = 1

include $(MAKE_HOME)/makefile.incl
include $(MAKE_HOME)/makefile.exec

