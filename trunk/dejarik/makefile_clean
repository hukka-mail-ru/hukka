SRCROOT = /home/ssy/devel/hukka

INCLUDES = \

CXXFLAGS =	-O2 -g -Wall -fmessage-length=0 $(INCLUDES) 

LIBPATH = 

LIBS = 

SRCS = dejarik.cpp Board.cpp

OBJDIR = ../bld/
DEPDIR = ../bld/

PROG = ../bld/dejarik

$(OBJDIR)/%.o: %.cpp
	mkdir -p $(OBJDIR)
	$(CXX) $(CXXFLAGS) -c $< -o $(OBJDIR)/$(patsubst %.cpp,%.o,$(notdir $<))
	
$(DEPDIR)/%.d: %.cpp
	mkdir -p $(DEPDIR)
	$(SHELL) -ec '$(CXX) -MM $(CXXFLAGS) $< \
	| sed '\''s/\($*\)\.o[ :]*/.\/$(OBJDIR)\/\1.o .\/$(DEPDIR)\/$(patsubst %.cpp,%.d,$(notdir $<)) : /g'\'' > $@; \
	[ -s $@ ] || rm -f $@'

OBJS = $(patsubst %.cpp,$(OBJDIR)/%.o,$(notdir $(SRCS)))
DEPS = $(patsubst %.cpp,$(DEPDIR)/%.d,$(notdir $(SRCS)))

$(PROG):	$(OBJS)
	$(CXX) -o  $(PROG) $(OBJS) $(LIBPATH) $(LIBS)
	


all:	$(PROG)

clean:
	rm -f $(OBJS) $(PROG)
