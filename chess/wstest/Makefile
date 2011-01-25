source_dirs := . tools
search_wildcards := $(addsuffix /*.cpp,$(source_dirs)) 

ifeq ($(ver),debug)
override compile_flags := -O0 -g3 -DGMS_DEBUG
endif

ifeq ($(ver),release)
override compile_flags := -O3
endif

all: wstest

wstest: $(notdir $(patsubst %.cpp,%.o,$(wildcard $(search_wildcards))))
		g++ $^ -lpthread -o $@

VPATH := $(source_dirs)
     
%.o: %.cpp
		g++ -c -MD $(addprefix -I ,$(source_dirs)) $(compile_flags) $<

include $(wildcard *.d) 

clean:
		rm -Rf *.o *.d wstest