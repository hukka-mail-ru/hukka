TARGET :=	test

source_dirs := . .. include ../engine 

search_wildcards := $(addsuffix /*.cpp, $(source_dirs))

include_dir   := ../../libs/sql ../../libs/header ../../libs/tools ../../libs/server ../../libs/socket

GCC              := g++ 

ifndef ver
ver:=debug
endif

ifeq ($(ver),debug)
override compile_flags := -O0 -g
endif

ifeq ($(ver),release)
override compile_flags := -O3 -funroll-loops -fomit-frame-pointer
endif

$(TARGET): $(notdir $(patsubst %.cpp, %.o, $(wildcard $(search_wildcards))))
	$(GCC) $^ -o $@ 

VPATH := $(source_dirs)

.cpp.o:
	$(GCC) -c -MD $(addprefix -I, $(source_dirs)) $(addprefix -I, $(include_dir)) $(compile_flags) $<
	
include $(wildcard *.d)

clean:
	rm -f *.o *.d $(TARGET)
