cmake_minimum_required(VERSION 2.4)

include_directories (src)  
set(CMAKE_CXX_FLAGS "-g -Wall")

add_executable (molotok src/System.cpp src/main.cpp src/UI.cpp src/Video.cpp src/BMP.cpp)
target_link_libraries (molotok GL SDL) 
set_target_properties(molotok PROPERTIES COMPILE_FLAGS "-D __lint__ -D __GL_FLOAT -D_GNU_SOURCE=1 -D_REENTRANT")
set_target_properties(molotok PROPERTIES LINK_FLAGS "-L/usr/lib")

