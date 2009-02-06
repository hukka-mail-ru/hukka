# Add executable called "helloDemo" that is built from the source files
# "demo.cxx" and "demo_b.cxx". The extensions are automatically found.
add_executable (dejarik src/dejarik.cpp src/Board.cpp)
 
add_executable (test_dejarik test/test.cpp src/Board.cpp) 
target_link_libraries (test_dejarik cppunit dl) 
include_directories (src) 

set(CMAKE_CXX_FLAGS "-g -Wall")
