set(src/Player.cpp src/Board.cpp src/Game.cpp) 


# DEJARIK executable
add_executable (dejarik src/dejarik.cpp src/Player.cpp src/Board.cpp src/Game.cpp)
 
# UNIT TESTs 
#add_executable (test_dejarik test/test.cpp ${SRC}) 
add_executable (test_dejarik test/test.cpp src/Player.cpp src/Board.cpp src/Game.cpp)
target_link_libraries (test_dejarik cppunit dl) 

include_directories (src) 
set(CMAKE_CXX_FLAGS "-g -Wall")
