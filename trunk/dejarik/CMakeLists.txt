set(SRC src/Board.cpp src/Piece.cpp src/Player.cpp) 


# DEJARIK executable
add_executable (dejarik src/dejarik.cpp ${SRC})
 
# UNIT TESTs 
add_executable (test_dejarik test/test.cpp ${SRC}) 
target_link_libraries (test_dejarik cppunit dl) 

include_directories (src include) 
set(CMAKE_CXX_FLAGS "-g -Wall")
