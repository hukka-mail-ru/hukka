include_directories (src) 
set(CMAKE_CXX_FLAGS "-g -Wall")
set(SRC src/Player.cpp src/Board.cpp src/Game.cpp) 


# DEJARIK executable
add_executable (dejarik src/main.cpp ${SRC})
 
# UNIT TESTs 
add_executable (test_dejarik test/test.cpp ${SRC}) 
target_link_libraries (test_dejarik cppunit dl) 
set_target_properties(test_dejarik PROPERTIES COMPILE_FLAGS "-D UNIT_TESTS")
