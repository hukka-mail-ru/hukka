include_directories (src/logic) 
set(CMAKE_CXX_FLAGS "-g -Wall")
set(SRC src/logic/Player.cpp src/logic/Board.cpp src/logic/Game.cpp) 


# DEJARIK executable
add_executable (dejarik src/logic/main.cpp ${SRC})
 
# UNIT TESTs 
add_executable (test_dejarik test/test.cpp ${SRC}) 
target_link_libraries (test_dejarik cppunit dl) 
set_target_properties(test_dejarik PROPERTIES COMPILE_FLAGS "-D UNIT_TESTS")

# UI
add_executable (ui_exe src/ui/ui.cpp)
target_link_libraries (ui_exe GL GLU SDL pthread) 
set_target_properties(ui_exe PROPERTIES COMPILE_FLAGS "-I/usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT")
set_target_properties(ui_exe PROPERTIES LINK_FLAGS "-L/usr/lib")
