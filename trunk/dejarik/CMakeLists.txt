include_directories (src) 
set(CMAKE_CXX_FLAGS "-g -Wall")
set(SRC src/Player.cpp src/Board.cpp src/Game.cpp) 


# DEJARIK executable
add_executable (dejarik src/main.cpp ${SRC})
 
# UNIT TESTs 
add_executable (test_dejarik test/test.cpp ${SRC}) 
target_link_libraries (test_dejarik cppunit dl) 
set_target_properties(test_dejarik PROPERTIES COMPILE_FLAGS "-D UNIT_TESTS")

# UI
add_executable (ui_exe ui/ui.cpp)
target_link_libraries (ui_exe GL GLU SDL pthread) 
set_target_properties(ui_exe PROPERTIES COMPILE_FLAGS "-I/usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT")
set_target_properties(ui_exe PROPERTIES LINK_FLAGS "-L/usr/lib")
