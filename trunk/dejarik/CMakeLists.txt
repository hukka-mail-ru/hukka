cmake_minimum_required(VERSION 2.4)

include_directories (src/common src/logic src/ui) 
set(CMAKE_CXX_FLAGS "-g -Wall")
set(SRC src/logic/Player.cpp src/logic/Board.cpp src/logic/Game.cpp src/common/Cell.cpp) 
set(SRC_UI src/ui/Glbasic.cpp src/ui/UI.cpp src/ui/Video.cpp src/ui/Animation.cpp)

# DEJARIK executable
add_executable (dejarik src/logic/main.cpp ${SRC} ${SRC_UI})
target_link_libraries (dejarik GL SDL pthread) 
set_target_properties(dejarik PROPERTIES COMPILE_FLAGS "-D LINUX_BUILD -I/usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT")
set_target_properties(dejarik PROPERTIES LINK_FLAGS "-L/usr/lib")

# DEJARIK executable
add_executable (win_dejarik src/logic/main.cpp ${SRC} ${SRC_UI})
target_link_libraries (win_dejarik GL SDL pthread) 
set_target_properties(win_dejarik PROPERTIES COMPILE_FLAGS "-D WIN_BUILD -IC:\usr\include\SDL -IC:\usr\include\boost -D_GNU_SOURCE=1 -D_REENTRANT")
set_target_properties(win_dejarik PROPERTIES LINK_FLAGS "-L/usr/lib")
 

# UNIT TESTs 
add_executable (test_dejarik test/test.cpp src/ui/Animation.cpp ${SRC}) 
target_link_libraries (test_dejarik cppunit dl) 
set_target_properties(test_dejarik PROPERTIES COMPILE_FLAGS "-D UNIT_TESTS")