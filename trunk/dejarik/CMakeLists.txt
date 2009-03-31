cmake_minimum_required(VERSION 2.4)

include_directories (src/common src/logic src/ui) 
set(CMAKE_CXX_FLAGS "-g -Wall")
set(SRC src/logic/Player.cpp src/logic/Board.cpp src/logic/Game.cpp src/common/Cell.cpp src/common/System.cpp) 
set(SRC_UI src/ui/Glbasic.cpp src/ui/UI.cpp src/ui/Video.cpp src/ui/Animation.cpp src/ui/Window.cpp)

# DEJARIK executable
add_executable (dejarik src/logic/main.cpp ${SRC} ${SRC_UI})
target_link_libraries (dejarik GLES_CM) 
set_target_properties(dejarik PROPERTIES COMPILE_FLAGS "-D LINUX_BUILD -D OPENGL_ES_BUILD -D __lint__ -D __GL_FLOAT -D_GNU_SOURCE=1 -D_REENTRANT")
set_target_properties(dejarik PROPERTIES LINK_FLAGS "-L/usr/lib")

# UNIT TESTs 
add_executable (test_dejarik test/test.cpp src/ui/Animation.cpp ${SRC}) 
target_link_libraries (test_dejarik cppunit dl) 
set_target_properties(test_dejarik PROPERTIES COMPILE_FLAGS "-D UNIT_TESTS")