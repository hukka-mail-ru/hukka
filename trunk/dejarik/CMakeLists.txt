cmake_minimum_required(VERSION 2.4)

include_directories (src/logic src/ui src/system PVRTex PVRTools PVRTools/OGLES)  
set(CMAKE_CXX_FLAGS "-g -Wall")
set(SRC_LOGIC src/logic/Player.cpp src/logic/Board.cpp src/logic/Game.cpp) 
set(SRC_SYSTEM src/system/System.cpp src/system/Window.cpp src/system/BMP.cpp) 
set(SRC_UI src/ui/UI.cpp src/ui/Video.cpp src/ui/Animation.cpp )

# DEJARIK executable
add_executable (dejarik src/system/main.cpp ${SRC_LOGIC} ${SRC_SYSTEM} ${SRC_UI})
target_link_libraries (dejarik GLES_CM PVRTex) 
set_target_properties(dejarik PROPERTIES COMPILE_FLAGS "-D LINUX_BUILD -D OPENGL_ES_BUILD -D __lint__ -D __GL_FLOAT -D_GNU_SOURCE=1 -D_REENTRANT")
set_target_properties(dejarik PROPERTIES LINK_FLAGS "-L/usr/lib")

# UNIT TESTs 
add_executable (test_dejarik test/test.cpp src/ui/Animation.cpp ${SRC_LOGIC}) 
target_link_libraries (test_dejarik cppunit dl) 
set_target_properties(test_dejarik PROPERTIES COMPILE_FLAGS "-D UNIT_TESTS")