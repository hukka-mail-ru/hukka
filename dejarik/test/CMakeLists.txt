include_directories (/home/hukka/hukka_project/third_party/boost/ ~/hukka_project/third_party/cppunit/)

add_library(cppunit dl)

# Add executable called "helloDemo" that is built from the source files
# "demo.cxx" and "demo_b.cxx". The extensions are automatically found.
add_executable (test_dejarik dejarik_test.cpp) 