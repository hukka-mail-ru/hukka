include_directories (/home/hukka/hukka_project/third_party/boost/)

# Add executable called "helloDemo" that is built from the source files
# "demo.cxx" and "demo_b.cxx". The extensions are automatically found.
add_executable (dejarik dejarik.cpp Board.cpp) 