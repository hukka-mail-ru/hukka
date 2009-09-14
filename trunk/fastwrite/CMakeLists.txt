cmake_minimum_required(VERSION 2.4)

include_directories (src /usr/include/panel-2.0 /usr/include/gtk-2.0 /usr/include/libgnomeui-2.0 /usr/include/libbonoboui-2.0 /usr/lib/gtk-2.0/include /usr/include/atk-1.0 /usr/include/cairo /usr/include/pango-1.0 /usr/include/glib-2.0 /usr/lib/glib-2.0/include /usr/include/freetype2 /usr/include/libpng12 /usr/include/pixman-1 /usr/include/libart-2.0 /usr/include/gconf/2 /usr/include/gnome-keyring-1 /usr/include/libgnome-2.0 /usr/include/libgnomecanvas-2.0 /usr/include/gnome-vfs-2.0 /usr/lib/gnome-vfs-2.0/include /usr/include/orbit-2.0 /usr/include/libbonobo-2.0 /usr/include/bonobo-activation-2.0 /usr/include/libxml2 /usr/include/gail-1.0)  

set(CMAKE_CXX_FLAGS "-g -Wall")


add_executable (fastwrite src/fastwrite.cpp)

target_link_libraries (fastwrite panel-applet-2 gnomeui-2 SM ICE bonoboui-2 gnomevfs-2 gconf-2 gnomecanvas-2 gnome-2 popt art_lgpl_2 gtk-x11-2.0 gdk-x11-2.0 -latk-1.0 gdk_pixbuf-2.0 m pangocairo-1.0 pango-1.0 cairo bonobo-2 bonobo-activation gmodule-2.0 dl ORBit-2 gthread-2.0 rt gobject-2.0 glib-2.0) 


set_target_properties(fastwrite PROPERTIES COMPILE_FLAGS "-DORBIT2=1")
set_target_properties(fastwrite PROPERTIES LINK_FLAGS "-pthread")


