#include <gdk/gdk.h>
#include <stdlib.h>


// Get the pixmap of background (root) window
GdkPixmap* get_root_pixmap (void)
{
	GdkAtom actual_property_type = 0;
	gint actual_format = 0;
	gint actual_length = 0;
	guint* data = 0;
	gboolean res = FALSE;

	res = gdk_property_get (
			gdk_screen_get_root_window (gdk_screen_get_default()),
			gdk_atom_intern ("_XROOTPMAP_ID", TRUE),// 	the property to retrieve. 
			GDK_TARGET_PIXMAP, // 	the desired property type
			0,
			G_MAXLONG,
			FALSE,
			&actual_property_type,
			&actual_format,
			&actual_length,
			(gpointer)&data);

	if(!res) {	
		g_print("gdk_property_get failed");
		exit (1);
	}

	GdkPixmap *pixmap = gdk_pixmap_foreign_new_for_display (gdk_display_get_default(), data[0]);
	if (!pixmap) {
		g_print("gdk_pixmap_foreign_new_for_display failed");
		exit (1);
	}

	gdk_drawable_set_colormap (pixmap, gdk_colormap_get_system ());
	g_free (data);

	return pixmap;
}

// MAIN
int main (int argc, char *argv[]) 
{
	g_print("Hello\n"); 

	GdkWindow *window;
	GdkWindowAttr attributes;
	gint attributes_mask;
	GMainLoop *mainloop;

	if (!gdk_init_check (&argc, &argv)) {
		return FALSE;
	}

	gdk_rgb_init();

	attributes.window_type = GDK_WINDOW_TOPLEVEL;
	attributes.width = 400;
	attributes.height = 400;
	attributes.wclass = GDK_INPUT_OUTPUT;
	attributes.colormap = gdk_rgb_get_cmap ();
	attributes_mask = GDK_WA_COLORMAP;

	// create my window.
	window = gdk_window_new (NULL, &attributes, attributes_mask);
	gdk_window_show (window);

	// Copying the root window contents (background) into my window
        gdk_draw_drawable (window,
			   gdk_gc_new(window),
                           get_root_pixmap(),
                           0,0,
                           0,0,
			   300,300);


	// Create a new main loop object. 
	mainloop = g_main_new (TRUE);
	g_main_run (mainloop);


	return 0;
}

