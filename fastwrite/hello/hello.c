#include <gdk/gdk.h>
#include <stdlib.h>
#include <gtk/gtk.h>

// Get the pixmap of background (root) window
GdkPixmap* get_root_pixmap (void)
{
	GdkAtom actual_property_type = 0;
	gint actual_format = 0;
	gint actual_length = 0;
	guint* data = 0;
	gboolean res = FALSE;

	// Get the pointer to the root window
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




#define INIT_X		200
#define INIT_Y		200
#define INIT_WIDTH	400
#define INIT_HEIGHT	600


// =========================================================================================================
gboolean on_expose_window (GtkWidget *widget, GdkEventExpose *event, gpointer data) 
{
	// Draw background
	GdkWindow* gdk_window = GTK_WIDGET(widget)->window;

	GdkGC* gc = gdk_gc_new(gdk_window);
	if(!gc) {
		g_print("ERROR: gdk_gc_new failed\n");
		return 1;
	}

	gdk_draw_drawable (gdk_window,
			   gc,
		           get_root_pixmap(),
		           INIT_X,INIT_Y,
		           0,0,
			   INIT_WIDTH,INIT_HEIGHT);

	// Draw subscription
 	PangoRectangle link, logical;

	static  int bg_width = 200 ;
	static  int bg_height = 200;
	static  int logo_height = 200;

	gchar *version = g_strdup_printf ("version");
        PangoLayout* version_layout = gtk_widget_create_pango_layout (widget, version);

        pango_layout_set_alignment(version_layout, PANGO_ALIGN_RIGHT);
        pango_layout_get_pixel_extents(version_layout, &link, &logical);

        gdk_draw_layout(widget->window,
                        widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
                        bg_width - logical.width, logo_height,
                        version_layout);


	return TRUE;
}

// =========================================================================================================
void on_close_window (GtkWidget *widget, GdkEventExpose *event, gpointer data) 
{
	gtk_main_quit ();
}

// =========================================================================================================
void on_focus_out_window (GtkWidget *widget, GdkEventExpose *event, gpointer textview) 
{
	gtk_container_remove (GTK_CONTAINER (widget), GTK_OBJECT(textview));
}


// =========================================================================================================
int main(int argc,  char *argv[])
{
	gtk_init (&argc, &argv);

	GtkWidget* window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_container_set_border_width (GTK_CONTAINER (window), 7);

	gtk_widget_set_size_request (window, 400, 600);
	gtk_window_set_decorated(window, FALSE);

	GtkWidget* textview = gtk_text_view_new();
	
	gtk_container_add (GTK_CONTAINER (window), textview);

	gtk_signal_connect (GTK_OBJECT (window), "expose_event", GTK_SIGNAL_FUNC (on_expose_window), NULL);
	gtk_signal_connect (GTK_OBJECT (window), "delete_event", GTK_SIGNAL_FUNC (on_close_window), NULL);
	gtk_signal_connect (GTK_OBJECT (window), "focus_out_event", GTK_SIGNAL_FUNC (on_focus_out_window), textview);
	 


	gtk_widget_show_all (window);



	gtk_main ();

	return 0;
}

