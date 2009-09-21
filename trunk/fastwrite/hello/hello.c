#include <gdk/gdk.h>
#include <stdlib.h>
#include <gtk/gtk.h>

#define INIT_XPOS	200
#define INIT_YPOS	200

#define INIT_WIDTH	100
#define INIT_HEIGHT	200

gchar *Text;

// =========================================================================================================
// Get pixmap of the background (root) window
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

// =========================================================================================================
// Redraw background and text subscription
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
		           INIT_XPOS,INIT_YPOS, // src position
		           0,0,           // dst position
			   INIT_WIDTH,INIT_HEIGHT);

	// Draw subscription
	PangoLayout* text_layout = gtk_widget_create_pango_layout (widget, Text);

	PangoRectangle link, logical;
        pango_layout_get_pixel_extents(text_layout, &link, &logical);
        pango_layout_set_alignment(text_layout, PANGO_ALIGN_LEFT);

        gdk_draw_layout(widget->window,
                        widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
                        0, 0,
                        text_layout);


	return TRUE;
}

// =========================================================================================================
// Just close the application
void on_close_window (GtkWidget *widget, GdkEventExpose *event, gpointer data) 
{
	gtk_main_quit ();
}


// =========================================================================================================
// Hide edit control, grab its text, and show label
void on_focus_out_window (GtkWidget *widget, GdkEventExpose *event, gpointer textview) 
{
	gtk_widget_hide(textview);

	// Obtain text from textview
	GtkTextBuffer* buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));
	
	GtkTextIter start;
	GtkTextIter end;
	gtk_text_buffer_get_start_iter (buffer, &start);
	gtk_text_buffer_get_end_iter (buffer, &end);

	Text = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
}

// =========================================================================================================
// Show edit control
void on_focus_in_window (GtkWidget *widget, GdkEventExpose *event, gpointer textview) 
{
	gtk_widget_show(textview);
}


// =========================================================================================================
int main(int argc,  char *argv[])
{
	gtk_init (&argc, &argv);

	GtkWidget* window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

	gtk_widget_set_size_request (window, INIT_WIDTH, INIT_HEIGHT);
	gtk_window_set_decorated(GTK_WINDOW(window), FALSE);
	gtk_window_move(GTK_WINDOW(window), INIT_XPOS, INIT_YPOS);

	GtkWidget* textview = gtk_text_view_new();
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW (textview), TRUE);
	gtk_container_add (GTK_CONTAINER (window), GTK_WIDGET(textview));

	gtk_signal_connect (GTK_OBJECT (window), "expose_event",    GTK_SIGNAL_FUNC (on_expose_window), NULL);
	gtk_signal_connect (GTK_OBJECT (window), "delete_event",    GTK_SIGNAL_FUNC (on_close_window), NULL);
	gtk_signal_connect (GTK_OBJECT (window), "focus_out_event", GTK_SIGNAL_FUNC (on_focus_out_window), textview);
	gtk_signal_connect (GTK_OBJECT (window), "focus_in_event",  GTK_SIGNAL_FUNC (on_focus_in_window), textview);
	 


	gtk_widget_show_all (window);



	gtk_main ();

	return 0;
}

