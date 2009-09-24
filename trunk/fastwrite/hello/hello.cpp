#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <stdlib.h>
#include <fstream>
#include <string>


using namespace std;

#define DEFAULT_XPOS        200
#define DEFAULT_YPOS        200

#define DEFAULT_WIDTH        300
#define DEFAULT_HEIGHT        200

#define TEXTFILE        "save.txt"
#define CONFFILE        "config.txt"

// Main window and its properties
GtkWidget* Window;
gint XPos = DEFAULT_XPOS;
gint YPos = DEFAULT_YPOS;
gint Width = DEFAULT_WIDTH;
gint Height = DEFAULT_HEIGHT;

// =========================================================================================================
// Obtain text from textview
gchar* get_text_of(GtkTextView* textview)
{
        GtkTextBuffer* buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textview));

        GtkTextIter start;
        GtkTextIter end;
        gtk_text_buffer_get_start_iter (buffer, &start);
        gtk_text_buffer_get_end_iter (buffer, &end);

        return gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
}

// =========================================================================================================
// Load window parameters from config
void  load_config(const char* filename)
{
        ifstream in(filename);
        string str;
        char dummy[255];

        while(getline(in, str)) {
                if(str.find("XPos") != string::npos) 
                        sscanf(str.c_str(), "%s\t%d", dummy, &XPos);
                else if(str.find("YPos") != string::npos) 
                        sscanf(str.c_str(), "%s\t%d", dummy, &YPos);
                else if(str.find("Width") != string::npos) 
                        sscanf(str.c_str(), "%s\t%d", dummy, &Width);
                else if(str.find("Height") != string::npos) 
                        sscanf(str.c_str(), "%s\t%d", dummy, &Height);
                else
                        g_print("Unknown parameter: %s\n", str.c_str());       

        }
}


// =========================================================================================================
// Save window parameters into config
void save_config(const char* filename)
{
        ofstream out(filename);

        out << "XPos\t" << XPos << endl;
        out << "YPos\t" << YPos << endl;
        out << "Width\t" << Width << endl;
        out << "Height\t" << Height << endl;
}


// =========================================================================================================
// Read the saved text if exists, and populate the text edit with it
void load_text(const char* filename, GtkTextView* textview)
{
        ifstream in(TEXTFILE);
        string text;
        string s;
        while(getline(in, s)) {
                text += s;
                text += "\n";
        }
        
        GtkTextBuffer* buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW (textview));
        gtk_text_buffer_set_text(buffer, text.c_str(), text.length() );
}

// =========================================================================================================
// Saved text into the file
void save_text(const char* filename, GtkTextView* textview)
{
        ofstream out(TEXTFILE);
        out << get_text_of(GTK_TEXT_VIEW (textview));
}

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
                        gdk_atom_intern ("_XROOTPMAP_ID", TRUE),//         the property to retrieve. 
                        GDK_TARGET_PIXMAP, //         the desired property type
                        0,
                        G_MAXLONG,
                        FALSE,
                        &actual_property_type,
                        &actual_format,
                        &actual_length,
                        (guchar**)&data);

        if(!res) {        
                g_print("gdk_property_get failed");
                return 0;
        }

        GdkPixmap *pixmap = gdk_pixmap_foreign_new_for_display (gdk_display_get_default(), data[0]);
        if (!pixmap) {
                g_print("gdk_pixmap_foreign_new_for_display failed");
                return 0;
        }

        gdk_drawable_set_colormap (pixmap, gdk_colormap_get_system ());
        g_free (data);

        return pixmap;
}


// =========================================================================================================
// Redraw background and the text from edit control
gboolean on_window_expose (GtkWidget *widget, GdkEventExpose *event, gpointer textview) 
{
        // Draw background
        GdkWindow* gdk_window = widget->window;

        GdkGC* gc = gdk_gc_new(gdk_window);
        if(!gc) {
                g_print("ERROR: gdk_gc_new failed\n");
                return FALSE;
        }

        gtk_window_get_position(GTK_WINDOW(Window), &XPos, &YPos);

        gdk_draw_drawable (gdk_window,
                           gc,
                           get_root_pixmap(),
                           XPos,YPos, // src position
                           0,0, // dst position
                           DEFAULT_WIDTH,DEFAULT_HEIGHT);

        // Draw text
        PangoLayout* text_layout = gtk_widget_create_pango_layout (widget, get_text_of(GTK_TEXT_VIEW (textview)));

        //PangoRectangle link, logical;
       // pango_layout_get_pixel_extents(text_layout, &link, &logical);
        pango_layout_set_alignment(text_layout, PANGO_ALIGN_LEFT);

        gdk_draw_layout(widget->window,
                        widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
                        0, 0,
                        text_layout);


        return TRUE;
}

// =========================================================================================================
// Save text and close the application
void on_window_close (GtkWidget *widget, GdkEventExpose *event, gpointer textview) 
{
        save_text(TEXTFILE, GTK_TEXT_VIEW (textview));
        save_config(CONFFILE);

        gtk_main_quit ();
}


// =========================================================================================================
// Hide edit control
void on_window_focus_out (GtkWidget *widget, GdkEventExpose *event, gpointer textview) 
{
        g_print("on_window_focus_out\n");
      //  gtk_window_set_decorated(GTK_WINDOW(Window), FALSE);
        gtk_widget_hide(GTK_WIDGET (textview));
}

// =========================================================================================================
// Show edit control if the window is focused intentionally
static gboolean on_window_button_press (GtkWidget *event_box, GdkEventButton *event,  gpointer textview)
{
        g_print("on_window_button_press\n");

        gtk_widget_show(GTK_WIDGET  (textview));
        gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW (textview), TRUE);
      //  gtk_window_set_decorated(GTK_WINDOW(Window), TRUE);
     //   gtk_widget_show_all (Window);
        return FALSE;
}


// =========================================================================================================
// Don't let window to be minimized
// Here we got the focus on the window as a side-effect. 
void on_window_changed_state (GtkWidget *widget, GdkEvent *event, gpointer textview) 
{
        GdkWindowState new_state = event->window_state.new_window_state;

        if ((new_state & GDK_WINDOW_STATE_ICONIFIED) != 0) {
                gdk_window_focus(Window->window, gtk_get_current_event_time ());
        }
}



// =========================================================================================================
int main(int argc,  char *argv[])
{
        gtk_init (&argc, &argv);
     
        load_config(CONFFILE);

        // Main window
        Window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_widget_set_size_request (Window, Width, Height);
        gtk_window_set_decorated(GTK_WINDOW(Window), FALSE);
        gtk_window_set_keep_below(GTK_WINDOW(Window), TRUE);
        gtk_window_move(GTK_WINDOW(Window), XPos, YPos);

        // Event box
        GtkWidget* event_box = gtk_event_box_new ();
        gtk_container_add (GTK_CONTAINER (Window), event_box);

        // Text edit view
        GtkWidget* textview = gtk_text_view_new();
        GtkWrapMode wrap = GTK_WRAP_WORD; // see pango_get_log_attrs()!
        gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW (textview), wrap);
        gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW (textview), TRUE);
        gtk_container_add (GTK_CONTAINER (event_box), GTK_WIDGET(textview));

        // Read the saved text if exists, and populate the text edit with it
        load_text(TEXTFILE, GTK_TEXT_VIEW (textview));


        // Events
        gtk_signal_connect (GTK_OBJECT (event_box), "expose_event",    GTK_SIGNAL_FUNC (on_window_expose), textview);
        gtk_signal_connect (GTK_OBJECT (Window), "delete_event",    GTK_SIGNAL_FUNC (on_window_close), textview);
        gtk_signal_connect (GTK_OBJECT (Window), "focus_out_event", GTK_SIGNAL_FUNC (on_window_focus_out), textview);
        gtk_signal_connect (GTK_OBJECT (Window), "window-state-event",  GTK_SIGNAL_FUNC (on_window_changed_state), textview);
        gtk_signal_connect (GTK_OBJECT (event_box), "button_press_event",  GTK_SIGNAL_FUNC (on_window_button_press), textview);
        // drag_begin_event 

        gtk_widget_show_all (Window);
        gtk_widget_hide(GTK_WIDGET (textview));
        gtk_main ();

        return 0;
}

