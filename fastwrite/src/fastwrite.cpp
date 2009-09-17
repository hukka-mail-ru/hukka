#include <string.h>

#include <panel-applet.h>
#include <gtk/gtklabel.h>
#include <gtk/gtk.h>
#include <gtk/gtkbox.h>



gboolean on_button_press (GtkWidget *event_box, GdkEventButton *event,  gpointer data)
{
	if(event->button == 1 && event->type != GDK_2BUTTON_PRESS) // LEFT BUTTON
	{
		GtkWidget* window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
		gtk_window_set_keep_below ((GtkWindow*)window, TRUE);
		gtk_window_set_decorated((GtkWindow*)window, FALSE);
		//gtk_window_set_opacity((GtkWindow*)window, 0.5);

		g_object_notify(G_OBJECT (window), "background-transparent");
		/*g_timeout_add_full (GDK_PRIORITY_REDRAW,
							VTE_UPDATE_TIMEOUT,
							update_timeout, NULL,
							NULL);*/


		GtkWidget* box = gtk_vbox_new (TRUE, 12);
		gtk_container_add (GTK_CONTAINER (window), box);

		GtkWidget* answer = gtk_label_new ("Check this out");
		gtk_box_pack_start (GTK_BOX (box), answer, TRUE, TRUE, 12);

		gtk_widget_show_all (window);

		return TRUE;
	}
	/*
	else if(event->button == 1 && event->type == GDK_2BUTTON_PRESS) // DBL CLICK
	{

	}
	else if(event->button == 3) // RIGHT BUTTON: CONTEXT MENU
	{
	}*/
	return FALSE;
}



gboolean fastwrite_applet_init (PanelApplet *applet, const gchar *iid, gpointer data)
{

	if (strcmp (iid, "OAFIID:FastwriteApplet") != 0)
		return FALSE;

	// EVENT BOX
	GtkWidget* event_box = gtk_event_box_new ();

	g_signal_connect (G_OBJECT (event_box),
	                  "button_press_event",
	                  G_CALLBACK (on_button_press),
 	                  NULL);

	gtk_container_add (GTK_CONTAINER (applet), event_box);

	GtkWidget* label = gtk_label_new ("Fastwrite !!!");
	gtk_container_add (GTK_CONTAINER (event_box), label);
	gtk_widget_show_all (GTK_WIDGET (applet));



	return TRUE;
}



PANEL_APPLET_BONOBO_FACTORY ("OAFIID:FastwriteApplet_Factory",
                             PANEL_TYPE_APPLET,
                             "The Fastwrite Applet",
                             "0",
                             fastwrite_applet_init,
                             NULL);
