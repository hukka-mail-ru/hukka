#include <string.h>
#include <stdio.h>

#include <panel-applet.h>
#include <gtk/gtklabel.h>


PanelApplet* g_applet;
GtkWidget* g_label;
GtkWidget *event_box;

static gboolean on_button_press (GtkWidget *event_box, GdkEventButton *event,  gpointer data)
{
	printf("on_button_press");
	gtk_container_remove (GTK_CONTAINER (event_box), g_label);

	g_label = gtk_label_new ("Hello 2");
	gtk_container_add (GTK_CONTAINER (event_box), g_label);
	gtk_widget_show_all (GTK_WIDGET (g_applet));

	//	gtk_widget_hide (GTK_WIDGET (g_applet));

        gtk_widget_show_all (GTK_WIDGET (g_applet));
	printf("on_button_press exit");

	return TRUE;
}

static gboolean microtimer_applet_init (PanelApplet *applet, const gchar *iid, gpointer data)
{
	printf("myexample_applet_fill");

	
	GtkWidget *image;
	int i;

	g_applet = applet;


	if (strcmp (iid, "OAFIID:MicrotimerApplet") != 0)
		return FALSE;

//	sprintf(str, "Number");


	// TEXT LABEL
	g_label = gtk_label_new ("Hello !");
	//gtk_container_add (GTK_CONTAINER (g_applet), g_label);

	// EVENT BOX
	event_box = gtk_event_box_new ();
	g_signal_connect (G_OBJECT (event_box),
	                  "button_press_event",
	                  G_CALLBACK (on_button_press),
 	                  NULL);

	gtk_container_add (GTK_CONTAINER (event_box), g_label);
	gtk_container_add (GTK_CONTAINER (g_applet), event_box);



	gtk_widget_show_all (GTK_WIDGET (g_applet));


	return TRUE;
}



PANEL_APPLET_BONOBO_FACTORY ("OAFIID:MicrotimerApplet_Factory",
                             PANEL_TYPE_APPLET,
                             "The Microtimer Applet",
                             "0",
                             microtimer_applet_init,
                             NULL);
