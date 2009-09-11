#include <string.h>
#include <stdio.h>

#include <panel-applet.h>
#include <gtk/gtklabel.h>


PanelApplet* g_applet;
GtkWidget* g_label;
GtkWidget* g_event_box;


static gboolean run_timer(gpointer data)
{
	static int i;
	gtk_container_remove (GTK_CONTAINER (g_event_box), g_label);

	char str[10];
	sprintf(str, "Hello %d", i);
	g_label = gtk_label_new (str);

	gtk_container_add (GTK_CONTAINER (g_event_box), g_label);
	gtk_widget_show_all (GTK_WIDGET (g_applet));

	i++;
}



static gboolean on_button_press (GtkWidget *event_box, GdkEventButton *event,  gpointer data)
{
	int i;

	g_timeout_add (1000, run_timer, NULL);

	return TRUE;
}

static gboolean microtimer_applet_init (PanelApplet *applet, const gchar *iid, gpointer data)
{
	printf("myexample_applet_fill");

	
	GtkWidget* image;
	

	g_applet = applet;

	if (strcmp (iid, "OAFIID:MicrotimerApplet") != 0)
		return FALSE;

	// TEXT LABEL
	g_label = gtk_label_new ("Hello !");

	// EVENT BOX
	g_event_box = gtk_event_box_new ();
	g_signal_connect (G_OBJECT (g_event_box),
	                  "button_press_event",
	                  G_CALLBACK (on_button_press),
 	                  NULL);

	gtk_container_add (GTK_CONTAINER (g_event_box), g_label);
	gtk_container_add (GTK_CONTAINER (g_applet), g_event_box);



	gtk_widget_show_all (GTK_WIDGET (g_applet));


	return TRUE;
}



PANEL_APPLET_BONOBO_FACTORY ("OAFIID:MicrotimerApplet_Factory",
                             PANEL_TYPE_APPLET,
                             "The Microtimer Applet",
                             "0",
                             microtimer_applet_init,
                             NULL);
