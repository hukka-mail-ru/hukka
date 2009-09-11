#include <string.h>
#include <stdio.h>

#include <panel-applet.h>
#include <gtk/gtklabel.h>

PanelApplet* g_applet;

static gboolean
  on_button_press (GtkWidget      *event_box, 
                         GdkEventButton *event,
                         gpointer        data)
  {
	printf("on_button_press");

        /*GtkWidget *label;
	event_box = gtk_event_box_new ();
	label = gtk_label_new ("Hello 2");
	gtk_container_add (GTK_CONTAINER (g_applet), label);

        gtk_widget_show_all (GTK_WIDGET (g_applet));
*/
//	gtk_widget_hide (GTK_WIDGET (g_applet));

	return TRUE;
  }

static gboolean
microtimer_applet_init (PanelApplet *applet,
   const gchar *iid,
   gpointer data)
{
	printf("myexample_applet_fill");

	GtkWidget *label;
	GtkWidget *event_box;
	int i;

	g_applet = applet;
	

	if (strcmp (iid, "OAFIID:MicrotimerApplet") != 0)
		return FALSE;

//	sprintf(str, "Number");
	
	

	label = gtk_label_new ("Hello!");
	gtk_container_add (GTK_CONTAINER (applet), label);
/*
	event_box = gtk_event_box_new ();
	g_signal_connect (G_OBJECT (event_box), 
	                  "button_press_event",
	                  G_CALLBACK (on_button_press),
 	                  NULL);
	gtk_container_add (GTK_CONTAINER (applet), event_box);
*/
	gtk_widget_show_all (GTK_WIDGET (applet));


	return TRUE;
}



PANEL_APPLET_BONOBO_FACTORY ("OAFIID:MicrotimerApplet_Factory",
                             PANEL_TYPE_APPLET,
                             "The Microtimer Applet",
                             "0",
                             microtimer_applet_init,
                             NULL);
