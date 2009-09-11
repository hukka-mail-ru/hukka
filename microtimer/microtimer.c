#include <string.h>
#include <stdio.h>

#include <panel-applet.h>
#include <gtk/gtklabel.h>


PanelApplet* g_applet;
GtkWidget* g_label;
GtkWidget* g_event_box;
gboolean timer_on;
int tics;


static gboolean run_timer(gpointer data)
{
	if(!timer_on)
		return FALSE; // stop immediately!

	gtk_container_remove (GTK_CONTAINER (g_event_box), g_label);

	char str[100];
	if(tics%60 < 10)
		sprintf(str, "%d:0%d", tics/60, tics%60);
	else
		sprintf(str, "%d:%d", tics/60, tics%60);

	g_label = gtk_label_new (str);

	gtk_container_add (GTK_CONTAINER (g_event_box), g_label);
	gtk_widget_show_all (GTK_WIDGET (g_applet));

	tics++;
	return timer_on;
}

static void stop_timer()
{
	timer_on = FALSE;
	tics = 1;
}


static gboolean on_button_press (GtkWidget *event_box, GdkEventButton *event,  gpointer data)
{
	if(event->button == 1) // LEFT BUTTON
	{
		if(!timer_on)
		{
			g_timeout_add (1000, run_timer, NULL);
			timer_on = TRUE;
		}
		else
		{
			timer_on = FALSE;
		}
	}
	else
	{	
		stop_timer();

		gtk_container_remove (GTK_CONTAINER (g_event_box), g_label);
		g_label = gtk_label_new ("0:00");
		gtk_container_add (GTK_CONTAINER (g_event_box), g_label);
		gtk_widget_show_all (GTK_WIDGET (g_applet));		
	}
	return TRUE;
}

static gboolean microtimer_applet_init (PanelApplet *applet, const gchar *iid, gpointer data)
{
	g_applet = applet;

	if (strcmp (iid, "OAFIID:MicrotimerApplet") != 0)
		return FALSE;

	// TEXT LABEL
	stop_timer();
	g_label = gtk_label_new ("0:00");

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
