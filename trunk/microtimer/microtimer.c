// The MicroTimer Applet for Gnome-Panel 
// Coded by Sergey Sadovnikov, 2009

#include <string.h>
#include <stdio.h>

#include <panel-applet.h>
#include <gtk/gtklabel.h>
#include <gtk/gtk.h>


PanelApplet* applet;
GtkWidget* label;
GtkWidget* event_box;
gboolean timer_on = FALSE;
int tics = 0;


static void show_label(const char* str)
{
	gtk_container_remove (GTK_CONTAINER (event_box), label);
	label = gtk_label_new (str);
	gtk_container_add (GTK_CONTAINER (event_box), label);
	gtk_widget_show_all (GTK_WIDGET (applet));	
}


static gboolean tic_timer(gpointer data) // this function won't be called again, since it returns FALSE
{
	if(!timer_on)
		return FALSE; // stop timer immediately!

	tics++;
	
	char time[100];
	sprintf(time, "%d:%02d:%d", tics/10/60, tics/10%60, tics%10);
	show_label(time);
	
	return TRUE;
}


static void reset_timer()
{
	timer_on = FALSE;
	tics = 0;
	show_label("0:00");
}


static gboolean on_button_press (GtkWidget *evbox, GdkEventButton *event,  gpointer data)
{
	if(event->button == 1 && event->type != GDK_2BUTTON_PRESS)  // LEFT BUTTON: START/STOP TIMER
	{
		if(!timer_on) {
			g_timeout_add (100, tic_timer, NULL);
			timer_on = TRUE;
		} else 	{
			timer_on = FALSE;
		}
		return TRUE;
	} 
	else if(event->button == 1 && event->type == GDK_2BUTTON_PRESS)  // DBL CLICK: RESET TIMER
	{
		reset_timer();
		return TRUE;
	}
/*	else if(event->button == 3) { // RIGHT BUTTON: CONTEXT MENU
             gtk_menu_popup (GTK_MENU (docklet_menu), NULL, NULL, NULL, NULL,
                      event->button, event->time);
	}*/
	return FALSE;
}



static gboolean microtimer_applet_init (PanelApplet *app, const gchar *iid, gpointer data)
{
	applet = app;

	if (strcmp (iid, "OAFIID:MicrotimerApplet") != 0)
		return FALSE;

	event_box = gtk_event_box_new ();	
	g_signal_connect (G_OBJECT (event_box),
	                  "button_press_event",
	                  G_CALLBACK (on_button_press),
 	                  NULL);

	gtk_container_add (GTK_CONTAINER (applet), event_box);

	reset_timer();

	return TRUE;
}



PANEL_APPLET_BONOBO_FACTORY ("OAFIID:MicrotimerApplet_Factory",
                             PANEL_TYPE_APPLET,
                             "The Microtimer Applet",
                             "0",
                             microtimer_applet_init,
                             NULL);
