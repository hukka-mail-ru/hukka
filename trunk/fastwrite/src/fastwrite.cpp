#include <string.h>

#include <panel-applet.h>
#include <gtk/gtklabel.h>
#include <gtk/gtk.h>


PanelApplet* g_applet;
GtkWidget* g_label;
GtkWidget* g_event_box;
gboolean timer_on;
int tics;



static gboolean fastwrite_applet_init (PanelApplet *applet, const gchar *iid, gpointer data)
{
	g_applet = applet;

	if (strcmp (iid, "OAFIID:FastwriteApplet") != 0)
		return FALSE;

	// EVENT BOX
	g_label = gtk_label_new ("Fastwrite!");
	gtk_container_add (GTK_CONTAINER (g_applet), g_label);
	gtk_widget_show_all (GTK_WIDGET (g_applet));	

	return TRUE;
}



PANEL_APPLET_BONOBO_FACTORY ("OAFIID:FastwriteApplet_Factory",
                             PANEL_TYPE_APPLET,
                             "The Fastwrite Applet",
                             "0",
                             fastwrite_applet_init,
                             NULL);
