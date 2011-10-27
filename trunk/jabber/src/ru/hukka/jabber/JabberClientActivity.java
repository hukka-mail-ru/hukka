package ru.hukka.jabber;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;
import org.jivesoftware.smack.*;

public class JabberClientActivity extends Activity {
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
      //  setContentView(R.layout.main);
        TextView tv = new TextView(this);
        tv.setText("Hello, Android");
        setContentView(tv);
        
        // Create a connection to the igniterealtime.org XMPP server.
        try
        {
            Connection con = new XMPPConnection("igniterealtime.org");
            // Connect to the server
            con.connect();
            // Most servers require you to login before performing other tasks.
            con.login("jsmith", "mypass");
        }
        catch (XMPPException e)
        {
            tv.setText("Error: " + e.getMessage());
            setContentView(tv);
            
            System.out.println("Error: " + e.getMessage()); 
            e.printStackTrace();
        }
            
            
            
        // Start a new conversation with John Doe and send him a message.
        /*
        Chat chat = connection.getChatManager().createChat("jdoe@igniterealtime.org", new MessageListener() {
        

       public void processMessage(Chat chat, Message message) { 
           // Print out any messages we get back to standard out. 
           System.out.println("Received message: " + message); 
        } }); 
        
        chat.sendMessage("Howdy!"); // Disconnect from the server con.disconnect();
        */ 
       //}
    }
}