package com.example.helloworld;

import javax.microedition.io.Connector;
import javax.wireless.messaging.MessageConnection;
import javax.wireless.messaging.TextMessage;


public class SMS 
{
    public static void send(String number, String message) throws Exception
    {
    	Log.write("SEND: " + number + " " + message);
    	
    	if(number == null || number.length() == 0 || number.equals(UserData.Nothing))
    	{
    		throw new Exception("Please provide call center number");
    	}
    	
        try 
        {
          //sets address to send message
          String addr = "sms://"+number;
          // opens connection
          MessageConnection conn = (MessageConnection) Connector.open(addr);
          // prepares text message
          TextMessage msg = (TextMessage)conn.newMessage(MessageConnection.TEXT_MESSAGE);
          //set text
          msg.setPayloadText(message);
          // send message
          conn.send(msg);
          conn.close();
        } 
        catch (Exception e) 
        {
        	Log.write(e);
        }
      }

}
