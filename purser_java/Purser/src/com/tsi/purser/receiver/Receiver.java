package com.tsi.purser.receiver;

import java.io.IOException;

import com.tsi.data.Log;
import com.tsi.purser.protocol.Message;
import com.tsi.purser.protocol.MySocket;


public class Receiver
{
		
	public static void main(String args[])
	{
		MySocket socket = new MySocket();
		
		try 
		{
			socket.Listen();
		
			for(;;)
			{
				try
				{
					Message message = socket.ReceiveMessage();		
					
					
					Log.write(message.GetPhone());
					Log.write(message.GetText());	
					
				}
				catch(Exception e)
				{
					Log.write(e);
				}
			}
		
		} 
		catch (Exception e) 
		{
			Log.write(e);
		}
	}
}
