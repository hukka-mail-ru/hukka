package com.tsi.purser.sender;


import java.io.IOException;

import com.tsi.data.Log;
import com.tsi.purser.protocol.Message;
import com.tsi.purser.protocol.MySocket;

public class Sender
{
	
	public static void main(String args[])
	{
		MySocket socket = new MySocket();
		
		try
		{
			socket.ConnectToHost();
			
			Message mes = new Message();
			mes.SetPhone("79119089209");
			mes.SetText("Hello");

			socket.SendMessage(mes);
		}
		catch(Exception e)
		{
			Log.write(e);
		}
		finally
		{
			try 
			{
				socket.Close();
			} 
			catch (Exception e) 
			{
				Log.write(e);
			}
		}
	}
}