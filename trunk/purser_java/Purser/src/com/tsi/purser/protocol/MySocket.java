package com.tsi.purser.protocol;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;

import com.tsi.data.Log;
import com.tsi.purser.exceptions.ProtocolException;

public class MySocket 
{
	ObjectOutputStream out;
	
	ObjectInputStream in;
	
	ServerSocket serverSocket = null; 
	
	Socket socket = null;
	
	
	public void Listen() throws IOException 
	{					
		serverSocket = new ServerSocket(2004, 10);
	}
	
	
	public void ConnectToHost() throws UnknownHostException, IOException, ClassNotFoundException
	{				
		//1. creating a socket to connect to the server
		socket = new Socket("localhost", 2004);
		Log.write("Connected to localhost in port 2004");
		
		//2. get Input and Output streams
		out = new ObjectOutputStream(socket.getOutputStream());
		out.flush();
		in = new ObjectInputStream(socket.getInputStream());			
	}
	
	
	
	public Message ReceiveMessage() throws IOException, ClassNotFoundException, ProtocolException
	{
		Message message = null;
		
		try
		{			
			//2. Wait for connection
			Log.write("Waiting for connection");
			socket = serverSocket.accept();			
			Log.write("Connection received from " + socket.getInetAddress().getHostName());
			
			//3. get Input and Output streams
			out = new ObjectOutputStream(socket.getOutputStream());
			out.flush();
			in = new ObjectInputStream(socket.getInputStream());
			Log.write("Connection successful");
						
			//4.Read message
			String str = (String)in.readObject();			
			message = new Message(str);		
			
			Log.write("======= INCOMING  =======");
			Log.write("Phone: " + message.GetPhone() + "; Len " + message.GetPhone().length());
			Log.write("Text: "  + message.GetText() + "; Len " + message.GetText().length());
			Log.write("Len: "   + str.length());
			Log.write("--------------------------");
			
		}
		finally
		{
			Close();
		}
		
		return message;
	}
	
	
	public void SendMessage(Message mes) throws IOException 
	{
		Log.write("======= OUTGOING  =======");

		String str = mes.Serialize();


		Log.write("Phone: " + mes.GetPhone() + "; Len " + mes.GetPhone().length());
		Log.write("Text: "  + mes.GetText() + "; Len " + mes.GetText().length());
		Log.write("Len: "   + str.length());
		Log.write("Bytes: " + str);
		Log.write("--------------------------");
		//Log::WriteBytes(str);
		
		out.writeObject(str);
		out.flush();

	}
	
	
	public void Close() throws IOException
	{	
		if(in != null)
		{
			in.close();
		}
		
		if(out != null)
		{
			out.close();
		}
		
		if(socket != null)
		{
			socket.close();
		}
	}
	
	
}
