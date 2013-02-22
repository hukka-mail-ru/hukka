package com.example.helloworld;

import java.io.*;
import javax.microedition.io.Connector;
import javax.microedition.io.file.FileConnection;

public class UserData 
{
	public void write(String name, String flight)
	{
		writeTextFile("backup.txt", name);
		writeTextFile("backup.txt", flight);
	}

	
	public void read()
	{
		String res = readTextFile("backup.txt");
		System.out.println(res); 
	}
	
	 private String readTextFile(String fName)
	     {
	         InputStream is = null;
	         FileConnection fc = null;
	         String str = "";
	         try	         
	         {
	        	 String dir = System.getProperty("fileconn.dir.private"); 
	             fc = (FileConnection)Connector.open(dir + fName, Connector.READ_WRITE);

	             if(fc.exists()) 
	             {
	                 int size = (int)fc.fileSize();
	                 is= fc.openInputStream();
	                 byte bytes[] = new byte[size];
	                 is.read(bytes, 0, size);
	                 str = new String(bytes, 0, size);
	             }
	         } 
	         catch (IOException ioe) 
	         {
	        	 System.out.println(ioe.getMessage()); 
	         } 
	         finally 
	         { 
	             try 
	             { 
	                 if (null != is) 
	                     is.close(); 
	                 if (null != fc) 
	                     fc.close(); 
	             } 
	             catch (IOException e) 
	             { 
	                 System.out.println(e.getMessage()); 
	             } 
	         } 
	         return str;
	     }  

	 private void writeTextFile(String fName, String text) 
	     { 
	         OutputStream os = null; 
	         FileConnection fconn = null; 
	         try 
	         { 
	        	 String dir = System.getProperty("fileconn.dir.private"); 
	             fconn = (FileConnection) Connector.open(dir + fName, Connector.READ_WRITE); 
	             if (!fconn.exists()) 
	                 fconn.create();

	             os = fconn.openDataOutputStream();
	             os.write(text.getBytes()); 
	             fconn.setHidden(false);
//	           fconn.setReadable(true);
	         } 

	         catch (IOException e) 
	         { 
	             System.out.println(e.getMessage()); 
	         } 
	         finally 
	         { 
	             try 
	             { 
	                 if (null != os) 
	                     os.close(); 
	                 if (null != fconn) 
	                     fconn.close(); 
	             } 
	             catch (IOException e) 
	             { 
	                 System.out.println(e.getMessage()); 
	             } 
	         } 
	     }

}
