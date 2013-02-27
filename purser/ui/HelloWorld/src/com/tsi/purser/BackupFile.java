package com.tsi.purser;

import java.io.*;
import javax.microedition.io.Connector;
import javax.microedition.io.file.FileConnection;

class BackupFile
{
	private String mFilename;
	
	public BackupFile(String filename)
	{
		String dir = System.getProperty("fileconn.dir.private"); 
		mFilename = dir + filename;
		System.out.println(filename); 
	}
	

	public UserData load()
    {     	
        InputStream is = null;
        FileConnection fc = null;
        UserData userData = new UserData();
        try
        {
        	System.out.println("- Filename: " + mFilename); 
            fc = (FileConnection)Connector.open(mFilename, Connector.READ_WRITE);

             if(fc.exists()) 
             {
                 is = fc.openInputStream();                 
                
                 userData.name = readLine(is);
                 userData.flight = readLine(is);
                 userData.date = readLine(is);
                 userData.purser = readLine(is);
                 userData.callCenter = readLine(is);
             }
         } 
         catch (IOException e) 
         {
        	 System.out.println("Can't read file");
        	 System.out.println(e.getMessage()); 
        	 e.printStackTrace();        	 
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
            	 Log.write(e);                 
             } 
         } 
         
    	Log.write("LOAD:"); 
    	Log.write("userData.name " + userData.name); 
    	Log.write("userData.flight " + userData.flight); 
    	Log.write("userData.date " + userData.date); 
    	Log.write("userData.purser " + userData.purser); 
    	Log.write("userData.callCenter " + userData.callCenter); 
	    	
        return userData;
    }  

	 
	public void save(UserData userData)
	{
    	Log.write("SAVE:"); 
    	Log.write("userData.name " + userData.name); 
    	Log.write("userData.flight " + userData.flight); 
    	Log.write("userData.date " + userData.date); 
    	Log.write("userData.purser " + userData.purser); 
    	Log.write("userData.callCenter " + userData.callCenter); 
    	
		delete();

        OutputStream os = null; 
        FileConnection fconn = null; 
        try 
        {  
             fconn = (FileConnection) Connector.open(mFilename, Connector.READ_WRITE); 
             if (!fconn.exists()) 
                 fconn.create();

             os = fconn.openDataOutputStream();
             
             String endl = "\n";
             
             os.write(userData.name.getBytes()); 
             os.write(endl.getBytes());
             os.write(userData.flight.getBytes()); 
             os.write(endl.getBytes());
             os.write(userData.date.getBytes());             
             os.write(endl.getBytes());
             os.write(userData.purser.getBytes()); 
             os.write(endl.getBytes());
             os.write(userData.callCenter.getBytes()); 
             os.write(endl.getBytes());
             
             fconn.setHidden(false);
//	           fconn.setReadable(true);
         } 

         catch (IOException e) 
         { 
        	 Log.write(e);
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
            	 Log.write(e);
             } 
         } 
     }

	
	
	private void delete()
	{
		try
		{
		    // A File object to represent the filename
		    FileConnection fc = (FileConnection)Connector.open(mFilename, Connector.READ_WRITE);
		
		    // Make sure the file or directory exists and isn't write protected
		    if (!fc.exists())
		    	return;
		
		    if (!fc.canWrite())
		      throw new IOException("Delete: write protected: " + mFilename);
		
		    fc.delete();
		}
		catch(Exception e)
		{
    		System.out.println("Can't delete file"); 
    		e.printStackTrace();
		}
	}
	
    private String readLine(InputStream reader) throws IOException 
    {
        StringBuffer line = new StringBuffer();
        int c = reader.read();

        while (c != -1 && c != '\n') {
            line.append((char)c);
            c = reader.read();
        }

        if (line.length() == 0) {
            return null;
        }

        return line.toString();
    }

}
