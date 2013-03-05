package com.tsi.purser.data;

import java.io.*;


import javax.microedition.rms.*;


public class BackupFile
{
	public BackupFile() {}
		
	private RecordStore rs = null;
	static final String REC_STORE = "ReadWriteRMS";
	
	  
	public void save(UserData userData) throws IOException, RecordStoreNotFoundException, RecordStoreException
	{
		
		
    	Log.write("SAVE:"); 
    	Log.write("userData.name " + userData.name); 
    	Log.write("userData.flight " + userData.flight); 
    	Log.write("userData.date " + userData.date); 
    	Log.write("userData.purser " + userData.purser); 
    	Log.write("userData.callCenter " + userData.callCenter); 
    	
    	deleteRecStore();
    	
    	openRecStore();
    	
    	writeRecord(userData.name);
    	writeRecord(userData.flight);
    	writeRecord(userData.date);
    	writeRecord(userData.purser);
    	writeRecord(userData.callCenter);	  
    	
    	closeRecStore();
     }
	
	public UserData load() throws IOException, RecordStoreNotOpenException, InvalidRecordIDException, RecordStoreException
    {     	

		openRecStore();
		
		UserData userData = new UserData();
    	userData.name = readRecord(1);
    	userData.flight = readRecord(2);
    	userData.date = readRecord(3);
    	userData.purser = readRecord(4);
    	userData.callCenter = readRecord(5);	 
    		
    	Log.write("LOAD:"); 
    	Log.write("userData.name " + userData.name); 
    	Log.write("userData.flight " + userData.flight); 
    	Log.write("userData.date " + userData.date); 
    	Log.write("userData.purser " + userData.purser); 
    	Log.write("userData.callCenter " + userData.callCenter); 
		
		closeRecStore();
    	
		
        return userData;
    }  

	  
	private void openRecStore() throws RecordStoreFullException, RecordStoreNotFoundException, RecordStoreException
	{
	    rs = RecordStore.openRecordStore(REC_STORE, true );
	}    

	private void closeRecStore() throws RecordStoreNotOpenException, RecordStoreException
	{
		if(rs != null)
		{
			rs.closeRecordStore();
		}
	}
	  
	private void deleteRecStore() throws RecordStoreNotFoundException, RecordStoreException
	{
		if (RecordStore.listRecordStores() != null)
		{
			RecordStore.deleteRecordStore(REC_STORE);
		}      
	}


	private void writeRecord(String str) throws RecordStoreNotOpenException, RecordStoreFullException, RecordStoreException
	{
	    byte[] rec = str.getBytes();
	    rs.addRecord(rec, 0, rec.length);
	}

	private String readRecord(int i) throws RecordStoreNotOpenException, RecordStoreException
	{
		String res = null;
		try
		{
			byte[] recData = new byte[rs.getRecordSize(i)];
			int len = rs.getRecord(i, recData, 0);
			res = new String(recData, 0, len);
		}
		catch(InvalidRecordIDException e)
		{
			res = UserData.Nothing;
		}
        
		return res;                        
	}
	

}
