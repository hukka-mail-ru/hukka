package com.tsi.purser;

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;

import com.tsi.purser.data.*;
import com.tsi.purser.forms.*;

/**
 * @author 
 */
public class Midlet extends MIDlet
{

    private boolean midletPaused = false;
                    
    private Main main;
	private Settings settings;
	private Done done;
	private Logo logo;
	
    private UserData userData;
	private BackupFile backupFile = new BackupFile();
 
    private Display display = Display.getDisplay(this); 
      
    public UserData getUserData() { return userData; }
      
    public void showDone() 
    { 
    	display.setCurrent(done.getWidget());
    	display.setCurrentItem(done.getDefaultItem());
    }    
    
    public void showSettings() 
    { 
    	display.setCurrent(settings.getWidget());
    	display.setCurrentItem(settings.getDefaultItem());
    }
    
    public void showMain() 
    { 
    	display.setCurrent(main.getWidget());
    	display.setCurrentItem(main.getDefaultItem());
    }

    public void showMessage(String message) 
    { 
    	Log.show(display, (Form)display.getCurrent(), message);
    }
    
    public void showMessage(Exception e) 
    { 
    	Log.show(display, (Form)display.getCurrent(), e);
    }

    
    public void startMIDlet() 
    {     
    	try
    	{   
	        main = new Main(this);
	    	settings = new Settings(this);
	    	done = new Done(this);
	    	logo = new Logo();
	    	
	    	display.setCurrent(logo.getWidget());
	    	logo.animate(); 
	    	
	    	userData = backupFile.load();
	    	
	    	display.setCurrent(main.getWidget());
	    	
	    	main.setData(userData); 
	    	settings.setData(userData);
	    	
	    	showMain();
    	}
    	catch(Exception e)
    	{
    		Log.write(e);
    	}
    }  

    public void exitMIDlet()
    {  	 
    	try
    	{
	    	main.getData(userData);
	    	settings.getData(userData);
	    	backupFile.save(userData);
	    	
	        display.setCurrent(null);
	        destroyApp(true);
	        notifyDestroyed(); 
    	}
    	catch(Exception e)
    	{
    		Log.write(e);
    	}
    }
       
    /**
     * Called when MIDlet is started.
     * Checks whether the MIDlet have been already started and initialize/starts or resumes the MIDlet.
     */
    public void startApp() 
    {
        if (!midletPaused) 
        {
            startMIDlet ();
        }
        midletPaused = false;
    }

    /**
     * Called when MIDlet is paused.
     */
    public void pauseApp() 
    {
        midletPaused = true;
    }

    /**
     * Called to signal the MIDlet to terminate.
     * @param unconditional if true, then the MIDlet has to be unconditionally terminated and all resources has to be released.
     */
    public void destroyApp(boolean unconditional) 
    {
    }

}

