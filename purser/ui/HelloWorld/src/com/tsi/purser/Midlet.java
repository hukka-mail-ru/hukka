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
                    
    private Main main = new Main(this);
	private Settings settings = new Settings(this);
	private Done done = new Done(this);
	private Logo logo = new Logo();
	
    private UserData userData;
	private BackupFile backupFile = new BackupFile("BackupFile.txt");
 
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
    	display.setCurrent(logo.getWidget());
    	logo.animate(); 
    	
    	userData = backupFile.load();
    	
    	main.setData(userData); 
    	settings.setData(userData);
    	
    	showMain();     	        
    }  

    public void exitMIDlet() throws Exception
    {  	  
    	main.getData(userData);
    	settings.getData(userData);
    	backupFile.save(userData);
    	
        display.setCurrent(null);
        destroyApp(true);
        notifyDestroyed();     
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

