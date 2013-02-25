package com.example.helloworld;

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;
import java.util.TimeZone;

/**
 * @author 
 */
public class HelloWorldMidlet  extends MIDlet implements CommandListener, ItemCommandListener {

    private boolean midletPaused = false;
                    
    private Form form;
    
    private TextField name = new TextField("Name/ID:", "", 32, TextField.ANY);
    private TextField flight = new TextField("Flight:", "", 32, TextField.ANY); 
    private DateField calendar = new DateField("Date:", DateField.DATE, TimeZone.getTimeZone("GMT"));
    private ChoiceGroup purser = new ChoiceGroup("", Choice.MULTIPLE);
    private StringItem buttonOK = new StringItem("", "Send Request", Item.BUTTON);  

    private Command exitCommand = new Command("Exit", Command.EXIT, 0);    
    private Command okCommand = new Command("Ok", Command.OK, 0);   

    private BackupFile backupFile = new BackupFile("BackupFile.txt");
    
    private Display display = Display.getDisplay(this); 

    public HelloWorldMidlet () 
    {
    	purser.append("Purser", null);
    	                             
        buttonOK.addCommand(okCommand);
        buttonOK.setItemCommandListener(this);   
        buttonOK.setLayout(ImageItem.LAYOUT_CENTER);   
    }

    public Form createForm() 
    {
        if (form == null) 
        {                                 
            form = new Form("Crew Member Request");
            
            form.append(name); 
            form.append(flight);
            form.append(calendar); 
            form.append(purser); 
            form.append(buttonOK); 
                                  
            form.addCommand(exitCommand);
            form.setCommandListener(this);             
        }                         
        return form;
    }


    private void initialize() 
    {     
    	try
    	{
	    	UserData userData = backupFile.load();   
	    	
	    	System.out.println("userData.name " + userData.name); 
	    	System.out.println("userData.flight " + userData.flight); 
	    	System.out.println("userData.date " + userData.date); 
	    	System.out.println("userData.purser " + userData.purser); 
	    	
	    	name.setString(userData.name);
	    	flight.setString(userData.flight);
	    	//name.setString(userData.name);
	    	//name.setString(userData.name);
    	}
    	catch(Exception e)
    	{
    		Log.write(e);
    	}
    }                            

    
    public void startMIDlet() 
    {                	
        switchDisplayable(null, createForm());  
    }  
    
    public void exitMIDlet() throws Exception
    {  	
    	UserData userData = new UserData();
    	userData.name = name.getString();
    	userData.flight = flight.getString();
    	userData.date = calendar.getDate().toString();
    	userData.purser = "" + purser.getSelectedIndex();
    	
    	backupFile.save(userData);
    	
        switchDisplayable (null, null);
        destroyApp(true);
        notifyDestroyed();
     
    }

    public void resumeMIDlet() 
    {                                       
    }                              

    public void switchDisplayable(Alert alert, Displayable nextDisplayable) 
    {                                            
        Display display = getDisplay();                                               
        if (alert == null) {
            display.setCurrent(nextDisplayable);
        } else {
            display.setCurrent(alert, nextDisplayable);
        }                                             
    }                                   

    public void commandAction(Command command, Displayable displayable) 
    {      
    	try
    	{
	        if (displayable == form) {                                           
	            if (command == exitCommand) 
	            {                                         
	                exitMIDlet();                                           
	            }                                                  
	        }
    	}
    	catch(Exception e)
    	{
    		Log.show(display, form, e);
    	}
    }                               

    
    
    public void commandAction(Command command, Item item) 
    {   
    	try
    	{
	        if (item == buttonOK) 
	        {    
	        	Log.write("buttonOK");
	            if (command == okCommand) 
	            {       
	            	Log.write("okCommand");
	            	Log.write("name.getString(): '" + name.getString() + "'");
	            	if(name.getString() == null || name.getString().length() == 0)
	            	{
	            		Log.write("here");
	            		throw new Exception("Please provide Name/ID");
	            	}
	
	            	if(flight.getString() == null || flight.getString().length() == 0)
	            		throw new Exception("Please provide flight number");
	            	
	            	// send SMS
	            	
	            }                                                
	        }
    	}
    	catch(Exception e)
    	{
    		Log.show(display, form, e);
    	}                                  
    }                                    
                                     

    /**
     * Returns a display instance.
     * @return the display instance.
     */
    public Display getDisplay () {
        return Display.getDisplay(this);
    }


    /**
     * Called when MIDlet is started.
     * Checks whether the MIDlet have been already started and initialize/starts or resumes the MIDlet.
     */
    public void startApp() {
        if (midletPaused) {
            resumeMIDlet ();
        } else {
            initialize ();
            startMIDlet ();
        }
        midletPaused = false;
    }

    /**
     * Called when MIDlet is paused.
     */
    public void pauseApp() {
        midletPaused = true;
    }

    /**
     * Called to signal the MIDlet to terminate.
     * @param unconditional if true, then the MIDlet has to be unconditionally terminated and all resources has to be released.
     */
    public void destroyApp(boolean unconditional) {
    }

}

