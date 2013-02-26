package com.example.helloworld;

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;
import java.util.Date;
import java.util.TimeZone;

/**
 * @author 
 */
public class HelloWorldMidlet  extends MIDlet implements CommandListener, ItemCommandListener {

    private boolean midletPaused = false;
                    
    private Form form;
    
    private TextField nameField = new TextField("Name/ID:", "", 32, TextField.ANY);
    private TextField flightField = new TextField("Flight:", "", 32, TextField.ANY); 
    private DateField dateField = new DateField("Date:", DateField.DATE, TimeZone.getTimeZone("GMT"));
    private ChoiceGroup purserField = new ChoiceGroup("", Choice.MULTIPLE);
    private StringItem buttonOK = new StringItem("", "Send Request", Item.BUTTON);  

    private Command exitCommand = new Command("Exit", Command.EXIT, 0);    
    private Command okCommand = new Command("Ok", Command.OK, 0);   

    private BackupFile backupFile = new BackupFile("BackupFile.txt");
    
    private Display display = Display.getDisplay(this); 

    public HelloWorldMidlet () 
    {
    	purserField.append("Purser", null);
    	                             
        buttonOK.addCommand(okCommand);
        buttonOK.setItemCommandListener(this);   
        buttonOK.setLayout(ImageItem.LAYOUT_CENTER);   
    }

    public Form createForm() 
    {
        if (form == null) 
        {                                 
            form = new Form("Crew Member Request");
            
            form.append(nameField); 
            form.append(flightField);
            form.append(dateField); 
            form.append(purserField); 
            form.append(buttonOK); 
                                  
            form.addCommand(exitCommand);
            form.setCommandListener(this);             
        }                         
        return form;
    }


    private void initialize() 
    {     
    	UserData userData = backupFile.load();   
    	
    	Log.write("userData.name " + userData.name); 
    	Log.write("userData.flight " + userData.flight); 
    	Log.write("userData.date " + userData.date); 
    	Log.write("userData.purser " + userData.purser); 
    	
    	nameField.setString(userData.name);
    	flightField.setString(userData.flight);	
    	
    	try
    	{
	    	Date d = new Date(Long.parseLong(userData.date));
	    	dateField.setDate(d);
    	}
    	catch(Exception e)
    	{
    		Log.write(e);
    	}
	
    	
    	if(userData.purser.equals("true"))
    	{
    		purserField.setSelectedIndex(0, true);
    	}
    }                            

    
    public void startMIDlet() 
    {                	
        switchDisplayable(null, createForm());  
    }  
    
    public void exitMIDlet() throws Exception
    {  	
    	UserData userData = new UserData();
    	userData.name = nameField.getString();
    	userData.flight = flightField.getString();
    	userData.date = "" + dateField.getDate().getTime();
    	
    	boolean[] selected = new boolean[purserField.size()];  
    	purserField.getSelectedFlags(selected);
    	userData.purser = "" + selected[0];
    	
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
	        if (item == buttonOK && command == okCommand) 
            {       
            	if(nameField.getString() == null || nameField.getString().length() == 0)
            	{
            		throw new Exception("Please provide Name/ID");
            	}

            	if(flightField.getString() == null || flightField.getString().length() == 0)
            	{
            		throw new Exception("Please provide flight number");
            	}

            	if(dateField == null ||  
            	   dateField.getDate() == null ||
            	   dateField.getDate().toString() == null || 
            	   dateField.getDate().toString().length() == 0)
            	{
            		throw new Exception("Please provide flight date");
            	}

            	Log.show(display, form, "Sending SMS!");
            	boolean[] selected = new boolean[purserField.size()];
            	purserField.getSelectedFlags(selected);
            	
            	Log.write("purser: " + selected[0]);
            	// send SMS
            	
            	exitMIDlet();
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

