package com.example.helloworld;

import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;
import java.util.Date;
import java.util.TimeZone;

/**
 * @author 
 */
public class HelloWorldMidlet extends MIDlet implements CommandListener, ItemCommandListener {

    private boolean midletPaused = false;
                    
    private Form form;
    
    private TextField fieldName = new TextField("Name/ID:", "", 32, TextField.ANY);
    private TextField fieldFlight = new TextField("Flight:", "", 32, TextField.ANY); 
    private DateField fieldDate = new DateField("", DateField.DATE, TimeZone.getTimeZone("GMT"));
    private ChoiceGroup fieldPurser = new ChoiceGroup("", Choice.MULTIPLE);
    private StringItem buttonSend = new StringItem("", "Send Request", Item.BUTTON);  

    private Command commandExit = new Command("Exit", Command.EXIT, 0);    
    private Command commandSend = new Command("Send", Command.OK, 0);   

    private BackupFile backupFile = new BackupFile("BackupFile.txt");
    
    private Display display = Display.getDisplay(this); 
    
    private String Nothing = "__Nothing__";

    public HelloWorldMidlet () 
    {
    	fieldPurser.append("Purser", null);
    	                             
        buttonSend.addCommand(commandSend);
        buttonSend.setItemCommandListener(this);   
        buttonSend.setLayout(ImageItem.LAYOUT_CENTER);       
    }

    public Form createForm() 
    {
        if (form == null) 
        {                                 
            form = new Form("Crew Member Request");
            
            form.append(fieldName); 
            form.append(fieldDate); 
            form.append(fieldFlight);
            form.append(fieldPurser); 
            form.append(buttonSend);
                                  
            form.addCommand(commandExit);
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
    	    	
    	try
    	{
    		if(userData.name.compareTo(Nothing) != 0)
    		{
    			fieldName.setString(userData.name);
    		}
    		
    		if(userData.flight.compareTo(Nothing) != 0)
    		{
    			fieldFlight.setString(userData.flight);
    		}
    		
        	if(userData.date.compareTo(Nothing) != 0)
        	{
        		Date d = new Date(Long.parseLong(userData.date));
        		fieldDate.setDate(d);
        	}
	    	
	    	if(userData.purser.equals("true"))
	    	{
	    		fieldPurser.setSelectedIndex(0, true);
	    	}
    	}
    	catch(Exception e)
    	{
    		Log.write(e);
    	}	        	    	
    }                            

    
    public void startMIDlet() 
    {                	
        switchDisplayable(null, createForm());
        
        if(fieldName.getString() != null && fieldName.getString().length() != 0)
    	{        	
        	display.setCurrentItem(fieldDate);
    	}
    }  
    
    public void exitMIDlet() throws Exception
    {  	
    	UserData userData = new UserData();
    	userData.name = fieldName.getString().length() == 0 ? Nothing : fieldName.getString();
    	userData.flight = fieldFlight.getString().length() == 0  ? Nothing : fieldFlight.getString();
    	userData.date = fieldDate.getDate() == null ? Nothing : "" + fieldDate.getDate().getTime();
    	
    	boolean[] selected = new boolean[fieldPurser.size()];  
    	fieldPurser.getSelectedFlags(selected);
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
	            if (command == commandExit) 
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
	        if (item == buttonSend && command == commandSend) 
            {       
            	if(fieldName.getString() == null || fieldName.getString().length() == 0)
            	{
            		throw new Exception("Please provide Name/ID");
            	}

            	if(fieldFlight.getString() == null || fieldFlight.getString().length() == 0)
            	{
            		throw new Exception("Please provide flight number");
            	}

            	if(fieldDate == null ||  
            	   fieldDate.getDate() == null ||
            	   fieldDate.getDate().toString() == null || 
            	   fieldDate.getDate().toString().length() == 0)
            	{
            		throw new Exception("Please provide flight date");
            	}

            	Log.show(display, form, "Sending SMS!");
            	boolean[] selected = new boolean[fieldPurser.size()];
            	fieldPurser.getSelectedFlags(selected);
            	
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

