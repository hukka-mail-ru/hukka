package com.example.helloworld;

import java.util.Date;
import java.util.TimeZone;

import javax.microedition.lcdui.*;

public class Main implements ItemCommandListener, CommandListener 
{
	private Form form = new Form("Crew Member Request");
		    
	private TextField fieldName = new TextField("Name/ID:", "", 32, TextField.ANY);
	private TextField fieldFlight = new TextField("Flight:", "", 32, TextField.ANY); 
	private DateField fieldDate = new DateField("", DateField.DATE, TimeZone.getTimeZone("GMT"));
	private ChoiceGroup fieldPurser = new ChoiceGroup("", Choice.MULTIPLE);
	private StringItem buttonSend = new StringItem("", "Send Request", Item.BUTTON);  
	private StringItem buttonSettings = new StringItem("", "Settings", Item.BUTTON);  
	
	private Command commandExit = new Command("Exit", Command.EXIT, 0);    
	private Command commandSend = new Command("Send", Command.OK, 0);   
	private Command commandSettings = new Command("Settings", Command.OK, 0);   
	
	private HelloWorldMidlet midlet; 
	
	public Main(HelloWorldMidlet m)
	{
		midlet = m;
		
    	fieldPurser.append("Purser", null);
        
        buttonSend.addCommand(commandSend);
        buttonSend.setItemCommandListener(this);   
        buttonSend.setLayout(ImageItem.LAYOUT_CENTER);       
        
        buttonSettings.addCommand(commandSettings);
        buttonSettings.setItemCommandListener(this);   
        buttonSettings.setLayout(ImageItem.LAYOUT_CENTER);  
        
        form.append(fieldName); 
        form.append(fieldDate); 
        form.append(fieldFlight);
        form.append(fieldPurser); 
        form.append(buttonSend);
        form.append(buttonSettings);
                              
        form.addCommand(commandExit);
        form.setCommandListener(this);   
	}
	
	public Form getForm() 
	{        
		return form; 
	}
	
	public void setCurrentItem()
	{
        if(fieldName.getString() != null && fieldName.getString().length() != 0)
    	{        	
        	midlet.getDisplay().setCurrentItem(fieldDate);
    	}
	}
	
	
    public void getData(UserData userData)
    {
    	userData.name = fieldName.getString().length() == 0 ? 
    			UserData.Nothing : fieldName.getString();
    	
    	userData.flight = fieldFlight.getString().length() == 0  ? 
    			UserData.Nothing : fieldFlight.getString();
    	
    	userData.date = fieldDate.getDate() == null ? 
    			UserData.Nothing : "" + fieldDate.getDate().getTime();
    	
    	boolean[] selected = new boolean[fieldPurser.size()];  
    	fieldPurser.getSelectedFlags(selected);
    	userData.purser = "" + selected[0];    	    	
    }
    

    public void setData(UserData userData)
    {  	
    	try
    	{
    		if(!userData.name.equals(UserData.Nothing))
    		{
    			fieldName.setString(userData.name);
    		}
    		
    		if(!userData.flight.equals(UserData.Nothing))
    		{
    			fieldFlight.setString(userData.flight);
    		}
    		
        	if(!userData.date.equals(UserData.Nothing))
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
    
    
    public void commandAction(Command command, Displayable displayable) 
    {      
    	try
    	{
	        if (displayable == form && command == commandExit) 
            {                                         
            	midlet.exitMIDlet();                                           
            }                                                  
    	}
    	catch(Exception e)
    	{
    		midlet.showMessage(e);
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

            	boolean[] selected = new boolean[fieldPurser.size()];
            	fieldPurser.getSelectedFlags(selected);
            	            	
            	// send SMS
            	SMS.send(midlet.getUserData().callCenter, fieldName.getString());
            	
            	//midlet.exitMIDlet();
            }  
	        else if (item == buttonSettings && command == commandSettings)
            {  
	        	midlet.showSettings();
            }
    	}
    	catch(Exception e)
    	{
    		midlet.showMessage(e);
    	}                                  
    }   
}
