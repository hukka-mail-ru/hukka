package com.tsi.purser.forms;


import javax.microedition.lcdui.*;
import com.tsi.purser.Midlet;
import com.tsi.purser.data.*;

public class Done implements ItemCommandListener, CommandListener 
{
	private Form form = new Form(UserData.Header);
		    
	private StringItem  fieldDone = new StringItem ("Done",
			"Please wait for the answer by SMS");
	
	private StringItem buttonExit = new StringItem("", "Exit", Item.BUTTON);  
	private StringItem buttonNew = new StringItem("", "New request", Item.BUTTON);  
	
	private Command commandExit = new Command("Exit", Command.EXIT, 0); 
	private Command commandExitOK = new Command("Ok", Command.OK, 0); 
	private Command commandNew = new Command("New", Command.OK, 0);   
	
	private Midlet midlet; 
	
	public Done(Midlet m)
	{
		midlet = m;
		
		buttonExit.addCommand(commandExitOK);
		buttonExit.setItemCommandListener(this);   
		buttonExit.setLayout(ImageItem.LAYOUT_CENTER);       
        
		buttonNew.addCommand(commandNew);
		buttonNew.setItemCommandListener(this);   
		buttonNew.setLayout(ImageItem.LAYOUT_CENTER);  
        
        form.append(fieldDone); 
        form.append(buttonExit);
        form.append(buttonNew);
                              
        form.addCommand(commandExit);
        form.setCommandListener(this);   
	}
	
	public Form getForm() 
	{     
		midlet.getDisplay().setCurrentItem(buttonExit);
		return form; 
	}
	
	public void setCurrentItem()
	{
		midlet.getDisplay().setCurrentItem(buttonExit);
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
	        if (item == buttonExit && command == commandExitOK) 
            {       
            	midlet.exitMIDlet();
            }  
	        else if (item == buttonNew && command == commandNew) 
	        {
	        	midlet.showMain();
	        }	       
    	}
    	catch(Exception e)
    	{
    		midlet.showMessage(e);
    	}                                  
    }   
}
