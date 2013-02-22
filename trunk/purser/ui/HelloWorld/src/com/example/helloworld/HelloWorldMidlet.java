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
    }                            

    public void startMIDlet() 
    {                                      
        switchDisplayable(null, createForm());  
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
        if (displayable == form) {                                           
            if (command == exitCommand) 
            {                                         
                exitMIDlet();                                           
            }                                                  
        }                                                
    }                               

    
    
    public void commandAction(Command command, Item item) 
    {                                                 
        if (item == buttonOK) {                                                
            if (command == okCommand) 
            {              
            }                                                
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
     * Exits MIDlet.
     */
    public void exitMIDlet() {
        switchDisplayable (null, null);
        destroyApp(true);
        notifyDestroyed();
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

