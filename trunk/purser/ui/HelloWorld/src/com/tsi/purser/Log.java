package com.tsi.purser;

import javax.microedition.lcdui.*;

class Log 
{
	public static void write(String str)
	{
		System.out.println(str);
	}
	
	public static void write(Exception e)
	{
		 System.out.println("Exception!");
		 System.out.println(e.getMessage());
		 e.printStackTrace(); 
	}

	public static void show(Display display, Form form, Exception e)
	{	
		Log.write(e);
		Log.show(display, form, e.getMessage());
	}
	
	public static void show(Display display, Form form, String str)
	{				
		Alert alert = new Alert("Error");
		alert.setString(str);
		display.setCurrent(alert, form);
	}
}
