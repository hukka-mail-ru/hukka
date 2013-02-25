package com.example.helloworld;

import javax.microedition.lcdui.*;

public class Log 
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
		
		Alert alert = new Alert("Error");
		alert.setString(e.getMessage());
		display.setCurrent(alert, form);
	}
}
