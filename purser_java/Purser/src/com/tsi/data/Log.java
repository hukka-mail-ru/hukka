package com.tsi.data;

public class Log 
{	
	public static void write(String str)
	{
		Error error = new Error("");
		System.out.println(error.getStackTrace()[1] + "\t" + str);
	}
	
	public static void write(Exception e)
	{
		 System.out.println("Exception!");
		 System.out.println(e.getMessage());
		 e.printStackTrace(); 
	}
}
