package com.example.helloworld;

public class Log 
{
	public static void write(Exception e)
	{
		 System.out.println("Exception!");
		 System.out.println(e.getMessage());
		 e.printStackTrace(); 
	}
}
