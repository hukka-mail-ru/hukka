package com.tsi.purser.data;

public class UserData 
{
	public static final String Header = "Crew Member Request";
	public static final String Nothing = "__Nothing__";
	
	public UserData() 
	{
		name = Nothing;
		flight = Nothing;
		date = Nothing;
		purser = Nothing;
		callCenter = Nothing;
	}
		
	public String name;
	public String flight;
	public String date;
	public String purser;
	public String callCenter;
}
