package com.tsi.purser.forms;

import java.io.IOException;
import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Displayable;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Image;

import com.tsi.purser.data.Log;

public class Logo extends Canvas 
{
	private Image image;
	
	public Logo()
	{
		try 
		{
			image = Image.createImage("/logo.png");
		} 
		catch (IOException e) 
		{
			Log.write(e);
		}
	}
	
	protected void paint(Graphics g) 
	{
		g.drawImage(image, 0, 0, Graphics.TOP | Graphics.LEFT);
		
	}
	
	public Displayable getForm() 
	{        
		return this; 
	}
}