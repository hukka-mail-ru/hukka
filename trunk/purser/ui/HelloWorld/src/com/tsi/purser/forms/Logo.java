package com.tsi.purser.forms;

import java.io.IOException;
import javax.microedition.lcdui.Canvas;
import javax.microedition.lcdui.Displayable;
import javax.microedition.lcdui.Graphics;
import javax.microedition.lcdui.Image;
import javax.microedition.lcdui.Item;

import com.tsi.purser.data.Log;

public class Logo extends Canvas implements Widget
{
	private Image image;
	private Image progress;
	private int left = 33;
	
	public Logo()
	{
		try 
		{
			image = Image.createImage("/logo.png");
			progress = Image.createImage("/progress.png");
		} 
		catch (IOException e) 
		{
			Log.write(e);
		}
	}
	
	protected void paint(Graphics g) 
	{
		g.drawImage(image, 0, 0, Graphics.TOP | Graphics.LEFT);
		g.drawImage(progress, left, 290, Graphics.TOP | Graphics.LEFT);
	}
	
	public Displayable getWidget() 
	{        
		return this; 
	}
	
	public Item getDefaultItem()
	{
		return null;
	}
	
	public void animate() 
	{
		try
		{
			for(int i=0; i<255; i++)
			{
				left++;
				repaint();
				Thread.sleep(10);
			}
		}
		catch(Exception e)
		{
			Log.write(e);
		}
			
	}

}