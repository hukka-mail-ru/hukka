package com.tsi.purser.exceptions;



public class ProtocolException extends Exception
{
	private static final long serialVersionUID = 1L;

	public ProtocolException(String description)
	{
		super("Protocol error: " + description);
	}
}