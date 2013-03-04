package com.tsi.purser.exceptions;

public class InvalidSymbolException extends Exception
{
	public InvalidSymbolException(String where)
	{
		super("Invalid symbol(s) in '" + where + "' field");
	}
}
