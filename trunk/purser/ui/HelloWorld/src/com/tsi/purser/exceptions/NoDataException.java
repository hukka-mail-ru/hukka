package com.tsi.purser.exceptions;

public class NoDataException extends Exception
{
	public NoDataException(String missingData)
	{
		super("Please provide " + missingData);
	}
}
