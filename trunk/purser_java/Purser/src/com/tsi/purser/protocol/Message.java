package com.tsi.purser.protocol;

import com.tsi.data.Log;
import com.tsi.purser.exceptions.ProtocolException;



public class Message 
{	
	private int mPhoneLen;
	private int mTextLen;
	private String mPhone = new String("");
	private String mText = new String("");
	
	private final char ProtocolSignature = 'Z';
	private final int MaxPhoneLen = 32;
	private final int MaxTextLen = 220;
	private final int PhoneOffset = 4;
	private final int TextOffset = PhoneOffset + MaxPhoneLen;
	private final int MessageSize = PhoneOffset + MaxPhoneLen + MaxTextLen;

	
	public String GetPhone() { return mPhone; }
	public String GetText() { return mText; }
	
	public void SetPhone(String phone) { mPhone = phone; }
	public void SetText(String text) { mText = text; }
	
	
	public Message() {}
	
	public Message(String str) throws ProtocolException
	{
		Parse(str);
	}

	
	void Parse(String str) throws ProtocolException
	{
		char signature = str.charAt(0);

		if(signature != ProtocolSignature)
		{
			throw new ProtocolException("Wrong protocol signature");
		}

		mPhoneLen = str.charAt(1);
		mTextLen = str.charAt(2);
		char crc = str.charAt(3);
		
		Log.write("crc: " + crc);

		for(int i = PhoneOffset; i < PhoneOffset + mPhoneLen; i++)
		{
		   mPhone += str.charAt(i);
		}

		for(int i = TextOffset; i < TextOffset + mTextLen; i++)
		{
		   mText += str.charAt(i);
		}
		

		if(crc != GetCRC(mText))
		{
			throw new ProtocolException("Wrong CRC");
		}
	}
	
	
	
	public String Serialize() 
	{
		String str = "";

		char crc = GetCRC(mText);
		
		char[] zeroes = new char[MaxPhoneLen + MaxTextLen];
		for (int i=0; i< zeroes.length; i++) 
		{
			zeroes[i] = '0';
        }

		str += ProtocolSignature;
	    str += (char)mPhone.length();
		str += (char)mText.length();
		str += crc;
					
		str += mPhone;
		str += new String(zeroes, 0, MaxPhoneLen - mPhone.length());
		str += mText;
		str += new String(zeroes, 0, MaxTextLen - mText.length());
		
		Log.write("crc: " + crc);
		Log.write("mText: " + mText);

		return str;
	}
	

	private char GetCRC(String str) 
	{
	    char crc = 0;
	    for(int i=0; i<str.length(); i++)
	    {
	        crc ^= str.charAt(i);
	    }
	
	    return crc;
	}
}
