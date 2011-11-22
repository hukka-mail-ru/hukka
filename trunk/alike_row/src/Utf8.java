import java.io.IOException;
import java.io.UnsupportedEncodingException;


public class Utf8 
{
    static public void println(String str)
    {
        try
        {           
            byte[] b = str.getBytes("UTF-8");
            System.out.write(b);
            System.out.println("");
        }
        catch(UnsupportedEncodingException ue) 
        {               
            System.out.println("Unsupported Encoding : " + ue);              
        }        
        catch(IOException e)
        {               
            System.out.println("Error : " + e.getMessage());               
        }
    }
}
