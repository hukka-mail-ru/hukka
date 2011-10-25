
import java.io.*;
import java.util.regex.*;
import java.lang.Exception;


public class Alike {

    public static final Pattern pattern = Pattern.compile("\\w+\\s+[012]+\\D*");
    
    

    private static void readFile(String fileName) 
    {
        String thisLine;
        
        try {
            BufferedReader br = new BufferedReader(new FileReader(fileName));
          
            while ((thisLine = br.readLine()) != null) 
            {
              //  System.out.println(thisLine);
                match(thisLine);
            } 
        } 
        catch (IOException e) {
          System.err.println("Ошибка: " + e);
        }
        catch (Exception e) {
            System.err.println("Ошибка: " + e);
        }
        
    }
    
	
    private static void match(String str) throws Exception
    {
        String output = "Validation for " + str;
        Matcher matcher = pattern.matcher(str);
        if (!matcher.matches())
            throw new Exception("неправильно сформирована строка: " + str);
        
        System.out.println(output);
    }
    
    
	
    public static void main(String[] args) 
    {
        System.out.println("Здравствуй, мир!");  
        readFile(args[0]);
	}
}
