/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package chess.ejb;

import javax.ejb.Stateless;
import java.sql.*;
import javax.ejb.Remote;
import java.util.concurrent.Future;
import javax.ejb.AsyncResult;
import javax.ejb.Asynchronous;


/**
 *
 * @author ssy
 */
@Stateless
public class ChessBean implements Chess {

     @Override
     public String move()
     {
         String name = getData();
                  
         return name;
     }
     
     @Asynchronous
     @Override
     public Future<String> win()
     {
         return new AsyncResult<String>("Win!");
     }
     
         
     public String getData() 
     {
        Connection con = null;
        String url = "jdbc:mysql://localhost:3306/";
        String dbName = "WapServer3DB";

        String userName = "WapServer3";
        String password = "win74";
        
        String res = "none";
        try {
            Class.forName("com.mysql.jdbc.Driver");
            con = DriverManager.getConnection(url + dbName, userName, password);
            Statement st = con.createStatement();
            ResultSet rs = st.executeQuery("select User from wsUsers where GUID=1");

            rs.next();
            res = rs.getString(1);
                    
            /*
            HashMap row;
            while (rs.next()) {
                row = new HashMap();
                row.put("First_Name", rs.getString(1));
                row.put("Last_Name", rs.getString(2));
                row.put("Address",rs.getString(3));
                row.put("Nationality",rs.getString(4));
                list.add(row);
            }             
             */
        
            st.close();
        } catch (Exception e) {
                e.printStackTrace();
        }
        
        return res;
    }
        
    // Add business logic below. (Right-click in editor and choose
    // "Insert Code > Add Business Method")
    
}
