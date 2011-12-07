/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package chess.client;

import javax.ejb.EJB;
import chess.ejb.Chess;
import java.util.concurrent.Future;
import java.util.concurrent.ExecutionException;

/**
 *
 * @author ssy
 */
public class Main {

    @EJB
    private static Chess server;
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        String i = server.move();
        System.out.println(i);
        
        try
        {
            Future future = server.win();

            while (!future.isDone()){
                Thread.sleep(1000);
                System.out.println("I do other things ...");
            }

            String ret = (String)future.get();
            System.out.println("ret : "+ret);
        
        }catch (InterruptedException ie){
           ie.printStackTrace();
        }catch (ExecutionException ee){
            ee.printStackTrace();
        }
         
         
    }
}
