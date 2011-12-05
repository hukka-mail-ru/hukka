/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package chess.client;

import javax.ejb.EJB;
import chess.ejb.Chess;

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
    }
}
