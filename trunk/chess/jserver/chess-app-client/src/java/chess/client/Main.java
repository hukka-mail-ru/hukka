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
    private static Chess chess;
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        int i = chess.move();
        System.out.println(i);
    }
}
