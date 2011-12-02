/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package chess.ejb;

import javax.ejb.Stateless;

/**
 *
 * @author ssy
 */
@Stateless
public class ChessBean implements Chess {

     @Override
     public int move()
     {
         return 10;
     }
        
    // Add business logic below. (Right-click in editor and choose
    // "Insert Code > Add Business Method")
    
}
