/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package chess.ejb;

import javax.ejb.Remote;


@Remote
public interface Chess {
    
    public String move();

}
