/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package chess.ejb;


import javax.ejb.Remote;
import java.util.concurrent.Future;
import javax.ejb.Asynchronous;

@Remote
public interface Chess {
    
    public String move();
    
    public Future<String> win();


}
