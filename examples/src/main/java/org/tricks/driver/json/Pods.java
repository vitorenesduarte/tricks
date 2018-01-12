package org.tricks.driver.json;

import java.util.List;

/**
 *
 * @author Vitor Enes
 */
public class Pods extends Message {

    private String tag;
    private List<Pod> pods;
    
    public Pods(){
    }

    public String getTag() {
        return tag;
    }

    public List<Pod> getPods() {
        return pods;
    }
}