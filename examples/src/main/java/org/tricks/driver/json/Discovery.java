package org.tricks.driver.json;

/**
 *
 * @author Vitor Enes
 */
public class Discovery extends Message {

    private String tag;

    public Discovery() {
    }

    public Discovery(String expId, String tag) {
        super(expId, "discovery");
        this.tag = tag;
    }

    public String getTag() {
        return tag;
    }
}
