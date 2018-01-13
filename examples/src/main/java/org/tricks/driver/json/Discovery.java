package org.tricks.driver.json;

/**
 *
 * @author Vitor Enes
 */
public class Discovery extends Message {

    private String tag;
    private Integer min;

    public Discovery() {
    }

    public Discovery(String expId, String tag) {
        super(expId, "discovery");
        this.tag = tag;
    }

    public Discovery(String expId, String tag, Integer min) {
        super(expId, "discovery");
        this.tag = tag;
        this.min = min;
    }

    public String getTag() {
        return tag;
    }

    public Integer getMin() {
        return min;
    }
}
