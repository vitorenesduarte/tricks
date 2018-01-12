package org.tricks.driver.json;

/**
 *
 * @author Vitor Enes
 */
public class Message {

    private String expId;
    private String type;

    public Message() {
    }

    public Message(String expId, String type) {
        this.expId = expId;
        this.type = type;
    }

    public String getExpId() {
        return expId;
    }

    public String getType() {
        return type;
    }
}
