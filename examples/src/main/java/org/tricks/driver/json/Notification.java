package org.tricks.driver.json;

/**
 *
 * @author Vitor Enes
 */
public class Notification extends Message {

    private String eventName;
    private Integer value;

    public Notification() {
    }

    public String getEventName() {
        return eventName;
    }

    public Integer getValue() {
        return value;
    }
}
