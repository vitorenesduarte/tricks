package org.tricks.driver.json;

/**
 *
 * @author Vitor Enes
 */
public class Subscription extends Message {

    private String eventName;
    private Integer value;

    public Subscription() {
    }

    public Subscription(String expId, String eventName, Integer value) {
        super(expId, "subscription");
        this.eventName = eventName;
        this.value = value;
    }

    public String getEventName() {
        return eventName;
    }

    public Integer getValue() {
        return value;
    }
}
