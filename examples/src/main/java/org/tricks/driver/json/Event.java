package org.tricks.driver.json;

/**
 *
 * @author Vitor Enes
 */
public class Event extends Message {

    private String eventName;

    public Event() {
    }

    public Event(String expId, String eventName) {
        super(expId, "event");
        this.eventName = eventName;
    }

    public String getEventName() {
        return eventName;
    }
}
