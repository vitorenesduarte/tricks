package org.tricks.driver;

import java.io.IOException;
import java.util.List;
import org.tricks.driver.json.Discovery;
import org.tricks.driver.json.Event;
import org.tricks.driver.json.Notification;
import org.tricks.driver.json.Pod;
import org.tricks.driver.json.Pods;
import org.tricks.driver.json.Subscription;

/**
 *
 * @author Vitor Enes
 */
public class Tricks {

    private final Config config;
    private final Socket socket;

    public static Tricks init() throws IOException {
        Config config = Config.init();
        Socket socket = Socket.create(config.getTricksIp(), config.getTricksPort());
        Tricks tricks = new Tricks(config, socket);
        return tricks;
    }

    public Tricks(Config config, Socket socket) {
        this.config = config;
        this.socket = socket;
    }

    public Config getConfig() {
        return config;
    }

    public void registerEvent(String eventName) throws IOException {
        Event event = new Event(config.getExpId(), eventName);
        socket.send(event);
    }

    public void waitEvent(String eventName, Integer value) throws IOException {
        Subscription subscription = new Subscription(config.getExpId(), eventName, value);
        socket.send(subscription);

        Notification notification = socket.receive(Notification.class);
        assert (notification.getEventName().equals(eventName));
        assert (notification.getValue().equals(value));
    }

    public List<Pod> discover(String tag) throws IOException {
        Discovery discovery = new Discovery(config.getExpId(), tag);
        return discover(discovery);
    }

    public List<Pod> discover(String tag, Integer min) throws IOException {
        Discovery discovery = new Discovery(config.getExpId(), tag, min);
        return discover(discovery);
    }

    private List<Pod> discover(Discovery discovery) throws IOException {
        socket.send(discovery);

        Pods pods = socket.receive(Pods.class);
        assert (pods.getTag().equals(discovery.getTag()));
        return pods.getPods();
    }
}
