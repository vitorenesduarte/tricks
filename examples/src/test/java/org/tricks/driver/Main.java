package org.tricks.driver;

import java.io.IOException;
import org.tricks.driver.util.Env;

/**
 *
 * @author Vitor Enes
 */
public class Main {

    public static void main(String[] args) throws InterruptedException, IOException {
        String type = Env.readString("TYPE", "");
        switch (type) {
            case "loop":
                loop();
                break;
            case "event-after-discovery":
                eventAfterDiscovery();
                break;
            default:
                System.out.println("Invalid type: " + type);
                break;
        }
    }

    private static void loop() throws InterruptedException {
        Integer seconds = Env.readInteger("SECONDS", Integer.MAX_VALUE);

        for (int i = 0; i < seconds; ++i) {
            System.out.println("[" + i + "]");
            Thread.sleep(1000);
        }
    }

    private static void eventAfterDiscovery() throws IOException, InterruptedException {
        Tricks tricks = Tricks.init();
        String tag = tricks.getConfig().getTag();
        Integer replicaNumber = tricks.getConfig().getReplicas();

        tricks.discover(tag, replicaNumber);
        tricks.registerEvent("discovered");
    }
}
