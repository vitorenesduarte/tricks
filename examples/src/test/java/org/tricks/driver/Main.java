package org.tricks.driver;

/**
 *
 * @author Vitor Enes
 */
public class Main {

    public static void main(String[] args) throws InterruptedException {
        String type = Util.readEnvString("TYPE", "");
        switch (type) {
            case "loop":
                loop();
                break;
            default:
                System.out.println("Invalid type: " + type);
                break;
        }
    }

    private static void loop() throws InterruptedException {
        Integer iterations = Util.readEnvInt("ITERATIONS", Integer.MAX_VALUE);

        for (int i = 0; i < iterations; ++i) {
            System.out.println("[" + i + "]");
            Thread.sleep(1000);
        }
    }
}
