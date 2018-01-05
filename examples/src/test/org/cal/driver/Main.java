package org.cal.driver;

/**
 *
 * @author Vitor Enes
 */
public class Main {

    public static void main(String[] args) throws InterruptedException {
        String type = Util.readEnvString("TYPE");
        switch (type) {
            case "hello-world":
                helloWorld();
                break;
            case "server":
                server();
                break;
            case "client":
                client();
                break;
            default:
                System.out.println("Invalid type: " + type);
                break;
        }
    }

    private static void helloWorld() throws InterruptedException {
        Integer count = Util.readEnvInt("COUNT");

        for (int i = 0; i < count; ++i) {
            System.out.println(i + ") Hello World!");
            Thread.sleep(2000);
        }
    }

    private static void server() throws InterruptedException {
        while (true) {
            Thread.sleep(1000);
        }

    }

    private static void client() throws InterruptedException {
        Integer ops = Util.readEnvInt("OPS");
        for (int i = 0; i < ops; ++i) {
            System.out.println(i + ") OP!");
            Thread.sleep(2);
        }
    }
}
