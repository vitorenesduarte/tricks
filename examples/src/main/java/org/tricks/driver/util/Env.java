package org.tricks.driver.util;

/**
 *
 * @author user
 */
public class Env {

    public static Integer readInteger(String envVar) {
        return Integer.parseInt(System.getenv(envVar));
    }

    public static Integer readInteger(String envVar, Integer def) {
        Integer result = def;

        String val = System.getenv(envVar);
        if (val != null) {
            result = Integer.parseInt(val);
        }

        return result;
    }

    public static String readString(String envVar) {
        return System.getenv(envVar);
    }

    public static String readString(String envVar, String def) {
        String result = def;

        String val = System.getenv(envVar);
        if (val != null) {
            result = val;
        }

        return result;
    }
}
