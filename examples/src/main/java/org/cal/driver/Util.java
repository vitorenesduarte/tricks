package org.cal.driver;

/**
 *
 * @author user
 */
public class Util {

    public static Integer readEnvInt(String envVar) {
        return Integer.parseInt(System.getenv(envVar));
    }

    public static String readEnvString(String envVar) {
        return System.getenv(envVar);
    }
}
