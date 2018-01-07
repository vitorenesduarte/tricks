package org.tricks.driver;

/**
 *
 * @author user
 */
public class Util {

    public static Integer readEnvInt(String envVar) {
        return Integer.parseInt(System.getenv(envVar));
    }

    public static Integer readEnvInt(String envVar, Integer def) {
        Integer result = def;
        
        String val = System.getenv(envVar);
        if (val != null) {
            result = Integer.parseInt(val);
        }

        return result;
    }

    public static String readEnvString(String envVar) {
        return System.getenv(envVar);
    }
    
    public static String readEnvString(String envVar, String def) {
        String result = def;
        
        String val = System.getenv(envVar);
        if (val != null) {
            result = val;
        }

        return result;
    }
}
