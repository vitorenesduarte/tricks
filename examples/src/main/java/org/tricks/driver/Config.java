package org.tricks.driver;

import org.tricks.driver.util.Env;

/**
 *
 * @author Vitor Enes
 */
public class Config {

    private final String tag;
    private final Integer replicas;
    private final String expId;
    private final Integer podId;
    private final String podIp;
    private final String tricksIp;
    private final Integer tricksPort;

    public Config(String tag, Integer replicas, String expId, Integer podId, String podIp, String tricksIp, Integer tricksPort) {
        this.tag = tag;
        this.replicas = replicas;
        this.expId = expId;
        this.podId = podId;
        this.podIp = podIp;
        this.tricksIp = tricksIp;
        this.tricksPort = tricksPort;
    }

    public static Config init() {
        String tag = Env.readString("TAG");
        Integer replicas = Env.readInteger("REPLICAS");
        String expId = Env.readString("EXP_ID");
        Integer podId = Env.readInteger("POD_ID");
        String podIp = Env.readString("POD_IP");
        String tricksIp = Env.readString("TRICKS_IP");
        Integer tricksPort = Env.readInteger("TRICKS_PORT");

        Config config = new Config(tag, replicas, expId, podId, podIp, tricksIp, tricksPort);
        return config;
    }

    public String getTag() {
        return tag;
    }

    public Integer getReplicas() {
        return replicas;
    }

    public String getExpId() {
        return expId;
    }

    public Integer getPodId() {
        return podId;
    }

    public String getPodIp() {
        return podIp;
    }

    public String getTricksIp() {
        return tricksIp;
    }

    public Integer getTricksPort() {
        return tricksPort;
    }
}
