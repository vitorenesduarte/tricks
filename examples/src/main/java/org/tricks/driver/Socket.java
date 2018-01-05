package org.tricks.driver;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import org.tricks.driver.json.Message;

/**
 *
 * @author Vitor Enes
 */
public class Socket {

    private final DataRW rw;

    private Socket(DataRW rw) {
        this.rw = rw;
    }

    public static Socket create(Config config) throws IOException {
        java.net.Socket socket = new java.net.Socket(config.getHost(), config.getPort());
        socket.setTcpNoDelay(true);

        DataInputStream in = new DataInputStream(socket.getInputStream());
        DataOutputStream out = new DataOutputStream(socket.getOutputStream());
        DataRW rw = new DataRW(in, out);

        return new Socket(rw);
    }

    public void send(Message message) throws IOException {
        this.rw.write(message);
    }

    public Message receive() throws IOException {
        return this.rw.read();
    }

    public void closeRw() throws IOException {
        this.rw.close();
    }
}
