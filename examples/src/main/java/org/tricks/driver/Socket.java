package org.tricks.driver;

import java.io.Closeable;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import org.tricks.driver.json.Message;

/**
 *
 * @author Vitor Enes
 */
public class Socket implements Closeable {

    private final DataRW rw;

    private Socket(DataRW rw) {
        this.rw = rw;
    }

    public static Socket create(String ip, Integer port) throws IOException {
        java.net.Socket socket = new java.net.Socket(ip, port);
        socket.setTcpNoDelay(true);

        DataInputStream in = new DataInputStream(socket.getInputStream());
        DataOutputStream out = new DataOutputStream(socket.getOutputStream());
        DataRW rw = new DataRW(in, out);

        return new Socket(rw);
    }

    public void send(Message message) throws IOException {
        this.rw.write(message);
    }

    public <T extends Object> T receive(Class<T> classOfT) throws IOException {
        return this.rw.read(classOfT);
    }

    @Override
    public void close() throws IOException {
        this.rw.close();
    }
}
