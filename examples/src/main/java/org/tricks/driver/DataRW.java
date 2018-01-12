package org.tricks.driver;

import com.google.gson.Gson;
import java.io.Closeable;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import org.tricks.driver.json.Message;

/**
 *
 * @author Vitor Enes
 */
public class DataRW implements Closeable {

    private final DataInputStream in;
    private final DataOutputStream out;
    private final Gson gson;

    public DataRW(DataInputStream in, DataOutputStream out) {
        this.in = in;
        this.out = out;
        this.gson = new Gson();
    }

    public void write(Message message) throws IOException {
        byte[] data = gson.toJson(message).getBytes();
        out.writeInt(data.length);
        out.write(data, 0, data.length);
        out.flush();
    }

    public <T extends Object> T read(Class<T> classOfT) throws IOException {
        int length = in.readInt();
        byte data[] = new byte[length];
        in.readFully(data, 0, length);
        String json = new String(data);
        return gson.fromJson(json, classOfT);
    }

    @Override
    public void close() throws IOException {
        this.in.close();
        this.out.close();
    }
}
