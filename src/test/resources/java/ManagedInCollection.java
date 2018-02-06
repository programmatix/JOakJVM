import java.util.List;
import java.util.ArrayList;

class ManagedType {
    public String getString() {
        return "hello";
    }
}

public class ManagedInCollection {
    public static String test1() {
        List<ManagedType> list = new ArrayList<ManagedType>();
        ManagedType in = new ManagedType();
        list.add(in);
        return list.get(0).getString();
    }

    public static void main(String[] args) {
    }
}