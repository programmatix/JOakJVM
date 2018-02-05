import java.util.Arrays;
import java.util.List;

public class PassMultipleArgsToManaged {

    public static String test(int arg1, String arg2) {
        return arg2;
    }

    public static void main(String[] args) {
        String ret = test(5, "hello!");
    }

}
