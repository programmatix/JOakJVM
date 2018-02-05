import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

public class PassArrayListToCollection {

    public static int test(List<String> c, String someArg) {
        return c.size();
    }

    public static void main(String[] args) {
        int ret = test(Arrays.asList(new String[]{"help"}), "hello!");
    }

}
