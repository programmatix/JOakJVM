import java.util.Arrays;
import java.util.List;

public class ArrayAsList {
    public static List<String> test(List<String> list) {
        return list;
    }

    public static void main(String[] args) {
        List<String> list = test(Arrays.asList(new String[] { "p", "pid" }));
    }
}
