import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class PassArrayListToCollectionUnmanaged {

    public static void main(String[] args) {
        List<String> src = Arrays.asList(new String[]{"help"});
        List<String> dest = new ArrayList<String>();
//        Collections.copy(dest, src);
        boolean result = Collections.disjoint(dest, src);
    }

}
