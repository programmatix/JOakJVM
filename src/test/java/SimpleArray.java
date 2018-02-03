
public class SimpleArray {
    public static void main(String[] args) {
        int[] hello = new int[5];
        for(int i = 0; i < hello.length; i ++) {
            hello[i] = 1;
        }
        hello[1] = 10;
        int world = hello[0];
        int world2 = hello[1];
    }
}
