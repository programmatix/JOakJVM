public class ReturnValFromFunc {
    public static int test(int input) {
        return input + 3;
    }

    public static void main(String[] args) {
        int hello = test(5);
        int world = hello;
    }
}
