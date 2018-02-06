public class IfEqBoolean {
    public static void main(String[] args) {
        int ret = 10;
        boolean isEqual = ret == 0;
        if (isEqual) {
            ret = 8;
        }
        else {
            ret = 5;
        }
    }
}
