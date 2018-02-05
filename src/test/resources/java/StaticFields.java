class StaticClass {
    public static int hello = 5;
}

public class StaticFields {
    public static void main(String[] args) {
        int world = StaticClass.hello;
        StaticClass.hello = world + 5;
        int world2 = StaticClass.hello;
    }
}
