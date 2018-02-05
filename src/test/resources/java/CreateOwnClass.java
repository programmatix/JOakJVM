class OwnClass {
    private String str;

    public OwnClass(String str) {
        this.str = str;
    }
}

public class CreateOwnClass {
    public static void main(String[] args) {
        OwnClass oc = new OwnClass("hello");
    }
}
