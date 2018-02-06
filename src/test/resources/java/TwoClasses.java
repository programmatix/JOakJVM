class Unmanaged {
    String str = "unmanaged";

    public Managed newManaged() {
        return new Managed();
    }

    public String getString() {
        return "unmanaged-got";
    }
}

class Managed {
    String str = "managed";

    public Unmanaged newUnmanaged() {
        return new Unmanaged();
    }

    public String getString() {
        return "managed-got";
    }
}

public class TwoClasses {
    public static String test1() {
        Managed m = new Managed();
        Unmanaged um = m.newUnmanaged();
        return um.str;
    }

    public static String test2() {
        Unmanaged um = new Unmanaged();
        Managed m = um.newManaged();
        return m.str;
    }

    public static String test3() {
        Managed m = new Managed();
        Unmanaged um = m.newUnmanaged();
        return um.getString();
    }

    public static String test4() {
        Unmanaged um = new Unmanaged();
        Managed m = um.newManaged();
        return m.getString();
    }

    public static void main(String[] args) {

    }
}
