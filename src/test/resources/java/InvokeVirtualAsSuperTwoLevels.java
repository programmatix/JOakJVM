class Fruit { public String overrideMe() { return "fruit"; } }
class Citrus extends Fruit { @Override public String overrideMe() { return "citrus"; } }
class Lemon extends Citrus {  }

public class InvokeVirtualAsSuper {
    public static void main(String[] args) {
        Fruit lemon = new Lemon();
        String s = lemon.overrideMe(); // should be "citrus"
        System.out.println(s);
    }
}
