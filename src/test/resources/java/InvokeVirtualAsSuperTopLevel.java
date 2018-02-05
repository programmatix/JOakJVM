class Fruit2 { public String overrideMe() { return "fruit"; } }
class Citrus2 extends Fruit2 { }
class Lemon2 extends Citrus2 {  }

public class InvokeVirtualAsSuperTopLevel {
    public static void main(String[] args) {
        Fruit2 lemon = new Lemon2();
        String s = lemon.overrideMe(); // should be "fruit"
    }
}
