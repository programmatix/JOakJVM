class Car {
    public String overrideMe() {
        return "car";
    }
}

class Audi extends Car {
    @Override
    public String overrideMe() {
        return "audi";
    }
}

public class InvokeVirtual {
    public static void main(String[] args) {
        Audi car = new Audi();
        String s = car.overrideMe();
        System.out.println(s);
    }
}
