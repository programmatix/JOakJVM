public class InvokeVirtualAsSuper {
    public static void main(String[] args) {
        Car car = new Audi();
        String s = car.overrideMe();
        System.out.println(s);
    }
}
