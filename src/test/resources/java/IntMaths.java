public class IntMaths {
    public static int add() {
        int hello = 10;
        return hello + 33;
    }

    public static int multiply() {
        int hello = 10;
        return hello * 3;
    }

    public static int divide() {
        int hello = 10;
        return hello / 3;
    }

    public static int minus() {
        int hello = 10;
        return hello - 3;
    }

    public static int neg() {
        int hello = 10;
        return -hello;
    }

    // Bytes 8-bit signed two's-complement integers. -128 to 127
    public static byte i2b() {
        int hello = 10;
        byte b = (byte) hello;
        return b;
    }

    public static byte i2bNeg() {
        int hello = -10;
        byte b = (byte) hello;
        return b;
    }

    public static byte i2bLimitMax() {
        int hello = 127;
        byte b = (byte) hello;
        return b;
    }

    public static byte i2bLimitMin() {
        int hello = -128;
        byte b = (byte) hello;
        return b;
    }

    // Chars 16-bit unsigned integers representing Unicode characters. 0 to 65535
    public static char i2c() {
        int hello = 10;
        return (char) hello;
    }

    public static char i2cLimitMax() {
        int hello = 65535;
        return (char) hello;
    }

    public static char i2cNeg() {
        int hello = -10;
        return (char) hello;
    }

    // Ints: 32-bit signed two's-complement integers. -2147483648 to 2147483647
    // Longs: 64-bit signed two's-complement integers. -9223372036854775808 to 9223372036854775807
    public static long i2l() {
        int hello = 10;
        return (long) hello;
    }

    public static long i2lLimitMax() {
        int hello = 2147483647;
        return (long) hello;
    }

    public static long i2lLimitMin() {
        int hello = -2147483648;
        return (long) hello;
    }

    public static long i2lNeg() {
        int hello = -10;
        return (long) hello;
    }

    public static int ishl() {
        int hello = 1;
        return hello << 2;
    }

    public static int ishr() {
        int hello = 4;
        return hello >> 2;
    }

    public static int ixor() {
        int hello = 5;
        return hello ^ 6;
    }

    public static void main(String[] args) {
        add();
    }
}
