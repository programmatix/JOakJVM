        String s = new String("hello");

         0: new           #2                  // class java/lang/String
         3: dup
         4: ldc           #3                  // String hello
         6: invokespecial #4                  // Method java/lang/String."<init>":(Ljava/lang/String;)V
         
For core types like String, the closest in Java is newInstance(\[args\]) I think.  Approaches:

1. Enter a special scanning mode on new, gathering arguments to pass to newInstance. (NEWINST1)
2. Don't use the JVM at all, and create all objects even String in our own JVM.  Scary...
3. Create a special token on the new op and add it to stack, one that can be treated like a standard object ref.  It contains an Object that is supplied on the subsequent <init> call.

(NEWINST1) in more detail:
(1) didn't work, doesn't handle new String(byte[] (0x31)), so added and changed to (3) above

For types in the code being handled by our JVM, could:

1. Somehow turn the JVMClassFile into a proper Java Class.  Not sure if this is possible, and would mean code in that class would also be execute by the actual JVM. Kinda defeats the point...
2. Manage the memory and classes ourselves.  This is the approach I went with, JVMClassInstance.
