Note this must not be marked as a test source directory in IntelliJ.  E.g. IntelliJ must not be compiling these.
Otherwise they get added by IntelliJ to the test's classpath and then loaded by Java's classloader, rather than our own.
All sources in this dir are compiled by the tests, not IntelliJ.
