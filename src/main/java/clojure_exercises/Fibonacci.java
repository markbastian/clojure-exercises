package clojure_exercises;

import java.math.BigInteger;
import java.util.stream.Stream;

public class Fibonacci {
    private final BigInteger current;
    private final BigInteger next;

    private Fibonacci(BigInteger current, BigInteger next) {
        this.current = current;
        this.next = next;
    }

    private BigInteger current() {
        return current;
    }

    private Fibonacci next() {
        return new Fibonacci(next, current.add(next));
    }

    private static final Fibonacci SEED = new Fibonacci(BigInteger.ZERO, BigInteger.ONE);

    public static Stream<BigInteger> sequence(){
        return Stream.iterate(SEED, Fibonacci::next).map(Fibonacci::current);
    }
    
    //Old-style implementation
    public static long fibonacci(long n){
        long i = 0;
        long j = 1;
        for(long m = 0; m < n; m++){
            long tmp = j;
            j = i + j;
            i = tmp;
        }
        return i;
    }

    public static void main(String[] args){
        //BigInteger version?
        //Sequence vs. nth?
        for(int i = 0; i < 100; i++) {
            System.out.println(i + ": " + fibonacci(i));
        }
    }
}
