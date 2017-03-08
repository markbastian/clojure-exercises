package clojure_exercises;

public class Fibonacci {
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
