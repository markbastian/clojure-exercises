package clojure_exercises;

import java.math.BigInteger;

public class Factorial {
    public static long factorial(long n){
        long f = 1;
        for(long i = 1; i <= n; i++){
            f *= i ;
        }
        return f;
    }

    public static BigInteger bigfactorial(long n){
        BigInteger f = BigInteger.ONE;
        for(long i = 1; i <= n; i++){
            f = f.multiply(BigInteger.valueOf(i));
        }
        return f;
    }

    public static void main(String[] args){
        System.out.println(factorial(1));
        System.out.println(factorial(10));
        System.out.println(factorial(20));
        System.out.println(factorial(30));
        System.out.println(bigfactorial(30));
    }
}
