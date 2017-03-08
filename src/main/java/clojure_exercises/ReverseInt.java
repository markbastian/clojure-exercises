package clojure_exercises;

import java.util.ArrayList;
import java.util.List;

public class ReverseInt {
    public static long reverseInt(long n){
        long x = 0;
        do {
            x = 10 * x + n % 10;
            n /= 10;
        }while(n > 0);
        return x;
    }

    public static List<Integer> digits(long n){
        List<Integer> l = new ArrayList<>();
        do{
            l.add((int)(n % 10));
            n /= 10;
        }while(n > 0);
        return l;
    }

    public static void main(String[] args){
        System.out.println(reverseInt(1574720400));
        System.out.println(reverseInt(1572040932));
        System.out.println(digits(1574720400));
    }
}
