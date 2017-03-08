package clojure_exercises;

public class CryptoSquare {
    public static String cryptoSquare(String s){
        String n = s.replaceAll("\\W+","").toLowerCase();
        int l = (int)Math.ceil(Math.sqrt(n.length()));
        return s.replaceAll("\\W+","").toLowerCase();
    }

    public static void main(String[] args){
        System.out.println(cryptoSquare(
                "A beginning is the time for taking the most delicate care that the balances are correct."));
    }
}
