package clojure_exercises;

import java.util.Collection;
import java.util.LinkedList;

public class Reduce {
    public static interface ReduceOp<I, O>{
        public O reduce(O a, I b);
    }

    public static <I, O> O reduce(Collection<I> c, O identity, ReduceOp<I, O> op){
        O res = identity;
        for(I i : c){
            res = op.reduce(res, i);
        }
        return res;
    }

    public static void main(String[] args){
        Collection<Integer> numbers = new LinkedList<>();
        for(int i = 0; i < 5; i++){
            numbers.add(i);
        }

        //Iterative reduction
        Integer sum = 0;
        for(Integer i : numbers){
            sum += i;
        }
        System.out.println(sum);

        //FilterOp - No Lambdas
        System.out.println(reduce(numbers, 0, new ReduceOp<Integer, Integer>() {
            @Override
            public Integer reduce(Integer a, Integer b) {
                return a + b;
            }
        }));

        //Filtering with Lambdas
        System.out.println(reduce(numbers, 0, (a, b) -> a + b));

        //Java 9 Stream API
        System.out.println(numbers.stream().reduce(0, (i, j) -> i + j));

        //A "fold"
        System.out.println(reduce(numbers, "", (a, b) -> a + b));
    }
}
