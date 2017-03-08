package clojure_exercises;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.stream.Collectors;

public class Filter {
    public static interface FilterOp<I>{
        boolean filter(I input);
    }

    public static <I> Collection<I> filter(Collection<I> c, FilterOp<I> op){
        Collection<I> res = new LinkedList<>();
        for(I i : c){
            if(op.filter(i))
                res.add(i);
        }
        return Collections.unmodifiableCollection(res);
    }

    public static void main(String[] args){
        Collection<Integer> numbers = new LinkedList<>();
        for(int i = 0; i < 5; i++){
            numbers.add(i);
        }

        //Iterative filtering
        Collection<Integer> evens = new LinkedList<>();
        for(Integer i : numbers){
            if(0 == i % 2) evens.add(i);
        }
        System.out.println(evens);

        //FilterOp - No Lambdas
        System.out.println(filter(numbers, new FilterOp<Integer>() {
            @Override
            public boolean filter(Integer i) {
                return i % 2 == 0;
            }
        }));

        //Filtering with Lambdas
        System.out.println(filter(numbers, i -> i % 2 == 0));

        //Java 9 Stream API
        System.out.println(numbers.stream().filter(i -> i % 2 == 0).collect(Collectors.toList()));
    }
}
