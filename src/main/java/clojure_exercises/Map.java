package clojure_exercises;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.stream.Collectors;

public class Map {
    public static Collection<Integer> squareCollection(Collection<Integer> c){
        Collection<Integer> res = new LinkedList<>();
        for(Integer i : c){
            res.add(i * i);
        }
        return res;
    }

    public static interface MapOp<I, O>{
        O map(I input);
    }

    public static <I, O> Collection<O> map(Collection<I> c, MapOp<I, O> op){
        Collection<O> res = new LinkedList<>();
        for(I i : c){
            res.add(op.map(i));
        }
        return Collections.unmodifiableCollection(res);
    }

    public static void main(String[] args){
        Collection<Integer> numbers = new LinkedList<>();
        for(int i = 0; i < 5; i++){
            numbers.add(i);
        }

        //Iterative mapping
        Collection<Integer> squares = new LinkedList<>();
        for(Integer i : numbers){
            squares.add(i * i);
        }

        System.out.println(squares);

        System.out.println(squareCollection(numbers));

        //MapOp - No Lambdas
        System.out.println(map(numbers, new MapOp<Integer, Integer>() {
            @Override
            public Integer map(Integer input) {
                return input * input;
            }
        }));

        //Mapping with Lambdas
        System.out.println(map(numbers, input -> input * input));

        //Java 9 Stream API
        System.out.println(numbers.stream().map(input -> input * input).collect(Collectors.toList()));
    }
}
