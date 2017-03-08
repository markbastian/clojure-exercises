<pre class="clojure" style="font-size: 60px;width: 120%;left: -10%;;margin:auto">
<code>((juxt :clojure :java) problems)</code></pre>

```
#{ :twitter @mark_bastian
   :work-email mbastian@clearwateranalytics.com 
   :personal-email markbastian@gmail.com }
```

---

###Mapping: Square all members of a collection

<pre>
<caption>Java (Pre-lambdas)</caption>
<code>Collection&lt;Integer&gt; c = new LinkedList<>();
for(int i = 0; i < 5; i++){
    c.add(i);
}

Collection&lt;Integer&gt; res = new LinkedList<>();
for(Integer i : c){
    res.add(i * i);
}
</code></pre>

<pre>
<caption>Clojure</caption>
<code>(map #(* % %) (range 5))
=> (0 1 4 9 16)
</code></pre>

---

###Mapping

```java
public static interface MapOp<I, O>{
    public O map(I input);
}

public static Collection<Integer> map(Collection<Integer> c, 
MapOp<Integer, Integer> op){
    Collection<Integer> res = new LinkedList<>();
    for(Integer i : c){
        res.add(op.map(i));
    }
    return res;
}

System.out.println(map(c, new MapOp<Integer, Integer>() {
    @Override
    public Integer map(Integer input) {
        return input * input;
    }
}));
```

---

###Exercise 1: Factorial

<pre>
<caption>Java</caption>
<code>public static long factorial(long n){
    long f = 1;
    for(long i = 1; i <= n; i++){
        f *= i ;
    }
    return f;
}
</code></pre>

<pre>
<caption>Clojure</caption>
<code>(defn factorial [n]
  (reduce * (map inc (range n))))
</code></pre>

---

###Exercise 1A: Big Factorial

<pre>
<caption>Java</caption>
<code>
public static BigInteger bigfactorial(long n){
    BigInteger f = BigInteger.ONE;
    for(long i = 1; i <= n; i++){
        f = f.multiply(BigInteger.valueOf(i));
    }
    return f;
}
</code></pre>

<pre>
<caption>Clojure</caption>
<code>(defn factorial [n]
  (reduce * (map (comp bigint inc) (range n))))
</code></pre>

---

#Introduction to Clojure
##Mark Bastian

```clojure
#{ @mark_bastian
   mbastian@clearwateranalytics.com
   markbastian@gmail.com }
```

`git clone https://github.com/markbastian/clojure-intro.git`

---

#Quick Intro

----

#About Clojure
* Clojure is a Lisp dialect
* It is hosted
  * Clojure targets the JVM
  * ClojureScript targets JS
  * There are other efforts

----

#Why?
* There is a resurgence in functional languages
* Clojure is a full-stack, multipurpose language
* Clojure has extremely powerful abstractions on data
* Clojure is highly expressive
* Clojure has a consistent track record of success

----

#Who?
* Amazon
* eBay
* Puppet Labs
* Walmart
* Clearwater Analytics
* [And many, many others...](http://clojure.org/community/companies)

---

#Getting Started

----

#[Leiningen](http://leiningen.org/)
* The most popular Clojure build tool
* To get started, do the following:
  1. `lein new clojure-intro`
  2. `cd clojure-intro`
  3. `lein repl`
* REPL = Read-Evaluate-Print-Loop

----

#REPL Intro
Try the following:
* `(prn "Hello World!")`
* `(+ 0 1 2 3 4 5 6 7 8 9)`
* `(inc 0)`
* `(def r (range 10))`
* `(prn r)`
* `(map inc r)`

----

#REPL Help
Try the following:
* `(doc def)`
* `(source map)`
* `(javadoc java.lang.Math)`

----

#IDEs
* Clojure has low IDE dependence, but...
* Popular options
  * Cursive Clojure IntelliJ Plugin
  * Emacs
  * Lighttable
  * Nightcode
* We'll be using the Community Edition of Cursive

----

#Cursive Clojure
* Start IntelliJ IDEA
* File > Open...
* Navigate to the <font color="red">`clojure-intro`</font> folder
* Select the project.clj file and press OK

----

##Navigating your project
* Select the *Project* vertical tab on the left side of the frame
* If the above is not visible, click the box at bottom left and select *Project*
* You can see a tree view representing your project
* Your code will be located in the src folder

----

#Launching a REPL
* Right-click on the project folder
* Select "Run 'REPL for clojure-intro'"
* A REPL window will appear at right
* Ctrl+M will load a Clojure file (ns)
* Ctrl+T will evaluate the current form
* Everything works as it did in the shell

---

#Clojure is Data

----

#Clojure as Data
* Clojure is Homoiconic
  * This just means it is written in its own data structures
  * Code as data
* It excels at data manipulation
* The data abstraction is very powerful, as we shall see

----

# Basic Types
The following basic primitives are present in Clojure:
* Strings: `"This is a string"`
* Numbers
  * Double: `3.14159`
  * Long: `42`
  * Ratio (Clojure only): `22/7`
* Characters (Not in cljs): `\A`
* Booleans: `true` or `false`
* Keywords: `:these :are :keywords`
* `nil`

----

### Collections are Literal Data
In addition to the above primitives the following data 
structures are also treated literally....

----

# Vector
```clojure
[] ;An empty vector
[1 2 3 4 5 6]
```

A Clojure vector is an array-like object with the following properties:
* Fast lookup by index
* Efficient tail addition and removal

----

# Map
```clojure
{}
{ :name "Mark" :year 2016 }
```

A Clojure map is a hash map with the following properties:
* Efficient insertion and removal
* It is common to use keywords as map keys
* *Anything* can be a map key or value

----

# Set
```clojure
#{}
#{ 1 2 3 4 5 }
```

A Clojure set is a standard hashset with the following properties:
* Efficient insertion and removal

----

# List
```clojure
()
'(1 2 3 4 5)
```
A Clojure list has the following properties:
* Efficient head addition and removal
* Lists are not indexed
* Lists generally aren't used for data modeling
* Lists are evaluated unless quoted

----

#Summary
```clojure
(def all-the-data
  { :string "This is a string"
    :characters \A
    :numbers [1 2.3 (/ 22 7)]
    :booleans #{ true false }
    :nil nil})
```

----

#Exercise
Model this scenario using Clojure data structures:
* RPG Character Stats
  * Name: String
  * Race: Human, Elf, Orc, Halfling
  * Class: Figher, Magic User, Cleric
  * HP: Integer
  * Attributes: STR, CON, DEX, INT, WIS, CHA (3 - 18)

----

#A Solution
```clojure
{
  :name "Dirk the Daring"
  :race :elf
  :class :fighter
  :HP 24
  :attributes {
    :STR 15
    :CON 12
    :DEX 17
    :INT 9
    :WIS 12
    :CHA 4
  }}
```

----

#Congratulations!
You've just learned 1/2 of the Clojure programming language.

---

#Evaluation

----

#[Evaluation](https://clojure.org/reference/evaluation)
Clojure is a very simple language, following these rules:
1. Primitives evaluate to themselves
2. Symbols are resolved
3. Vectors, sets, and maps yield vectors, sets, and maps whose contents are evaluated
4. Lists are *calls* to a function, special form, or macro

This is the other 1/2 of the language

----

#Functions
Functions are generally defined by a **list** containing:
  * defn: A macro joining the def and fn special forms
  * A symbol naming the function
  * A vector of arguments
  * The function body
  * The last form is returned
  
```clojure
(defn add [a b] (+ a b))
=> #'user/add
(add 2 4)
=> 6
```

----

#Defining Functions
Explicit Functions
```clojure
(defn add[a b](+ a b))
```

Lambdas
```clojure
(fn [a b](+ a b))
```

Shorthand Lambda Syntax
```clojure
#(+ %1 %2)
#(Math/sqrt %)
```

----

#Exercise
Write a function that computes the average of 3 numbers.

----

#Solutions
```clojure
(defn avg[a b c]
  (/ (+ a b c) 3.0))
=> #'examples.rpg/avg
(avg 7 42 3.14)
=> 17.38
((fn [a b c](/ (+ a b c) 3.0))
  7 42 3.14)
=> 17.38
(#(/ (+ %1 %2 %3) 3.0)
  7 42 3.14)
=> 17.38
```

----

#[Special Forms](http://clojure.org/reference/special_forms)
* Calls that do not follow the regular rules of evaluation
* Required building blocks for the language
* `(pprint (keys (. clojure.lang.Compiler specials)))`
* `&, monitor-exit, case*, try, reify*, finally, loop*, do, letfn*, if, 
clojure.core/import*, new, deftype*, let*, fn*, recur, set!, ., var, 
quote, catch, throw, monitor-enter, def`

----

#The if special form
What if this expression followed the standard rules of evaluation?
```clojure
(if true 
  (cheap-calculation) 
  (really-expensive-calculation-followed-by-world-war-3))
```
* Normally all inner forms are evaluated
* `if` only evaluates one branch
* Special forms are otherwise like regular functions

----

#Macros
* Functions evaluate their contents
* Macros take unevaluated forms, process them, then return the result for evaluation
* This allows developers to extend the language as desired
* This is what makes Clojure homoiconic

----

###Macros: Motivational Example
Q: Are these statements identical?

Clojure
```clojure
(+ 1 2)
```

Java
```java
1 + 2;
```
<p class="fragment">A: No, because the Clojure form is unevaluated.</p>

----

###Macros: Motivational Example
* Until evaluation, `(+ 1 2)` is a list containing a symbol and two values.
* A macro can manipulate this form prior to evaluation.
* This is what is meant as *homoiconicity* or "code as data."

----

#A Simple Macro
```clojure
(defmacro bizarro [[f & r]]
  (case f
    + (apply - r)
    - (apply + r)
    / (apply * r)
    * (apply / r)))
```

----

#A Bizarre World of Macros
```clojure
(+ 1 2)
=> 3
(bizarro (+ 1 1))
=> 0
(+ 3 2)
=> 5
(bizarro (+ 3 2))
=> 1
```

----

#Macros
* Take unevaluated forms
* Manipulate them
* Return the manipulated form to be evaluated
* Allow you to extend the language

----

##Clojure in 1 slide
###Clojure is data...
```clojure
(def all-the-data
  { :string "This is a string"
    :characters \A
    :numbers [1 2.3 (/ 22 7)]
    :booleans #{ true false }
    :nil nil})
```
###...That can be evaluated
* Resolve the symbols
* Lists are calls to special forms, macros, or functions
* Evaluate everything else
* (ᗧ • • •)

---

#Functional Sorcery

----

#First Class & Higher order functions
* First class: functions are a type of the language
* Higher order: functions can be arguments and/or 
return values from other functions
* Can you think of a higher order function from mathematics?
  * <p class="fragment">Derivative</p>
  * <p class="fragment">Integral</p>
  * <p class="fragment">Differential Equation</p>

----

#Partial
* Partially apply a function of arity n
* If m arguments are applied, the resulting function has (- n m) arguments

```clojure
(defn add[a b](+ a b))
(def add-3 (partial add 3))
(add-3 7)
=> 10
```

----

#Composition
Compose functions right to left, as in mathematics

```clojure
(def x (comp dec count str inc))
;What do these do?
(x 1)
(x 1000)
```

----

##Collections are Functions
* Vectors are functions of their indices
* Maps are functions of their keys
* Sets are functions of their contents

```clojure
([1 "ABC" 3.14 42] 1)
({:one 1 :str "ABC" :pi 3.14 :the-answer 42} :pi)
(#{1 "ABC" 3.14 42} "ABC")
```

----

#Keywords are Functions
* Keywords as fields is common
* Makes data modeling easy

```clojure
(:pi {:one 1 :str "ABC" :pi 3.14 :the-answer 42})
```

----

#And More...
* There are many other functions for creating functions
* `identity`, `repeatedly`, `complement`, `juxt`, ...

---

#Collections & [The Sequence Abstraction](http://clojure.org/reference/sequences)

----

##Collections
Collections have a uniform interface for 
deep access, modification, and update

```clojure
(get [1 2 3 4] 3)
=> 4
(get #{"ABC" 4} 4)
=> 4
(get-in {:a 1 :b {:c [1 2 3 4]}} [:b :c 0])
=> 1
;Associative collections
(assoc [1 2 3 4] 1 5)
=> [1 5 3 4]
(assoc-in {:a 1 :b {:c 4}} [:b :c] {:4 4})
=> {:a 1, :b {:c {:4 4}}}
(update [1 2 3 4] 1 inc)
=> [1 3 3 4]
(update-in {:a 1 :b {:c 4}} [:b :c] #(* % %))
=> {:a 1, :b {:c 16}}
```

----

>"It is better to have 100 functions operate on one data structure 
than 10 functions on 10 data structures."

*-Alan Perlis*

----

##The Sequence Abstraction
* Clojure collections provide access to their elements as sequences
* `seq`: An abstraction over sequences in Clojure
* There are a **huge** number of functions that operate on seqs, and thus, all collections
* A few: `map`, `reduce`, `filter`, `first`, `rest`, `reverse`, 
`count`, `distinct`, `partition`, `flatten`, `sort`, `filter`, 
`frequencies`, and many, many more
* Let's look at some of the most common

----

#Map
* Apply a function to all members of a collection
* If n collections are provided, the function takes n arguments
and is applied to each collection simultaneously

```clojure
(map inc [1 2 3 4 5])
=> (2 3 4 5 6)
(map * [1 2 3][2 9 5])
=> (2 18 15)
(map #(%1 %2) [str inc dec](repeat 3))
=> ("3" 4 2)
```

----

#Reduce
* Computes a single value by applying a function pairwise
to each member of a collection
* A "seed" value can be provided as a first argument
to the first function application

```clojure
(reduce + [1 2 3 4 5])
=> 15
(reduce #(str %1 %2) "" [1 2 3 4 5])
=> "12345"
```

----

#Filter
* Keep only elements passing a predicate
```clojure
(filter even? (range 10))
=> (0 2 4 6 8)
```

----

#Exercises
Open *examples/rpg.clj* to find the following data structure:
```clojure
(def fellowship
  [{:name "Gandalf"
    :race :human
    :class :magic-user
    :HP 100
    :attributes {:STR 13 :CON 13 :DEX 17 :INT 18 :WIS 18 :CHA 16}}
   {:name "Frodo"
    :race :halfling
    :class :thief
    :HP 24
    :attributes {:STR 9 :CON 12 :DEX 17 :INT 16 :WIS 12 :CHA 16}}
    ...
```
Then, work through the following exercises.

----

##Questions:
* How many members of the party are there?
  * <p class="fragment">`(count fellowship)`</p>
* What are the names of each character?
  * <p class="fragment">`(map :name fellowship)`</p>
* What is the total number of HP in the group?
  * Hint: Combine map and reduce
  * <p class="fragment">`(reduce + (map :HP fellowship))`</p>
* Increase all HP by 10
  * <p class="fragment">`(map #(update % :HP + 10) fellowship)`</p>

----

#Bonus Question
What is the distribution of the classes of the halflings?
* Hint `frequencies`

<pre class="fragment">
<code>(frequencies
  (map
  :class
  (filter 
    (comp #{:halfling} :race) 
    fellowship)))
</code>
</pre>

----

#Threading Macros
Values can be "threaded" through forms
* Each value is fed into the next form
* The computed value is likewise fed forward
* `->`, `->>` are most common (Thread first and last, respectively)
* cond, some, and as variants also exist (e.g. `some->>`)
* `->>` is often used to make pipelines

```clojure
(->> fellowship
     (filter (comp #{:halfling} :race))
     (map :class)
     frequencies)
```

<p class="fragment">Look Ma, less parens!</p>

---

##Case Studies
* Identifying Names in Text
* Analyzing Election Data

---

##Case Study:
##Identifying names in text
Open `examples.gutenberg.clj`
```clojure
(ns examples.gutenberg
  (:require [clojure.string :as s]))
```

----

##Challenge
* Can we extract proper nouns from text?
* Can we identify a work from these proper nouns?

----

##Source & Normalize Data
```clojure
(defonce pride-and-prejudice
         (slurp "https://www.gutenberg.org/files/1342/1342-0.txt"))

(defonce normalized
         (-> pride-and-prejudice
             (s/replace #"\W+" "|")
             (s/split #"\|")
             (->> (map s/upper-case))))
```

----

##REPL'ing Around (1)
```clojure
(->> normalized
     frequencies
     (sort-by second #(compare %2 %1))
     (map first)
     (take 20))
```
* `("THE" "TO" "OF" "AND" "HER" ...)`
* The most frequent words
* Mostly conjunctions, articles, pronouns, etc.

----

##REPL'ing Around (2)
```clojure
(->> normalized
     (partition 2 1)
     (map (partial s/join " "))
     frequencies
     (sort-by second #(compare %2 %1))
     (map first)
     (take 20))
```
* `("OF THE" "TO BE" "IN THE" "I AM" "MR DARCY" ...)`
* The most frequent two word pairs
* Mostly conjunctions, articles, pronouns, etc.
* We found a name!
* What if we removed the "junk words?"

----

##Common words filter
```clojure
(defonce common-words
         (->> "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"
              slurp
              s/split-lines
              (map s/upper-case)))

(defn top-words [n]
  (complement (into #{} (take n common-words))))
```

----

##Frequent Word Pairs
```clojure
(def filtered-normalized
  (->> normalized
       (filter (top-words 500))))

(def word-pairs
  (->> filtered-normalized
       (partition 2 1)
       (map (partial s/join " "))))

(->> word-pairs
     frequencies
     (sort-by second #(compare %2 %1))
     (take 10))
```
<pre class="fragment">
<code>(["MR DARCY" 272]
 ["MRS BENNET" 153]
 ["MR COLLINS" 150]
 ["LADY CATHERINE" 116]
 ["MR BINGLEY" 115]
 ["MR BENNET" 89]
 ["MISS BINGLEY" 87]
 ["MR WICKHAM" 73]
 ["MISS BENNET" 65]
 ["MRS GARDINER" 58])
</code>
</pre>

----

##Bring it all together
```clojure
(defn normalize [text]
  (-> text
      (s/replace #"\W+" "|")
      (s/split #"\|")
      (->> (map s/upper-case))))

(defn name-finder [url & {:keys [filter-top] :or {filter-top 1500}}]
  (->> url
       slurp
       normalize
       ;Defined previously
       (filter (top-words filter-top))
       (partition 2 1)
       (map (partial s/join " "))
       frequencies
       (sort-by second #(compare %2 %1))))
```

----

##Pride & Prejudice
```clojure
(take 20
      (name-finder
        "https://www.gutenberg.org/files/1342/1342-0.txt"
        :filter-top 200))
```
<pre class="fragment">
<code>(["MR DARCY" 272]
 ["MRS BENNET" 153]
 ["MR COLLINS" 150]
 ["LADY CATHERINE" 116]
 ["MR BINGLEY" 115]
 ["MR BENNET" 89]
 ["PROJECT GUTENBERG" 87]
 ["MISS BINGLEY" 87]
 ["MR WICKHAM" 73]
 ["MISS BENNET" 65]
 ["MRS GARDINER" 58]
 ["GUTENBERG TM" 57]
 ["SIR WILLIAM" 44]
 ["MISS DARCY" 39]
 ["YOUNG MAN" 38]
 ["TOO MUCH" 37]
 ["MR GARDINER" 33]
 ["DARE SAY" 31]
 ["COLONEL FITZWILLIAM" 30]
 ["MRS COLLINS" 29])
</code>
</pre>

----

#What is this?
<p class="fragment">Alice’s Adventures in Wonderland</p>
```clojure
(["PROJECT GUTENBERG" 87]
 ["GUTENBERG TM" 57]
 ["MOCK TURTLE" 56]
 ["MARCH HARE" 31]
 ["THOUGHT ALICE" 27]
 ["WHITE RABBIT" 22]
 ["TM ELECTRONIC" 18]
 ["ELECTRONIC WORKS" 17]
 ["ALICE THOUGHT" 14]
 ["GUTENBERG LITERARY" 13]
 ["LITERARY ARCHIVE" 13]
 ["ARCHIVE FOUNDATION" 13]
 ["POOR ALICE" 11]
 ["ALICE COULD" 11]
 ["ALICE DID" 11]
 ["DON KNOW" 10]
 ["OFF HEAD" 10]
 ["UNITED STATES" 10]
 ["1 F" 10]
 ["ALICE HERSELF" 10])
```

----

#What is this?
<p class="fragment">Frankenstein; Or, The Modern Prometheus</p>
```clojure
(["PROJECT GUTENBERG" 87]
 ["GUTENBERG TM" 57]
 ["OLD MAN" 34]
 ["TM ELECTRONIC" 18]
 ["ELECTRONIC WORKS" 17]
 ["NATIVE COUNTRY" 15]
 ["NATURAL PHILOSOPHY" 14]
 ["HUMAN BEING" 14]
 ["TAKEN PLACE" 13]
 ["GUTENBERG LITERARY" 13]
 ["LITERARY ARCHIVE" 13]
 ["MR KIRWIN" 13]
 ["ARCHIVE FOUNDATION" 13]
 ["FELLOW CREATURES" 12]
 ["BEFORE EYES" 10]
 ["DEAR VICTOR" 10]
 ["LOOKED UPON" 10]
 ["NOTHING COULD" 10]
 ["UNITED STATES" 10]
 ["1 F" 10])
```

----

#What is this?
<p class="fragment">A Tale of Two Cities</p>
```clojure
(["MR LORRY" 342]
 ["MISS PROSS" 156]
 ["MADAME DEFARGE" 122]
 ["MR CRUNCHER" 101]
 ["PROJECT GUTENBERG" 87]
 ["DOCTOR MANETTE" 80]
 ["GUTENBERG TM" 57]
 ["MR STRYVER" 55]
 ["CHARLES DARNAY" 54]
 ["WINE SHOP" 52]
 ["MISS MANETTE" 51]
 ["SAINT ANTOINE" 50]
 ["MENDER ROADS" 45]
 ["YOUNG LADY" 36]
 ["BEFORE HIM" 36]
 ["SYDNEY CARTON" 35]
 ["UPON HIM" 34]
 ["MR DARNAY" 32]
 ["MONSIEUR DEFARGE" 31]
 ["MR CARTON" 29])
```

----

##Observations
* Clojure development is interactive
* Work with the data until you've made your discoveries
* Gather the useful parts into functions

---

##Case Study:
##Analyzing Election Data
Open `examples.election_data.clj`
```clojure
(ns examples.election-data
  (:require [dk.ative.docjure.spreadsheet :as xl]))
```

----

##Load Popular Vote Data
```clojure
(def popular-vote
  (->> (xl/load-workbook "2012pres.xls")
       (xl/select-sheet "Table 1. 2012 Pres Popular Vote")
       (xl/select-columns {:A :candidate-party
                           :B :votes
                           :C :votes-by-percent})
       (filter (fn [{:keys [votes votes-by-percent]}]
                 (every? number? [votes votes-by-percent])))
       (map #(update % :votes-by-percent * 100))
       (filter #(> (:votes-by-percent %) 0.5))))
```
* Keep only those with > 0.5% of the vote
* Who's left (2012)?

<pre class="fragment">
<code>({:candidate-party "Barack Obama (Democrat)", 
  :votes 6.5915795E7, :votes-by-percent 51.06370658000776}
 {:candidate-party "Mitt Romney (Republican)", 
  :votes 6.0933504E7, :votes-by-percent 47.20402096565367}
 {:candidate-party "Gary Johnson (Libertarian, Independent)", 
  :votes 1275971.0, :votes-by-percent 0.9884703468811851})
</code>
</pre>

----

##Load state data
```clojure
(def state-vote
  (->> (xl/load-workbook "2012pres.xls")
       (xl/select-sheet "Table 2. Electoral &  Pop Vote")
       (xl/select-columns {:A :state
                           :B :dem-electors
                           :C :gop-electors
                           :D :dem-popular
                           :E :gop-popular
                           :F :other-popular
                           :G :total-vote})
       (drop 4)
       (take 51)
       ;Not shown - gap fill empty values with 0 
       ; and perform casting
       (map update-data)))
```

----

##How were the electors split?
```clojure
(->> state-vote (map :dem-electors) (reduce +))
(->> state-vote (map :gop-electors) (reduce +))
```
<pre class="fragment">
<code>(->> state-vote (map :dem-electors) (reduce +))
=> 332
(->> state-vote (map :gop-electors) (reduce +))
=> 206
</code>
</pre>

----

##Q: N to change the outcome?
```clojure
(defn pct-diff [{:keys [dem-popular gop-popular]}]
  (let [diff (Math/abs (- dem-popular gop-popular))]
    (* 100 (/ diff (+ dem-popular gop-popular)))))
    
;How many top battleground states would you need to 
; flip to change the outcome?
(->> state-vote
     (sort-by pct-diff)
     ?)
```

----

##A: N to change the outcome?
```clojure    
;How many top battleground states would you need to 
; flip to change the outcome?
(->> state-vote
     (sort-by pct-diff)
     (filter (comp zero? :gop-electors))
     (take 4)
     (map :dem-electors)
     (reduce + (->> state-vote (map :gop-electors) (reduce +))))
```

----

##Which states to change the election outcome?
```clojure
;Which top battleground states would you need
; to flip to change the election?
;What was the per-state major party vote difference?
(loop [votes (->> state-vote (map :gop-electors) (reduce +))
       [{:keys [dem-electors] :as n} & r]
       (->> state-vote
            (sort-by pct-diff)
            (filter (comp zero? :gop-electors)))
       flipped []]
  (if (< votes 270)
    (recur (+ votes dem-electors) r (conj flipped n))
    (into {} (map (fn [{:keys [state dem-popular gop-popular]}]
                    {state (int (- dem-popular gop-popular))})
                  flipped))))
```
<pre class="fragment">
<code>{"FL" 74309, "OH" 166272, "VA" 149298, "PA" 309840}
</code>
</pre>

----

##Observations
* Clojure eats data for breakfast
* Clojure abstractions over data make data manipulation easy

---

#Java Interop

----

##Case Study:
##Farenheit/Celcius Converter
Open `examples.javaclj`
```clojure
(ns examples.java
  (:import (javax.swing JFrame JLabel Box JFormattedTextField)
           (java.awt BorderLayout Component)
           (java.awt.event ActionListener FocusListener)))
```

---

#Topics not covered
* State (atoms, agents, refs)
* JavaScript

---

#THE END
Questions?

---

## Additional Resources
 * [Clojure Evaluation](http://clojure.org/reference/evaluation): 
 This is an excellent read on the basic types and literal collections of Clojure.
 It also discusses symbols, functions, macros, and other topics not covered in this 
 tutorial.

----

## License

Copyright © 2016 Mark Bastian

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
