Homework 02 - Functional Expression Language Type Inference
===============================================================================

Deadline: December 4th 2016, by 23:59.

The task at hand
------------------------------------------------------------------------------

We will have to implement a function `type-check` that will check if a given program has valid types. It will do so by *_inference_*, i.e. type for annotations are only optional (remember that the FELI language from last time did not have *any* type annotations).

We will be working on the same AST as we have been for the first homework, with some minor modifications. We have removed `Letrec`, and `FloaV`s. *_If you wish to support optional annotations you you will have to extend the `Expr` data structure yourself (hint: it might be a good idea to represent these annotations as a `Maybe`)_* 

```Haskell
data Val = IntV Integer
         | CharV Char
         | StringV String
         | BooleanV Bool
         | LambdaV [String] Expr Env
     deriving (Show, Eq)

-----------------------------------------------------------
data Expr = 
            Const Val
          | Var String
          | Expr :*: Expr 
          | Expr :+: Expr 
          | Expr :==: Expr 
          | If Expr Expr Expr
          | Let String Expr Expr
          | Lambda [String] Expr 
          | Apply Expr [Expr]
     deriving (Show, Eq)

data Env = EmptyEnv
         | ExtendEnv String Val Env
     deriving (Show, Eq)
```

There are no tests for this homework. You can test your type-checker on the examples you wrote for your first homework. Also, you can infer formal typing rules for the language from those examples.


Hints
------------------------------------------------------------------------------
Take the following program:
```Haskell
-- \x -> x
-- the identity function.
Lambda ["x"]  (Var x)
```

Should this program type check? If yes, what is its type? Well, when you have such questions you should probably take a page out of Haskell's book, and see if that might shed some light. If we run the following in the `ghci` interpreter:
```Haskell
> :type \x -> x
\x -> x :: t -> t
```

You can see that the type of the identity function is `t -> t`. Meaning that it's a function from *any* value of type `t` to a value of type `t`. Since the type-checker doesn't have any information other than the `\x -> x` it cannot possibly assign more concrete types to the function, so it plugs in generic type variables. Your type-checker should do the same.

Now, when we try:
```
> :type (\x -> x + x)

(\x -> x + x) :: Num a => a -> a
```

We get a more concrete type than a plain generic `t` or `a`, or `whatever`. The type checker now tells us that the types of the function have to be numbers, because the `+` operation supplied more information.  


Recommended reading:
------------------------------------------------------------------------------
 * check out the lectures from [weeks 3 and 4](http://bigfoot.cs.upt.ro/~marius/curs/plda/index.html)
 * *Programming Languages: Application and Interpretation*, Shriram Krishnamurthi, [chapter 15](http://cs.brown.edu/courses/cs173/2012/book/types.html), specifically related to inference is [subsection 15.3.2](http://cs.brown.edu/courses/cs173/2012/book/types.html#(part._.Type_.Inference))
 * implementations (in Ocaml) from the *Type and Programming Languages* book by Benjamin C. Pierce are available [online](http://www.cis.upenn.edu/~bcpierce/tapl/). You can access this book in [draft form](http://ropas.snu.ac.kr/~kwang/520/pierce_book.pdf) for free.
   * a Haskell port of the implementations from the TAPL book are also [available](https://code.google.com/p/tapl-haskell/)  

* [Essentials of Programming Languages](https://karczmarczuk.users.greyc.fr/TEACH/Doc/EssProgLan.pdf), Friedman et. al. Chapter 7.