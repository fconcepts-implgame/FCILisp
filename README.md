# FCILisp

A Pure Lisp implementation for Fundamental Concepts Implementation Game.

# Requirements

- PureScript v0.13.2
- Spago v0.8.5.0
- Node v12.6.0


# Run tests

```
spago test
```


# Syntax

Everything is an expression in FciLisp.

Expressions consist of lists and others, called atoms.

Atoms are further subdevided.

FciLisp has pairs and atoms as first-class objects (hereafter, the first-class object is called "value").

Atoms are natural numbers, symbols, functions, and recursive functions.

Note that the term "atom" is used here in two senses, one as a grammer and the other as data.


## Literals and Constructors

### Atoms

Atoms are just strings that are not lists.


#### nil, t

`t` and `nil` are special symbols.

Only `nil` is falsy, and any other value is truthy.

You can use `t` to represent explicitly truthy.

They are true and false in other languages.


#### Natural numbers

Internally it wraps a PureScript Int type, so is a 32-bit unsigned interger.

See the [Puresuit](https://pursuit.purescript.org/builtins/docs/Prim#t:Int).


#### Symbols

Symbols are non-litaral strings of available charactgers.

You can use alphanumeric characters and symbols: `!`, `@`, `#`, `$`, `%`, `^`, `&`, `*`, `-`, `_`, `=`, `+`, `\`, `~`, `|`, `;`, `:`, `'`, `"`, `,`, `<`, `.`, `>`, `/`, `?`.

So far symbols are strins that are not nutural number literals, but in the future it may be strings that not another literals, such as quoted symbol literals, character literals, or string literals.

Symbols can be bound to values and can be treated as variables.

In FciLisp, all variables are constant and all values are immutable.


#### Functions

As in other functional languages, functions are first-class objects in FciLisp, so they can be passed to functions, bound to symbols, and create instances in FciLisp.

```lisp
(fun x (+ x 2))
```

`fun` takes only one name and one expression each.

To create a function with multiple arguments, you can nest it.

```lisp
(fun x (fun y (+ x y)))
```


#### Recursive functions

`fun` can not express recursion. That's because functions created with `fun` have no way to call themselves.

`recur` can take a name that binds itself, so generated functions can invoke itself recursively.

```lisp
(recur fact n (if (= n 0) 1 (* n (fact (- n 1)))))
```


### Pairs

There are no such value as lists, just pairs in FciLisp.

But you can construct lists by using pairs as below.

```lisp
(cons 0 (cons 1 (cons 2 nil))) ;; this can be considered a list like '(0 1 2)
```

`cons` stands for "constructor" and takes two expressions and returns a pair value.

Of course it can also behave as a pair.

## Predicates

Builtin predicates can have multiple arguments.


### `atom?`

Takes one expression and returns `nil` if it is a pair, otherwise returns `t`.

Because it is an atom other than a pair.


## `eq?`

`nil`, `t`, and natural numbers are comparable.

Other functions, recursive functions, and pairs are not comparable.

Symbols are evaluated to one of the above.

In the future, if `quote` is implemented, the symbol will also be comparable.


## Operators

Builtin operators can have multiple arguments.


### Pair operators

#### `head`

Takes one pair and returns the left value from it.

When you look at a pair as a list, this operation can be seen as getting the top element.


#### `tail`

Takes one pair and returns the right value from it.

When you look at a pair as a list, this operation can be seen as getting the rest except the first element.


### Natural number operators

#### `+`

Takes two natural numbers and returns the sum of them.


#### `-`

Takes two natural numbers and returns their difference, but returns 0 if it is less than 0


#### `*`

Takes two natural numbers and returns their product.


#### `div`

Takes two natural numbers and returns the natural number that is their quotient.

An error occurs if the second argument is 0.


#### `mod`

Takes two natural numbers and returns their remainder.

An error occurs if the second arguments is 0.


#### `<`

Takes two natural numbers and returns `t` if the first is less than the second, otherwise returns `nil`.


#### `<`

Takes two natural numbers and returns `t` if the first is greater than the second, otherwise returns `nil`.


#### `<=`

Takes two natural numbers and returns `t` if the first is less than or equal to the second, otherwise returns `nil`.


#### `<`

Takes two natural numbers and returns `t` if the first is greater than or equal to the second, otherwise returns `nil`.


## Conditions

Conditions does not necessarily evaluate arguments in advance.


### `if`

Takes a conditional expression and two expressions, evaluate the third expression if the evaluation result of the conditional expression is nil, otherwise evaluate the second expression.

Always one of the expressions is not evaluated.


## Applications

Take one of a predicate, an operator, a function, and a recursive function and expressions, and returns the result of applying the expressions to them.

Symbols are evaluated to one of the above.

In addition, functions and recursive functions always take only one argument.
