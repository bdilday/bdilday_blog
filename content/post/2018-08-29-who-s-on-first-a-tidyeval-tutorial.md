---
title: 'Who''s on First: A tidyeval tutorial'
author: ~
date: '2018-08-29'
slug: who-s-on-first-a-tidyeval-tutorial
categories: []
tags: []
---


> Abbott: ... Well, let's see, we have on the bags, Who's on first, What's on second, I Don't Know is on third...
>
> Costello: That's what I want to find out.
>
> Abbott: I say Who's on first, What's on second, I Don't Know's on third.
> 
> Costello: Are you the manager?
>
> Abbott: Yes.
>
> Costello: You gonna be the coach too?
>
> Abbott: Yes.
> 
> Costello: And you don't know the fellows' names?
> 
> Abbott: Well I should.
> 
> Costello: Well then who's on first?
> 
> Abbott: Yes.
> 
> Costello: I mean the fellow's name.
> 
> Abbott: Who.
> 
> Costello: The guy on first.
> 
> Abbott: Who.
> 
> Costello: The first baseman.
> 
> Abbott: Who.
> 
> Costello: The guy playing...
> 
> Abbott: Who is on first!
> 
> Costello: I'm asking YOU who's on first.
> 
> Abbott: That's the man's name.
> 
> Costello: That's who's name?
> 
> Abbott: Yes.
> 
> Costello: Well go ahead and tell me.
> 
> Abbott: That's it.
> 
> Costello: That's who?
> 
> Abbott: Yes.


If you're reading this there's a good chance you're familiar with the Abbott and Costello routine **Who's on First**. If not you should do yourself a favor and go [watch it now](https://www.youtube.com/watch?v=kTcRRaXV-fg&index=2&list=RDairT-m9LcoY&t=50). The full script is [available here](http://www.baseball-almanac.com/humor4.shtml). The crux of the bit is that the ball players have "weird" names: *who*, *what*, *I dont know*, etc. The straight man - Abbott - keeps trying to tell the players names to his bumbling partner - Costello - who keeps trying to ask what they are. Ultimately the confusion comes from the fact that Abbott and Costello are using the same symbol - for example `who` - to refer to two different concepts. If that reminds you of non-standard evaluation in R and `tidyeval` then you've come to the right place!

## whos on first

### environments

Let's break down the bit in a somewhat systematic way and try to relate it to `tidyeval` concepts. As the bit begins, Abbott tells Costello the names of the first, second, and third basemen. Abbott's using the symbol `who` to refer to the name of the first baseman. Costello is interpreting it, however, as a symbol referring the the question "who". 

In terms of `R` code, there are three different *environments* here. There's the **objective truth** environment in which the names of the players are known. There's the environment that Abbott is operating in, that has access to objective truth, and in which the symbol `who` refers to the value of the variable `first`. Additionally, Abbott interprets the phrase `is on` as checking equality. 

In code we could express this in terms of environments, non-standard expression evaluation and quosures, using tools from the `rlang` package. `rlang` has three options for creating environments - `env`, `new_enviornment`, and `child_env`. The difference between `env` and `new_enviornment` is that `env ` inherits from the current environment while `new_enviornment` builds on a completely empty environment. Here I use `env` so that objective truth will have access to base `R` functions.


```r
library(rlang)
objective_truth =
  rlang::env(first="who", second="what", third="idk",
              lf="why", cf="because", rf="I don\'t give a darn",
              pitcher="tomorrow", catcher="today")
```


Because Abbott's environment includes access to objective truth, we can define his environment using `objective_truth` as a parent environment. Note I am also defining a new in-fix operator `%on%`, I'll elaborate on below.


```r
abbott_env =
  rlang::child_env(
    objective_truth,
    `%on%` = function(a, b) {
      ea = enquo(a)
      lhs = quo_text(ea)
      eb = enquo(b)
      rhs = eval_tidy(eb, env = abbott_env)
      lhs == rhs
    }
  )
```


In the `objective_truth` environment, we can see that indeed the name of the player on first is "who".


```r
with(objective_truth, first == "who")
#> [1] TRUE
```

### Abbott

In the `abbott_env` environment we can now ask for the result of *who's on first*, in the following way.


```r
rlang::eval_tidy(expr(who %on% first), env=abbott_env)
#> [1] TRUE
```

this warrants some additional explanation. In the abstract the phrase "who's on first" is an unevaulated expression. 


```r
uneval_ex = expr(who %on% first)
```

If I simply try and evaluate it, I get an error because the `%on%` infix operator is not defined. 


```r
eval(uneval_ex)
#> Error in who %on% first: could not find function "%on%"
```

In the `abbott_env` it has meaning, however. Let's break down the code for `%on%`


```r
`%on%` = function(a, b) {
      ea = enquo(a)
      lhs = quo_text(ea)
      eb = enquo(b)
      rhs = eval_tidy(eb, env = abbott_env)
      lhs == rhs
}
```

The term 

``` 
ea = enquo(a)
```

captures the argument `a` as an enquosure - that is it quotes the value of `a`, in the calling environment, as opposed to the function environment. When Abbott passes the argument `who` as a symbol, the result is `ea ` becomes a quosure capturing the symbol `who`. If we has used `quo` it would have captured the symbol `a` instead. 

The term 

```
lhs = quo_text(ea)
```

takes the contents of `ea` and casts them to a string. So the ultimate outcome is `lhs` is a character vector with the value `"who"`.

Similarly, the terms

```
eb = enquo(b)
rhs = eval_tidy(eb, env = abbott_env)
```

captures the thing passed in the variable `b` (the symbol `first` in this context) and then evaluates it in the context of `abbott_env`. Because Abbott has access to `objective_truth`, it evaluates to the `objective_truth` value of the symbol `first`, i.e. the character vector `"who"`.

With the exposition out of the way, I can refactor the code to be more concise 


```r
abbott_env =
  rlang::child_env(
    objective_truth,
    `%on%` = function(a, b) {
      quo_text(enquo(a)) == eval_tidy(enquo(b), env = abbott_env)
    }
  )
```

Abbott has another function that does return the name of the player, which is `name`. It can be defined in the following way.


```r
abbott_env =
  rlang::child_env(
    objective_truth,
    `%on%` = function(a, b) {
      quo_text(enquo(a)) == eval_tidy(enquo(b), env = abbott_env)
    },
    name = function(a) {
      eval_tidy(enquo(a), env = abbott_env)
    }
  )
```

So now when Abbott evaluates `expr(name(first))` the result is the string "who"


```r
rlang::eval_tidy(expr(name(first)), env = abbott_env)
#> [1] "who"
```

### Costello

Costello on the other hand does not have access to `objective_truth` but instead exists in an environment of `ignorance`.


```r
ignorance =
  rlang::new_environment(
    data=list(first=NULL, idk=TRUE)
  )
```

For Costello, the `%on%` operator also takes on a different meaning, which is to access the value of the symbol - similar to the `name` function in Abbott's environment.


```r
costello_env =
  rlang::child_env(
    ignorance,
    who = function(s) {s},
    `%on%` = function(a, b) {
      f = enquo(a)
      do.call(eval_tidy(f, costello_env), list(b))
    }
  )
```

Because he exists in a stat of ignorance, if he tries to evaluate it in his own environment, he is left wanting


```r
rlang::eval_tidy(expr(who %on% first), env = costello_env)
#> NULL
```

### The routine

When Costello submits his query - who's on first - to Abbott, he's expecting Abbott to evaluate who as the query function, in an environment that has access to objective truth. Here I define such an environment. 


```r
questioning_env =
  rlang::child_env(objective_truth,
                   who = function(s) {s},
                   what = function(s) {s},
                   nameof = function(s) {s},
                   `%on%` = function(a, b) {
                     f = enquo(a)
                     do.call(eval_tidy(f), list(b))
                   }
  )
```

His expectation is 


```r
rlang::eval_tidy(expr(who %on% first), env = questioning_env)
#> [1] "who"
```

Instead, Abbott interprets the question according to his own environment


```r
rlang::eval_tidy(expr(who %on% first), env = abbott_env)
#> [1] TRUE
```

On the other hand when he asks for the guys name on first he expects the same result as `expr(who %on% first)` because to him those questions are identical. And in fact Abbott does return the name of the player - "who".


```r
rlang::eval_tidy(expr(name(first)), env = abbott_env)
#> [1] "who"
```

But - and here in lies the humor! - that value, "who", is the name of a symbol that Costello has taken to be a function. 

### conclusion 

The classic comedy bit *Who's on First* is a textbook example of the concepts that underlie non-standard evaluation and computing on the language - namely that values (e.g. "who") can be interpreted as symbols that have varying meaning depeding on the evaluation environment. Thinking through the bit helps us build a mental model for the meta-programming features of languages such as `R`.






