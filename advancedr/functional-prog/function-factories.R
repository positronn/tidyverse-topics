# function-factories.R
library(purrr)

#  10.1 Introduction

# A function factory is a function that makes functions.
# Here’s a very simple example: we use a function factory
# (power1()) to make two child functions (square() and cube()):

power1 <- function(exp) {
    function(x) {
        x ^ exp
    }
}
square <- power1(2)
cube <- power1(3)

square(3)
cube(3)

# In R, you bind a function to a name in the same way as you bind any
# object to a name: with <-.
# a function captures (encloses) the environment in which it is created.
# a function creates a new execution environment every time it is run.
# This environment is usually ephemeral, but here it becomes the
# enclosing environment of the manufactured function.

library(rlang)
library(ggplot2)
library(scales)

# The enclosing environment of the manufactured function is an
# execution environment of the function factory.

#  10.2.1 Environments 
square

cube

# It’s obvious where x comes from, but how does R find the value
# associated with exp? Simply printing the manufactured functions
# is not revealing because the bodies are identical; the contents
# of the enclosing environment are the important factors. 

#  We can get a little more insight by using rlang::env_print().
# That shows us that we have two different environments (each of
# which was originally an execution environment of power1()). The
# environments have the same parent, which is the enclosing
# environment of power1(), the global environment.
env_print(square)
env_print(cube)

# env_print() shows us that both environments have a binding
# to exp, but we want to see its value. We can do that by first
# getting the environment of the function, and then extracting the values:
fn_env(square)$exp
fn_env(cube)$exp

# This is what makes manufactured functions behave differently
# from one another: names in the enclosing environment are bound
# to different values.

#  10.2.3 Forcing evaluation
# There’s a subtle bug in power1() caused by lazy evaluation.
# To see the problem we need to introduce some indirection:
x <- 2
square <- power1(x)
x <- 3

# What should square(2) return? You would hope it returns 4:
square(2)

# Unfortunately it doesn’t because x is only evaluated lazily
# when square() is run, not when power1() is run. In general,
# this problem will arise whenever a binding changes in between
# calling the factory function and calling the manufactured function.
# This is likely to only happen rarely, but when it does,
# it will lead to a real head-scratcher of a bug.

# We can fix this problem by forcing evaluation with force():
power2 <- function(exp) {
    force(exp)
    function(x) {
        x ^ exp
    }
}

x <- 2
square <- power2(x)
x <- 3
square(2)


# Whenever you create a function factory, make sure every argument
# is evaluated, using force() as necessary if the argument
# is only used by the manufactured function.

#  10.2.4 Stateful functions 
# Function factories also allow you to maintain state across function invocations
# There are two things that make this possible:
# The enclosing environment of the manufactured function is unique and constant.
# R has a special assignment operator, <<-, which modifies bindings in the enclosing environment.

# The usual assignment operator, <-, always creates a binding in the
# current environment. The super assignment operator,
# <<- rebinds an existing name found in a parent environment.

# The following example shows how we can combine these ideas to
# create a function that records how many times it has been called:
new_counter <- function() {
    i <- 0
    function() {
        i <<- i + 1
        i
    }
}

# When the manufactured function is run i <<- i + 1 will modify i in
# its enclosing environment. Because manufactured functions have
# independent enclosing environments, they have independent counts:

counter_one <- new_counter()
counter_two <- new_counter()


# Stateful functions are best used in moderation. As soon as your
# function starts managing the state of multiple variables, it’s better
# to switch to R6, the topic of Chapter 14.


#  10.2.5 Garbage collection 
# With most functions, you can rely on the garbage collector to 
# clean up any large temporary objects created inside a function.
# However, manufactured functions hold on to the execution environment,
# so you’ll need to explicitly unbind any large temporary objects with rm().
# Compare the sizes of g1() and g2() in the example below:
f1 <- function(n) {
    x <- runif(n)
    m <- mean(x)
    function() m
}
g1 <- f1(1e6)
lobstr::obj_size(g1)



f2 <- function(n) {
    x <- runif(n)
    m <- mean(x)
    rm(x)
    function() m
}
g2 <- f2(1e6)
lobstr::obj_size(g2)


# Create a function pick() that takes an index, i, as an
# argument and returns a function with an argument x that subsets x with i.
x <- c(0, 1, 2, 3)
pick <- function(i) {
    function(x) {
        x[[i]]
    }
}
pick(1)(x)
x[[1]]

lapply(mtcars, pick(5))
lapply(mtcars, function(x) x[[5]])


# What happens if you use <- instead of <<-? Make predictions,
# then verify with the code below.
# if <- is used instead of <<-, i will always go back to its initial
# state when the function was created, i.e. i = 0, and the counter
# will always return 1
new_counter3 <- function() {
    i <- 0
    function() {
        i <<- i + 1
        i
    }
}

nc3 <- new_counter3()
nc3()
nc3()
nc3()


new_counter3 <- function() {
    i <- 0
    function() {
        i <- i + 1
        i
    }
}
nc3 <- new_counter3()
nc3()
nc3()
nc3()
