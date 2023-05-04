#

#  18.1 Introduction

# To compute on the language, we first need to understand its structure.
# That requires some new vocabulary, some new tools, and some new ways
# of thinking about R code. The first of these is the distinction
# between an operation and its result. Take the following code, which
# multiplies a variable x by 10 and saves the result to a new variable
# called y. It doesn’t work because we haven’t defined a variable called x
y <- x * 10

# It would be nice if we could capture the intent of the code without executing
# it. In other words, how can we separate our description of the action from
# the action itself?
# One way is to use rlang::expr():
z <- rlang::expr(y <- x * 10)
z

# expr() returns an expression, an object that captures the structure
# of the code without evaluating it (i.e. running it). If you have
# an expression, you can evaluate it with base::eval():
x <- 4
eval(z)
y


library(rlang)
library(lobstr)

#  18.2 Abstract syntax trees 
# Expressions are also called abstract syntax trees (ASTs) because the
# structure of code is hierarchical and can be naturally
# represented as a tree. Understanding this tree structure is 
# crucial for inspecting and modifying expressions (i.e. metaprogramming).
lobstr::ast(f(x, "y", 1))
lobstr::ast(f(g(1, 2), h(3, 4, i())))
# The depth within the tree is determined by the nesting of function
# calls. This also determines evaluation order, as evaluation generally
# proceeds from deepest-to-shallowest, but this is not guaranteed because
# of lazy evaluation (Section 6.5). Also note the appearance of i(),
# a function call with no arguments; it’s a branch with a single
# (symbol) leaf.

#  18.2.3 Infix calls 
# Every call in R can be written in tree form because any call
# can be written in prefix form (Section 6.8.1). Take y <- x * 10 again
# what are the functions that are being called? It is not as easy to
# spot as f(x, 1) because this expression contains two infix calls:
# <- and *. That means that these two lines of code are equivalent:
y <- x * 10
`<-`(y, `*`(x, 10))
# And they both have this AST
lobstr::ast(y <- x * 10)
# There really is no difference between the ASTs, and if
# you generate an expression with prefix calls, R will still
# print it in infix form:
expr(`<-` (y, `*` (x, 10)))

# The order in which infix operators are applied is governed by a
# set of rules called operator precedence, and we’ll use lobstr::ast()
# to explore them
lobstr::ast(f(g(h(i(1, 2, 3)))))
lobstr::ast(f(1, g(2, h(3, i()))))
lobstr::ast(f(g(1, 2), h(3, i(4, 5))))



lobstr::ast(`x` + `y`)
lobstr::ast(x ** y)

lobstr::ast(1 -> x)

lobstr::ast(function(x = 1, y = 2) {})


# 18.3 Expressions 
# Collectively, the data structures present in the AST are called
# expressions. An expression is any member of the set of base types
# created by parsing code: constant scalars, symbols, call objects,
# and pairlists. These are the data structures used to represent
# captured code from expr(), and is_expression(expr(...)) is always
# true. Constants, symbols and call objects are the most important,
# and are discussed below. Pairlists and empty symbols are more
# specialised and we’ll come back to them in Sections 18.6.1 and
# Section 18.6.2.
# Scalar constants are the simplest component of the AST.
# More precisely, a constant is either NULL or a length-1 atomic
# vector (or scalar, Section 3.2.1) like TRUE, 1L, 2.5 or "x".
# You can test for a constant with rlang::is_syntactic_literal().
is_syntactic_literal('a')
is_syntactic_literal(1)
is_syntactic_literal(x)
is_syntactic_literal(TRUE)
is_syntactic_literal(c(1, 2))


identical(expr(TRUE), TRUE)
identical(expr(1), 1)
identical(expr(2L), 2L)
identical(expr("x"), "x")


#  18.3.2 Symbols 
# A symbol represents the name of an object like x, mtcars,
# or mean. In base R, the terms symbol and name are used
# interchangeably (i.e. is.name() is identical to is.symbol()),
# but in this book I used symbol consistently because “name”
# has many other meanings.

# You can create a symbol in two ways: by capturing code
# that references an object with expr(), or turning a string
# into a symbol with rlang::sym():
expr(x)
sym('x')

expr(x) |> 
    as_string()

# You can recognise a symbol because it’s printed without quotes,
# str() tells you that it’s a symbol, and is.symbol() is TRUE:
str(expr(x))
is.symbol(expr(x))

# The symbol type is not vectorised, i.e. a symbol is always length 1.
# If you want multiple symbols, you’ll need to put them in a list,
# using (e.g.) rlang::syms().

lobstr::ast(read.table('important.csv', row.names = FALSE))
x <- expr(
    read.table('important.csv', row.names = FALSE)
)
typeof(x)
is.call(x)
x[[1]]
is.symbol(x[[1]])

# You can extract individual arguments with [[ or, if named, $:
x[[2]]
x$row.names

# Extracting specific arguments from calls is challenging
# because of R’s flexible rules for argument matching: it
# could potentially be in any location, with the full name,
# with an abbreviated name, or with no name. To work around
# this problem, you can use rlang::call_standardise() which
# standardises all arguments to use the full name: 
rlang::call_standardise(x)

# Calls can be modified in the same way as lists:
x$header <- TRUE
x

#  18.3.3.2 Function position 
# The first element of the call object is the function position.
# This contains the function that will be called when the object
# is evaluated, and is usually a symbol:
lobstr::ast(foo())

# While R allows you to surround the name of the function with
# quotes, the parser converts it to a symbol:
lobstr::ast('foo'())

# However, sometimes the function doesn’t exist in the current
# environment and you need to do some computation to retrieve it:
# for example, if the function is in another package, is a method
# of an R6 object, or is created by a function factory. In this
# case, the function position will be occupied by another call:
lobstr::ast(pkg::foo(1))
lobstr::ast(obj$foo(1))
lobstr::ast(foo(1)(2))

#  18.3.3.3 Constructing 
# You can construct a call object from its components using
# rlang::call2(). The first argument is the name of the function
# to call (either as a string, a symbol, or another call).
# The remaining arguments will be passed along to the call:
call2('mean', x = expr(x), na.rm = TRUE)
call2(expr(base::mean), x = expr(x), na.rm = TRUE)

# Infix calls created in this way still print as usual.
call2('<-', expr(x), 10)


#  18.4 Parsing and grammar 
# The process by which a computer language take sa string and constructs
# an expression is called pársing, and is governed by a set of rules
# known as a grammar.

# ambiguity
lobstr::ast(1 + 2 * 3)

# Predicting the precedence of other operators is harder.
# There’s one particularly surprising case in R: ! has a much
# lower precedence (i.e. it binds less tightly) than you might expect.
# This allows you to write useful operations like:
lobstr::ast(!x %in% y)

# R has over 30 infix operators divided into 18 precedence groups.
# While the details are described in ?Syntax, very few people have
# memorised the complete ordering. If there’s any confusion, use parentheses!
lobstr::ast((1 + 2) * 3)
# Note the appearance of the parentheses in the AST as a call to the ( function.



#  18.4.2 Associativity 
# The second source of ambiguity is introduced by repeated usage
# of the same infix function. For example, is 1 + 2 + 3 equivalent
# to (1 + 2) + 3 or to 1 + (2 + 3)? This normally doesn’t matter
# because x + (y + z) == (x + y) + z, i.e. addition is associative,
# but is needed because some S3 classes define + in a non-associative
# way. For example, ggplot2 overloads + to build up a complex plot
# from simple pieces; this is non-associative because earlier
# layers are drawn underneath later layers (i.e. geom_point() +
# geom_smooth() does not yield the same plot as geom_smooth() +
# geom_point()).
# In R, most operators are left-associative, i.e. the operations on
# the left are evaluated first:
lobstr::ast(1 + 2 +3)

# There are two exceptions: exponentiation and assignment.
lobstr::ast(2^2^3)
lobstr::ast(x <- y <- z)

#  18.4.3 Parsing and deparsing 
# Most of the time you type code into the console, and R takes
# care of turning the characters you’ve typed into an AST. But
# occasionally you have code stored in a string, and you want to
# parse it yourself. You can do so using rlang::parse_expr():
x1 <- 'y <- x + 10'
x1
is.call(x1)

x2 <- rlang::parse_expr(x1)
x2
is.call(x2)

# parse_expr() always returns a single expression.
# If you have multiple expression separated by ; or \n, you’ll
# need to use rlang::parse_exprs(). It returns a list of expressions:
x3 <- 'a <- 1; a + 1'
rlang::parse_exprs(x3)


# in base r
as.list(parse(text = x1))


# The inverse of parsing is deparsing: given an expression, you want 
# the string that would generate it. This happens automatically when
# you print an expression, and you can get the string with rlang::expr_text():
z <- expr(y <-     x + 10)
expr_text(z)

# Parsing and deparsing are not perfectly symmetric because parsing
# generates an abstract syntax tree. This means we lose backticks
# around ordinary names, comments, and whitespace:
cat(
    expr_text(
        expr({
            # this is a comment
            x <-       `x` + 1
        })
    )
)
# Be careful when using the base R equivalent, deparse(): it returns
# a character vector with one element for each line. Whenever you
# use it, remember that the length of the output might be greater
# than one, and plan accordingly.


# excercises
lobstr::ast(f((1)))
lobstr::ast(`(`(1+1))


#  18.5 Walking AST with recursive functions 
expr_type <- function(x) {
    if (rlang::is_syntactic_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is.pairlist(x)) {
        "pairlist"
    } else {
        typeof(x)
    }
}
expr_type(expr("a"))
expr_type(expr(x))
expr_type(expr(f(1, 2)))

# We’ll couple this with a wrapper around the switch function:
switch_expr <- function(x, ...) {
    switch(expr_type(x),
           ...,
           stop("Don't know how to handle type", typeof(x), call.=FALSE)
    )
}
# With these two functions in hand, we can write a basic template for any
# function that walks the AST using switch()
recurse_call <- function(x) {
    switch_expr(x,
                # base cases
                symbol = ,
                constant = ,
                # recursive cases
                call = ,
                pairlist = )
}
# Typically, solving the base case is easy, so we’ll do that first, then check the results. The recursive cases are trickier,
# and will often require some functional programming.


#  18.5.1 Finding F and T 
# We’ll start with a function that determines whether another function
# uses the logical abbreviations T and F because using them is often
# considered to be poor coding practice. Our goal is to return TRUE
# if the input contains a logical abbreviation, and FALSE otherwise.
expr_type(expr(TRUE))
expr_type(expr(T))
# TRUE is parsed as a logical vector of length one, while T
# is parsed as a name. This tells us how to write our base cases
# for the recursive function: a constant is never a logical
# abbreviation, and a symbol is an abbreviation if it’s “F” or “T”:
logical_abbr_rec <- function(x) {
    switch_expr(x,
                constant = FALSE,
                symbol = as_string(x) %in% c('F', 'T'))
}
logical_abbr_rec(expr(TRUE))
logical_abbr_rec(expr(T))

# I’ve written logical_abbr_rec() function assuming that the input
# will be an expression as this will make the recursive operation
# simpler. However, when writing a recursive function it’s common
# to write a wrapper that provides defaults or makes the function a
# little easier to use. Here we’ll typically make a wrapper that
# quotes its input (we’ll learn more about that in the next chapter),
# so we don’t need to use expr() every time.
logical_abbr <- function(x) {
    logical_abbr_rec(enexpr(x))
}
logical_abbr(T)
logical_abbr_rec(FALSE)

# Next we need to implement the recursive cases. Here we want to do
# the same thing for calls and for pairlists: recursively apply
# the function to each subcomponent, and return TRUE if any
# subcomponent contains a logical abbreviation. This is made easy
# by purrr::some(), which iterates over a list and returns TRUE
# if the predicate function is true for any element.
logical_abbr_rec <- function(x) {
    switch_expr(x,
                # base cases
                constant = FALSE,
                symbol = as_string(x) %in% c('F', 'T'),
                # recursive cases
                call = ,
                pairlist = purrr::some(x, logical_abbr_rec)
    )
}
logical_abbr(mean(x, na.rm = T))
logical_abbr(function(x, na.rm = T) FALSE)

# 18.5.2 Finding all variables created by assignment 
ast(x <- 10)
# Assignment is a call object where the first element is the symbol <-,
# the second is the name of variable, and the third is the value to be assigned.

# Next, we need to decide what data structure we’re going to use for the results.
# Here I think it will be easiest if we return a character vector.
# If we return symbols, we’ll need to use a list() and that makes things a
# little more complicated.

# With that in hand we can start by implementing the base cases and providing
# a helpful wrapper around the recursive function. Here the base cases are
# straightforward because we know that neither a symbol nor a constant
# represents assignment.
find_assign_rec <- function(x) {
    switch_expr(x,
        constant = ,
        symbol = character()
    )
}
find_assign <- function(x) find_assign_rec(enexpr(x))

find_assign('x')
find_assign(x)

# Next we implement the recursive cases. This is made easier by a function
# that should exist in purrr, but currently doesn’t.
# flat_map_chr() expects .f to return a character vector of arbitrary length,
# and flattens all results into a single character vector.
flat_map_chr <- function(.x, .f, ...) {
    purrr::flatten_chr(purrr::map(.x, .f, ...))
}
flat_map_chr(letters[1:3], ~rep(., sample(3, 1)))

# the recursive case for pairlists is straighforward: we iterate over every
# element of the pairlist (i.e. each function argument) and combine
# the results. The case for calls is a little bit more complex: if this
# is a call to <- then we should return the second element of the call:
find_assign_rec <- function(x) {
    switch_expr(x,
        # base cases
        constant = ,
        symbol = character(),
        
        # recursive cases
        pairlist = flat_map_chr(as.list(x), find_assign_rec),
        call = {
            if (is_call(x, '<-')) {
                as_string(x[[2]])
            } else {
                flat_map_chr(as.list(x), find_assign_rec)
            }
        }
        )
}

find_assign(a <- 1)
find_assign({
    a <- 1
    {
        b <- 2
    }
})
# Now we need to make our function more robust by coming up with examples
# intended to break it. What happens when we assign to the same variable
# multiple times?
find_assign({
    a <- 1
    a <- 2
})

# It’s easiest to fix this at the level of the wrapper function:
find_assign <- function(x) unique(find_assign_rec(enexpr(x)))
find_assign({
    a <- 1
    a <- 2
})
# What happens if we have nested calls to <-? Currently we only return the first.
# That’s because when <- occurs we immediately terminate recursion.
find_assign({
    a <- b <- c <- 1
})

# Instead we need to take a more rigorous approach. I think it’s best
# to keep the recursive function focused on the tree structure, so I’m
# going to extract out find_assign_call() into a separate function.
find_assign_call <- function(x) {
    if (is_call(x, '<-') && is_symbol(x[[2]])) {
        lhs <- as_string(x[[2]])
        children <- as.list(x)[-1]
    } else {
        lhs <- character()
        children <- as.list(x)
    }
    c(lhs, flat_map_chr(children, find_assign_rec))
}


find_assign_rec <- function(x) {
    switch_expr(x,
        # base cases
        constant = ,
        symbol = character(),
        # recursive cases
        pairlist = flat_map_chr(x, find_assign_rec),
        call = find_assign_call(x)
    )
}

find_assign(a <- b <- c <- 1)
find_assign(system.time(x <- print(y <- 5)))
find_assign(system.time(x <- z <- 3 * 3 + x))

#  18.6 Specialised data structures 
# There are two data structures and one special symbol that we need to cover
# for the sake of completeness. They are not usually important in practice.
#  18.6.1 Pairlists 
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
args
typeof(args)

# Fortunately, whenever you encounter a pairlist, you can treat it just like a regular list:
pl <- pairlist(x = 1, y = 2)
length(pl)
pl$x

#  18.6.2 Missing arguments 
# The special symbol that needs a little extra discussion is the
# empty symbol, which is used to represent missing arguments
# (not missing values!). You only need to care about the missing symbol
# if you’re programmatically creating functions with missing arguments;
# we’ll come back to that in Section 19.4.3.
missing_arg()
typeof(missing_arg())

is_missing(missing_arg())

# You’ll find them in the wild in function formals:
f <- expr(function(x, y = 10) x + y)
args <- f[[2]]
is_missing(args[[1]])

# This is particularly important for ... which is always associated with an empty symbol:
f <- expr(function(...) list(...))
args <- f[[2]]
is_missing(args[[1]])

# The empty symbol has a peculiar property: if you bind it to a variable,
# then access that variable, you will get an error:
m <- missing_arg()
m

# But you won’t if you store it inside another data structure!
ms <- list(missing_arg(), missing_arg())
ms[[1]]

# If you need to preserve the missingness of a variable, rlang::maybe_missing()
# is often helpful. It allows you to refer to a potentially missing
# variable without triggering the error. 


