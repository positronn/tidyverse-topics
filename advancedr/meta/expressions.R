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
