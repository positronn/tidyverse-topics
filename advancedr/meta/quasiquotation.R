# quasiquotation.R
# introduction:
# 
# Now that you understand the tree structure of R code, it’s time to
# return to one of the fundamental ideas that make expr() and ast() work:
# quotation. In tidy evaluation, all quoting functions are actually
# quasiquoting functions because they also support unquoting. Where quotation
# is the act of capturing an unevaluated expression, unquotation is the
# ability to selectively evaluate parts of an otherwise quoted expression.
# Together, this is called quasiquotation. Quasiquotation makes it easy
# to create functions that combine code written by the function’s author
# with code written by the function’s user. This helps to solve a wide
# variety of challenging problems.

# Quasiquotation is one of the three pillars of tidy evaluation.
# You’ll learn about the other two (quosures and the data mask) in
# Chapter 20. When used alone, quasiquotation is most useful for programming,
# particularly for generating code. But when it’s combined with the other
# techniques, tidy evaluation becomes a powerful tool for data analysis.

library(rlang)
library(purrr)


# Quoting functions have deep connections to Lisp macros. But macros are
# usually run at compile-time, which doesn’t exist in R, and they always
# input and output ASTs. See Thomas Lumley95 for one approach to implementing
# them in R. Quoting functions are more closely related to the more esoteric
# Lisp fexprs, functions where all arguments are quoted by default. These terms
# are useful to know when looking for related work in other programming languages.

#  19.2 Motivation 
# We’ll start with a concrete example that helps motivate the need
# for unquoting, and hence quasiquotation. Imagine you’re creating a
# lot of strings by joining together words:
paste('Good', 'morning', 'Hadley')
paste('Good', 'afternoon', 'Alice')

# You are sick and tired of writing all those quotes, and instead you
# just want to use bare words. To that end, you’ve written the following function. 
cement <- function(...) {
    args <- ensyms(...)
    paste(map(args, as_string), collapse = " ")
}
cement(Good, morning, Hadley)
cement(Good, afternoon, Alice)


#mFormally, this function quotes all of its inputs. You can think of
# it as automatically putting quotation marks around each argument.
# That’s not precisely true as the intermediate objects it generates are
# expressions, not strings, but it’s a useful approximation, and the root
# meaning of the term “quote”.

# This function is nice because we no longer need to type quotation marks.
# The problem comes when we want to use variables. It’s easy
# to use variables with paste(): just don’t surround them with quotation marks.
name <- 'Hadley'
time <- 'morning'

paste('Good', time, name)
# Obviously this doesn’t work with cement() because every input is automatically quoted:
cement(Good, time, name)

# We need some way to explicitly unquote the input to tell cement()
# to remove the automatic quote marks. Here we need time and name to be
# treated differently to Good. Quasiquotation gives us a standard tool
# to do so: !!, called “unquote”, and pronounced bang-bang. !! tells
# a quoting function to drop the implicit quotes:
cement(Good, !!time, !!name)

# It’s useful to compare cement() and paste() directly. paste()
# evaluates its arguments, so we must quote where needed; cement()
# quotes its arguments, so we must unquote where needed.
paste('Good', time, name)
cement(Good, !!time, !!name)


#  19.2.1 Vocabulary 
# The distinction between quoted and evaluated arguments is important:
#   An evaluated argument obeys R’s usual evaluation rules.
#   A quoted argument is captured by the function, and is processed in some custom way.
# paste() evaluates all its arguments; cement() quotes all its arguments.

# Talking about whether an argument is quoted or evaluated is a more precise
# way of stating whether or not a function uses non-standard evaluation
# (NSE). I will sometimes use “quoting function” as short-hand for a
# function that quotes one or more arguments, but generally, I’ll talk
# about quoted arguments since that is the level at which the difference applies.


#  19.3 Quoting 
# The first part of quasiquotation is quotation: capturing an expression
# without evaluating it. We’ll need a pair of functions because the
# expression can be supplied directly or indirectly, via lazily-evaluated
# function argument. I’ll start with the rlang quoting functions,
# then circle back to those provided by base R.

#  19.3.1 Capturing expressions 
# There are four important quoting functions. For interactive exploration,
# the most important is expr(), which captures its argument exactly as provided:
expr(x + y)
expr(1 / 2 - 3     + 4 / 5)
# (Remember that white space and comments are not part of the expression,
# so will not be captured by a quoting function.)

# expr() is great for interactive exploration, because it captures
# what you, the developer, typed. It’s not so useful inside a function:
f1 <- function(x) expr(x)
f1(a + b + c)

# We need another function to solve this problem: enexpr(). This captures
# what the caller supplied to the function by looking at the internal
# promise object that powers lazy evaluation
f2 <- function(x) enexpr(x)
f2(a + b + c)

# (It’s called “en”-expr() by analogy to enrich. Enriching someone makes
# them richer; enexpr()ing a argument makes it an expression.)
# to capure all arguemnts in ..., use enexprs()
f <- function(...) enexprs(...)
f(x = 1, y = 10 * z)

# Finally, exprs() is useful interactively to make a list of expressions:
exprs(x = x ^ 2, y = y ^ 2, z = z ^ 4)

# In short, use enexpr() and enexprs() to capture the expressions supplied
# as arguments by the user. Use expr() and exprs() to capture
# expressions that you supply.

#  19.3.2 Capturing symbols 
# Sometimes you only want to allow the user to specify a variable name,
# not an arbitrary expression. In this case, you can use ensym() or ensyms().
# These are variants of enexpr() and enexprs() that check the captured
# expression is either symbol or a string (which is converted to a symbol96).
# ensym() and ensyms() throw an error if given anything else
f <- function(...) ensyms(...)
f(x)
f('x')


#  19.3.3 With base R 
# Each rlang function described above has an equivalent in base R.
# Their primary difference is that the base equivalents do not support
# unquoting (which we’ll talk about very soon). This make them quoting functions,
# rather than quasiquoting functions.
# The base equivalent of expr() is quote():
quote(x + y)
# The base function closest to enexpr() is substitute():
fd <- function(x) substitute(x)
fd(x + y)
# The base equivalent to exprs() is alist():
alist(x = 1, y = x + 2)
# The equivalent to enexprs() is an undocumented feature of substitute():
f <- function(...) as.list(substitute(...()))
f(x = 1, y = 10 * z)

# The equivalent to enexprs() is an undocumented feature of substitute()97:
#   bquote() provides a limited form of quasiquotation, and is discussed
#       in Section 19.5.
#   ~, the formula, is a quoting function that also captures the environment
#       It’s the inspiration for quosures, the topic of the next chapter,
#       and is discussed in Section 20.3.4.



#  19.4 Unquoting 
# So far, you’ve only seen relatively small advantages of the rlang quoting
# functions over the base R quoting functions: they have a more consistent
# naming scheme. The big difference is that rlang quoting functions
# are actually quasiquoting functions because they can also unquote.

# Unquoting allows you to selectively evaluate parts of the expression
# that would otherwise be quoted, which effectively allows you to
# merge ASTs using a template AST. Since base functions don’t use
# unquoting, they instead use a variety of other techniques, which
# you’ll learn about in Section 19.5.

#  19.4.1 Unquoting one argument 
# Use !! to unquote a single argument in a function call. !! takes a
# single expression, evaluates it, and inlines the result in the AST.
x <- expr(-1)
f <- function(...) enexprs(...)
expr(f(!!x, y))

# As well as call objects, !! also works with symbols and constants:
a <- sym('y')
b <- 1
expr(f(!!a, !!b))

# If the right-hand side of !! is a function call, !! will
# evaluate it and insert the results:
mean_rm <- function(var) {
    var <- ensym(var)
    expr(mean(!!var, na.rm = TRUE))
}
expr(!!mean_rm(x) + !!mean_rm(y))
expr(mean_rm(x)   +   mean_rm(y))

# !! preserves operator precedence because it works with expressions.
x1 <- expr(x + 1)
x2 <- expr(x + 2)

expr(!!x1 / !!x2)

#  19.4.2 Unquoting a function 
# !! is most commonly used to replace the arguments to a function,
# but you can also use it to replace the function. The only challenge
# here is operator precedence: expr(!!f(x, y)) unquotes the result of
# f(x, y), so you need an extra pair of parentheses.
f <- expr(foo)
expr(!!f(x, y))
expr((!!f)(x, y))

# This also works when f is a call:
f <- expr(pkg::foo)
expr((!!f)(x, y))
# Because of the large number of parentheses involved, it can be clearer to use rlang::call2():
f <- expr(pkg::foo)
call2(f, expr(x), expr(y))

#  19.4.3 Unquoting a missing argument 
# Very occasionally it is useful to unquote a missing argument 
# (Section 18.6.2), but the naive approach doesn’t work:
arg <- missing_arg()
expr(foo(!!arg, !!arg))

# You can work around this with the rlang::maybe_missing() helper:
expr(foo(!!maybe_missing(arg), !!maybe_missing(arg)))

#  19.4.4 Unquoting in special forms 
# There are a few special forms where unquoting is a syntax error.
# Take $ for example: it must always be followed by the name of
# a variable, not another expression. This means attempting to
# unquote with $ will fail with a syntax error:
expr(df$!!x)

# To make unquoting work, you’ll need to use the prefix form (Section 6.8.1):
x <- expr(x)
expr(`$`(df, !!x))

#  19.4.5 Unquoting many arguments 
# !! is a one-to-one replacement. !!! (called “unquote-splice”,
# and pronounced bang-bang-bang) is a one-to-many replacement.
# It takes a list of expressions and inserts them at the location
# of the !!!:
xs <- exprs(1, a, -b)
expr(f(!!!xs, y))

# or with names
ys <- set_names(xs, c('a', 'b', 'c'))
expr(f(!!!ys, d = 4))

# !!! can be used in any rlang function that takes ... regardless
# of whether or not ... is quoted or evaluated. We’ll come back to
# this in Section 19.6; for now note that this can be useful in call2().
call2('f', !!!xs, expr(y))


#  19.4.6 The polite fiction of !! 
# So far we have acted as if !! and !!! are regular prefix operators
# like + , -, and !. They’re not. From R’s perspective, !! and
# !!! are simply the repeated application of !:
!!TRUE
!!!TRUE

# !! and !!! behave specially inside all quoting functions powered
# by rlang, where they behave like real operators with precedence
# equivalent to unary + and -. This requires considerable work inside
# rlang, but means that you can write !!x + !!y instead of (!!x) + (!!y).

# The biggest downside98 to using a fake operator is that you might get
# silent errors when misusing !! outside of quasiquoting functions.
# Most of the time this is not an issue because !! is typically
# used to unquote expressions or quosures. Since expressions are
# not supported by the negation operator, you will get an argument type error in this case:

x <- quote(variable)
!!x
# But you can get silently incorrect results when working with numeric values:
df <- data.frame(x = 1:5)
y <- 100
with(df, x + !!y)


#  19.4.7 Non-standard ASTs 
# With unquoting, it’s easy to create non-standard ASTs, i.e. ASTs
# that contain components that are not expressions. (It is also
# possible to create non-standard ASTs by directly manipulating the
# underlying objects, but it’s harder to do so accidentally.) These
# are valid, and occasionally useful, but their correct use is beyond
# the scope of this book. However, it’s important to learn about
# them, because they can be deparsed, and hence printed, in misleading ways.
# For # example, if you inline more complex objects, their attributes
# are not printed. This can lead to confusing output:
x1 <- expr(class(!!data.frame(x = 10)))
x1    
eval(x1)
# You have two main tools to reduce this confusion: rlang::expr_print() and lobstr::ast():
expr_print(x1)
lobstr::ast(!!x1)

# Another confusing case arises if you inline an integer sequence:
x2 <- expr(f(!!c(1L, 2L, 3L, 4L, 5L)))
x2
expr_print(x2)
lobstr::ast(!!x2)

# It’s also possible to create regular ASTs that can not be generated from code because of operator precedence.
# In this case, R will print parentheses that do not exist in the AST:
x3 <- expr(1 + !!expr(2 + 3))
x3

lobstr::ast(!!x3)


#  19.6 ... (dot-dot-dot) 
# !!! is useful because it’s not uncommon to have a list of expressions
# that you want to insert into a call. It turns out that this
# pattern is common elsewhere. Take the following two motivating problems:
dfs <- list(
    a = data.frame(x = 1, y = 2),
    b = data.frame(x = 3, y = 4)
)
# You could solve this specific case with rbind(dfs$a, dfs$b), but
# how do you generalise that solution to a list of arbitrary length?
# What do you do if you want to supply the argument name indirectly?
# For example, imagine you want to create a single column data
# frame where the name of the column is specified in a variable:
var <- 'x'
val <- c(4, 3, 9)

# In this case, you could create a data frame and then change names
# (i.e. setNames(data.frame(val), var)), but this feels inelegant.
# How can we do better?
# One way to think about these problems is to draw explicit parallels to quasiquotation:
# Row-binding multiple data frames is like unquote-splicing: we
# want to inline individual elements of the list into the call:
dplyr::bind_rows(!!!dfs)
# When used in this context, the behaviour of !!! is known as
# “spatting” in Ruby, Go, PHP, and Julia. It is closely related to
# *args (star-args) and **kwarg (star-star-kwargs) in Python,
# which are sometimes called argument unpacking. 
# The second problem is like unquoting the left-hand side of =:
# rather than interpreting var literally, we want to use the value
# stored in the variable called var:
tibble::tibble(!!var := val)  # book uses this one, but it does not work

tibble::tibble(var := val)

tibble::tibble(!!var = val)

# Note the use of := (pronounced colon-equals) rather than =.
# Unfortunately we need this new operation because R’s grammar
# does not allow expressions as argument names:
# tibble::tibble(!!var = value)
#> Error: unexpected '=' in "tibble::tibble(!!var ="
# := is like a vestigial organ: it’s recognised by R’s parser,
# but it doesn’t have any code associated with it. It looks like
# an = but allows expressions on either side, making it a more
# flexible alternative to =. It is used in data.table for similar reasons. 

# We say functions that support these tools, without quoting arguments, have tidy dots100. To gain tidy dots behaviour in your own function, all you need to do is use list2().
#  19.6.1 Examples 
# One place we could use list2() is to create a wrapper around
# attributes() that allows us to set attributes flexibly:
set_attr <- function(.x, ...) {
    attr <- rlang::list2(...)
    attributes(.x) <- attr
    .x
}

attrs <- list(x = 1, y = 2)
attr_name <- 'z'

1:10 %>% 
    set_attr(w = 0, !!!attrs, !!attr_name := 3) %>% 
    str()


#  19.6.2 exec() 
# directly
exec('mean', x = 1:10, na.rm = TRUE, trim = 0.1)

# indirectly
args <- list(x = 1:10, na.rm = TRUE, trim = 0.1)
exec('mean', !!!args)

# mixed
params <- list(na.rm = TRUE, trim = 0.1)
exec('mean', x = 1:10, !!!params)

# rlang::exec() also makes it possible to supply argument names indirectly:
arg_name <- 'na.rm'
arg_val <- TRUE
exec('mean', 1:10, !!arg_name := arg_val)

# And finally, it’s useful if you have a vector of function names or a list of functions that you want to call with the same arguments:
x <- c(runif(10), NA)
x
funs <- c('mean', 'median', 'sd')
purrr::map_dbl(mean, exec, x, na.rm = TRUE)
purrr::map_dbl(funs, exec, x, na.rm = TRUE)

# exec() is closely related to call2(); where call2() returns an expression, exec() evaluates it.


#  19.6.4 With base R
# Base R provides a Swiss army knife to solve these problems: do.call(). do.call() has two main arguments. The first argument, what, gives a function to call. The second argument, args, is a list of arguments to pass to that function, and so do.call("f", list(x, y, z)) is equivalent to f(x, y, z).
#] do.call() gives a straightforward solution to rbind()ing together many data frames:
df1 <- data.frame(
    y = 1:5,
    z = 3:-1,
    x = 5:1
)
df2 <- data.frame(
    y = 1:5,
    z = 3:-1,
    x = 5:1
)
do.call("rbind", list(df1, df2))

# With a little more work, we can use do.call() to solve the second problem. We first create a list of arguments, then name that, then use do.call():
args <- list(val)

names(args) <- var

do.call("data.frame", args)


#  19.7 Case studies 

library(tibble)
library(dplyr)
df <- tibble(
    x = seq(1, 5, length.out = 100),
    y = seq(1, 3, length.out = 100),
    z = seq(1, 9, length.out = 100)
) %>% 
    mutate(w = y ** 2)

df %>% 
    select(x)

x <- 'z'
df %>% 
    select(!!x)

x <- c('w', 'z')
df %>% 
    select(!!x)
