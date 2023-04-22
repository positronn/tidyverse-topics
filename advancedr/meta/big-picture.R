library(rlang)
library(lobstr)

# The first big idea is that code is data: you can capture code
# and compute on it as you can with any other type of data.
# The first way you can capture code is with rlang::expr(). You can
# think of expr() as returning exactly what you pass in:
expr(mean(x, na.rm=TRUE))
expr(10 + 100 + 1000)

# more formally, captured code is called an expression.
# An expression isnt a single type of object, but is a collective term
# for any of four types:
#   call, 
#   symbol,
#   constant,
#   pairlist

# expr() lets you capture code that you’ve typed. You need a
# different tool to capture code passed to a function because
# expr() doesn’t work:
capture_it <- function(x) {
    expr(x)
}
capture_it(a + b + c)

# Here you need to use a function specifically designed to capture
# user input in a function argument: enexpr(). Think of the “en” in the
# context of “enrich”: enexpr() takes a lazily evaluated argument and
# turns it into an expression:
capture_it <- function(x) {
    enexpr(x)
}
capture_it(a + b + c)
# Because capture_it() uses enexpr() we say that it automatically quotes its first argument.

# Once you have captured an expression, you can inspect and modify it.
# Complex expressions behave much like lists. That means you can modify
# them using [[ and $:
f <- expr(f(x = 1, y = 2))

# add a new argument
f$z <- 3
f

# or remove an argument
f[[2]] <- NULL
f
# The first element of the call is the function to be called,
# which means the first argument is in the second position.

# 13.3 Code is a tree
# To do more complex manipulation with expressions, you need to
# fully understand their structure. Behind the scenes, almost every
# programming language represents code as a tree, often called the
# abstract syntax tree, or AST for short. R is unusual in that you
# can actually inspect and manipulate this tree.
lobstr::ast(f(a, 'b'))

# Nested function calls create more deeply branching trees:
lobstr::ast(f1(f2(a, b), f3(1, f4(2))))
# Because all function forms can be written in prefix form, every R expression
# can be displayed in this way:
lobstr::ast(1 + 2 * 3 - 4 / 5)
# Displaying the AST in this way is a useful tool for exploring R’s grammar

#  17.4 Code can generate code 
# As well as seeing the tree from code typed by a human, you can also use code to create new trees.
# There are two main tools: `call2()` and unquoting.
# `rlang::call2()` constructs a function call from its components: the
# function to call, and the arguments to call it with.
call2('f', 1, 2, 3)
call2('+', 1, call2('*', 2, 3))

# call2() is often convenient to program with, but is a bit clunky for
# interactive use. An alternative technique is to build complex code
# trees by combining simpler code trees with a template. expr()
# and enexpr() have built-in support for this idea via !! (pronounced bang-bang),
# the unquote operator.
# The precise details are the topic of Section 19.4, but basically !!x
# inserts the code tree stored in x into the expression. This makes
# it easy to build complex trees from simple fragments:
xx <- expr(x + x)
yy <- expr(y + y)

expr(!!xx / !!yy)

# Notice that the output preserves the operator precedence so we get
# (x + x) / (y + y) not x + x / y + y (i.e. x + (x / y) + y). This
# is important, particularly if you’ve been wondering if it wouldn’t be
# easier to just paste strings together.

# Unquoting gets even more useful when you wrap it up into a function,
# first using enexpr() to capture the user’s expression, then expr()
# and !! to create a new expression using a template. The example below
# shows how you can generate an expression that computes the
# coefficient of variation:
cv <- function(var) {
    var <- enexpr(var)
    expr(sd(!!var) / mean(!!var))
}
cv(x)
cv(x + y)

# (This isn’t very useful here, but being able to create this
# sort of building block is very useful when solving more complex problems.)

# Importantly, this works even when given weird variable names:
cv(`)`)


#  17.5 Evaluation runs code 
# Inspecting and modifying code gives you one set of powerful tools.
# You get another set of powerful tools when you evaluate, i.e. execute 
# or run, an expression. Evaluating an expression requires an environment,
# which tells R what the symbols in the expression mean.
# The primary tool for evaluating expressions is base::eval(), which takes
# an expression and an environment:
eval(expr(x + y), env(x = 1, y = 10))
eval(expr(x + y), env(x = 2, y = 100))

# If you omit the environment, eval uses the current environment:
eval(expr(x + y))

# One of the big advantages of evaluating code manually is that you can
# tweak the environment. There are two main reasons to do this:
#       To temporarily override functions to implement a domain specific language.
#       To add a data mask so you can refer to variables in a data frame as if they
#       are variables in an environment.


#  17.6 Customising evaluation with functions 
# Here I evaluate code in a special environment where * and + have been
# overridden to work with strings instead of numbers:
string_math <- function(x) {
    e <- env(
        caller_env(),
        `+` = function(x, y) paste0(x, y),
        `*` = function(x, y) strrep(x, y)
    )
    eval(enexpr(x), e)
}
name <- 'Marco'
string_math('Hello' + ' ' + name)
string_math(('x' * 2 + '-y'))
string_math(('x' * 2 + '-y') * 3)

# dplyr takes this idea to the extreme, running code in
# an environment that generates SQL for execution in a remote database:
library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), filename = ":memory:")
mtcars_db <- copy_to(con, mtcars)
mtcars_db %>% 
    filter(cyl > 2) %>% 
    select(mpg:hp) %>% 
    head(10) %>% 
    show_query()


mtcars_db %>% 
    filter(cyl > 2) %>% 
    select(mpg:hp) %>% 
    count(mpg, cyl, disp) %>% 
    arrange(desc(mpg), desc(cyl)) %>% 
    head(100) %>% 
    show_query()

DBI::dbDisconnect(con)
