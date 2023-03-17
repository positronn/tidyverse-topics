# s3.R
# S3 is very flexible, which means it allows you to do things that are quite
# ill-advised. If you’re coming from a strict environment like Java this will
# seem pretty frightening, but it gives R programmers a tremendous amount of freedom.
# It may be very difficult to prevent people from doing something you don’t
# want them to do, but your users will never be held back because there is
# something you haven’t implemented yet. Since S3 has few built-in constraints,
# the key to its successful use is applying the constraints yourself. This chapter
# will therefore teach you the conventions you should (almost) always follow.
library(sloop)

#  13.2 Basics 
# An S3 object is a base type with at least a class attribute (other attributes
# may be used to store other data). For example, take the factor.
# Its base type is the integer vector, it has a class attribute of “factor”,
# and a levels attribute that stores the possible levels:
f <- factor(c('a', 'b', 'c'))

typeof(f)

attributes(f)

# You can get the underlying base type by unclass()ing it, which strips the
# class attribute, causing it to lose its special behaviour:
unclass(f)

# An S3 object behaves differently from its underlying base type whenever
# it’s passed to a generic (short for generic function). The easiest way to tell
# if a function is a generic is to use sloop::ftype() and look for “generic” in
# the output:
ftype(print)

ftype(str)

ftype(unclass)

# A generic function defines an interface, which uses a different
# implementation depending on the class of an argument (almost always the first
# argument). Many base R functions are generic, including the important print():
print(f)

print(unclass(f))

# Beware that str() is generic, and some S3 classes use that generic to
# hide the internal details. For example, the POSIXlt class used to represent
# date-time data is actually built on top of a list, a fact which is hidden by
# its str() method:
time <- strptime(c("2017-01-01", "2020-05-04 03:21"), "%Y-%m-%d")
str(time)

str(unclass(time))

# The generic is a middleman: its job is to define the interface (i.e. the arguments)
# then find the right implementation for the job. The implementation
# for a specific class is called a method, and the generic finds that method
# by performing method dispatch.
# You can use sloop::s3_dispatch() to see the process of method dispatch:

s3_dispatch(print(f))

# We’ll come back to the details of dispatch in Section 13.4.1,
# for now note that S3 methods are functions with a special naming scheme,
# generic.class(). For example, the factor method for the print() generic
# is called print.factor(). You should never call the method directly,
# but instead rely on the generic to find it for you.

# Generally, you can identify a method by the presence of . in the
# function name, but there are a number of important functions in base R
# that were written before S3, and hence use . to join words. If you’re
# unsure, check with sloop::ftype():
ftype(t.test)
ftype(t.data.frame)

# Unlike most functions, you can’t see the source code for most S3
# methods68 just by typing their names. That’s because S3 methods are
# not usually exported: they live only inside the package, and are not
# available from the global environment. Instead, you can use
# sloop::s3_get_method(), which will work regardless of where the method lives:
weighted.mean.Date

s3_get_method(weighted.mean.Date)


#  13.3 Classes 
# If you have done object-oriented programming in other languages, you may
# be surprised to learn that S3 has no formal definition of a class:
# to make an object an instance of a class, you simply set the class attribute.
# You can do that during creation with structure(), or after the fact with
# class<-():

# create and assign class in one step
x <- structure(list(), class = 'my_class')

# create, then set class
x <- list()
class(x) <- 'my_class'

# You can determine the class of an S3 object with class(x), and see
# if an object is an instance of a class using inherits(x, "classname").
class(x)
inherits(x, 'my_class')
inherits(x, 'your_class')

# The class name can be any string, but I recommend using only letters
# and _. Avoid . because (as mentioned earlier) it can be confused
# with the . separator between a generic name and a class name. When using
# a class in a package, I recommend including the package name in the
# class name. That ensures you won’t accidentally clash with a class
# defined by another package.
# S3 has no checks for correctness which means you can change the class of existing objects:

# create a linear model
mod <- lm(log(mpg) ~ log(disp), data = mtcars)
class(mod)
print(mod)

# turn it into a data (?!)
class(mod) <- 'Date'
# Unsurprisingly this doesn't work very well
print(mod)

# If you’ve used other OO languages, this might make you feel queasy,
# but in practice this flexibility causes few problems. R doesn’t
# stop you from shooting yourself in the foot, but as long as you don’t
# aim the gun at your toes and pull the trigger, you won’t have a problem.

# To avoid foot-bullet intersections when creating your own class,
# I recommend that you usually provide three functions:
#  * A low-level constructor, new_myclass(), that efficiently creates
#       new objects with the correct structure.
#  * A validator, validate_myclass(), that performs more computationally
#       expensive checks to ensure that the object has correct values.
#  * A user-friendly helper, myclass(), that provides a convenient way
#       for others to create objects of your class.

# You don’t need a validator for very simple classes, and you can skip
# the helper if the class is for internal use only, but you should
# always provide a constructor.

#  13.3.1 Constructors 
# S3 doesn’t provide a formal definition of a class, so it has no
# built-in way to ensure that all objects of a given class have the
# same structure (i.e. the same base type and the same attributes
# with the same types). Instead, you must enforce a consistent
# structure by using a constructor.
# The constructor should follow three principles:
#   * Be called new_myclass().
#   * Have one argument for the base object, and one for each attribute.
#   * Check the type of the base object and the types of each attribute.

# To start, lets make a constructor for the simplest S3 class:
# Date. A Date is just a double with a single attribute: its
# class is “Date”. This makes for a very simple constructor:
new_Date <- function(x = double()) {
    stopifnot(is.double(x))
    structure(x, class = 'Date')
}

new_Date(c(-1, 0, 1))

# The purpose of constructors is to help you, the developer.
# That means you can keep them simple, and you don’t need to
# optimise error messages for public consumption. If you expect
# users to also create objects, you should create a friendly
# helper function, called class_name(), which I’ll describe shortly.
# A slightly more complicated constructor is that for difftime,
# which is used to represent time differences. It is again built
# on a double, but has a units attribute that must take one of a
# small set of values:
new_difftime <- function(x = double(), units = 'secs') {
    stopifnot(is.double(x))
    units <- match.arg(units, c('secs', 'mins', 'hours', 'days', 'weeks'))
    
    structure(x,
        class = 'difftime',
        units = units
    )
}

new_difftime(c(1, 10, 3600), 'secs')
new_difftime(52, 'weeks')

# The constructor is a developer function: it will be called
# in many places, by an experienced user. That means it’s OK to
# trade a little safety in return for performance, and you should
# avoid potentially time-consuming checks in the constructor.

#  13.3.2 Validators 
# More complicated classes require more complicated checks for validity.
# Take factors, for example. A constructor only checks that types are
# correct, making it possible to create malformed factors:
new_factor <- function(x = integer(), levels = character()) {
    stopifnot(is.integer(x))
    stopifnot(is.character(levels))
    structure(x,
        levels = levels,
        class = 'factor'
    )
}
new_factor(1:5, 'a')
new_factor(0:1, 'a')


# Rather than encumbering the constructor with complicated checks,
# it’s better to put them in a separate function. Doing so allows you
# to cheaply create new objects when you know that the values are correct,
# and easily re-use the checks in other places.
validate_factor <- function(x) {
    values <- unclass(x)
    levels <- attr(x, 'levels')
    if (!all(!is.na(values) & values > 0)) {
        stop(
            'All `x` values must be non-missing and greater than zero',
            call. = FALSE
        )
    }
    if (length(levels) < max(values)) {
        stop(
            'There must be at least as many `levels` as possible values in`x',
            call. = FALSE
        )
    }
    x
}

validate_factor(new_factor(1:5, 'a'))
validate_factor(new_factor(0:1, 'a'))

# This validator function is called primarily for its side-effects
# (throwing an error if the object is invalid) so you’d expect it to
# invisibly return its primary input (as described in Section 6.7.2).
# However, it’s useful for validation methods to return visibly,
# as we’ll see next.

#  13.3.3 Helpers 
# If you want users to construct objects from your class, you
# should also provide a helper method that makes their life as easy as
# possible. A helper should always:
#   Have the same name as the class, e.g. myclass().
#   Finish by calling the constructor, and the validator, if it exists.
#   Create carefully crafted error messages tailored towards an end-user.
#   Have a thoughtfully crafted user interface with carefully
#       chosen default values and useful conversions.

# The last bullet is the trickiest, and it’s hard to give general advice.
# However, there are three common patterns:
#   * Sometimes all the helper needs to do is coerce its inputs to the
#       desired type. For example, new_difftime() is very strict,
#       and violates the usual convention that you can use an integer vector wherever you can use a double vector:
new_difftime(1:10)

# It’s not the job of the constructor to be flexible, so here we create
# a helper that just coerces the input to a double.
difftime <- function(x = double(), units = 'secs') {
    x <- as.double(x)
    new_difftime(x, units = units)
}

difftime(1:10)

# Often, the most natural representation of a complex object is a string.
# For example, it’s very convenient to specify factors with a character
# vector. The code below shows a simple version of factor(): it takes a
# character vector, and guesses that the levels should be the unique
# values. This is not always correct (since some levels might not be seen
# in the data), but it’s a useful default.
factor <- function(x = character(), levels = unique(x)) {
    ind <- match(x, levels)
    validate_factor(new_factor(ind, levels))
}

factor(c('a', 'a', 'b'))

# Some complex objects are most naturally specified by multiple simple
# components. For example, I think it’s natural to construct a
# date-time by supplying the individual components (year, month, day etc).
# That leads me to this POSIXct() helper that resembles the existing
# ISODatetime() function70:
POSIXct <- function(year = integer(),
                    month = integer(),
                    day = integer(),
                    hour = 0L,
                    minute = 0L,
                    sec = 0,
                    tzone = '') {
    ISOdatetime(year, month, day, hour, minute, sec, tz = tzone)
}
POSIXct(2020, 1, 1, tzone = 'America/New_York')

# For more complicated classes, you should feel free to go beyond
# these patterns to make life as easy as possible for your users.


