# base-types.R
library(sloop)

# The sloop package (think “sail the seas of OOP”)
# provides a number of helpers that fill in missing pieces in base R.
# The first of these is sloop::otype(). It makes it easy to figure out the OOP
# system used by a wild-caught object:

otype(1:10)

otype(mtcars)

mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
otype(mle_obj)


#  12.2 Base versus OO objects 
# To tell the difference between a base and OO object, use is.object() or sloop::otype():

# A base object:
is.object(1:10)
sloop::otype(1:10)

# An OO object
is.object(mtcars)
sloop::otype(mtcars)


# Technically, the difference between base and OO objects is that OO objects have a “class” attribute:
attr(1:10, "class")
attr(mtcars, "class")


x <- matrix(1:4, nrow = 2)
class(x)

sloop::s3_class(x)

#  12.3 Base types 
# While only OO objects have a class attribute, every object has a base type:
typeof(1:10)
typeof(mtcars)

