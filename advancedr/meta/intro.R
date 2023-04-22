# intro.R
# One of the most intriguing things about R is its ability to
# do metaprogramming. This is the idea that code is data that can
# be inspected and modified programmatically.

# Closely related to metaprogramming is non-standard evaluation,
# NSE for short. This term, which is commonly used to describe the
# behaviour of R functions, is problematic in two ways. Firstly, NSE
# is actually a property of the argument (or arguments) of a function,
# so talking about NSE functions is a little sloppy. Secondly, it’s
# confusing to define something by what it’s not (standard), so in this
# book I’ll introduce more precise vocabulary.

# Specifically, this book focuses on tidy evaluation (sometimes
# called tidy eval for short). Tidy evaluation is implemented in the rlang package
