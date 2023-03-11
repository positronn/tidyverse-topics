# pivoting.R
#
#
library(tidyr)
# Longer 
# String data in column names 
# The relig_income dataset stores counts based on a survey which (among other things) asked
# people about their religion and annual income:
relig_income

relig_income %>% 
    pivot_longer(cols = -religion,
                 names_to = 'income',
                 values_to = 'count')

# * cols describes which columns need to be reshaped. In this case, it’s every column
#   apart from religion
# * names_to gives the name of the variable that will be created from the data
#   stored in the column names, i.e. income
# * values_to gives the name of the variable that will be created from the
#   data stored in the cell value, i.e. count.
# Neither the names_to nor the values_to column exists in relig_income,
# so we provide them as strings surrounded by quotes.

# Numeric data in column names 
# The billboard dataset records the billboard rank of songs in the year 2000.
# It has a form similar to the relig_income data, but the data encoded in the 
# column names is really a number, not a string.
billboard

billboard %>% 
    pivot_longer(
        cols = starts_with('wk'),
        names_to = 'week',
        values_to = 'rank',
        values_drop_na = TRUE
    )

# It would be nice to easily determine how long each song stayed in the charts,
# but to do that, we’ll need to convert the week variable to an integer.
# We can do that by using two additional arguments: names_prefix strips off
# the wk prefix, and names_transform converts week into an integer:
billboard %>% 
    pivot_longer(
        cols = starts_with('wk'),
        names_to = 'week',
        names_prefix = 'wk',
        names_transform = as.integer,
        values_to = 'rank',
        values_drop_na = TRUE
    )

# Alternatively, you could do this with a single argument by using
# readr::parse_number() which automatically strips non-numeric components:
billboard %>% 
    pivot_longer(
        cols = starts_with("wk"), 
        names_to = "week", 
        names_transform = readr::parse_number,
        values_to = "rank",
        values_drop_na = TRUE,
    )

# Many variables in column names 
# A more challenging situation occurs when you have multiple variables crammed
# into the column names. For example, take the who dataset:

