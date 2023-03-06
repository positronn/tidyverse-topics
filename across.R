# across.R
#
# 
# across() makes it easy to apply the same transformation to multiple columns,
# allowing you to use select() semantics inside in "data-masking" functions like
# summarise() and mutate()

# if_any() and if_all() apply the same predicate function to a selection of columns and
# combine the results into a single logical vector: if_any() is TRUE when the predicate is
# TRUE for any of the selected columns, if_all() is TRUE when the predicate is TRUE for
# all selected columns.

# If you just need to select columns without applying a transformation to each of them,
# then you probably want to use pick() instead.

# across() supersedes the family of "scoped variants" like summarise_at(), summarise_if(),
# and summarise_all().

# across() typically returns a tibble with one column for each column in .cols and each
# function in .fns. If .unpack is used, more columns may be returned depending on how the
# results of .fns are unpacked.

# if_any() and if_all() return a logical vector.

iris <- as_tibble(iris)

iris %>% 
    mutate(across(c(Sepal.Length, Sepal.Width), round))

iris %>% 
    mutate(across(c(1, 2), round))

iris %>% 
    mutate(across(where(is.double) & c(Petal.Length, Petal.Width), round))

# purrr-style
iris %>% 
    group_by(Species) %>% 
    summarise(across(starts_with("Sepal"), ~ mean(., na.rm = TRUE)))

# a named list of functions
iris %>% 
    group_by(Species) %>% 
    summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd, median = median)))

iris %>% 
    group_by(Species) %>% 
    summarise(across(starts_with("Sepal"), mean, .names = "mean_{.col}"))


iris %>% 
    group_by(Species) %>% 
    summarise(across(starts_with('Sepal'), list(mean = mean, sd = sd ), .names = '{.col}.{.fn}'))

# # When the list is not named, .fn is replaced by the function's position
iris %>% 
    group_by(Species) %>% 
    summarise(across(starts_with('Sepal'), list(mean, sd), .names = '{.col}.fn{.fn}'))


# When the functions in .fns return a data frame, you typically get a
# "packed" data frame back
quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
    tibble(quantile = probs, value = quantile(x, probs))
}


iris %>%
    reframe(across(starts_with("Sepal"), quantile_df))

# Use .unpack to automatically expand these packed data frames into their
# individual columns
iris %>%
    reframe(across(starts_with("Sepal"), quantile_df, .unpack = TRUE))

# .unpack can utilize a glue specification if you don't like the defaults
iris %>%
    reframe(across(starts_with("Sepal"), quantile_df, .unpack = "{outer}.{inner}"))


# This is also useful inside mutate(), for example, with a multi-lag helper
multilag <- function(x, lags = 1:3) {
    names(lags) <- as.character(lags)
    purrr::map_dfr(lags, lag, x = x)
}

iris %>%
    group_by(Species) %>%
    mutate(across(starts_with("Sepal"), multilag, .unpack = TRUE)) %>%
    select(Species, starts_with("Sepal"))



# if_any, if_all
iris %>%
    filter(if_any(ends_with("Width"), ~ . > 4))

iris %>%
    filter(if_all(ends_with("Width"), ~ . > 2))

