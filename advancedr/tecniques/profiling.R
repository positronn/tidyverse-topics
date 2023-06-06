
library(profvis)
library(bench)


source("advancedr/tecniques/profiling-example.R")
profvis(f())


# garbage collector
profvis({x <- integer()
    for (i in 1:1e4) {
        x <- c(x, i)
}})



x <- runif(100)
(lb <- bench::mark(
    sqrt(x),
    x ^ 0.5
))

plot(lb)


lb[c("expression", "min", "median", "itr/sec", "n_gc")]





#  24 Improving performance 

mean1 <- function(x) mean(x)
mean2 <- function(x) sum(x) / length(x)

x <- runif(1e5)
bench::mark(
    mean1(x),
    mean2(x)
)[c("expression", "min", "median", "itr/sec", "n_gc")]



x <- runif(1e2)

bench::mark(
    mean(x),
    mean.default(x)
)[c("expression", "min", "median", "itr/sec", "n_gc")]
# An even riskier optimisation is to directly call the underlying .Internal function. This is faster because it doesn’t do any input checking or handle NA’s, so you are buying speed at the cost of safety

x <- runif(1e2)
bench::mark(
    mean(x),
    mean.default(x),
    .Internal(mean(x))
)[c("expression", "min", "median", "itr/sec", "n_gc")]

# NB: Most of these differences arise because x is small. If you increase the size the differences basically disappear, because most of the time is now spent computing the mean, not finding the underlying implementation. This is a good reminder that the size of the input matters, and you should motivate your optimisations based on realistic data.
x <- runif(1e4)
bench::mark(
    mean(x),
    mean.default(x),
    .Internal(mean(x))
)[c("expression", "min", "median", "itr/sec", "n_gc")]



quickdf <- function(l) {
    class(l) <- "data.frame"
    attr(l, "row.names") <- .set_row_names(length(l[[1]]))
    l
}

l <- lapply(1:26, function(i) runif(1e3))
names(l) <- letters

bench::mark(
    as.data.frame = as.data.frame(l),
    quick_df      = quickdf(l)
)[c("expression", "min", "median", "itr/sec", "n_gc")]


# Again, note the trade-off. This method is fast because it’s dangerous. If you give it bad inputs, you’ll get a corrupt data frame:
quickdf(list(x = 1, y = 1:2))


#  24.6 Avoiding copies 

random_string <- function() {
    paste(sample(letters, 50, replace = TRUE), collapse = "")
}
strings10 <- replicate(10, random_string())
strings100 <- replicate(100, random_string())

collapse <- function(xs) {
    out <- ""
    for (x in xs) {
        out <- paste0(out, x)
    }
    out
}

bench::mark(
    loop10  = collapse(strings10),
    loop100 = collapse(strings100),
    vec10   = paste(strings10, collapse = ""),
    vec100  = paste(strings100, collapse = ""),
    check = FALSE
)[c("expression", "min", "median", "itr/sec", "n_gc")]


#  24.7 Case study: t-test 
m <- 1000
n <- 50
X <- matrix(rnorm(m * n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, each = n / 2)


system.time(
    for (i in 1:m) {
        t.test(X[i, ] ~ grp)$statistic
    }
)

system.time(
    for (i in 1:m) {
        t.test(X[i, grp == 1], X[i, grp == 2])$statistic
    }
)
# Of course, a for loop computes, but doesn’t save the values. We can map_dbl() (Section 9.2.1) to do that. This adds a little overhead:
compT <- function(i){
    t.test(X[i, grp == 1], X[i, grp == 2])$statistic
}
system.time(t1 <- purrr::map_dbl(1:m, compT))




#  25 Rewriting R code in C++ 
library(Rcpp)
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

add
add(1, 2, 3)


signR <- function(x) {
    if (x > 0) {
        1
    } else if (x == 0) {
        0
    } else {
        -1
    }
}


cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')



#  25.2.3 Vector input, scalar output 
# One big difference between R and C++ is that the cost of loops is much lower in C++. For example, we could implement the sum function in R using a loop. If you’ve been programming in R a while, you’ll probably have a visceral reaction to this function!
sumR <- function(x) {
    total <- 0
    for (i in seq_along(x)) {
        total <- total + x[i]
    }
    total
}

cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

x <- runif(1e3)
bench::mark(
    sum(x),
    sumC(x),
    sumR(x)
)[1:6]



#  25.2.4 Vector input, vector output 
# Next we’ll create a function that computes the Euclidean distance between a value and a vector of values:
