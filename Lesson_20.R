library(tidyverse)

# There are 2 basic types of vectors atomic(integer,double,raw,logical,character,complex) and recursive(lists)
# Vectors can contain additional metadata called as attributes which are used to build augmented vectors(dates and datetimes,factors,tibbles)


# Q.1 ---------------------------------------------------------------------

# Describe the difference between is.finite(x) and !is.infinite(x).
# Answer :
# is.finite(x) checks whether x is finite (i.e. it is not NA / NaN / Inf / -Inf)
# !is.infinite(x) checks whether x is not one of Inf / _inf. Hence both are different. 
# Basically !is.infinite(x) will miss NA and NaN as seen below

is.finite(NaN)      # returns FALSE
!is.infinite(NaN)   # returns TRUE



# Q.2 ---------------------------------------------------------------------

# Read the source code for dplyr::near() (Hint: to see the source code, drop the ()). How does it work?
# Answer : Below is the source code of dplyr::near 

function (x, y, tol = .Machine$double.eps^0.5) 
{
  abs(x - y) < tol
}

# It can be seen that this function uses square root of  a variable called 
# .Machine$double.eps which is the the smallest positive floating-point number x 
# such that 1 + x != 1. It tries to find the difference between both values and returns
# TRUE if the difference in two values is LESS THAN square root of .Machine$double.eps



# Q.3 ---------------------------------------------------------------------

# A logical vector can take 3 possible values. 
# How many possible values can an integer vector take? 
# How many possible values can a double take? Use google to do some research.

# Answer :
# Integer vector can take .Machine$integer.max values while Double vector can take .Machine$double.xmax values
.Machine$integer.max  # returns 2147483647
.Machine$double.xmax  # returns 1.797693e+308