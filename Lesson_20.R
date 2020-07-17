#----------------------------------------------- START--------------------------------------------------
library(tidyverse)

# 20.3.5 ------------------------------------------------------------------


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



# Q.4 ---------------------------------------------------------------------
#Brainstorm at least four functions that allow you to convert a double to an integer. 
# How do they differ? Be precise.

# Answer : We will convert variable 'd' to integers
d = 100 # double

# Function 1 : Using L :  this notation dates from the C language on 16-bit computers
# where integers were 16-bits and longs were 32-bit (and R has no 'long' type).
int1 = 100L # typeof(int1) gives integer

# Function 2 # using as.integer : Longer and less efficient than method#1 as 
# the coercion to integer is not done at parse time
int2 = as.integer(d) # typeof(int1) gives integer

# Function 3 & 4: There are many functions the like round, ceiling, floor 
# but using them and then checking the type of the result using typeof() gives double 

# Q.5 ---------------------------------------------------------------------

# What functions from the readr package allow you to turn a string
# into logical, integer, and double vector?

# Answer : parse_*()
a_string = "1"
d1 = parse_double(a_string) 
d2 = parse_integer(a_string)
d3 = parse_logical(a_string)



# 20.4.6 ------------------------------------------------------------------

# Q.1 ---------------------------------------------------------------------
# What does mean(is.na(x)) tell you about a vector x? What about sum(!is.finite(x))?
# Answer: 
x = c(1,2,3,NA,5,6)
mean(is.na(x))     # This operation tells average number of missing values in the vector
sum(!is.finite(x)) # This function gives sum of NA / NaN / Inf / -Inf in the vector

# Q.2 ---------------------------------------------------------------------
# Carefully read the documentation of is.vector(). What does it actually test for? 
# Why does is.atomic() not agree with the definition of atomic vectors above?

# Answer :
# is.vector returns TRUE if x is a vector of the specified mode 
# having no attributes other than names. It returns FALSE otherwise.

is.vector(c(1,2,3,4,5))                  # TRUE as its a simple numeric vector with default mode == "any"
is.vector(c(1,2,3,4,5),mode = "logical") # FALSE as mode is differnet
is.vector(c(Sys.Date(),Sys.Date()-1))    # FALSE as vector has attribute 
is.vector(NULL)                          # FALSE as NULL == absenceo f vector  

# is.atomic returns TRUE if x is of an atomic type (or NULL) and FALSE otherwise.
is.atomic(c(Sys.Date(),Sys.Date()-1))    # TRUE as mode and attributes is not checked 
is.atomic(NULL)                          # TRUE 

# Q.3 ---------------------------------------------------------------------
# Compare and contrast setNames() with purrr::set_names().
x = c(1,2,3,NA,5,6)
newnames = c("a","b","c","d","e","f")
n1 = setNames(object = x,nm = newnames)
n2 = purrr::set_names(x = x,nm = newnames)

# purrr::set_names is equivalent to stats::setNames(), 
# with more features and stricter argument checking.

# Q.4 ---------------------------------------------------------------------

# Create functions that take a vector as input and returns:
x = c(1,2,3,NA,5,6)

# 1. The last value. Should you use [ or [[?
get_last_value = function(x){ x[length(x)] } 
get_last_value(x)  # returns 6

# 2. The elements at even numbered positions.
get_even_pos = function(x) { x[((1 : length(x)) %% 2) == 0] }
get_even_pos(x)    # returns 2 NA 6

# 3. Every element except the last value.
drop_last_value = function(x) { x[-length(x)] } 
drop_last_value(x) # returns 1  2  3 NA  5  

# 4. Only even numbers (and no missing values).
get_non_na_evens = function(x) { 
  
  non_na_values = x[ !is.na(x) ]              # Get non na values
  even_values_pos = non_na_values %% 2 == 0   # Get even values pos
  non_na_even_values = non_na_values[even_values_pos] # subset non na values 
  return(non_na_even_values)
  }

get_non_na_evens(x) # returns 2 6   

# Q.5 ---------------------------------------------------------------------
# Why is x[-which(x > 0)] not the same as x[x <= 0]?
x = c(-2,-1,0,1,2,3,4,5,6)
x[-which(x > 0)] # This code drops the elements which are greater than 0
x[x <= 0]        # THis code selects the elements which are less than equal to 0   


# Q.6 ---------------------------------------------------------------------
# What happens when you subset with a positive integer that’s bigger than the length of the vector? 
x[ length(x) + 1 ]   # returns NA

# What happens when you subset with a name that doesn’t exist?
x[ "non_existent name" ]  # returns NA


# 20.5.4 ------------------------------------------------------------------


# Q.1 ---------------------------------------------------------------------


# 1. Draw the following lists as nested sets:
# 1. list(a, b, list(c, d), list(e, f))
# 2. list(list(list(list(list(list(a))))))
# Cannot draw here, except on paper


# Q.2 ---------------------------------------------------------------------


# What happens if you subset a tibble as if you’re subsetting a list?
tb = tibble(x = c(1,2,3), y = c(4,5,6))
# Answer :
# [ ] works on tibble as it does on a list. [ ] subsets a larger tibble into a smaller tibble. 
tb[1]  # returns a tibble

# [[]] works on tibble as it does on a list. [[ ]] subsets a larger tibble into a vector.
tb[[1]] # returns a vector

# $ works on tillbe as it does on a list. 
tb$x    # returns a vector
 
# What are the key differences between a list and a tibble?
# Answer :
# 1. lists can hold hierarchical data, tibble cannot hold hierarchical data
# 2. lists are heterogenous. tibble are also heterogenous but each column of tibble is homogenous
# 3. lists can be N dimensional. Tibbles are 2 dimensional.
# 4. The difference between a tibble and a list is that all the elements of a data frame must be vectors with the same length. 




# 20.7.4 ------------------------------------------------------------------


# Q.1 ---------------------------------------------------------------------
# 1. What does hms::hms(3600) return? How does it print? 
t = hms::hms(3600) 
t                   # returns 01:00:00 
# What primitive type is the augmented vector built on top of? 
typeof(t)           # double

# What attributes does it use?
attributes(t)       # units attribute # class attribute  

# Q.2 ---------------------------------------------------------------------
# Try and make a tibble that has columns with different lengths. What happens?
# Answer :
# throws an error Tibble columns must have consistent lengths, only values of length one are recycled:
tibble(x = c(1,2,3,4,5), y = c(1,2,3))

# Q.3 ---------------------------------------------------------------------
# Based on the definition above, is it ok to have a list as a column of a tibble?
# Answer :
# It is NOT OK to have list as a column of tibble because 
# Lists are heterogenous and by definition column in a tibble must be vectors all the elements 
# of a data frame must be vectors with the same length.

#----------------------------------------------- END--------------------------------------------------

