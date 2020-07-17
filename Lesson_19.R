# ----------------------------------------- 19.2.1 Exercises  ----------------------------------------- 

# Q.1 ------------------------------------------
# Why is TRUE not a parameter to rescale01()?
# What would happen if x contained a single missing value, and na.rm was FALSE?

rescale01 = function(x) {
  rng = range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Answer: TRUE is not a parameter to rescale01() because the range() call within rescale01()
#         returns NA for any vector containing missing value as shown below.
#         Since, na.rm = TRUE is mandatory for rescale01 to function properly,TRUE is not optional, 
#         hence not a parameter.

x = c(1,2,3,4,5, NA)
range(x, na.rm = FALSE)


# Q.2 ------------------------------------------ 
# In the second variant of rescale01(), infinite values are left unchanged. 
# Rewrite rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1.

rescale01 = function(x) {
  rng = range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

x = c(-Inf,1:10,Inf) # Infinity values added to the input
rescale01(x)         # Infinity values are left unchanged

# Answer :
# Rewriting the function

rescale01 = function(x){
  
  rng = range(x,na.rm = TRUE,finite = FALSE)        # Calculating the range, allowing infinte values
  min_rng = rng[1]                                  # Finding min
  max_rng = rng[2]                                  # Finding max
  
  # Finding if minimum is -Inf
  if(min_rng == -Inf){
  
    rng_non_inf = range(x,na.rm = TRUE,finite = TRUE) # if yes, then find the range removing infinite values
    rng_non_inf_min = rng_non_inf[1]                  # Find minimum removing infinite values  
    x[which(x == min_rng)] = rng_non_inf_min - 1      # Reset -Inf to most minimum finite value 
  }
  
  # Finding if maximum is Inf
  if(max_rng == Inf){
    rng_non_inf = range(x,na.rm = TRUE,finite = TRUE) # if yes, then find the range removing infinite values
    rng_non_inf_max = rng_non_inf[2]                  # Find maximum removing infinite values   
    x[which(x == max_rng)] = rng_non_inf_max + 1       # Reset Inf to most maximum finite value
    
  }
  
  rng2 = range(x,na.rm = TRUE,finite = FALSE)         # recalculate the range
  (x - rng2[1]) / (rng2[2] - rng2[1])                 # return the result 
  
   
}


# Testing the funtion
x_inf = c(-Inf,1:10,Inf)   # Infinity values added to the input
rescale01(x_inf)           # Infinity values are left unchanged

x_no_NA = c(1:10)          # Infinity values removed from the input
rescale01(x_no_NA)         # Works fine


# Q.3 ------------------------------------------
# Practice turning the following code snippets into functions. 
# Think about what each function does. What would you call it? How many arguments does it need? 
# Can you rewrite it to be more expressive or less duplicative?

# Answer:
x = c(NA,c(1:10,3,2,1,6,5,7,8,11,12),NA)           

# Snippet 1 
mean(is.na(x))  

# This snippet finds average number of NA values in a vector.
# It can be rewritten into a more descriptive function : find_average_NAs()
# It only needs one argument : the vector to be analyzed

find_average_NAs = function(x){
  
  number_of_NAs = sum(is.na(x))              # Find the number of NAs
  average_NAs = number_of_NAs / length(x)    # Divide by number of elements in vector   
  return(average_NAs)
}

find_average_NAs(x)                          # Testing the function  

# Snippet 2
x / sum(x, na.rm = TRUE)
# This snippet divides each element of a vector by the sum of all elements of that vector
# If can be renamed into a  more descriptive function : divide_by_sum()
# This function needs only one argument : the vector to be analyzed

divide_by_sum = function(x){
  
  sum_of_elements = sum(x,na.rm = TRUE)     # Find the sum
  result = x / sum_of_elements              # Divide vector by sum
  return(result)
}

divide_by_sum(x)                             # Testing the function


# Snippet 3 
sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
# This function finds coefficient of variation of a vector
# If can be renamed into a more descriptive function : find_coeff_var()
# It needs only one input : the vector to be analyzed

find_coeff_var = function(x){
  
  std_dev = sd(x,na.rm = TRUE)            # find standard deviation
  avg_x   = mean(x,na.rm = TRUE)          # find average
  result = std_dev / avg_x                # find result
  return(result)
  
}

find_coeff_var(x)                         # Testing the function  



# Q.4------------------------------------------
# write your own functions to compute the variance and skewness of a numeric vector

# Answer:
find_variance = function(x){
  
  x = x[!is.na(x)]               # Remove all NAs
  avg_x = mean(x)                # Find the mean
  error_x = x - avg_x            # Find the error
  error_x_sqrd = error_x ^ 2     # Square the error
  sum_squrd_errors = sum(error_x_sqrd)  # Sum of squared errors
  divide_by = length(x) - 1      # Find the denominator 
  variance = sum_squrd_errors / divide_by  # Calculate variance
  return(variance)

}

find_skewness = function(x){
  
  x = x[!is.na(x)]               # Remove all NAs   
  avg_x = mean(x)                # Find the mean
  error_x = x - avg_x            # Find the error
  error_x_cube = error_x ^ 3     # Cube of the error
  sum_cubed_errors = sum(error_x_cube)  # Sum of squared errors
  divide_by = length(x) - 2      # Find the denominator for sub_cubed_errors
  skew_num = sum_cubed_errors / divide_by # Find numerator of skewness formula 
  skew_den = find_variance(x) ^ (3/2) # Find denominator of skewness formula
  skewness = skew_num / skew_den # find skewness
  return(skewness)
  
}

# test the functions
find_variance(x)                 
find_skewness(x)

# Q.5 ------------------------------------------
# Write both_na(), a function that takes two vectors of the same length and 
# returns the number of positions that have an NA in both vectors.

# Answer:
# Define input vectors
x = c(1,2,3,NA,5,6)
y = c(10,20,NA,40,NA,60)

find_na_positions = function(x,y){
  
  x_na = which(is.na(x))
  y_na = which(is.na(y))
  result = list("NA position in X : " = x_na,
                "NA position in Y : " = y_na)
  return(result)
}

#  Q.6 ------------------------------------------
# What do the following functions do? Why are they useful even though they are so short?
# Answer:

#1.
is_directory <- function(x) file.info(x)$isdir         
# This function finds if argument supplied to it is a valid existing directory.
# It is useful because it is descriptive and easy to understand  
# as file.info - a function used for checking whether the argument is a valid directory
# returns a list of many elements where this info is hidden and the user is freed from the burden of
# understanding what is going inside that function by using a simple is_directory() function

#2.
is_readable <- function(x) file.access(x, 4) == 0     
# This function finds if directory supplied as an argument has read permission.
# It is useful because it is descriptive and easy to understand 
# and user is freed from burden to understand the internal working on file.accesss() function


#  Q.7 --------------------------------------------------------
# Read the complete lyrics to “Little Bunny Foo Foo”. 
# There’s a lot of duplication in this song. 
# Extend the initial piping example to recreate the complete song, 
# and use functions to reduce the duplication.
# Original lyrics is as follows :
"
Little Bunny Foo Foo,
Hopping through the forest,
Scooping up the field mice,
And bopping them on the head.

(Spoken)
Down came the Good Fairy, and she said,

Little Bunny Foo Foo,
I don't want to see you,
Scooping up the field mice
And bopping them on the head.


I'll give you three chances,
And if you don't behave,
I'm gonna turn you into a goonie!

The next day... 
Little Bunny Foo Foo,
Hopping through the forest,
Scooping up the field mice,
And bopping them on the head.

(Spoken)
Down came the Good Fairy, and she said,

Little Bunny Foo Foo,
I don't want to see you,
Scooping up the field mice
And bopping them on the head.


I'll give you two more chances,
And if you don't behave,
I'm gonna turn you into a goonie!


The next day... 
Little Bunny Foo Foo,
Hopping through the forest,
Scooping up the field mice,
And bopping them on the head.

(Spoken)
Down came the Good Fairy, and she said,

Little Bunny Foo Foo,
I don't want to see you,
Scooping up the field mice
And bopping them on the head.

I'll give you one more chance... 
And if you don't behave,
I'm gonna turn you into a goonie!

I gave you three chances,
And you didn't behave,
And now I'm gonna turn you into a goonie. POOF!
"
'
# Answer :

# A function like one below can be written 
# to turn foo_foo into goonie by the good fairy
turn_into_goonie = function(foo_foo){
  
  goonie = good_fairy_magic(foo_foo) 
  return(goonie)
}


# foo_foo starts his work
for(i in 1:4){
  
  # foo foo has 3 chances to mend his ways
  if(i %in% c(1,2,3)){
    # But......
    foo_foo %>%
      hop(through = forest) %>%             # foo foo hops 
      scoop(up = field_mice) %>%            # foo foo scoops up 
      bop(on = head)                        # foo foo bops
    
    # the good fairy scold foo foo
    foo_foo %>% 
      gets_scolded(by = the_good_fairy)
    
  }else{
    # In the end the good fairy turns foo foo into goonie
    turn_into_goonie(foo_foo)
  }
}# i loop ends
'

# 19.3.1 Exercises --------------------------------------------------------

# Q.1 ---------------------------------------------------------------------
# Read the source code for each of the following three functions, 
# puzzle out what they do, and then brainstorm better names.

# 1. 
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

# This function finds if a string has certain prefix
# It can be renamed check_prefix()
check_prefix = function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

check_prefix("Mr.India", "Mr.") # returns TRUE
check_prefix("Mr.India", "Mrs") # returns FALSE


# 2. 
f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

# This function drops the last element of any vector supplied to it
# It can be renamed drop_last_element()

drop_last_element = function(x){
  
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}


drop_last_element(c(1,2,3,4)) # Returns 1 2 3
drop_last_element(c(1)) # Returns NULL

#3. 
f3 <- function(x, y) {
  rep(y, length.out = length(x))
}

# This function repeats the second parameter only the number of times of the
# length of the first parameter. It can be renamed as repeat_y_till_length_of_x

repeat_y_till_length_of_x = function(x, y) {
  rep(y, length.out = length(x))
}

repeat_y_till_length_of_x(x = c(1,2,3,4), y = c(2,5))   # Retirns 2 5 2 5


#Q.2 --------------------------------------------------------------------

# Take a function that you’ve written recently and spend 5 minutes 
# brainstorming a better name for it and its arguments.

# Answer :
# The code in above Q.1 can be renamed to create_vector_having_equal_length()
# x --- > of_vector
# y --- > using_values


create_vector_having_equal_length = function(of_vector, using_values) {
  rep(using_values, length.out = length(of_vector))
}


# Q.3 ---------------------------------------------------------------------

# Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?

# Answer : As seen below, both the function return random variables where mean and Standard deviation
# is defined. Both can be made consistent by changing the parameter names in anyone of them
# to match with the other
rnorm(n = 100,mean = 1,sd = 1) 
MASS::mvrnorm(n = 100,mu = 1,Sigma = 1)



# Q.4 ---------------------------------------------------------------------

# Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm().
# Answer :  Since rnorm, dnorm etc. create normal distribution and belong to the same family
#           of functions, it is advisable to start them with norm_xxx() so that they are easy to 
#           search using autopopulation functionality

# Make a case for the opposite.
# Answer :  rnorm() sounds easy to remember than norm_r(). May be our brain remembers shorter names easily.


# 19.4.1 Exercises --------------------------------------------------------


# Q.1 ---------------------------------------------------------------------

# What’s the difference between if and ifelse()? 
# Carefully read the help and construct three examples that illustrate the key differences.

# ?`if`
# if returns the value of the expression evaluated, or NULL invisibly if none was
# (which may happen if there is no else).

# ?ifelse
# ifelse returns a value with the same shape as test

# eg  : "ifelse" returns value of same length as the test condition  and returns result even if any
# of the test expression is TRUE while "if" returns only evaluated expression

if(TRUE){
  
  "Hi"

}else{
  
  "Bye"
  
}

ifelse(c(FALSE,TRUE,FALSE),"HI","BYE")


# Q.2 ---------------------------------------------------------------------

# Write a greeting function that says “good morning”, “good afternoon”, or “good evening”, 
# depending on the time of day. (Hint: use a time argument that defaults to lubridate::now(). 
# That will make it easier to test your function.)

library(hms)
library(dplyr)

greet = function(){
  
  # Get the current full time
  ctime_full = as.character(lubridate::now()) 
  ctime = unlist(strsplit(ctime_full," "))[2] %>% hms::parse_hms()   # Get current time
  
  noon = hms::as_hms("12:00:00")    # Set noon time
  evening = hms::as_hms("18:00:00") # Set evening time
  
  # condition
  ifelse(ctime < noon,"Good Morning",
         ifelse((noon < ctime) && (ctime < evening),"Good Afternoon","Good Evening"))  
    
}

greet()           # Test the function



# Q.3 ---------------------------------------------------------------------

# Implement a fizzbuzz function. It takes a single number as input. 
# If the number is divisible by three, it returns “fizz”. 
# If it’s divisible by five it returns “buzz”. 
# If it’s divisible by three and five, it returns “fizzbuzz”. 
# Otherwise, it returns the number. 
# Make sure you first write working code before you create the function.

# Answer : 
fizz_or_buzz = function(a_number){
  
  # If number is divisible by both 3 and 5 return fizzbuzz
  if((a_number %% 3 == 0) && (a_number %% 5 == 0)){
    
    "fizzbuzz"
  
  # If number is divisible by both 3  return fizz
  }else if(a_number %% 3 == 0){
    
    "fizz"
  
  # If number is divisible by both 3  return buzz  
  }else if(a_number %% 5 == 0){
    
    "buzz"
  }else{
    # If number is not divisible by either 3  or 5 return the number 
    a_number
  }   

}

fizz_or_buzz(15)   # returns fizzbuzz
fizz_or_buzz(9)    # returns fizz
fizz_or_buzz(10)   # returns buzz
fizz_or_buzz(8)    # returns the number



# Q.4 ---------------------------------------------------------------------

# How could you use cut() to simplify this set of nested if-else statements?

if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}

temp_range = -50 : 50
# Answer : 
cut(x = temp_range,
    breaks = c(-50,0,10,20,30,50),include.lowest = TRUE,
    labels = c("freezing","cold","cool","warm","hot"))

# How would you change the call to cut() if I’d used < instead of <=? 
cut(x = temp_range,
    breaks = c(-50,1,9,19,29,50),include.lowest = TRUE,
    labels = c("freezing","cold","cool","warm","hot"))

# What is the other chief advantage of cut() for this problem? 
# (Hint: what happens if you have many values in temp?)
# Answer : cut() helps to categorize on a vector of values, while if() works only on one value 


# Q.5 ---------------------------------------------------------------------

# What happens if you use switch() with numeric values?
# Answer : As seen from below block of code, it throws errors

a = function(x,choices){
  
  switch(choices,
         1 = mean(x),
         2 = median(x),
         3 = sd(x)
         )
}



# Q.6 ---------------------------------------------------------------------
# What does this switch() call do? 
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

# Answer : The above switch call returns "ab" or "cd" depending on 'x' passed

# What happens if x is “e”?
x = "e"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)


# 19.5.1 Exercises --------------------------------------------------------


# Q.1 ---------------------------------------------------------------------

# What does commas(letters, collapse = "-") do? Why?
commas <- function(...) stringr::str_c(..., collapse = ", ")

#Answer :
# The above commas() function collapses any input character vector into one string where 
# each element is separated by comma as seen below
commas(letters)

# Where as commas(letters, collapse = "-") throws an error because ... is reserved for 
# data of character vectors. collapse = "-" can not be passed into str_c(...) 
commas(letters, collapse = "-")


# Q.2 ---------------------------------------------------------------------

# It’d be nice if you could supply multiple characters to the pad argument, 
# e.g. rule("Title", pad = "-+"). Why doesn’t this currently work? How could you fix it?

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

rule("Title", pad = "-+/")
# Answer : the above code is working, no idea why the question says that current code doesnt work



# Q.3 ---------------------------------------------------------------------

# What does the trim argument to mean() do? When might you use it?
# Answer : The trim argument removes certain fraction of arguments before computing the mean
# From the example given in the documentation, consider a vector x having values 0 to 10, and 
# an outlier value of 50. It can be seen that trim function is useful in cases where outliers
# are far away from the majority of observations in the data. 
x = c(0:10, 50)  # a vector considering outlier
xm = mean(x)     # returns 8.75
xm2 = mean(x, trim = 0.10) # returns 5.5 as outliers 0 and 50 are removed.




# Q.4 ---------------------------------------------------------------------

# The default value for the method argument to cor() is c("pearson", "kendall", "spearman"). 
# What does that mean? What value is used by default?
# Answer : From the documentation, the default value of method is "pearson".


x = c(1,5,10,15,20)
y = c(2,7,15,20,21)

cor(x,y,method = "pearson")   # 0.970279
cor(x,y,method = "kendall")   # 1
cor(x,y,method = "spearman")  # 1


