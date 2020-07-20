
# START -------------------------------------------------------------------

# import libraries
library(tidyverse)
library(purrr)

# 21.2.1 Exercises --------------------------------------------------------
# Q.1 ---------------------------------------------------------------------

#1. Write for loops to:

#1. Compute the mean of every column in mtcars.
# Answer
mt_col_mean = vector(mode = "double",length = ncol(mtcars)) # output
for(i in seq_along(mtcars)){                                # sequence 
  
  mt_col_mean[[i]] = mean(mtcars[[i]],na.rm = TRUE)         # body
}
mt_col_mean

#2. Determine the type of each column in nycflights13::flights.
df = nycflights13::flights
nyc_col_types = vector(mode = "character",length = ncol(df)) # output

for(i in seq_along(df)){                         # sequence  
    
  nyc_col_types[[i]] = typeof(df[[i]])           # body
  
}
nyc_col_types

#3. Compute the number of unique values in each column of iris.

iris_unique = vector(mode = "integer",length = ncol(iris))  # output
for(i in seq_along(iris)){                                  # sequence

  iris_unique[[i]] = length(unique(iris[[i]]))              # body
  
}
iris_unique

#4. Generate 10 random normals from distributions with means of -10, 0, 10, and 100.
rn_means = c(-10,0,10,100)
rn_output = list()   # output

for(i in seq_along(rn_means)){ # sequence
  
  rn_output[[i]] = rnorm(n = 10,mean = rn_means[[i]])
  
}

rn_output

# Q.2 ---------------------------------------------------------------------

# Eliminate the for loop in each of the following examples by
# taking advantage of an existing function that works with vectors:

#1
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
# Answer
out = paste0(letters,collapse = "")
out              #  returns "abcdefghijklmnopqrstuvwxyz"

#2.
x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))

# Answer :
sd(x)

#3.
x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}

# Answer:
cumsum(x)

# Q.3 ---------------------------------------------------------------------

# Combine your function writing and for loop skills:

# 1. Write a for loop that prints() the lyrics to the children’s song “Alice the camel”.

# Answer :
# Here is the original lyrics

'
Alice the Camel has one hump.
Alice the Camel has one hump.
Alice the Camel has one hump.
Go Alice go!

Ruby the Rabbit has two ears.
Ruby the Rabbit has two ears.
Ruby the Rabbit has two ears.
Go Ruby go!
 
Sally the Sloth has three toes.
Sally the Sloth has three toes.
Sally the Sloth has three toes.
Go Sally go!

Felix the Fox has four legs.
Felix the Fox has four legs.
Felix the Fox has four legs.
Go Felix go!

Lilly the Ladybug has five spots.
Lilly the Ladybug has five spots.
Lilly the Ladybug has five spots.
Go Lilly go!

Andy the Ant has six legs.
Andy the Ant has six legs.
Andy the Ant has six legs.
Go Andy go!

Larry the Lizard has seven stripes.
Larry the Lizard has seven stripes.
Larry the Lizard has seven stripes.
Go Larry go!

Sammy the Spider has eight legs.
Sammy the Spider has eight legs.
Sammy the Spider has eight legs.
Go Sammy go!

'


# Introduce the characters and adding them attributes
characters = c("Alice","Ruby","Sally","Felix","Lilly","Andy","Larry","Sammy")
animal =  c("Camel","Rabbit","Sloth","Fox","Ladybug","Ant","Lizard","Spider")
body_part_num =  c(1,2,3,4,5,6,7,8)
body_part_name <-  c("hump","ears","toes","legs","spots","legs","stripes","legs")

# A function to get prose for printing
get_prose_to_print = function(i){
  
  # get all details
  char_name = characters[i]          
  animal_name = animal[i]
  animal_body_part_num = body_part_num[i]
  animal_body_part_name = body_part_name[i]

  for( j in 1 : 4 ){  # Since we need 4 rows for each animal
       
    # for the fourth iteration, where we need Go **** go!
    if( j == 4 ){
      
      print(paste("Go",char_name,"go!"))
      
    }else{  # for iteration 1 to 3, get the sentence as needed
      
      print(paste(char_name,"the",animal_name,"has",animal_body_part_num,paste0(animal_body_part_name,".")))
      
    } 
    
  } # j loop ends
  
  
}

# Actual printing loop
for (i in seq_along(characters)){ # loop over characters
  
  get_prose_to_print(i)  # Get 4 rows for each animals
  print("")              # Get a break  
  
  } 



# 2. Convert the nursery rhyme “ten in the bed” to a function. 
# Generalise it to any number of people in any sleeping structure.

# answer:
# Function 1 : Parent function for  printing the song
print_N_in_bed = function(number_of_people){
  
  sequence_to_loop = rev(seq(1 : number_of_people))
  
  for(i in sequence_to_loop) {  # sequence
    
    get_text_to_print(i)        # body
    print("")                   # break
    
  }
}


# Function 2 : Get each line of text for all iterations 
get_text_to_print = function(i){  
  
  if(i == 1){
    
    string_to_print_last(i)
    
  }else{
    
    string_to_print_all(i)
    
  }

}
# Function 3 : Get each line of text for all iterations except last
string_to_print_all = function(i){
  
 for( j in seq(1,4)){
   
   if(j == 1){
     
     print(paste("There were",i,"in the bed"))
     
   }else if (j == 2){
     
     print("And the little one said,")
     
   }else if (j == 3){
     
     print("Roll over! Roll over!")
     
   }else{
     
     print("So they all rolled over and one fell out")
   }
   
   
 }

}


# Funcion 4 : Get each line of text for last iteration 
string_to_print_last = function(i) {
  
  for(j in seq(1,3)) {
    
    if(j == 1){
      
      print(paste("There were",i,"in the bed"))
      
    }else if (j == 2) {
      
      print("And the little one said,")
      
    }else {
      
      print("Alone at last!")
      
    }
  }
}

# Generalized code
number_of_people = 10
print_N_in_bed(number_of_people)


# 3. Convert the song “99 bottles of beer on the wall” to a function. 
# Generalise to any number of any vessel containing any liquid on any surface.

# Answer:

print_N_beer_bottles = function(number_of_bottles) {
  
  # range of bottles
  number_of_bottles_seq = seq(number_of_bottles,0)
  
  for(i in number_of_bottles_seq){
    
    get_text_for_beer_bottles(i,number_of_bottles)
    print("")
  }
}

get_text_for_beer_bottles = function(i,number_of_bottles) {
  
  if(i == 0) {
    
    print("No more bottles of beer on the wall, no more bottles of beer.")
    print(paste("Go to the store and buy some more, ",number_of_bottles ,"bottles of beer on the wall."))
    
  } else if(i == 1) {
    
    print(paste(i,"bottles of beer on the wall, ",i,"bottles of beer."))
    print("Take one down and pass it around, no more bottles of beer on the wall.")
    
  } else {
    
    print(paste(i,"bottles of beer on the wall, ",i,"bottles of beer."))
    print(paste("Take one down and pass it around, ",i - 1," bottles of beer on the wall."))
  }
  
}

number_of_bottles = 10
print_N_beer_bottles(number_of_bottles) 

# Q.4 ---------------------------------------------------------------------

# It’s common to see for loops that don’t preallocate the output and instead increase the length of a vector at each step:

output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output

# How does this affect performance? Design and execute an experiment.

# Answer

# Function 1 : Find average execution time without pre-allocating any output
get_average_time_wo_pre_allocation = function(number_of_runs){
  
  x = nycflights13::flights                                             # the iris dataset
  all_exec_time = vector(mode = "numeric",length = 100) # variable for experimentation
  
  # loop over 100 iterations
  for(j in seq(1,number_of_runs)){
    
    # Code for finding something without pre-allocation
    t1 = Sys.time()  # start measuring time
    
    output <- vector("integer", 0)
    for (i in seq_along(x)) {
      output <- c(output, length(x[[i]]))
    } # i loop ends
    output
    
    t2 = Sys.time() # end measuring time
    
    all_exec_time[[j]] = t2 - t1 # execution time
    
  }# j loop ends
  
  average_exec_time = mean(all_exec_time) # average execution time
  
}


# Function 2 : Find average execution time With pre-allocating an output
get_average_time_with_pre_allocation = function(number_of_runs){
  
  x = nycflights13::flights                                             # the iris dataset
  all_exec_time = vector(mode = "double",length = number_of_runs) # variable for experimentation
  
  # loop over 100 iterations
  for(j in seq(1,number_of_runs)){
    
    t1 = Sys.time()        # start measuring time
    output <- vector("integer", ncol(x))  # output vector with proper pre allocation
    
    # code for proper pre-allocation
    for (i in seq_along(x)) {
      
      output[[i]] = length(x[[i]])
      
    }
    output
    t2 = Sys.time() # end measuring time
    
    all_exec_time[[j]] = t2 - t1 # execution time
    
  }# i loop ends
  
  average_exec_time = mean(all_exec_time) # average execution time
  
}

# Experimentation results show that pre-allocation is always useful
number_of_runs = 1000
time_wo_pa = get_average_time_wo_pre_allocation(number_of_runs)  # without proper pre-allocation
time_w_pa = get_average_time_with_pre_allocation(number_of_runs) # with proper pre-allocation

if(time_wo_pa > time_w_pa){
  
  print("Pre-allocation of output vector is useful")
  
}else{
  
  print("Pre-allocation of output vector is NOT useful")
}



# 21.3.5 ------------------------------------------------------------------



# Q.1 ---------------------------------------------------------------------
# Imagine you have a directory full of CSV files that you want to read in. 
# You have their paths in a vector, 
# files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), 
# and now want to read each one with read_csv(). 
# Write the for loop that will load them into a single data frame.

# Answer :
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)  # read in files

df_list = vector(mode = "list",length = length(files))         # object to store output 

for(i in seq_along(files)){                                    # sequence   
  
  df_list[[i]] = read.csv(files[i])                            # body   
  
}

combined_df = dplyr::bind_rows(df_list)                        # combined all objects into a DF
combined_df

# Q.2 ---------------------------------------------------------------------
# What happens if you use for (nm in names(x)) and x has no names? 
# Answer :
a_vec_no_names = c("a","b","c")                 # vector with no names 
for(nm in names(a_vec_no_names)) print(nm)      # nothing happens, loop is executed with no result

# What if only some of the elements are named? 
a_vec_with_some_names = c(A = "a","b", C = "c","d")   # vector with no names 
for(nm in names(a_vec_with_some_names)) print(nm)     # all elements of names() vector are returned

# What if the names are not unique?
a_vec_with_repeat_names = c(A = "a",X = "b", C = "c",X = "d") # vector with repeated names
for(nm in names(a_vec_with_repeat_names)) print(nm)           # all elements of names() vector are returned


# Q.3 ---------------------------------------------------------------------
# Write a function that prints the mean of each numeric column in a data frame,
# along with its name. For example, show_mean(iris) would print:

show_mean(iris)
#> Sepal.Length: 5.84
#> Sepal.Width:  3.06
#> Petal.Length: 3.76
#> Petal.Width:  1.20

# Answer 
show_mean = function(a_df){
  
  df_col_names = colnames(a_df)               # get colnames
  
  for(i in seq_along(a_df)){                  # sequence
    
    col_name = df_col_names[[i]]              # get column name
    
    if(is.numeric(a_df[[col_name]])){                             # check if mean is not NA
      
      col_mean = mean(a_df[,col_name])                # get column mean   
      cat(paste(col_name, ": ",col_mean,"\n"))           # print necessary data
      
    }
  }
  
}

show_mean(iris)

# Q.4 ---------------------------------------------------------------------
# What does this code do? How does it work?

# Answer:
trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)

for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])  
}

# Step 1 : Creates a list called "trans" with elements disp and am which are 2 functions of variable x
# Step 2 : # Loop over elements in "trans" and modifies the column with same name in mtcars using function in the list

# it works in following 4 steps as follows:
for (var in names(trans)) {
  
  called_function = trans[[var]]               # get the function from list
  on_object = mtcars[[var]]                    # get the object to work upon from DF
  modified_result = called_function(on_object) # modify the object by applying function
  mtcars[[var]] = modified_result              # modufy the DF
}


# 21.4.1 ------------------------------------------------------------------


# Q.1 ---------------------------------------------------------------------
# Read the documentation for apply().
# In the 2d case, what two for loops does it generalise?

# Answer:  
?apply
# In 2D case, it applies for loops over columns and rows for applying any function

# Q.2 ---------------------------------------------------------------------
# Adapt col_summary() so that it only applies to numeric columns 
# You might want to start with an is_numeric() function that returns a 
# logical vector that has a TRUE corresponding to each numeric column.

col_summary = function(df){
  
  # Output summary function
  summaries = vector(mode = "list",length = ncol(df))
  
  # loop over each column
  for(i in seq_along(df)){
    
    if(is.numeric(df[[i]])){          # if column is numeric
      
      summaries[[i]] = summary(df[[i]])  # get summary
      
    }else{                            # if column is not numeric
      
      summaries[[i]] = NA                # get NA
    }
    
  }
  
  return(summaries)
}

col_summary(iris) # Call the function


# 21.5.3 ------------------------------------------------------------------


# Q.1 ---------------------------------------------------------------------
# Write code that uses one of the map functions to:

# Compute the mean of every column in mtcars.
# Answer:
mtcars %>% map_dbl(mean)

# Determine the type of each column in nycflights13::flights.
# Answer:
nycflights13::flights %>% map(typeof)

# Compute the number of unique values in each column of iris.
iris %>% map(unique) %>% map(length)

# Generate 10 random normals from distributions 
# with means of -10, 0, 10, and 100.
means_vec = c(-10, 0,10,100)
map(.x = means_vec,.f = function(x) rnorm(n = 10,mean = x))


# Q.2 ---------------------------------------------------------------------

# How can you create a single vector that for each column in 
# a data frame indicates whether or not it’s a factor?
map_lgl(.x = a_df,.f = function(x) is.factor(x))   # a_df is DF of your choice



# Q.3 ---------------------------------------------------------------------

# What happens when you use the map functions on vectors that aren’t lists? 
# Answer : Except map() function which returns list as output, 
# other map_* funtions return vectors as output

x_vec = 1:5                               # input vector
map(.x = x_vec,.f = function(x) x^2)      # returns list
map_dbl(.x = x_vec,.f = function(x) x^2)  # returns vector

# What does map(1:5, runif) do? Why?
map(1:5, runif)            
# Answer:
# This function returns a list with random numbers between 0 and 1
# with vectors of length 1 to 5 as map function applies runif function
# on each element of the vector


# Q.4 ---------------------------------------------------------------------

# What does map(-2:2, rnorm, n = 5) do? Why? 
map(-2:2, rnorm, n = 5)

# Answer:
# This function works on vector (-2,-1,0,1,2) and applies rnorm function
# to each of its element. The n argument to rnorm is 5.
# Hence each elements output is a vector of length 5

# What does map_dbl(-2:2, rnorm, n = 5) do? Why?
map_dbl(-2:2, rnorm, n = 5)

# Answer :
# The above code throws an error :
# Error: Result 1 must be a single double, not a double vector of length 5  Run `rlang::last_error()` to see where the error occurred.
# This error is due to the fact that map_dbl() function returns 1 vector as a result
# Applying rnorm(n = 5) produces a vector of length 5, thus total 5 vectors are created for input
# map_dbl() cannot handle 5 vectors as an output, hence it throws error.
# Below code works:
map_dbl(-2:2, rnorm, n = 1)   # n = 1 , not 5

# Q.5 ---------------------------------------------------------------------

# Rewrite map(x, function(df) lm(mpg ~ wt, data = df)) 
# to eliminate the anonymous function.
# Answer : ~ sign can be used to eliminate the function() argument
mtcars %>% split(.$cyl) %>% map(~lm(mpg ~ wt, data = .))

# Other way is to define a function and then use the map function 
lin_reg = function(df){
  
  lm(formula = mpg ~ wt,data =df )
  
  
}

mtcars %>% split(.$cyl) %>% map(.f = lin_reg)



# 21.9.3 --------------------------------------------------------------------
  

# Q.1 ---------------------------------------------------------------------
# Implement your own version of every() using a for loop. Compare it with purrr::every(). 

# Answer 
my_every = function(a_list_or_vec, some_function){                       # my function
  
  my_every_op = vector(mode = "list",length = length(a_list_or_vec))     # output vector
  
  for(i in seq_along(a_list)){                                           # sequence
    
    my_every_op[[i]] = some_function(a_list_or_vec[[i]])                 # body   
    
  }
  
  result = all(my_every_op %>% unlist())                                 # check if all conditions are evaluated to TRUE
  return(result)
}  

# What does purrr’s version do that your version doesn’t?
# Answer : 
# purrr::every() has ... option where other arguments to the predicate can be passed which my function does not have 
# This can be seen below : is_bare_character() has an extra argument "n" which can be passed in purrr::every()
# but not in my function.

test = c("A","B")
purrr::every(.x = test,.p = is_bare_character, n = 2)


# Q.2 ---------------------------------------------------------------------
# Create an enhanced col_summary() that applies a summary function to every numeric column in a data frame.


col_summary_enhanced = function(a_df){
  
  a_df %>%                                      # input df   
    keep(is.numeric) %>%                        # only keep the numeric column
    map(summary)                                # use map function to apply summary function to each column
  
}

col_summary_enhanced(iris)

# Q.3 ---------------------------------------------------------------------
# A possible base R equivalent of col_summary() is:

col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}

# But it has a number of bugs as illustrated with the following inputs:

df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)


col_sum3(df, mean)      # OK
col_sum3(df[1:2], mean) # Has problems: don't always return numeric vector
col_sum3(df[1], mean)   # returns an empty list if the first column is not numeric  
col_sum3(df[0], mean)   # returns error

# What causes the bugs?
# Answer : 
# There's no if else logic to evaluate if an empty DF is being passed. Hence the last line of code breaks