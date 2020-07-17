#----------------------------------------------- START--------------------------------------------------

# install and load libraries
install.packages("magrittr")
library("magrittr")

# In this chapter we study about pipes
# Apart from the normal pipe operator " %>% ", we are introduced to 3 more new types of pipes
# available in the magrittr package


# -------------------------------------1. The T pipe " %T>% " ------------------------------------------
# This pipe returns the left side of the operation as a result ,
# unlike the normal pipe " %>% " which returns the right side of the operation as a result
# It is useful when your operation ends up using functions which do not return anythins
# eg. plot() in the below case, but you still need to operate upon the data in the flow

rnorm(100) %>%                  # create a vector with normally distributed random variables and
  matrix(ncol = 2) %T>%         # convert it to a matrix. Then, use The T pipe to, 
  plot() %>%                    # plot() a graph. Even though plot () does not return any object
  str()                         # the T pipe returns the matrix (left side of the operation) whose str() can be found out


# ------------------------------------2. The dollar pipe " %$% "----------------------------------------
# This pipe "explodes" the a dataframe into its columns
# It is mainly useful in base functions, which don't have an API for using dataframes directly
# eg. In below case, cor() does not have an API for dataframe,

mtcars %$%                      # the dollar pipe facilitates passing the 
  cor(disp,mpg)                 # disp and mpg columns directly to the cor() function
                                # easy shorthand for cor(mtcars$disp,mtcars$mpg)      



# ------------------------------------3. The assignment pipe " %<>% "----------------------------------------
# This pipe is useful for assignment of a result without explicitly using the assign operator
# eg. In below case, assignment operator is internally assigning to mtcars the new value post transformation

mtcars %<>% 
  transform(cyl = cyl * 3)      # Basically a shorthand for mtcars <- mtcars %>% transform(cyl = cyl * 3)

#----------------------------------------------- END--------------------------------------------------