library(tidyverse)
library(modelr)


# 23.2.1 Exercises ------------------------------------------------------------------

# 1. One downside of the linear model is that it is sensitive to unusual values 
# because the distance incorporates a squared term. 

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

# Fit a linear model to the simulated data below, and visualise the results. 
mod_res = lm(formula = y ~ x,data = sim1a)                   # using the lm function to get the intercept and slope
int0 = as.vector(coef(mod_res)[1])                           # intercept
sl0 = as.vector(coef(mod_res)[2])                             # slope

# draw the model and the data
ggplot(data = sim1a,mapping = aes(x,y)) + 
  geom_point() +
  geom_abline(aes(intercept = int0, slope = sl0))

# Rerun a few times to generate different simulated datasets. 
# What do you notice about the model?

# Answer 
# simulating new data and visualizing the  model for couple of times, tells us that the best fit model,
# is heavily dependent on outlier values and tend to move towards the values and away from the most of the data.



# 2. One way to make linear models more robust is to use a different distance measure.
# For example, instead of root-mean-squared distance, you could use mean-absolute distance:

# Answer :

model1 <- function(a, data) {           # Arbitrarily defined model
  a[1] + data$x * a[2]
}

measure_distance <- function(mod, data) {   # RMSE replaced by MAD 
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}

# Use optim() to fit this model to the simulated data above and compare it to the linear model.
# Finding a good model using optim function
good_model1 <- optim(c(0,0),measure_distance,data = sim1a)
int1 <- good_model1$par[1]               # new intercept
sl1 <- good_model1$par[2]                # new slope     

colors = c("RMSE" = "red","MAD" = "blue")  # A vector to store color for different plotting method

# Plotting RMSE and MAD together
ggplot(data = sim1a,mapping = aes(x,y)) + 
  geom_point() +
  geom_abline(aes(intercept = int0, slope = sl0, color = "RMSE")) +
  geom_abline(aes(intercept = int1, slope = sl1, color = "MAD" )) +
  labs(color = "Legend") +
  scale_color_manual(values = colors)

# Answer : MAD seems to be better statistic than RMSE being used in LM function


# 3. One challenge with performing numerical optimisation is that it’s only guaranteed to find one local optimum. 
# What’s the problem with optimising a three parameter model like this?

model1 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}

# Answer : 
good_model2 <- optim(c(0,0),measure_distance,data = sim1a)      # error
# If we use optim function on the above model, it throws an error : 
# Error in optim(c(0, 0), measure_distance, data = sim1a) :  function cannot be evaluated at initial parameters
# This is because only 2 initialization points c(0,0) is provided
# It is possible to get a solution for this by providing 3 initialization points c(0,0,0) as below
good_model2 <- optim(c(0,0,0),measure_distance,data = sim1a)    # success



# 23.3.1 Predictions ------------------------------------------------------



# 1. create a grid that covers where our data lies
sim1_mod = lm(formula = y~x, data = sim1)        # The model for testing  
grid1 = sim1 %>% modelr::data_grid(x)            # Add the independent variable (sim1$x in this case)
grid1 = grid1 %>% add_predictions(sim1_mod)      # Add predictions 
sim1 = sim1 %>% add_residuals(sim1_mod)          # Add residuals

# 2. Visualize the model
ggplot(sim1, aes(x,y)) +
  geom_point() +
  geom_line(mapping = aes(y = pred), data = grid1, color = "red", size = 1)



ggplot(data = sim1,mapping = aes(resid)) +
  geom_freqpoly(binwidth = 0.5)


ggplot(data = sim1, mapping = aes(x,resid)) +
  geom_ref_line(h = 0) +
  geom_point()




# Q.1 ---------------------------------------------------------------------

# Instead of using lm() to fit a straight line, you can use loess() to fit a smooth curve. 
# Repeat the process of model fitting, grid generation, predictions, and visualisation on sim1 using loess() instead of lm(). 
# How does the result compare to geom_smooth()?

  
sim1_mod_2 = loess(formula = y ~ x, data = sim1)        # Model fitting
grid2 = sim1 %>% modelr::data_grid(x)                   # Independent variable
grid2 = grid2 %>% modelr::add_predictions(sim1_mod_2)   # Adding predictions

# Visualization
ggplot(data = sim1, mapping = aes(x,y)) +
  geom_point() +
  geom_smooth(mapping = aes(y = pred), data = grid2, color = "red") 

# Answer : It seems geom_smooth doesnt fit a straight line but a more "adjusted / smoothed line"

# Q.2 ---------------------------------------------------------------------

# add_predictions() is paired with gather_predictions() and spread_predictions().
# How do these three functions differ?
  
# Answer:
# add_predictions() adds a new column "pred" to the data as it can take only single model.
# But gather_predictions() and spread_predictions() can take  multiple models and hence 
# can compare predictions of different models

grid3 = grid %>% modelr::gather_predictions(sim1_mod,sim1_mod_2)   # Adds predictions vertically (long)
grid4 = grid %>% modelr::spread_predictions(sim1_mod,sim1_mod_2)   # Adds predictions horizontally (broad)

# Q.3 ---------------------------------------------------------------------

# What does geom_ref_line() do?
# What package does it come from? 
# Why is displaying a reference line in plots showing residuals useful and important?

# Answer:
# geom_ref_line() adds a reference line to the plot created using ggplot2 package
# It comes from modelr package
# Showing a reference line in plots showing residual helps us to visually check if residuals are
# random and that they don't exhibit a pattern and are not concentrated in some region 
  
# Q.4 ---------------------------------------------------------------------

# Why might you want to look at a frequency polygon of absolute residuals? 
# What are the pros and cons compared to looking at the raw residuals?

# Answer :

sim1 %>% 
  add_residuals(sim1_mod) %>% 
  mutate(resid = abs(resid)) %>% 
  ggplot(mapping = aes(x)) +
  geom_freqpoly(mapping = aes(resid),binwidth = 0.25)    # frequency polygon of absolute residuals


# PROS : 
# frequency polygon of absolute residuals helps us to visualize the "magnitude" of error of the model
# If the distribution is skewed towards right, we can say that the magnitude of error by model is low, as model made small mistakes.
# If the distribution is skewed towards left, we can say that the magnitude of error by model is high, as model made large mistakes.

# CONS :
# frequency polygon of absolute residuals does not help us to visualize if the model is over-predicting / under - predicting / captures signal in the data properly.
# If we visualize frequency polygon of residuals, we can see if the model is capturing signal in data properly of the residuals are nomally distributed
# If the residuals are skewed in any one direction, we can say that the model is biased.
sim1 %>% 
  add_residuals(sim1_mod) %>% 
  ggplot(mapping = aes(x)) +
  geom_freqpoly(mapping = aes(resid),binwidth = 0.5)   # frequency polygon of residuals





df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)

model_matrix(df, y ~ x1 + x2)

View(sim2)
ggplot(data = sim2,mapping = aes(x,y)) +
  geom_point()

mod2 = lm(formula = y ~ x, data = sim2)

grid <- sim2 %>% 
          data_grid(x) %>% 
          add_predictions(mod2)


ggplot(data = sim2,mapping = aes(x,y)) +
  geom_point() +
  geom_point(mapping = aes(y = pred),data = grid,color = "red",size = 4)


tibble(x = "d") %>% 
  add_predictions(mod2)





mod1 = lm(formula = y ~ x1 + x2, data = sim3) # independent evaluation
mod2 = lm(formula = y ~ x1 * x2, data = sim3) # interaction evaluation


grid1 = sim3 %>% 
          data_grid(x1, x2) %>% 
          gather_predictions(mod1, mod2)


ggplot(data = sim3,mapping = aes(x = x1,y = y,color = x2)) +
  geom_point() +
  geom_line(data = grid1, mapping = aes(y = pred)) +
  facet_wrap(~ model)


sim3 = sim3 %>% 
          gather_residuals(mod1,mod2)


ggplot(sim3, aes(x = x1,y =  resid,color = x2)) +
  geom_point() +
  facet_grid( model ~ x2)


sim4


mod1 = lm(formula =  y ~ x1 + x2, data = sim4)  # Independent evaluation
mod2 = lm(formula =  y ~ x1 * x2, data = sim4)  # Interactive evaluation

grid = sim4 %>% 
        data_grid(
          x1 = seq_range(x1, 5),
          x2 = seq_range(x2, 5)
        ) %>% 
      gather_predictions(mod1,mod2)


x1 = rcauchy(100)

seq_range(x1, n = 5, trim = 0.5)


x2 = c(0,1)

seq_range(x = x2,n = 5,expand = 0.5)


  ggplot(data = grid, mapping = aes(x1,x2)) +
    geom_tile(mapping = aes(fill = pred)) +
    facet_wrap(~ model)
  
  
  
ggplot(data = grid, mapping = aes(x = x1, y = pred,color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)


ggplot(data = grid, mapping = aes(x = x2, y = pred,color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)



df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)

mod_1 = y ~ x^2 + x
mod_2 = y ~ I(x^2) + x


model_matrix(df,mod_1)
model_matrix(df,mod_2)


model_matrix(df, y ~ poly(x = x,degree = 2))
library(splines)
model_matrix(data = df, formula = y ~ ns(x,2))

sim5 = tibble(
  x = seq(from = 0,to =  3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

library(help = "splines")
ggplot(data = sim5, mapping = aes(x,y)) +
  geom_point()


mod1 = lm(formula = y ~ ns(x = x, df = 1),data = sim5)
mod2 = lm(formula = y ~ ns(x = x, df = 2),data = sim5)
mod3 = lm(formula = y ~ ns(x = x, df = 3),data = sim5)
mod4 = lm(formula = y ~ ns(x = x, df = 4),data = sim5)
mod5 = lm(formula = y ~ ns(x = x, df = 5),data = sim5)


seq_range(x, n = 50, expand = 0.1)

grid = sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%  
  gather_predictions(mod1,mod2,mod3,mod4,mod5,.pred = "y")


ggplot(data = sim5,mapping = aes(x,y)) +
  geom_point() +
  geom_line(data = grid,color = "red") + 
  facet_wrap(~model)




# 23.4.5 Exercises -------------------------------------------------------------------------

# Q.1 ---------------------------------------------------------------------
# What happens if you repeat the analysis of sim2 using a model without an intercept.

# Answer  : 
mod2 = lm(formula = y ~ x,data = sim2)         # model with intercept
grid  = sim2 %>%                               # Creating the grid
  data_grid(x) %>% 
  add_predictions(mod2)

mod2_wi = lm(formula = y ~ x - 1, data = sim2)         # model without intercept
grid_wi  = sim2 %>%                                    # Creating the grid                               
  data_grid(x) %>% 
  add_predictions(mod2_wi)

# What happens to the model equation? 
# Answer : Model equation for model without intercept does not have intercept column    
model_matrix(data = sim2,formula = y ~ x)        # Has an intercept column
model_matrix(data = sim2,formula = y ~ x - 1)    # Does not have an intercept column

# What happens to the predictions?
# Visualizing both models show that predictions remain same.
ggplot(data = sim2,mapping = aes(x,y)) +
  geom_point() +
  geom_point(mapping = aes(y = pred), data = grid, color = "red", size = 4) 


ggplot(data = sim2,mapping = aes(x,y)) +
  geom_point() +
  geom_point(mapping = aes(y = pred), data = grid_wi, color = "blue", size = 4)

# Q.2 ---------------------------------------------------------------------
# Use model_matrix() to explore the equations generated for the models that  I fit to sim3 and sim4. 
# Why is * a good shorthand for interaction?

# Answer : On sim3 dataset
model_matrix(data = sim3,formula = y ~ x1 + x2)  # returns independent evaluation of variables : Intercept, x1, x2b, x2c, x2d (as x2 is categorical) 
model_matrix(data = sim3,formula = y ~ x1 * x2)  # returns interactive evaluation of variables : Other than the independent variables above, x1:x2b, x1:x2c, x1:x2d 

# Answer : On sim4 dataset
model_matrix(data = sim4,formula = y ~ x1 + x2)  # returns independent evaluation of variables : x1, x2 
model_matrix(data = sim4,formula = y ~ x1 * x2)  # returns interactive evaluation : Other than independent variables above : include x1:x2


# * is good shorthand for interaction because it saves time to write long formulae
# for eg. y ~ x1 * x2 is a shorthand for y = a0 + a1 * x + a2 * x + a12 * x1x2

# Q.3 ---------------------------------------------------------------------
# Using the basic principles, convert the formulas in the following two models into functions. 
# (Hint: start by converting the categorical variable into 0-1 variables.)

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

# Answer creating one function for both forumlae
get_2_var_linear_model = function(dataset = "sim3",interaction ="no"){

    # converting categorical columns to 0-1 variables (One hot encoding)
  
    unique_x2 = unique(sim3$x2)                 # find each unique element in the vector
  
  
    for(each in unique_x2){                     # loop over each unique variable 
    
      new_column = ifelse(sim3$x2 == each,1,0)  # create a new variable with 0 1 values
      sim3[paste0("x2",each)] = new_column      # create a new column in DF
    }
  
  
  # create prediction based on interaction
  
    if(interaction == "no"){                    # If interaction is not needed
    
      mm = sim3 %>% 
        mutate("Intercept" = 1) %>%             # Create intercept with defalt value of 1
        dplyr::select("Intercept","x1","x2b","x2c","x2d")  # select only independent variables
      
    
    }else{                                      # If interaction is yes
    
      mm = sim3 %>%  
        mutate("Intercept" = 1,                 # create default intercept value = 1
               "x1:x2b" = x1 * x2b,             # interaction of x1 with x2b 
               "x1:x2c" = x1 * x2c,             # interaction of x1 with x2c
               "x1:x2d" = x1 * x2d) %>%         # interaction of x1 with x2d
        dplyr::select("Intercept","x1","x2b","x2c","x2d","x1:x2b","x1:x2c","x1:x2d")  # select interactive variables
    
    }
    
    return(mm)
}

# Answer 
get_2_var_linear_model(sim3,"no")     # Interaction == no : output similar to mod1
get_2_var_linear_model(sim3,"yes")     # Interaction == no : output similar to mod2



# Q.4 ---------------------------------------------------------------------
# For sim4, which of mod1 and mod2 is better?
# I think mod2 does a slightly better job at removing patterns, but it’s pretty subtle. 
# Can you come up with a plot to support my claim?

# Answer
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

# Creating a grid of residuals
grid = gather_residuals(data = sim4,mod1,mod2)

# If we do a group by on grid4, we can prove numerically that 
# 1. the sum of all residuals of mod2 is less than mod1
# 2. the mean of all residuals of mod2 is less than mod1 
# 3. RMSE of mod2 is less than that of mod1 
grid %>% 
  group_by(model) %>% 
  summarize(mean_resid = mean(resid),
            sum_resid = sum(resid),
            rmse_resid = sqrt(mean(resid^2)))

