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
