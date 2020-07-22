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