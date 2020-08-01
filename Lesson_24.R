library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)



# 24.2.3 Exercises ------------------------------------------------------------------

# Q.1 -----------------------------------------------------------------------
# In the plot of lcarat vs. lprice, there are some bright vertical strips. 
# What do they represent?

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

plot_a <-  ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

plot_a
# Answer :
# The bright vertical lines capture the variation in price for diamonds having the same carat value
# It tells that there are many diamonds having the same carat value, but different prices.
# The reason for these different prices is that there are other variables like cut, color and clarity that affect the prices.
# If we use a model that captures these variables other than carat and check the hex plot
# We should see shorter bright vertical strips than getting shown in simply by plotting lcarat vs lprice
# This can be seen below.

mod_diamond2 <-  lm(lprice ~ lcarat + color + cut + clarity,data = diamonds2)

grid <- diamonds2 %>% 
          add_predictions(model = mod_diamond2,var = "lprice2") 

plot_b <- ggplot(data = grid,mapping = aes(x = lcarat,y = lprice2)) +
  geom_hex(bins = 50)

library("gridExtra")
grid.arrange(plot_a,plot_b,ncol = 2)

# Q.2 -----------------------------------------------------------------------
# If log(price) = a_0 + a_1 * log(carat), 
# what does that say about the relationship between price and carat?

mod1 = lm(log(price) ~ log(carat),data = diamonds2)


grid1 = diamonds2 %>% 
          data_grid(carat,price) %>% 
          mutate(log_price = log(price),
                 log_carat = log(carat)) %>% 
          add_predictions(model = mod1,var = "pred_log_price")

#This model shows that if carat increases by some amount, price will increase exponentially
ggplot(data = grid1,mapping = aes(x = log_carat,y = pred_log_price)) +
  geom_hex(bins = 40)



# Q.3 -----------------------------------------------------------------------
# Extract the diamonds that have very high and very low residuals. 
# Is there anything unusual about these diamonds? 
# Are they particularly bad or good, or do you think these are pricing errors?


# Answer : The below table shows that diamonds with high residuals are pricing errors as diamonds with mostly
# lowest quality of cut (Fair), medium quality of color (F), lowest quality of clarity(SI2) are highly priced

diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2") %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)


# Q.4 -----------------------------------------------------------------------
# Does the final model, mod_diamond2, do a good job of predicting diamond prices? 
# Would you trust it to tell you how much to spend if you were buying a diamond?

# Answer : There needs to be some metric that should decide if the model is good or bad
# One such metric is RMSE which we see is very less as compare to the range of prices
range(diamonds$price)                                       # range of price in diamonds dataset
rmse(model = mod_diamond2,data = diamonds2)                 # rmse of the model   


