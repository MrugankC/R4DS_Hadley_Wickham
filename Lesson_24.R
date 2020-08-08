library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)
library(splines)


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
mae(model = mod_diamond2,data = diamonds2)



# 24.3.5 Exercises ------------------------------------------------------------------


# Q.1 ---------------------------------------------------------------------
# Use your Google sleuthing skills to brainstorm why there were fewer than expected flights on Jan 20, May 26, and Sep 1. 
# (Hint: they all have the same explanation.) How would these days generalise to another year?

# Answer :
# Creating the data

daily = flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarize(n = n()) %>% 
  mutate(wday = wday(x = date,label = TRUE))
  

# creating the model : number of flights is dependent on weekday
mod = lm(formula = n ~ wday,data = daily)

# Adding residuals
daily = daily %>% 
  add_residuals(mod)

# Filtering on the given date
q1_dates = lubridate::as_date(c("2013-01-20","2013-05-26","2013-09-01"))

# Getting residuals of the given three dates. As we can see it is more than 100 for every day
q1_df = daily %>% 
          filter(date %in% q1_dates)

# All these dates were Sundays which were preceding Mondays which were national holidays in the US
# Hence people did not travel much on these days due to long weekend

# Q.2 ---------------------------------------------------------------------
# What do the three days with high positive residuals represent? 
# How would these days generalise to another year?
daily %>% 
  top_n(3, resid)

# Answer :
# Top 3 positive residuals means there were higher number of  actual flights than predicted flights
# These 3 dates 30th November (Saturday), 1st December(Sunday), 28th December(Saturday)
# had higher than expected traffic because they may denote weekends that beginning of the holiday season


# Q.3 ---------------------------------------------------------------------
# Create a new variable that splits the wday variable into terms, but only for Saturdays, 
# i.e. it should have Thurs, Fri, but Sat-summer, Sat-spring, Sat-fall.
# How does this model compare with the model with every combination of wday and term?

# creating a function where term is calculated only for week day
sat_term = function(data,sat_symbol){
  
  # Creating the terms
  data$term = cut(x = data$date,
                  breaks = ymd(20130101, 20130605, 20130825, 20140101),
                  labels = c("spring","summmer","fall"))
  
  # Converting factor to character
  data$wday  = as.character(data$wday)
  
  # Creating term for saturday and leaving other Weekdays as it is 
  data$sat_term_n_wday = ifelse(data$wday == sat_symbol,
                         paste(data$wday,data$term,sep = "_"),
                         data$wday)
  
  return(data)
}

# Creating the new column for term + saturday
daily <- sat_term(daily,"Sat")

mod1 = lm(formula = n ~ wday + term,data = daily)  # model 1 : independent evaluation of term and week day
mod2 = lm(formula = n ~ wday * term,data = daily)  # model 2 : interactive evaluation of term and weekday 
mod3 = lm(formula = n ~ sat_term_n_wday,data = daily) # model 3: evaluation of sat_term _ wday

# Creating a results comparison table
results <- tibble(
  
  mod_names = c("mod1","mod2","mod3"),
  RMSE = c(rmse(mod1,daily),rmse(mod2,daily),rmse(mod3,daily)),
  MAE = c(mae(mod1,daily),mae(mod2,daily),mae(mod3,daily))
  
 
)

# Answer :
# As we can see the new model (mod3) performs worse than other 2 models


# Q.4 ---------------------------------------------------------------------
# Create a new wday variable that combines the day of week, term (for Saturdays), and public holidays. 
# What do the residuals of that model look like?

# Answer : 
# Getting list of 2013 US public holidays from google
public_holidays = lubridate::as_date(c("2013-01-01","2013-01-21","2013-05-27",
                                       "2013-07-04","2013-09-02","2013-11-11",
                                       "2013-11-28","2013-12-25"))

# Creating a public holiday column
daily$pub_hol = ifelse(daily$date %in% public_holidays,TRUE,FALSE)

# Combining the wday+sat_term column with public holiday column
daily$wday_sat_term_pub_hol = ifelse(daily$pub_hol,paste(daily$sat_term_n_wday,"PH",sep = "_"),daily$sat_term_n_wday)

# Creating a model
mod4 = lm(formula = n ~ wday_sat_term_pub_hol,data = daily)


# Plotting the residuals : We can see that the model captures trend properly but is 
daily %>% 
  gather_residuals(model1 = mod1,model4 = mod4) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "white", size = 2) 


# The RMSE and MAE is less than other 3 models
results = rbind(results,tibble(
                          mod_names = "mod4",
                          RMSE = rmse(mod4,daily),
                          MAE = mae(mod4,daily)
          ))

# Q.5 ---------------------------------------------------------------------
# What happens if you fit a day of week effect that varies by month (i.e. n ~ wday * month)? 
# Why is this not very helpful?

# Answer
# Create a function to convert date to month
get_month = function(date = daily$date){
  
  cut(x = date,
      breaks = lubridate::as_date(c("2013-01-01","2013-01-31","2013-02-28","2013-03-31","2013-04-30","2013-05-31","2013-06-30","2013-07-31","2013-08-31","2013-09-30","2013-10-31","2013-11-30","2014-01-01")),
      labels = c(1,2,3,4,5,6,7,8,9,10,11,12)
      )
  
}

# getting the month column
daily$month = get_month(daily$date)

# creating model 
mod5 = lm(formula = n ~ wday * month,data = daily)

# plotting the residual graph
daily %>% 
  add_residuals(mod5,"resid5") %>% 
  ggplot(aes(date,resid5)) +
  geom_line() + 
  geom_hline(yintercept = 0,colour = "white", size = 2)

# Comparing the RMSE and MAE statistic
results = rbind(results,tibble(
  mod_names = "mod5",
  RMSE = rmse(mod5,daily),
  MAE = mae(mod5,daily)
))

# Although this model has lower RMSE and MAE, the residual graph shows that it does
# not perform well, particularly for months of November and December, suggesting that flying behaviour
# for the months on weekdays of November and December is different from that of other months.

# Q.6 ---------------------------------------------------------------------
# What would you expect the model n ~ wday + ns(date, 5) to look like? 
# Knowing what you know about the data, why would you expect it to be not particularly effective?

# Answer:
# Creating the model
mod6 = lm(formula = n ~ wday + ns(date, 5), data = daily)

# plotting the residuals
daily %>% 
  gather_residuals(model1 = mod1, model6 = mod6) %>% 
  ggplot(mapping = aes(date,resid,colour = model)) +
  geom_line() +
  geom_hline(yintercept = 0,size = 2, color = "white")
  
# getting the results
results = rbind(results,tibble(
  mod_names = "mod6",
  RMSE = rmse(model = mod6,data = daily),
  MAE = mae(model = mod6,data = daily)
))

# It seems that the model does not work well, because natural splines of a date variable
# are not helpful to predict number of flight. After all natural spline of a date
# are just numbers and have no significance in predicting the number of flights


# Q.7 ---------------------------------------------------------------------
# We hypothesised that people leaving on Sundays are more likely to be business travellers who need to be somewhere on Monday. 
# Explore that hypothesis by seeing how it breaks down based on distance and time: 
# if it’s true, you’d expect to see more Sunday evening flights to places that are far away.

# Answer
# Creating the data that is needed for testing this hypothesis


sunday_fights = flights %>% 
                  dplyr::select(year,month,day,sched_dep_time,distance) %>%       # select necessary columns 
                  mutate(date = make_date(year, month, day),                      # create date out of year month and day
                         sched_dep_part_of_day = cut(x = sched_dep_time,             
                                                     breaks = c(000,600,1000,1500,2000,2400),
                                                     labels = c("Early Morning","Morning","Noon","Evening","Night")),  # create sched dep part of day based on time
                         wday = wday(date,label = TRUE)                           # create weekday 
                           ) %>% 
                  dplyr::filter(wday == "Sun") %>%                                # filter sunday data
                  group_by(sched_dep_part_of_day) %>%                             # group by scheduled departure part of day
                  summarize(number_of_filghts = n(),                              # number of flights scheduled in evening  
                            cum_distance = sum(distance))                         # sum of distances of all flights travelled

# As we can see from the above data the number of scheduled flights on Sundays 
# and cumulative distance travelled is greater in Evening between 3pm and 8 pm


# Q.8 ---------------------------------------------------------------------
# It’s a little frustrating that Sunday and Saturday are on separate ends of the plot. 
# Write a small function to set the levels of the factor so that the week starts on Monday.
# Answer :
# Create a vector of new order that we want to specify
new_wday_order = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

# Create the graph 
daily %>% 
  add_predictions(model = mod,var = "pred_only_wday") %>% 
  mutate(wday = factor(x = wday,levels = new_wday_order)) %>%   # use the factor() to set new levels
  ggplot(mapping = aes(x = wday, y = n)) +
  geom_boxplot()