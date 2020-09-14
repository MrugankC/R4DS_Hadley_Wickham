
# Setup -------------------------------------------------------------------

library(modelr)
library(tidyverse)
library(gapminder)

# Start -------------------------------------------------------------------

gapminder %>% 
  ggplot(mapping = aes(x = year,y = lifeExp,group = country)) +
  geom_line()

ind <- gapminder %>% 
            filter(country == c("India")) 

full_data <- ind %>% 
    ggplot(mapping = aes(x = year,y = lifeExp)) +
    geom_line() +
    ggtitle("Full Data")


ind_mod <- lm(formula = lifeExp ~ year,data = ind)

lin_trend <- ind %>% 
  add_predictions(model = ind_mod) %>% 
  ggplot(mapping = aes(year,pred)) +
  geom_line() +
  ggtitle("Linear trend + ")

rem_pattern <- ind %>% 
  add_residuals(model = ind_mod) %>% 
  ggplot(mapping = aes(year,resid)) +
  geom_line() +
  ggtitle("Remaining Pattern")

gridExtra::grid.arrange(full_data,lin_trend,rem_pattern,nrow = 1)


# Nested Dataframe
by_country <- gapminder %>% 
                group_by(country, continent) %>% 
                nest()

by_country$data[[1]]


# create a model for all the countries
country_model <- function(df){
  
  lm(formula = lifeExp ~ year,data = df)
  
}

# applying the country model generalized function to each DF in the nested df using pmap function
by_country <- by_country %>% 
                mutate(model = map(.x = data,.f = country_model))

by_country <- by_country %>% 
                mutate(resids = map2(.x = data,.y = model,.f = add_residuals))


# unnest the nested DF
resids <- unnest(data = by_country,cols = resids)


View(resids)
resids %>% 
  ggplot(mapping = aes(x = year,y = resid)) +
  geom_line(mapping = aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE)


resids %>% 
  ggplot(mapping = aes(x = year,y = resid)) +
  geom_line(mapping = aes(group = country), alpha = 1/3) +
  facet_wrap(~continent)



broom::glance(x = ind_mod)


glance = by_country %>% 
  mutate(glance = map(model,broom::glance)) %>% 
  unnest(glance,.drop = TRUE)



bad_fit = glance %>% 
            filter(r.squared < 0.25)

View(bad_fit)