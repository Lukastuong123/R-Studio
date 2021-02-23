##########dplyr#################################
## install, load dplyr
#install.packages("dplyr") ## do this once only to install the package on your computer.
library(dplyr)

## load gapminder
library(gapminder)        # this is the package name
str(gapminder)            # there's still a data frame named 'gapminder'


## practice dplyr::filter()
filter(gapminder, lifeExp < 29)
filter(gapminder, country == "Mexico")
filter(gapminder, country %in% c("Mexico", "Afghanistan"))

## base R alternatives to dplyr::filter()
gapminder[gapminder$lifeExp < 29, ]           ## repeat `gapminder`, [i, j] indexing is distracting
subset(gapminder, country == "Mexico")        ## almost same as filter ... but wait ...

## pipe operator %>%
gapminder %>% head # this...
head(gapminder) # ...is the same as this!
gapminder %>% head(3) # can pass arguments! this...
head(gapminder, 3) # ...is the same as this!

## practice dplyr::select() with %>%
select(gapminder, year, lifeExp) # this...
gapminder %>% select(year, lifeExp) # ...is the same as this!

## practice with %>% chains
gapminder %>%
  select(year, lifeExp) %>% 
  head(4) # doesn't have to be a dplyr function

gapminder %>%
  filter(country == "Cambodia") %>%
  select(-continent, -lifeExp)                 # same as select(country, year, pop, gdpPercap) 

# compare to base R, hard to read!
gapminder[gapminder$country == "Cambodia", c("country", "year", "pop", "gdpPercap")]
subset(gapminder, country == "Cambodia", select = c(country, year, pop, gdpPercap))

## dplyr::mutate() adds new columns
gapminder %>%
  filter(country == "Cambodia") %>%
  select(-continent, -lifeExp) %>%
  mutate(gdp = pop * gdpPercap)

## dplyr::summarize() or summarise() adds new column when grouping
gapminder %>%
  filter(country == "Cambodia") %>%
  select(-continent, -lifeExp) %>%
  mutate(gdp = pop * gdpPercap) %>%
  group_by(country) %>%
  summarize(mean_gdp = mean(gdp)) %>%
  ungroup() # if you use group_by, also use ungroup() to save heartache later

## summarize for all countries (replaces our for loop!)
gapminder %>%
  select(-continent, -lifeExp) %>%
  mutate(gdp = pop * gdpPercap) %>%
  group_by(country) %>%
  summarize(mean_gdp = mean(gdp)) %>%
  ungroup() # if you use group_by, also use ungroup() to save heartache later




##########tidyr#################################
## install tidyr
#install.packages("tidyr")
library(tidyr)

## load wide data - make sure you have the file in your directory
## use getwd() to see where your working drive is and use setwd() if you need to set it
gap_wide <- read.csv('E:/NorthEastern University/Data Mining/M3/Project/gapminder_wide.csv')
head(gap_wide)
str(gap_wide)


## practice tidyr::gather() wide to long
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         -continent, -country) 
# or 
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         dplyr::starts_with('pop'),
         dplyr::starts_with('lifeExp'),
         dplyr::starts_with('gdpPercap'))
# or (but always be wary of numerics because they could change silently)
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         3:38)  # could also do -1, -2: 'not column one, not column two


## gather() and separate() to create our original gapminder
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         -continent, -country) %>%
  separate(obstype_year,
           into = c('obs_type','year'),
           sep="_")

## practice: can still do calculations in long format
gap_long %>% 
  group_by(continent, obs_type) %>%
  summarize(means = mean(obs_values))

## spread() from normal to wide
gap_normal <- gap_long %>% 
  spread(obs_type, obs_values) %>%
  select(country, continent, year, lifeExp, pop, gdpPercap)
# or 
gap_normal <- gap_long %>% 
  spread(obs_type, obs_values)
gap_normal <- gap_normal[,names(gapminder)]

## check that all.equal()
all.equal(gap_normal,gapminder)

## unite() and spread(): convert gap_long to gap_wide
head(gap_long) # remember the columns

gap_wide_new <- gap_long %>% 
  # first unite obs_type and year into a new column called var_names. Separate by _
  unite(col = var_names, obs_type, year, sep = "_") %>% 
  # then spread var_names out by key-value pair.
  spread(key = var_names, value = obs_values)
str(gap_wide_new)