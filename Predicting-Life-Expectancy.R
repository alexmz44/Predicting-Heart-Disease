# Read in the data
library(tidyverse)
life_data <- read_csv("Life Expectancy Data.csv")

#EXPLORE AND ANALYZE THE DATA
View(life_data)
summary(life_data)

# select my relevant varibles
life_data_clean <- life_data %>%
  select(GDP, Population,Schooling, `Total expenditure`, BMI,`Adult Mortality`,`Life expectancy`, Status, Year, Country) %>%
  filter(Year == 2000 | Year == 2002 | Year == 2004 |Year == 2006 | Year == 2008 | Year == 2010 | Year == 2012 | Year == 2014)

summary(life_data_clean)


# impute the median for all the missing values in the data
library(Hmisc)
life_data <- as.data.frame(apply(life_data_clean[,-(8:10)], 2, function(x){impute(x, median)}))
life_data <- cbind(life_data_clean[,c(9,  10,8)], life_data)
# check if there is any multicollinearity between the x variables
life_data %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

# there isnot any significant correlation between the variables
life_data %>%
  keep(is.numeric) %>%
  cor()
# rename the variables for ease of running
life_data <- life_data %>% 
  rename(lifexp = `Life expectancy`) %>%
  rename(pop = `Population`) %>%
  rename(totalexp = `Total expenditure`) %>%
  rename(adultmort = `Adult Mortality`) 
# create a dummy variable for developing countries
life_data <- life_data %>%
  mutate(developing = ifelse(Status == "Developing", 1, 0)) %>%
  select(-Status)
# now we have panel data, so it would be a good idea to make sure we treat it as such
library(plm)

panel_life_data <-  pdata.frame(life_data, index = c("Country", "Year"))

#check if the panel data is balanced and it is
pdim(panel_life_data)
View(panel_life_data)

# we find that developing dummy variable does not vary over time 
pvar(panel_life_data)

# run a regression wiht the FE estimator