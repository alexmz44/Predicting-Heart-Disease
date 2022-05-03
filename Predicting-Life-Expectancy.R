# Read in the data
library(janitor)
library(car)
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

###################################################################################
# now we have panel data, so it would be a good idea to make sure we treat it as such
library(plm)

panel_life_data <-  pdata.frame(life_data, index = c("Country", "Year"))
panel_life_data <- panel_life_data %>%
  select(-pop, -BMI)
#check if the panel data is balanced and it is
pdim(panel_life_data)
View(panel_life_data)

# we find that developing dummy variable does not vary over time 
pvar(panel_life_data)

# check the correlation plot of all the variables
library(corrplot)
data_cor_plot <-  panel_life_data %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

data_cor_plot
# multicollinearity test
vif(reg.ols)

panel_life_data$yr <- factor(panel_life_data$Year)
panel_life_data$ctry <- factor(panel_life_data$Country)
#doing a fixed effect regression to see how a developing country is effected on life expectancy over time
summary(plm(lifexp ~ GDP + Schooling + totalexp +adultmort + factor(Year) * developing, data = panel_life_data, model = "within"))


# run a regression with the FE, RE, and OLS models
reg.ols <- plm(lifexp ~ GDP + Schooling + totalexp  +adultmort + developing,index = c("Country", "Year"), data = panel_life_data, model = "pooling")

reg.re <- plm(lifexp ~ GDP + Schooling + totalexp +adultmort + developing, index = c("Country", "Year"),data = panel_life_data, model = "random")

reg.fe <- plm(lifexp ~ GDP + Schooling + totalexp +adultmort + developing, index = c("Country", "Year"), data = panel_life_data, model = "within")

# summary of all the models
summary(reg.ols)
summary(reg.re)
summary(reg.fe)
# table for the results without showing year dummy variables
library(stargazer)
stargazer(reg.ols, reg.re, reg.fe, type = "text",
          column.labels = c("OLS", "RE", "FE"), keep.stat = c("rsq", "n"),
          keep = c("GD", "Sc","to","ad","de"))


# test to see if RE model is consistent; it is not, so stick to fe
phtest(reg.fe, reg.re)
# test if the fixed effects is better than ols; it is
pFtest(reg.fe, reg.ols)

# test if we need to use the time fixed effects, we do not need to use time fixed effects
# create the fixed time model
reg.fe.time <- plm(lifexp ~ GDP + Schooling + totalexp +adultmort + developing+ factor(Year), data = panel_life_data, model = "within", effect= "twoways")
plmtest(reg.fe, c("time"), type = ("bp"))

#checking for heteroskedasticity; there is some present

library(lmtest)
bptest(reg.fe, studentize = FALSE)

# we want to get the robust standard errors
coeftest(reg.fe, vcovHC, type = "HC0")

# we can then run a regression with the robust standard errors




##############################################################################################################################################
#Two different regression models on countries that are developing and countries that are not developing 



