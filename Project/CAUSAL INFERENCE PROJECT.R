library(rddensity)
library(tidyverse)
library(cli)
library(haven)
library(rmarkdown)
library(haven)
library(stargazer)
library(estimatr)
library(rdd)
library(rdrobust)
library(multilevelPSA)
library(cobalt)
library(MatchIt)
library(optmatch)
library(jtools)
library(ggplot2)
library(cobalt)
library(plyr)
library(plotly)
library(knitr)
library(dummies)
library(tidyverse)
library(magrittr)
library(broom)
library(estimatr)
library(forcats)
library(ggthemes)
library(RDHonest)

New_uber <- New_uber %>% 
  as_tibble() %>%  
  janitor::clean_names() %>%
  glimpse() 


#QUESTION:Does uber charge differently based on temperature levels?
#Control for: Time of day, trip time, trip type, city, trip start address, trip end address. 
#Outcome variable: Rubles per km
#Running variable: Temperature value
#Cutoff point: 0 degrees celcius


New_uber$cutoff=ifelse(New_uber$temperature_value<=0,1,0)

#-------------Density Plot for Manipulation------------------------------------------------------

density <- rddensity(New_uber$temperature_value, c = 0)
rdplotdensity(density, New_uber$temperature_value)

#There appears to be a rather significant gap around the cutoff point of zero degrees. 
#This is evidence that there is difference in uber ride prices when the temperature
#is above and below the freezing point. 
#--------------------------------------McCrary Density Histogram------------------------------------------------------

ggplot(New_uber, aes(x=New_uber$temperature_value))+
  geom_histogram(binwidth=2, color="black")+
  geom_vline(xintercept=0)+
  labs(title = "McCrary Density for Running Variable", x = "Running Variable: Temperature Value", y = "Density")+
  theme_minimal()

#The histogram does not seem to be taking on a smooth shape. This is indicative of manipulaion in the running
#variable. 

#--------------------------Checking for Covariate Balance----------------------------------------------------
New_uber2 <- New_uber %>%
  mutate(temperature_value_2 = temperature_value - 0)
New_uber2 <- New_uber2 %>%
  filter(temperature_value_2 >=-20 & temperature_value_2 <= 20)
#Trimming the temperature level to look only at values between -20 and 20 degrees celcius

#-----------------------------TIME OF DAY---------------------------------------------------------------
No_rush_hour <- New_uber %>%
  filter(time_of_day_disc <=10 & time_of_day_disc >=13)

Time_of_day = lm(time_of_day_disc ~ cutoff*temperature_value_2, data=New_uber2)
stargazer(Time_of_day, type = "text")

#------------------------------TRIP TIME--------------------------------------------------------------
Trip_time = lm(trip_time_minutes ~ cutoff*temperature_value_2, data=New_uber2)
stargazer(Trip_time, type = "text")

#------------------------------TRIP TYPE--------------------------------------------------------------
Trip_type= lm(trip_type_dummy ~ cutoff*temperature_value_2, data=New_uber2)
stargazer(Trip_type, type = "text")

#--------------------------------CITY------------------------------------------------------------
City= lm(if_city_sp ~ cutoff*temperature_value_2, data=New_uber2)
stargazer(City, type = "text")

#--------------------------------Distance_kms------------------------------------------------------------
Distance= lm(distance_kms ~ cutoff*temperature_value_2, data=New_uber2)
stargazer(Distance, type = "text")

#--------------------------------Exchange Rate------------------------------------------------------------
Exchange_Rate= lm(rub_usd_exchange_rate ~ cutoff*temperature_value_2, data=New_uber2)
stargazer(Exchange_Rate, type = "text")

#--------------------------------Driver Gender------------------------------------------------------------
Gender= lm(driver_gender_dummy ~ cutoff*temperature_value_2, data=New_uber2)
stargazer(Gender, type = "text")

stargazer(Time_of_day,Trip_time,Trip_type,City,Distance,Exchange_Rate,Gender, type = "text")








categories <- New_uber2$temperature_value

New_uber2 <- New_uber %>%
  filter(temperature_value >= 0 & temperature_value <= 20)

Time_of_day <- split(New_uber2$time_of_day, cut(New_uber2$temperature_value, 11)) %>%
  lapply(mean) %>%
  unlist()
agg_New_uber_Time_of_day <- data.frame(Time_of_day = Time_of_day, temperature_value = seq(-20, 20, by = 4))



Trip_time <- split(New_uber2$trip_time_minutes, cut(New_uber2$temperature_value, 11)) %>%
  lapply(mean) %>%
  unlist()
agg_New_uber_Trip_time <- data.frame(Trip_time = Trip_time, temperature_value = seq(-20, 20, by = 4))


Trip_type <- split(New_uber2$trip_type_dummy, cut(New_uber2$temperature_value, 11)) %>%
  lapply(mean) %>%
  unlist()
agg_New_uber_Trip_type <- data.frame(Trip_type = Trip_type, temperature_value = seq(-20, 20, by = 4))


City <- split(New_uber2$if_city_sp, cut(New_uber2$temperature_value, 11)) %>%
  lapply(mean) %>%
  unlist()
agg_New_uber_City <- data.frame(City = City, temperature_value = seq(-20, 20, by = 4))



Distance <- split(New_uber2$distance_kms, cut(New_uber2$temperature_value, 11)) %>%
  lapply(mean) %>%
  unlist()
agg_New_uber_Distance <- data.frame(Distance = Distance, temperature_value = seq(-20, 20, by = 4))



Exchange_Rate <- split(New_uber2$rub_usd_exchange_rate, cut(New_uber2$temperature_value, 11)) %>%
  lapply(mean) %>%
  unlist()
agg_New_uber_Exchange_Rate <- data.frame(Exchange_Rate = Exchange_Rate, temperature_value = seq(-20, 20, by = 4))



Gender<- split(New_uber2$driver_gender_dummy, cut(New_uber2$temperature_value, 11)) %>%
  lapply(mean) %>%
  unlist()
agg_New_uber_Gender <- data.frame(Gender = Gender, temperature_value = seq(-20, 20, by = 4))




New_uber2 <- New_uber2 %>%
  mutate(gg_group = case_when(temperature_value > 0 ~ 20, TRUE ~ 0))




#Time_of_day
linear_Time_of_day <- ggplot(New_uber2, aes(temperature_value, Time_of_day)) +
  geom_point(aes(x = temperature_value, y = Time_of_day), data = agg_New_uber_Time_of_day) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Time_of_day, group = gg_group), method = "lm") + 
  labs(x = "Temperature Value")
quadratic_Time_of_day <- ggplot(New_uber2, aes(temperature_value, Time_of_day)) +
  geom_point(aes(x = temperature_value, y = Time_of_day), data = agg_New_uber_Time_of_day) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Time_of_day, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Temperature Value")



#Trip_time
linear_Trip_time <- ggplot(New_uber2, aes(temperature_value, Trip_time)) +
  geom_point(aes(x = temperature_value, y = Trip_time), data = agg_New_uber_Trip_time) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Trip_time, group = gg_group), method = "lm") +
  labs(x = "Temperature Value")
quadratic_Trip_time <- ggplot(New_uber2, aes(temperature_value, Trip_time)) +
  geom_point(aes(x = temperature_value, y = Trip_time), data = agg_New_uber_Trip_time) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Trip_time, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Temperature Value")



#Trip_type
linear_Trip_type <- ggplot(New_uber2, aes(temperature_value, Trip_type)) +
  geom_point(aes(x = temperature_value, y = Trip_type), data = agg_New_uber_Trip_type) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Trip_type, group = gg_group), method = "lm") +
  labs(x = "Temperature Value")
quadratic_Trip_type <- ggplot(New_uber2, aes(temperature_value, Trip_type)) +
  geom_point(aes(x = temperature_value, y = Trip_type), data = agg_New_uber_Trip_type) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Trip_type, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Temperature Value")



#City
linear_City <- ggplot(New_uber2, aes(temperature_value, City)) +
  geom_point(aes(x = temperature_value, y = City), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, City, group = gg_group), method = "lm") +
  labs(x = "Temperature Value")
quadratic_City <- ggplot(New_uber2, aes(temperature_value, City)) +
  geom_point(aes(x = temperature_value, y = Cityd), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, City, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Temperature Value") 


#Distance
linear_Distance <- ggplot(New_uber2, aes(temperature_value, Distance)) +
  geom_point(aes(x = temperature_value, y = Distance), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Distance, group = gg_group), method = "lm") +
  labs(x = "Temperature Value")
quadratic_Distance<- ggplot(New_uber2, aes(temperature_value, Distance)) +
  geom_point(aes(x = temperature_value, y = Cityd), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Distance, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Temperature Value") 


#Exchange_Rate
linear_Exchange_Rate <- ggplot(New_uber2, aes(temperature_value, Exchange_Rate)) +
  geom_point(aes(x = temperature_value, y = Exchange_Rate), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Exchange_Rate, group = gg_group), method = "lm") +
  labs(x = "Temperature Value")
quadratic_Exchange_Rate <- ggplot(New_uber2, aes(temperature_value, Exchange_Rate)) +
  geom_point(aes(x = temperature_value, y = Cityd), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Exchange_Rate, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Temperature Value") 


#Gender
linear_Gender <- ggplot(New_uber2, aes(temperature_value, Gender)) +
  geom_point(aes(x = temperature_value, y = Gender), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Gender, group = gg_group), method = "lm") +
  labs(x = "Temperature Value")
quadratic_Gender <- ggplot(New_uber2, aes(temperature_value, Gender)) +
  geom_point(aes(x = temperature_value, y = Cityd), data = agg_New_uber_City) +
  geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
  stat_smooth(aes(temperature_value, Gender, group = gg_group), method = "lm", formula = y ~ x + I(x^2)) +
  labs(x = "Temperature Value") 

library(gridExtra)
grid.arrange(linear_Time_of_day, linear_Trip_time, linear_Trip_type, linear_City,linear_Distance,linear_Exchange_Rate,linear_Gender, nrow=7, top="Linear Discontinuity: predetermined Characteristics")
grid.arrange(quadratic_Time_of_day, quadratic_Trip_time, quadratic_Trip_type, quadratic_City, quadratic_Distance, quadratic_Gender, quadratic_Exchange_Rate, nrow=7, top="Quadratic Discontinuity: predetermined Characteristics") 


RDestimate(price_per_kms_usd ~ temperature_value, cutpoint = 0, bw = 20, data=New_uber)
RDHonest(price_per_kms_usd ~ temperature_value, cutoff = 0, h=20, kern="triangular", M=0.1, sclass="T", order = 1, data=New_uber)

RDestimate(price_per_kms_usd ~ temperature_value | temperature_value*cutoff, cutpoint = 0, bw = 20, data=New_uber)
RDHonest(price_per_kms_usd ~ temperature_value + temperature_value*cutoff, cutoff = 0, h=20, kern="triangular", M=0.1, sclass="T", order = 1, data=New_uber)

RDestimate(price_per_kms_usd ~ temperature_value | temperature_value*cutoff + (temperature_value^2)*cutoff, cutpoint = 0, bw = 20, data=New_uber)
RDHonest(price_per_kms_usd ~ temperature_value + temperature_value*cutoff + (temperature_value^2)*cutoff, cutoff = 0, h=20, kern="triangular", M=0.1, sclass="T", order = 1, data=New_uber)

RDestimate(price_per_kms_usd ~ temperature_value, cutpoint = 0, bw = 40, data=New_uber)
RDHonest(price_per_kms_usd ~ temperature_value, cutoff = 0, h=40, kern="triangular", M=0.1, sclass="T", order = 1, data=New_uber)

RDestimate(price_per_kms_usd ~ temperature_value | temperature_value*cutoff, cutpoint = 0, bw = 40, data=New_uber)
RDHonest(price_per_kms_usd ~ temperature_value + temperature_value*cutoff, cutoff = 0, h=40, kern="triangular", M=0.1, sclass="T", order = 1, data=New_uber)

RDestimate(price_per_kms_usd ~ temperature_value | temperature_value*cutoff + (temperature_value^2)*cutoff, cutpoint = 0, bw = 40, data=New_uber)
RDHonest(price_per_kms_usd ~ temperature_value + temperature_value*cutoff + (temperature_value^2)*cutoff, cutoff = 0, h=40, kern="triangular", M=0.1, sclass="T", order = 1, data=New_uber)




