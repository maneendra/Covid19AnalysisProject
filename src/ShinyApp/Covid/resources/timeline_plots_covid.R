## Plots 
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(data.table)


john_deaths <- read.csv("resources/time_series_covid19_deaths_global_24_06_20_mod.csv")
john_recovered <- read.csv("resources/time_series_covid19_recovered_global_24_06_20_mod.csv")
john_confirmed <- read.csv("resources/time_series_covid19_confirmed_global_24_06_20_mod.csv")

country1 = "India"
country2 = "Germany"
country3 = "Italy"
country4 = "Brazil"
country5 = "US"
country6 = "Spain" 


## only confirmed ##
jc1 <- john_confirmed %>% filter(Country.Region == country1) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jc2 <- john_confirmed %>% filter(Country.Region == country2) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jc3 <- john_confirmed %>% filter(Country.Region == country3) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jc4 <- john_confirmed %>% filter(Country.Region == country4) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jc5 <- john_confirmed %>% filter(Country.Region == country5) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jc6 <- john_confirmed %>% filter(Country.Region == country6) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))

john_selected_confirmed <- t(rbind(jc1,jc2,jc3,jc4,jc5,jc6))
colnames(john_selected_confirmed) <- c("India","Germany","Italy","Brazil","US","Spain" )
john_selected_confirmed


confirmed.df <- as.data.frame(t(john_selected_confirmed))
confirmed.df

confirmed.df$country <- c("India", "Germany", "Italy", "Brazil", "US", "Spain")
confirmed.df


# Line plots with dots - confirmed
confirmed_timeline_plot <- function() {
  return(ggplot(confirmed.df, aes(x = country)) + 
           geom_line(aes(y=a,group=2, color = "2020-01-24"), size = 0.5) +
           geom_line(aes(y=b,group=2, color = "2020-01-27"), size = 0.5) +
           geom_line(aes(y=c,group=2, color = "2020-01-30"), size = 0.5) +
           geom_line(aes(y=d,group=2, color = "2020-02-15"), size = 0.5) +
           geom_line(aes(y=e,group=2, color = "2020-02-24"), size = 0.5) +
           geom_line(aes(y=f,group=2, color = "2020-03-02"), size = 0.5) +
           geom_line(aes(y=g,group=2, color = "2020-03-11"), size = 0.5) +
           geom_line(aes(y=h,group=2, color = "2020-03-16"), size = 0.5) +
           geom_line(aes(y=I,group=2, color = "2020-03-19"), size = 0.5) +
           geom_line(aes(y=j,group=2, color = "2020-03-22"), size = 0.5) +
           geom_line(aes(y=k,group=2, color = "2020-03-29"), size = 0.5) +
           geom_line(aes(y=l,group=2, color = "2020-04-01"), size = 0.5) +
           geom_line(aes(y=m,group=2, color = "2020-04-06"), size = 0.5) +
           geom_line(aes(y=n,group=2, color = "2020-04-15"), size = 0.5) +
           geom_line(aes(y=o,group=2, color = "2020-04-20"), size = 0.5) +
           geom_line(aes(y=p,group=2, color = "2020-04-27"), size = 0.5) +
           geom_line(aes(y=q,group=2, color = "2020-05-06"), size = 0.5) +
           geom_line(aes(y=r,group=2, color = "2020-05-13"), size = 0.5) +
           geom_line(aes(y=s,group=2, color = "2020-05-17"), size = 0.5) +
           geom_line(aes(y=t,group=2, color = "2020-05-19"), size = 0.5) +
           geom_line(aes(y=u,group=2, color = "2020-05-21"), size = 0.5) +
           geom_line(aes(y=v,group=2, color = "2020-06-04"), size = 0.5) +
           geom_line(aes(y=w,group=2, color = "2020-06-14"), size = 0.5) +
           geom_line(aes(y=x,group=2, color = "2020-06-16"), size = 0.5) +
           
           
           geom_point(aes(y=a))+
           geom_point(aes(y=b))+
           geom_point(aes(y=c))+
           geom_point(aes(y=d))+
           geom_point(aes(y=e))+
           geom_point(aes(y=f))+ 
           geom_point(aes(y=g))+ 
           geom_point(aes(y=h))+ 
           geom_point(aes(y=I))+ 
           geom_point(aes(y=j))+ 
           geom_point(aes(y=k))+ 
           geom_point(aes(y=l))+ 
           geom_point(aes(y=m))+ 
           geom_point(aes(y=n))+ 
           geom_point(aes(y=o))+ 
           geom_point(aes(y=p))+ 
           geom_point(aes(y=q))+ 
           geom_point(aes(y=r))+ 
           geom_point(aes(y=s))+ 
           geom_point(aes(y=t))+ 
           geom_point(aes(y=u))+ 
           geom_point(aes(y=v))+ 
           geom_point(aes(y=w))+ 
           geom_point(aes(y=x))+
           
           labs(x="country", y="confirmed", color = "Legend") +
           coord_flip() +
           labs(title="Number of confirmed cases - in the space of time from 2020-01-24 to 2020-06-16", 
                subtitle="based on Johns Hopkins data set and the timline") +  
           theme(
             legend.position = c(.95, .80),
             legend.justification = c("right", "top") )
  )}




## only recovered ##
jr1 <- john_recovered %>% filter(Country.Region == country1) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jr2 <- john_recovered %>% filter(Country.Region == country2) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jr3 <- john_recovered %>% filter(Country.Region == country3) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jr4 <- john_recovered %>% filter(Country.Region == country4) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jr5 <- john_recovered %>% filter(Country.Region == country5) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jr6 <- john_recovered %>% filter(Country.Region == country6) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))

john_selected_recovered <- t(rbind(jr1,jr2,jr3,jr4,jr5,jr6))
colnames(john_selected_recovered) <- c("India","Germany","Italy","Brazil","US","Spain" )
john_selected_recovered

recovered.df <- as.data.frame(t(john_selected_recovered))
recovered.df

recovered.df$country <- c("India", "Germany", "Italy", "Brazil", "US", "Spain")
recovered.df

# Line plots with dots - recovered
recovered_timeline_plot <- function() {
  return(ggplot(recovered.df, aes(x = country)) +
           geom_line(aes(y=a,group=2, color = "2020-01-24"), size = 0.5) +
           geom_line(aes(y=b,group=2, color = "2020-01-27"), size = 0.5) +
           geom_line(aes(y=c,group=2, color = "2020-01-30"), size = 0.5) +
           geom_line(aes(y=d,group=2, color = "2020-02-15"), size = 0.5) +
           geom_line(aes(y=e,group=2, color = "2020-02-24"), size = 0.5) +
           geom_line(aes(y=f,group=2, color = "2020-03-02"), size = 0.5) +
           geom_line(aes(y=g,group=2, color = "2020-03-11"), size = 0.5) +
           geom_line(aes(y=h,group=2, color = "2020-03-16"), size = 0.5) +
           geom_line(aes(y=I,group=2, color = "2020-03-19"), size = 0.5) +
           geom_line(aes(y=j,group=2, color = "2020-03-22"), size = 0.5) +
           geom_line(aes(y=k,group=2, color = "2020-03-29"), size = 0.5) +
           geom_line(aes(y=l,group=2, color = "2020-04-01"), size = 0.5) +
           geom_line(aes(y=m,group=2, color = "2020-04-06"), size = 0.5) +
           geom_line(aes(y=n,group=2, color = "2020-04-15"), size = 0.5) +
           geom_line(aes(y=o,group=2, color = "2020-04-20"), size = 0.5) +
           geom_line(aes(y=p,group=2, color = "2020-04-27"), size = 0.5) +
           geom_line(aes(y=q,group=2, color = "2020-05-06"), size = 0.5) +
           geom_line(aes(y=r,group=2, color = "2020-05-13"), size = 0.5) +
           geom_line(aes(y=s,group=2, color = "2020-05-17"), size = 0.5) +
           geom_line(aes(y=t,group=2, color = "2020-05-19"), size = 0.5) +
           geom_line(aes(y=u,group=2, color = "2020-05-21"), size = 0.5) +
           geom_line(aes(y=v,group=2, color = "2020-06-04"), size = 0.5) +
           geom_line(aes(y=w,group=2, color = "2020-06-14"), size = 0.5) +
           geom_line(aes(y=x,group=2, color = "2020-06-16"), size = 0.5) +
           
           
           geom_point(aes(y=a))+
           geom_point(aes(y=b))+
           geom_point(aes(y=c))+
           geom_point(aes(y=d))+
           geom_point(aes(y=e))+
           geom_point(aes(y=f))+
           geom_point(aes(y=g))+
           geom_point(aes(y=h))+
           geom_point(aes(y=I))+
           geom_point(aes(y=j))+
           geom_point(aes(y=k))+
           geom_point(aes(y=l))+
           geom_point(aes(y=m))+
           geom_point(aes(y=n))+
           geom_point(aes(y=o))+
           geom_point(aes(y=p))+
           geom_point(aes(y=q))+
           geom_point(aes(y=r))+
           geom_point(aes(y=s))+
           geom_point(aes(y=t))+
           geom_point(aes(y=u))+
           geom_point(aes(y=v))+
           geom_point(aes(y=w))+
           geom_point(aes(y=x))+
           
           labs(x="country", y="recovered", color = "Legend") +
           coord_flip() +
           labs(title="Number of recovered cases - in the space of time from 2020-01-24 to 2020-06-16",
                subtitle="based on Johns Hopkins data set and the timline") +
           # theme(legend.position="top")
           theme(
             legend.position = c(.95, .80),
             legend.justification = c("right", "top") )
  )}

## only deaths ##
jd1 <- john_deaths %>% filter(Country.Region == country1) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jd2 <- john_deaths %>% filter(Country.Region == country2) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jd3 <- john_deaths %>% filter(Country.Region == country3) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jd4 <- john_deaths %>% filter(Country.Region == country4) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jd5 <- john_deaths %>% filter(Country.Region == country5) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))
jd6 <- john_deaths %>% filter(Country.Region == country6) %>% select(c("a","b","c","d","e","f","g","h","I","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x"))

john_selected_deaths <- t(rbind(jd1,jd2,jd3,jd4,jd5,jd6))
colnames(john_selected_deaths) <- c("India","Germany","Italy","Brazil","US","Spain" )
john_selected_deaths

deaths.df <- as.data.frame(t(john_selected_deaths))
deaths.df

deaths.df$country <- c("India", "Germany", "Italy", "Brazil", "US", "Spain")
deaths.df

# Line plots with dots - deaths
deaths_timeline_plot <- function() {
  return(ggplot(deaths.df, aes(x = country)) +
           geom_line(aes(y=a,group=2, color = "2020-01-24"), size = 0.5) +
           geom_line(aes(y=b,group=2, color = "2020-01-27"), size = 0.5) +
           geom_line(aes(y=c,group=2, color = "2020-01-30"), size = 0.5) +
           geom_line(aes(y=d,group=2, color = "2020-02-15"), size = 0.5) +
           geom_line(aes(y=e,group=2, color = "2020-02-24"), size = 0.5) +
           geom_line(aes(y=f,group=2, color = "2020-03-02"), size = 0.5) +
           geom_line(aes(y=g,group=2, color = "2020-03-11"), size = 0.5) +
           geom_line(aes(y=h,group=2, color = "2020-03-16"), size = 0.5) +
           geom_line(aes(y=I,group=2, color = "2020-03-19"), size = 0.5) +
           geom_line(aes(y=j,group=2, color = "2020-03-22"), size = 0.5) +
           geom_line(aes(y=k,group=2, color = "2020-03-29"), size = 0.5) +
           geom_line(aes(y=l,group=2, color = "2020-04-01"), size = 0.5) +
           geom_line(aes(y=m,group=2, color = "2020-04-06"), size = 0.5) +
           geom_line(aes(y=n,group=2, color = "2020-04-15"), size = 0.5) +
           geom_line(aes(y=o,group=2, color = "2020-04-20"), size = 0.5) +
           geom_line(aes(y=p,group=2, color = "2020-04-27"), size = 0.5) +
           geom_line(aes(y=q,group=2, color = "2020-05-06"), size = 0.5) +
           geom_line(aes(y=r,group=2, color = "2020-05-13"), size = 0.5) +
           geom_line(aes(y=s,group=2, color = "2020-05-17"), size = 0.5) +
           geom_line(aes(y=t,group=2, color = "2020-05-19"), size = 0.5) +
           geom_line(aes(y=u,group=2, color = "2020-05-21"), size = 0.5) +
           geom_line(aes(y=v,group=2, color = "2020-06-04"), size = 0.5) +
           geom_line(aes(y=w,group=2, color = "2020-06-14"), size = 0.5) +
           geom_line(aes(y=x,group=2, color = "2020-06-16"), size = 0.5) +
           
           
           geom_point(aes(y=a))+
           geom_point(aes(y=b))+
           geom_point(aes(y=c))+
           geom_point(aes(y=d))+
           geom_point(aes(y=e))+
           geom_point(aes(y=f))+
           geom_point(aes(y=g))+
           geom_point(aes(y=h))+
           geom_point(aes(y=I))+
           geom_point(aes(y=j))+
           geom_point(aes(y=k))+
           geom_point(aes(y=l))+
           geom_point(aes(y=m))+
           geom_point(aes(y=n))+
           geom_point(aes(y=o))+
           geom_point(aes(y=p))+
           geom_point(aes(y=q))+
           geom_point(aes(y=r))+
           geom_point(aes(y=s))+
           geom_point(aes(y=t))+
           geom_point(aes(y=u))+
           geom_point(aes(y=v))+
           geom_point(aes(y=w))+
           geom_point(aes(y=x))+
           
           labs(x="country", y="deaths", color = "Legend") +
           coord_flip() +
           labs(title="Number of deaths - in the space of time from 2020-01-24 to 2020-06-16",
                subtitle="based on Johns Hopkins data set and the timline") +
           theme(
             legend.position = c(.95, .80),
             legend.justification = c("right", "top") )
         
         
  )}