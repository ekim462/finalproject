# Meta --------------------------------------------------------------------
## Title:         ECON 470 Final Project
## Author:        Genia Kim
# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes,
               scales, plotly, gganimate, cobalt, ivpack, stargazer, haven, ggthemes,
               magick, rdrobust, haven, estimatr, lfe, rddensity, here, modelsummary,
               fixest, dotwhisker, kableExtra, stargazer)

#load data
source('data/cdc-tax-data.rds')
source('data/purchaseproh.csv')

# cdc_tax_data <- readRDS("C:/Users/Genia/Desktop/Spring 2022/ECON 470/cdc-tobacco/cdc-tobacco/cdc-tax-data.rds")
# purchase_prohib <- read.csv("C:/Users/Genia/Desktop/Spring 2022/ECON 470/cdc-tobacco/cdc-tobacco/purchaseproh.csv", fileEncoding="UTF-8-BOM")

cdc_tax_data= readRDS('data/cdc-tax-data.rds')
purchase_prohib= read.csv('data/purchaseproh.csv', fileEncoding='UTF-8-BOM')


#clean and merge data
purchase_prohib <- purchase_prohib %>%
  rename("state" = "Location.Description") %>%
  rename("enacted" = "Enacted.Date") %>%
  rename("effective" = "Effective.Date") %>%
  filter(Quarter ==4)  %>%
  filter(state != "District of Columbia")%>%
  select(Year, state, Provision, Value, enacted, effective)

smoke.dat <- cdc_tax_data %>%
  filter(state != "District of Columbia")%>%
  left_join(purchase_prohib, by=c("state","Year")) 

# clean data so enacted/effective years appear for all years
smoke.dat <- smoke.dat %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(enacted=last(enacted)) %>%
  mutate(effective=last(effective))%>%
  mutate(Provision=last(Provision))

# clean data so Value shows up depending on when if the provision was enacted in that year
smoke.dat <- smoke.dat %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(Value= ifelse(Year>=enacted, "Yes", "No Provision"))


# add columns depending on time of treatment 
smoke.dat <- smoke.dat %>%
  mutate(early_prohib = ifelse(enacted<=1985, "Early", NA),
         pre_prohib = ifelse(enacted>1985 & enacted<=1990, "Pre", NA),
         norm_prohib = ifelse(enacted>1990 & enacted<=1995, "Norm", NA),
         post_prohib = ifelse(enacted>1995 & enacted<=2000, "Post", NA),
         late_prohib = ifelse(enacted>2000, "Late", NA),
         act_time = coalesce(early_prohib,pre_prohib, norm_prohib, post_prohib, late_prohib))

smoke.dat$act_time <- factor(smoke.dat$act_time,                 # Relevel group factor
                         levels = c("Early", "Pre", "Norm", "Post", "Late"))

# was a purchase prohibition policy enacted ever?
policy <- smoke.dat %>%
  select(Year, state, Value) %>%
  ungroup() %>%
  group_by(state) %>%
  pivot_wider(names_from ="Year", values_from = "Value")
policy$policy_ever <- ifelse(Reduce(`|`, lapply(policy, `==`, "Yes")),1,0)
policy$policy_ever[is.na(policy$policy_ever)] <- 0

policy <- policy %>%
  select(state, policy_ever) 

smoke.dat <- smoke.dat %>%
  left_join(policy, by=c("state"))

#graph: States with Youth Purchase Prohibition Laws Over Time
graph <- smoke.dat %>%
  ungroup() %>%
  group_by(Year, Value) %>%
  select(Year, Value) %>%
  summarize(prohib = n()) %>%
  filter(Value == "Yes") %>%
  ggplot(aes(x=Year, y=prohib)) +
  stat_summary(fun="mean", geom="line") +
  labs(
    x="Year",
    y="Number of States",
    title = "States with Youth Purchase Prohibition Laws Over Time"
  ) + 
  theme_bw()
graph



# graph: sales per capita for states that did introduce policy specifically prohibiting purchase of
# cigarettes by youths v. states that did not
graph1 <- smoke.dat
graph1$policy_ever = as.logical(graph1$policy_ever)
graph1<- graph1 %>% ungroup() %>%
  group_by(Year,policy_ever) %>%
  ggplot(aes(x=Year, y= sales_per_capita, group=policy_ever, color=policy_ever)) +
  geom_vline(xintercept=1992, color="black") +
  stat_summary(fun="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="Sales per Capita",
    title="Average Cigarette Pack Sales Per Capita"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,10))
graph1


#graph2: Among states that have policy, sales per capita based on policy time
graph2 <- smoke.dat %>% 
  ungroup() %>%
  filter(!is.na(act_time)) %>%
  group_by(Year,act_time) %>%
  ggplot(aes(x=Year, y= sales_per_capita, group=act_time, color=act_time)) +
  geom_vline(xintercept=1992, color="black") +
  stat_summary(fun="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="Sales per Capita",
    title="Average Cigarette Pack Sales Per Capita for States with Youth Purchase Prohibition Laws"
  )+
  scale_x_continuous(breaks = seq(1970, 2020,10))
graph2


#table: sales for states before and post synar act, based on if a policy was ever enacted
smoke.dat.new <- smoke.dat %>%
  mutate(pol_time = ifelse(Year>1985 & Year<=1990, "Before Synar", 
                           ifelse(Year>1995 & Year<=2000,"Post Synar", NA))) 

smoke.dat.new$policy_ever <- as.logical(smoke.dat.new$policy_ever)

smoke.tbl <- smoke.dat.new %>% filter(!is.na(policy_ever)) %>%
  filter(pol_time=="Before Synar"|pol_time=="Post Synar") %>%
  group_by(policy_ever, pol_time) %>% summarize(mean=mean(sales_per_capita)) %>%
  pivot_wider(names_from ="pol_time", values_from = "mean")
smoke.tbl


# only states that expanded 1990-1995 v. those that did not
smoke.dat.norm <- smoke.dat %>%
  mutate(post = ifelse(Year>=1990,1, 0)) %>%
  mutate(treat=post*policy_ever) %>%
  filter(is.na(enacted) | enacted>1990 & enacted<=1995)

# DD v. FE
summary(lm(log(sales_per_capita) ~ post + policy_ever + post*policy_ever, data=smoke.dat.norm))
summary(feols(log(sales_per_capita) ~ treat | state + Year, data=smoke.dat.norm))


# all states, regardless of treatment time (FE)
smoke.dat.all <- smoke.dat %>% 
  mutate(post = (Year>=enacted), treat=post*policy_ever)

summary(feols(log(sales_per_capita) ~ treat | state + Year, data=smoke.dat.all))


 save.image("finalproj.RData")










