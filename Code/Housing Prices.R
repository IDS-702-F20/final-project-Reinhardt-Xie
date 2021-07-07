
###### Clear environment and load libraries
rm(list = ls())
library(ggplot2) #Time to pivot to ggplot2
library(lme4)
library(rstan)
library(brms)
library(broom)
library(sjPlot) #another option for making nice html tables
library(lattice) #for ranef
library(magrittr)
library(dplyr)
library(knitr)
library(xtable)
library(kableExtra)
library(stargazer)
library(rms) #for VIF
library(MASS)
library(arm)
library(pROC)
library(e1071)
library(caret)
require(gridExtra)
library(mice) #for missing data imputation

#################################################
# Load the data
#################################################

housing <- read.csv(file= "/Users/Reinhard/702_Statistical_Modelling_Final_Project/df9.csv", header=TRUE, sep=",")
summary(housing)
str(housing)

#################################################
# Data Preparation
#################################################


# Missing Data
#################################################

housing$community_name_f <- factor(housing$community_name)

housing$floor_plan_f <- factor(housing$floor_plan)

housing$facing_direction <- factor(housing$facing_direction)

housing$floor_level <- factor(housing$floor_level)

housing$decoration <- factor(housing$decoration)

housing$year_f <- factor(housing$year)

housing$month_f <- factor(housing$month)

housing$weekday_name_f <- factor(housing$weekday_name)

dim(housing)
str(housing)
summary(housing)

md.pattern(housing)

housing_imp <- mice(housing,m=3,
                    defaultMethod=c("norm","logreg","polyreg","polr"),
                    print=F)

densityplot(housing_imp)

d1 <- complete(housing_imp, 1)
d1

str(d1)
write.csv(d1, file = "d1_imp.csv")

#################################################
# Reload the data
#################################################

housing <- read.csv(file= "/Users/Reinhard/702_Statistical_Modelling_Final_Project/df14.csv", header=TRUE, sep=",")
summary(housing)
str(housing)

# Catagorizing Variables
#################################################

housing$community_name <- factor(housing$community_name)

housing$floor_plan <- factor(housing$floor_plan)

housing$facing_direction <- factor(housing$facing_direction)

housing$floor_level <- factor(housing$floor_level)

housing$decoration <- factor(housing$decoration)

housing$year <- factor(housing$year)

housing$month <- factor(housing$month)

housing$weekday_name <- factor(housing$weekday_name)

# Continuous Variable
#################################################
housing$year_built <- 2020 - housing$year_built
str(housing$year_built)

housing$living_area <- housing$livable_area
str(housing$living_area)

housing$price_per_sqmt <- housing$price_per_sqmt/1000

housing$log_price_per_sqmt <- log(housing$price_per_sqmt)

housing$sqrt_price_per_sqmt <- sqrt(housing$price_per_sqmt)

housing$log_living_area <- log(housing$living_area)

housing$log_year_built <- log(housing$year_built)

# Mean Centering Continous Variables
#################################################
housing$livable_area_c <- housing$livable_area - mean(housing$livable_area)

housing$year_built_c <- housing$year_built - mean(housing$year_built)


#################################################
# EDA
#################################################

str(housing)

dev.off()

#histogram
##################################################

ggplot(housing,aes(x=price_per_sqmt)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(25), bins = 25) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Price per Square Meter",y="Density") + 
  theme_classic() + theme(legend.position="none")


ggplot(housing,aes(x=log_price_per_sqmt)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(25),binwidth = 0.1) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Log Price per Square Meter",y="Density") + 
  theme_classic() + theme(legend.position="none")

#Sampling
##################################################
set.seed(1000)
sample_community <- sample(unique(housing$community_name),25,replace=F)

ggplot(housing[is.element(housing$community_name,sample_community),],
       aes(x=community_name, y=price_per_sqmt, fill=community_name)) +
       geom_boxplot() +
       labs(title="Price per Square Meter by Community",
       x="Communities",y="Price per Square Meter") + theme_classic() +
       theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(housing, aes(living_area, y=price_per_sqmt)) + 
  geom_point(alpha = .5,colour="blue3") + 
  geom_line(y=0, col="green3") + 
  geom_smooth(method = "lm", col = "red3") + xlab("Living Area") +
  ylab("Price/Square Meter") +
  #labs(caption="Price/Square Meter vs Living Area") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20)) + 
  facet_wrap(~year,ncol = 5)

#Box Plots
##################################################
ggplot(housing,aes(x=year, y=price_per_sqmt, fill=year)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Paired") +
  labs(title="Price/Square Meter vs Year by Floor Plan",x="Year",y="Price/Square Meter") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ floor_plan)

ggplot(housing,aes(x=year, y=price_per_sqmt, fill=year)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Paired") +
  labs(title="Price/Square Meter vs Year by Floor Plan",x="Year",y="Price/Square Meter") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ floor_level)

#################################################
# Model fitting
#################################################

###### MLR  
#################################################

model1 <- lm(price_per_sqmt ~ livable_area, data=housing)
summary(model1)

model2 <- lm(price_per_sqmt ~ livable_area + year_built, data=housing)
summary(model2)

model3 <- lm(price_per_sqmt ~ livable_area + year_built + floor_plan, data=housing)
summary(model3)

model4 <- lm(price_per_sqmt ~ livable_area + year_built + floor_plan + facing_direction, data=housing)
summary(model4)

model5 <- lm(price_per_sqmt ~ livable_area + year_built + floor_plan + facing_direction + floor_level, data=housing)
summary(model5)

model6 <- lm(price_per_sqmt ~ livable_area + year_built + floor_plan + facing_direction + floor_level + decoration, data=housing)
summary(model6)

model61 <- lm(price_per_sqmt ~ livable_area + year_built + floor_plan + facing_direction + floor_level + decoration + community_name + year + month, data=housing)
summary(model61)

###### Boxcox Transformation  
#################################################

boxcox(model61)

boxcox(model61, lambda = seq(-0.25, 0.75, by = 0.05),  plotit = TRUE)

model61_cox <- lm((price_per_sqmt^0.3 - 1) / 0.3 ~ livable_area + year_built + floor_plan + facing_direction + floor_level + decoration + community_name + year + month, data=housing)
summary(model61_cox)

res1 <- residuals(model61_cox,"resp")

pred1 <- predict(model61_cox)

pred_res1 <- data.frame(pred1, res1)
var_plot <- ggplot(pred_res1, aes(pred1, y=res1)) +
  geom_point(alpha = .5,colour="blue3") + 
  geom_line(y = 0, col = "red3") + 
  geom_smooth(method="loess",col="red3") + 
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(caption="Residuals vs Fitted values") + 
  theme(plot.caption = element_text(hjust = 0.5, size = 20))
var_plot

std_res1 <- (res1 - mean(res1)) / sd(res1)
std_res_df1 <- data.frame(std_res1)
q_plot <- qplot(sample = std_res1, data = std_res_df1, color=I("blue3"), alpha=.5) +
  geom_abline(intercept = 0, slope = 1, col="red3") + 
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") + 
  labs(caption="Normal Q-Q") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20), legend.position = "none") 
q_plot

###### Hierarchical Linear Model
#################################################

model7 <- lmer(data = housing, price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + year + month + weekday_name + (1 | community_name ) )
summary(model7)
tab_model(model7)

model71 <- lmer(data = housing, log_price_per_sqmt ~ log_living_area + year_built + floor_plan + facing_direction + floor_level + decoration + year + month + weekday_name + (1 | community_name ) )
summary(model7)
tab_model(model71)

model8 <- lmer(data = housing, price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1|year) + month + weekday_name + (1 | community_name ))
summary(model8)
tab_model(model8)

model81 <- lmer(data = housing, log_price_per_sqmt ~ log_living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1|year) + month + weekday_name + (1 | community_name ))
summary(model81)
tab_model(model81)

model9 <- lmer(data = housing, price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1|month))
summary(model9)

model10 <- lmer(data = housing, price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1|weekday_name))
summary(model10)

model11 <- lmer(data = housing, price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year))
summary(model11)

###Main Effects
#################################################

str(housing)

model12 <- lmer(data = housing, price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year) + (1|month))
summary(model12)
tab_model(model12)

model13 <- lmer(data = housing, price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year) + (1|month)+ (1|weekday_name))
summary(model13)

model14 <- lmer(data = housing, log_price_per_sqmt ~ living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year) + (1|month))
summary(model14)
tab_model(model14)
dotplot(ranef(model14, condVar = TRUE))

model15 <- lmer(data = housing, log_price_per_sqmt ~ log_living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year) + (1|month))
summary(model15)
tab_model(model15)

model16 <- lmer(data = housing, log_price_per_sqmt ~ log_living_area + year_built + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year) + (1|month) + (1|weekday_name))
summary(model16)
tab_model(model16)

model17 <- lmer(data = housing, log_price_per_sqmt ~ log_living_area + log_year_built + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year) + (1|month))
summary(model17)
tab_model(model17)

model18 <- lmer(data = housing, log_price_per_sqmt ~ log_living_area + (1 | year_built) + floor_plan + facing_direction + floor_level + decoration + (1 | community_name) + (1|year) + (1|month))
summary(model18)
tab_model(model18)

anova(model7, model11) 

anova(model11, model12) 

anova(model12, model13) 

anova(model12, model14) 

anova(model14, model15) 

anova(model15, model16) 

###### Interactions
#################################################

###Livable_area* others
#####################
model19 <- lmer(data = housing, price_per_sqmt ~ livable_area_c*(floor_plan + facing_direction + floor_level + decoration + year_built)  + (1 | community_name) + (1|year) + (1|month))
summary(model19)
tab_model(model19) #nonsense: floor_plan,interactions

anova(model12, model19) 

model20 <- lmer(data = housing, log_price_per_sqmt ~ livable_area_c*(floor_plan + facing_direction + floor_level + decoration + year_built)  + (1 | community_name) + (1|year) + (1|month))
summary(model20)
tab_model(model20) #nonsense: floor_plan,interactions

model21 <- lmer(data = housing, price_per_sqmt ~ livable_area_c*(year_built + facing_direction + floor_level + decoration) + floor_plan + (1 | community_name) + (1|year) + (1|month))
summary(model21)
tab_model(model21)  # small interactions, year_built sign change

anova(model12, model21) 

model22 <- lmer(data = housing, price_per_sqmt ~ livable_area_c*( facing_direction + year_built ) + floor_level + decoration + floor_plan  + (1 | community_name) + (1|year) + (1|month))
summary(model22)
tab_model(model22) # nonsense or small interactions

model23 <- lmer(data = housing, log_price_per_sqmt ~ livable_area_c*( facing_direction + year_built ) + floor_level + decoration + floor_plan  + (1 | community_name) + (1|year) + (1|month))
summary(model23)
tab_model(model23) # nonsense or small interactions

anova(model12, model23) 

str(housing)

model24 <- lmer(data = housing, price_per_sqmt ~ livable_area_c*( decoration + facing_direction ) + floor_level + year_built + floor_plan  + (1 | community_name) + (1|year) + (1|month))
summary(model24)
tab_model(model24) # small interactions 

anova(model12, model24) 

model25 <- lmer(data = housing, log_price_per_sqmt ~ livable_area_c*( decoration + facing_direction ) + floor_level + year_built + floor_plan  + (1 | community_name) + (1|year) + (1|month))
summary(model25)
tab_model(model25) # small interactions 


model26 <- lmer(data = housing, price_per_sqmt ~ livable_area_c*( decoration + facing_direction ) + floor_level*floor_plan + year_built + (1 | community_name) + (1|year) + (1|month))
summary(model26)
tab_model(model26) # small interactions 

model27 <- lmer(data = housing, log_price_per_sqmt ~ livable_area_c*( decoration + facing_direction ) + floor_level*floor_plan + year_built + (1 | community_name) + (1|year) + (1|month))
summary(model27)
tab_model(model27) # small interactions 

# Dotplot
#################################################
dotplot(ranef(model14, condVar = TRUE))

###Bayesian Approach
#################################################

#Model conducted
#################
model_bayes <- brm(price_per_sqmt ~ living_area + year_built + floor_plan + 
                     facing_direction + floor_level + decoration + (1 | community_name) + 
                     (1|year) + (1|month), data=housing)
summary(model_bayes)
head(predict(model_bayes))
dim(posterior_samples(model_bayes))
#head(posterior_samples(model_bayes))

library(broom.mixed)
tidy(model_bayes)
tidy(model_bayes, parameters = "^sd_", conf.int = FALSE)
tidy(model_bayes, effects = "fixed", conf.method="HPDinterval")

#visualize results: plot estimated OR's with 2 SD error bars ea. side
plot_model(model_bayes,show.values = TRUE)
plot_model(model_bayes,type="pred")
p <- plot_model(model_bayes,type="re",show.values = TRUE,ri.nr = c(1,2))

#Model for next run
#################
model_bayes1 <- brm(log_price_per_sqmt ~ living_area + year_built + floor_plan + 
                    facing_direction + floor_level + decoration + (1 | community_name) + 
                    (1|year) + (1|month), data=housing,iter = 4000, chains = 4,
                    control = list(adapt_delta = 0.995, max_treedepth = 20))
summary(model_bayes1)
head(predict(model_bayes1))
dim(posterior_samples(model_bayes1))

#################################################
# Model Assumptions Checking
#################################################

res <- residuals(model14,"resp")

pred <- predict(model14)

#Linearity
#################################################
ggplot(housing, aes(living_area, y=res)) +
  geom_point(alpha = .5,colour="blue3") +
  geom_line(y=0, col="red3") +
    xlab("living_area") +
    ylab("Residuals") +
  labs(caption="Residuals vs living_area") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20))


ggplot(housing, aes(year_built, y=res)) +
  geom_point(alpha = .5,colour="blue3") +
  geom_line(y=0, col="red3") +
  xlab("livable_area") +
  ylab("Residuals") +
  labs(caption="Residuals vs year built") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20))

# Independence and Equality of Variance
#################################################
pred_res <- data.frame(pred, res)
var_plot <- ggplot(pred_res, aes(pred, y=res)) +
  geom_point(alpha = .5,colour="blue3") + 
  geom_line(y = 0, col = "red3") + 
  geom_smooth(method="loess",col="red3") + 
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(caption="Residuals vs Fitted values") + 
  theme(plot.caption = element_text(hjust = 0.5, size = 20))
var_plot

# Normality
#################################################
std_res <- (res - mean(res)) / sd(res)
std_res_df <- data.frame(std_res)
q_plot <- qplot(sample = std_res, data = std_res_df, color=I("blue3"), alpha=.5) +
  geom_abline(intercept = 0, slope = 1, col="red3") + 
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") + 
  labs(caption="Normal Q-Q") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20), legend.position = "none") 
q_plot

# Check outlier with high influencial
#################################################
cook <- cooks.distance(model12)
lev <- hatvalues(model12)
cookd <- data.frame(lev, std_res, cook)
ggplot(cookd, aes(lev, std_res)) + 
  geom_point(aes(size=cook), col="blue3", alpha=.5) + 
  geom_smooth(method="loess", col="red3") +
  xlab("Leverage") +
  ylab("Standardized Residuals") + 
  labs(caption="Standardized Residuals vs Leverage") + 
  theme(plot.caption = element_text(hjust = 0.5, size = 20))
