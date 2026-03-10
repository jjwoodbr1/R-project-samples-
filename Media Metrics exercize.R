install.packages("effects")
install.packages("ggpubr")
install.packages("ggcorrplot")
install.packages("ggridges")
install.packages("seasonal")
install.packages("seasonalview")
install.packages("lubridate")
library(effects)
library(readxl)
library(ggplot2)
library(dbplyr)
library(xlsx)
library(aod)
library(dplyr)
library(ggpubr)
library(readxl)
library(ggcorrplot)
library(ggridges)
library(tseries)
library(forecast)
library(seasonal)
library(seasonalview)
library(lubridate)


ad_ops <- read_excel("C:/Users/jvwoo/OneDrive/Documents/Employment/Work Case Study/Performance_Data.xlsx")
#Summary stats (Outliars, differences between means and medians, wide range, indicates heavy skewness)
view(ad_ops)
head(ad_ops)
summary(ad_ops)
range(ad_ops$Impressions)
#missing data check 
ad_ops_df <- data.frame(ad_ops)
sapply(ad_ops_df, function(x) sum(is.na(x)))

# Number unique values by variable 
length(unique(ad_ops$`Placement ID`))
unique(ad_ops$`Placement ID`)
length(unique(ad_ops$Campaign))
unique(ad_ops$Campaign)
#quantitative data correlations
ad_ops_corr <- ad_ops %>% 
  as.data.frame %>% 
   select( Impressions, Clicks, `Active View: Viewable Impressions`, `Active View: Measurable Impressions`, `Active View: Eligible Impressions`, 
         PageViews, EmailCapture, Transactions, Revenue) %>%
  as.matrix %>%
    cor()

ggcorrplot(ad_ops_corr)

## Number of zeros for our quantitative variables? (High percentage of variables)
ad_ops_quant <- ad_ops %>% 
  as.data.frame %>% 
  select( Impressions, Clicks, `Active View: Viewable Impressions`, `Active View: Measurable Impressions`, `Active View: Eligible Impressions`, 
          PageViews, EmailCapture, Transactions, Revenue) 

(colSums(ad_ops_quant==0)/nrow(ad_ops_quant))*100 
#boxplot for our quantitative variables (high outliar skew)
boxplot(ad_ops_quant, col = rainbow(ncol(ad_ops_quant)))

## Scatterplot revenue by Transactions (indicates something is off)
ggscatter(ad_ops, x = "Transactions", y = "Revenue", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Transactions", ylab = "Revenue")
###color coded by country 
ggplot(ad_ops, aes(x = Transactions , y = Revenue, col = Placement_Country)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula= y ~ log(x),  se = F, aes(group = 1), col = "black") + 
  theme_minimal()

#normality graph check (both fail)
ggplot(data = ad_ops, aes(sample = Revenue)) +
  stat_qq() +
  stat_qq_line()
ggplot(data = ad_ops, aes(sample = Transactions)) +
  stat_qq() +
  stat_qq_line()
# revenue visualizations and histogram 
ggplot(data = ad_ops) +
  geom_histogram(mapping = aes(x = Revenue)) +
  labs(x = "Revenue")

boxplot(ad_ops$Revenue, 
        main = "Revenue Boxpolot",
        xlab = "Revenue",
        ylab = "Amount",
        col = "orange",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)

#revenue by campaign ridgeplot 
ggplot(ad_ops, aes(x = Revenue, y = Campaign, fill = Campaign)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
# Categorical visualizations 
ggplot(ad_ops_df, aes(x=Campaign,y=Revenue, fill=Creative_Country))+geom_boxplot() 
  
# Time Series & Anomaly detection (Page views)
##categorical variables 
unique(ad_ops$Campaign)
unique(ad_ops$Partner)
unique(ad_ops$Placement_CostModel)
unique(ad_ops$Placement_Country)
unique(ad_ops$Placement_TargetingStrategy)
unique(ad_ops$Placement_AdSize)
unique(ad_ops$Creative_AdSize)
unique(ad_ops$Creative_Color)
unique(ad_ops$Creative_Animation)
unique(ad_ops$Creative_Country)
# Choosing the data granularity level 
ts_ad_ops <- ad_ops_df %>%
  filter(Campaign=="2022_US_Evergreen-Brand") %>%
  filter(Campaign.ID=="27139565") %>%
  filter(Placement_CostModel=="dCPM") %>%
  filter(Partner=="Media Partner - 2") %>%
  filter(Placement.ID=="328398207") %>%
  filter(Placement_TargetingStrategy=="Affinity") %>%
  filter(Creative_AdSize=="160x600") %>%
  filter(Creative.ID=="167196934")
  filter(Creative_Type=="StandardBanner") %>% 
  filter(Creative_Animation=="Static") %>%
  filter(Creative_Color=="Aqua") %>%
  group_by(Date) %>%
  arrange(Date)
  
  ggplot(data = ts_ad_ops, aes(x = Date, y = PageViews)) +
    geom_point() + geom_smooth() +
    labs(x = "Date",
         y = "Page Views",
         title = "Page Views over time")

ts_ad_ops2 <- ts_ad_ops[, c("Date", "PageViews")]

#Page_Views <- ts(ts_ad_ops2$PageViews, start =  c(ts_ad_ops2$Date), frequency = 365)
#plot.ts(Page_Views)

#Linear Regression: 
#(Would not do, assumptions violated 1. Multicollinearity 2. Non-normality among residents 3. Linear relationship, but I thought would be fun!) 
summary(ad_ops_quant)
rev_model <- lm(Revenue ~ Impressions + Clicks + PageViews +
                  EmailCapture + Transactions, data = ad_ops_quant)
plot(rev_model)
