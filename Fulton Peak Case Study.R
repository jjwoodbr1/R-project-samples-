#  mf_RentStudioMean 
#mf_RentSameStoreStudioMean1YearChange 
#mf_RentSameStoreStudioMean1YearChange 
#mf_RentSameStore3BedPlusMean
#mf_RentSameStore3BedPlusMeanQuarterlyChange 
#mf_RentSameStore3BedPlusMean3YearChange 


install.packages("effects")
library(effects)
library(readxl)
library(ggplot2)
library(dbplyr)
library(xlsx)
library(aod)


library(readxl)
fulton_case_study <- read_excel("C:/Users/jvwoo/OneDrive/Documents/Employment/Work Case Study/Fulton_Peak_Raw_Data.xlsx")
view(fulton_case_study)
head(fulton_case_study)

summary(fulton_case_study)

fulton_DF <- data.frame(fulton_case_study)
sapply(fulton_DF, function(x) sum(is.na(x)))

pct_miss(fulton_DF)

gg_miss_var(fulton_DF, show_pct = TRUE)

fulton_top_50 <- read_excel("C:/Users/jvwoo/OneDrive/Documents/Employment/Work Case Study/Fulton_top_50.xlsx")

fulton_top_50 <- fulton_top_50 %>%
  rename(number_of_units = `# of Units` )

ggplot(data = fulton_top_50, mapping = aes(x = number_of_units)) + labs(x = "number of multi-family units") +
  geom_histogram(aes(y=..density..), alpha=0.5) + geom_density( alpha = 0.2)

boxplot(fulton_top_50$number_of_units, 
        main = "Top 50 zipcodes number of apartment units",
        xlab = "MF Units",
        ylab = "Zipcode",
        col = "orange",
        border = "black",
        horizontal = TRUE,
        notch = FALSE)


