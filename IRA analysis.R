library(readxl)
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)
library(quantmod)

roth_df <- excel_sheets("Finance/Stocks/Roth Contributory IRA-Positions-2026-02-18-112103.xlsx") |>
  map_dfr(~ read_excel("Finance/Stocks/Roth Contributory IRA-Positions-2026-02-18-112103.xlsx", sheet = .x), .id = "sheet")
#sheet 1 
df1 <- df[roth_df$sheet == 1, ]
df1 <- df1[, 2:4]
#sheet 2
df2 <- df[roth_df$sheet == 2, ]
df2 <- df2 %>%
  select(where(~ any(!is.na(.))))
df2 <- df2[, -1]
#sheet 3 
df3 <- df[roth_df$sheet == 3, ]
df3 <- df3 %>%
  select(where(~ any(!is.na(.))))
df3 <- df3[, -1]
colnames(df3) <- df3[2, ]
rownames(df3) <- NULL
df3 <- df3[-c(1,2), ]

df3 <- df3 %>%
  left_join(df1[, c("Row Labels", names(df1)[3])],
            by = c("Symbol" = "Row Labels"))
df3 <- df3[, -ncol(df3)]
names(df3)[ncol(df3)] <- "Sector"
#Renaming Columns 
names(df3)[12] <- "Total Gain %"
names(df3)[8] <- "Market_Value"
names(df3)[19] <-"PE_Ratio"
names(df3)[7] <- "Market_Value_Dollars"
names(df3)[20] <- "52_Week_High"
names(df3)[21] <- "52_Week_Low"
names(df3)[5] <- "Quantity"
names(df3)[2] <- "Price Change %"
names(df3)[17] <- "Dividend Yield"
names(df3)[16] <- "% of Account"
names(df3)[6] <- "Price Change $"
df3$`Market_Value` <- as.numeric(gsub("[,$]", "", df3$`Market_Value`))
df3$`% of Account` <- as.numeric(gsub("%", "", df3$`% of Account`)) / 100
#Gain by sector
df_plot <- df3[1:(nrow(df3)-2), ]
sector_gain <- df_plot %>%
  group_by(Sector) %>%
  summarise(
    total_gain = sum(as.numeric(gsub("%","", `Total Gain %`)), na.rm = TRUE)
  )

ggplot(sector_gain, aes(x = reorder(Sector, total_gain), y = total_gain)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Total Gain by Sector (Roth IRA 2026)",
    x = "Sector",
    y = "Total Gain (%)"
  )

#portfolio by sector 

df3 %>%
  group_by(Sector) %>%
  summarise(total_value = sum(`Market_Value`, na.rm = TRUE))
#average PE
df_PE <- df3 %>%
  mutate(PE_Ratio = as.numeric(PE_Ratio))
df_PE <- df_PE %>%
  filter(!is.na(PE_Ratio) & PE_Ratio != "")
mean(df_PE$PE_Ratio, na.rm = TRUE) 
#Our Average PE Ratio across our portfolio is 27.88067 


#Prep for analysis 
clean_currency <- function(x) {
  as.numeric(gsub("[$,]", "", x))
}

clean_percent <- function(x) {
  as.numeric(gsub("%", "", x)) / 100
}

clean_number <- function(x) {
  as.numeric(gsub(",", "", x))
}
df3 <- df3 %>%
  mutate(across(c(1, 2, 5:7, 9:12, 17:21), as.numeric))
df3 <- df3 %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

df3 <- df3 %>%
  mutate(
    `Market_Value_Dollars`    = clean_currency(`Market_Value_Dollars`),
    `Cost Basis`      = clean_currency(`Cost Basis`),
    `Price Change $`  = clean_currency(`Price Change $`),
    Price             = clean_currency(Price),
    `52_Week_High`    = clean_currency(`52_Week_High`),
    `52_Week_Low`     = clean_currency(`52_Week_Low`),
    Quantity          = clean_number(Quantity),
    `PE_Ratio`       = clean_number(`PE_Ratio`),
    `Total Gain %`    = clean_percent(`Total Gain %`),
    `Price Change %`  = clean_percent(`Price Change %`),
    `Dividend Yield`  = clean_percent(`Dividend Yield`),
    `% Of Account`    = clean_percent(`% Of Account`)
  )
mutate(
  Total_Gain_Dollar = `Market_Value_Dollars` - `Cost Basis`,
  Dist_From_52W_High = (Price - `52_Week_High`) / `52_Week_High`,
  Dist_From_52W_Low  = (Price - `52_Week_Low`) / `52_Week_Low`,
  Est_Annual_Dividend_Income = `Market_Value_Dollars` * `Dividend Yield`,
  Weight = `% Of Account`,
  Return_Contribution = Weight * `Total Gain %`
)

  sector_summary <- df3 %>%
    group_by(Sector) %>%
    summarise(
      Market_Value_Dollars = sum(`Market_Value_Dollars`, na.rm = TRUE),
      Cost_Basis = sum(`Cost Basis`, na.rm = TRUE),
      Gain_Dollar = sum(Total_Gain_Dollar, na.rm = TRUE),
      Avg_Gain_Pct = mean(`Total Gain %`, na.rm = TRUE),
      Weighted_Return = weighted.mean(`Total Gain %`, `Market_Value_Dollars`, na.rm = TRUE),
      Dividend_Income = sum(Est_Annual_Dividend_Income, na.rm = TRUE),
      Weighted_PE = weighted.mean(`P/E Ratio`, `Market_Value_Dollars`, na.rm = TRUE),
      Return_Contribution = sum(Return_Contribution, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Market_Value))
  
  sector_summary
  #Gain percentage plot by sector
  sector_gain_percent_sum <- df3 %>%
    group_by(Sector) %>%
    summarise(
      summed_gain_pct = sum(`Total Gain %`, na.rm = TRUE),
      avg_gain_pct = mean(`Total Gain %`, na.rm = TRUE),
      median_gain_pct = median(`Total Gain %`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(summed_gain_pct))
  
  sector_gain_percent_sum
  
  ggplot(sector_gain_percent_sum, aes(x = reorder(Sector, avg_gain_pct), y = avg_gain_pct)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = percent) +
    labs(
      title = "Average Total Gain % by Sector",
      x = "Sector",
      y = "Average Gain %"
    )
# Weighted return by sector 
  sector_weighted_return <- df3 %>%
    group_by(Sector) %>%
    summarise(
      sector_market_value = sum(`Market_Value_Dollars`, na.rm = TRUE),
      weighted_return = weighted.mean(`Total Gain %`, w = `Market_Value_Dollars`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(weighted_return))
  
  sector_weighted_return
#correlation between P/E ratio, yeild ect...
  numeric_subset <- df3 %>%
    select(`Market_Value_Dollars`, `Total Gain %`, `Dividend Yield`, `PE_Ratio`)
  
  cor(numeric_subset, use = "complete.obs")