library(dplyr)

##Change working directory using setwd() to the folder where you have the input files
setwd("C:/Users/Shuoshuo Wang/Desktop/Op 70/HW_3")

##Read in the files into data frames named according to the instructions (in principle you could name them anything you wanted,
##as long as you kept track, but for now, just keep the names as below)
weekly_sales_df <- read.csv('sales_order_weekly.csv')
item_df <- read.csv('item_master.csv')
inv_df <- read.csv('inventory_weekly_summary.csv')

####Question 1####

##For each item for each week, add up all of the units sold for the different sales orders for that item during that week
item_week_summary <-  
  weekly_sales_df %>%
  group_by(ITEM,WEEK) %>% 
  summarize(sum(UNITS_SOLD))

##Given the sales for each item in each week, take the average across weeks for each item, and sort in decreasing order
item_week_average <- 
  item_week_summary %>%
  group_by(ITEM) %>% 
  summarize(AVG = mean(`sum(UNITS_SOLD)`)) %>% 
  arrange(desc(AVG))

##Since we sorted in decreasing order, the item with the highest average weekly unit sales will be in the first row
##(although to be safe, we should inspect the data frame by clicking on item_week_average in the "Environment" at top right
##and make sure there are no ties)


##Add up the averages for all of the items to get the average total units sold
sum(item_week_average$AVG)


####Question 2####

##Calculate sales for each week in separate data frames, rename columns.
##This part is a bit more advanced than what I'm teaching you to do in R, 
##so don't worry, I do not and will not expect you to know how to do this.
##However, you will need to have correctly generated item_week_summary in Question 1
##for this to work
week_1_sales <- item_week_summary %>% filter(WEEK == 1) 
week_1_sales <- week_1_sales[c(1,3)]
colnames(week_1_sales) <- c("ITEM", "WEEK_1_UNITS_SOLD")
week_2_sales <- item_week_summary %>% filter(WEEK == 2)
week_2_sales <- week_2_sales[c(1,3)]
colnames(week_2_sales) <- c("ITEM", "WEEK_2_UNITS_SOLD")
week_3_sales <- item_week_summary %>% filter(WEEK == 3)
week_3_sales <- week_3_sales[c(1,3)]
colnames(week_3_sales) <- c("ITEM", "WEEK_3_UNITS_SOLD")
week_4_sales <- item_week_summary %>% filter(WEEK == 4)
week_4_sales <- week_4_sales[c(1,3)]
colnames(week_4_sales) <- c("ITEM", "WEEK_4_UNITS_SOLD")
week_5_sales <- item_week_summary %>% filter(WEEK == 5)
week_5_sales <- week_5_sales[c(1,3)]
colnames(week_5_sales) <- c("ITEM", "WEEK_5_UNITS_SOLD")

##Join data frames together for the different weeks.
##You can really see how the pipe operator helps here. Without it this would either  
##be a very ugly nested statement, or we would have to do it in several steps
all_weeks_sales <- week_1_sales %>% full_join(week_2_sales, by = "ITEM") %>% 
                                    full_join(week_3_sales, by = "ITEM") %>% 
                                    full_join(week_4_sales, by = "ITEM") %>% 
                                    full_join(week_5_sales, by = "ITEM")

##Replace blanks with zeros so that the average calculates correctly
all_weeks_sales[is.na(all_weeks_sales)] <- 0

##For each item, take the average weekly unit sales over the five week period. Since the values are in
##different columns, we can't use the mean function here. Instead, 
## create a new column defined by just adding Week 1 Sales + Week 2 Sales +... (using the appropriate formulas and '$' references) and then dividing the total by 5
##to get the average
all_weeks_sales <-  all_weeks_sales %>% 
                    ungroup() %>% 
                    mutate(Total_Week_Sales = rowSums(.[2:6])/5)

##Add up the averages for all of the items to get our final answer
sum(all_weeks_sales$Total_Week_Sales)

####Question 3####
##Group by ITEM and CUSTOMER_NUMBER, add up the units sold for each item-customer pair, and then sort in decreasing order
Q3 <-  weekly_sales_df %>% 
  group_by(ITEM,CUSTOMER_NUMBER) %>%
  summarize(Total = sum(UNITS_SOLD)) %>% 
  arrange(desc(Total))

####Question 4####
##Group by item and take average to get average inventory level, then sort in decreasing order
avg_inv_levels <-  inv_df %>%
                   group_by(ITEM) %>% 
                   summarize(Average_Inv_Levels = sum(UNITS_ON_HAND)/5) %>% 
                   arrange(desc(Average_Inv_Levels))

##Since we sorted in decreasing order, the item with the highest average weekly unit sales will be in the first row
##(although to be safe, we should inspect the data frame by clicking on item_week_average in the "Environment" at top right
##and make sure there are no ties)
(top_inv_item <- avg_inv_levels[1,])

##Add up the averages for all of the items to get the average total units sold
(sum(avg_inv_levels$Average_Inv_Levels))


####Question 5####

##Join item_df and avg_inventory_level using the ITEM fields in both data frames as the key
Combined_data <-  item_df %>% full_join(avg_inv_levels, by = "ITEM")

##Calculate average inventory value using the new, merged data frame by multiplying the inventory values by the
##average inventory levels
Avg_Inv_Value <-  Combined_data[,4] * Combined_data[,6]


##Add up all of the average inventory values to get the total AIV across all items
(Total_AIV <- sum(Avg_Inv_Value))


####Question 6####

##Calculate total sales for each item in a data frame called item_total_sales
item_total_sales <-  weekly_sales_df %>% group_by(ITEM) %>% summarize(Total_Sales = sum(UNITS_SOLD))

##Join item_total_sales with the previously created item_df_with_inventory
item_df_all <-  Combined_data %>% full_join(item_total_sales, by = "ITEM")

##Calculate COGS and Dollars_Sold by multiplying the appropriate columns
item_df_all$COGS <- item_df_all[,4] * item_df_all[,7]
item_df_all$Dollars_Sold <- item_df_all[,5] * item_df_all[,7]

##Compute gross margin by subtracting COGS from Dollars Sold
item_df_all$Gross_Margin <- item_df_all$Dollars_Sold - item_df_all$COGS

##Add up gross margin for all items to get our answer
(sum(item_df_all$Gross_Margin))


####Question 7####

##Calculate gross margin by category.
item_df_all %>% 
  group_by(CATEGORY) %>% 
  summarize(Gross_Margin_By_Cat = sum(Gross_Margin)) %>% 
  filter(CATEGORY == "Dairy")

####Question 8####

##Calculate overall GMROI by adding up the gross margins and inventories, and dividing one by the other
(GMORI <-  sum(item_df_all$Gross_Margin)/Total_AIV)
