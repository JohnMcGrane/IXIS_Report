# Load tidyverse
library(tidyverse)
library(openxlsx)

# Import data 
data <- read_csv('/Users/johnfox/Desktop/sessionCounts.csv')
cartdata <- read_csv('/Users/johnfox/Desktop/addsToCart.csv')

# Visualize dataframe
head(data,20)

# Convert dim_date column to formatted so we can group by month
data$dim_date <- as.Date(data$dim_date, format = "%m/%d/%y")

# Create new column Month from dim_date column using mutate function. 
# We can then use this new column to group by month
# Year included for clarity, though not needed. 
data <- mutate(data,Month = format(dim_date, paste("%Y","%m")))


# Group data by month using Month column
# Summarize data by total sessions, transactions, quantity, & ECR
grouped_data <- group_by(.data = data, Month, dim_deviceCategory) %>% 
  summarize(sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY),
            ECR = mean(transactions/sessions))

# Visualize data with barchart
ggplot(data=grouped_data, aes(x=Month, y=QTY, fill=dim_deviceCategory)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ylab("Quantity Sold") +   
  scale_fill_manual("Device",values=c("orange","red",'blue')) + 
  scale_x_discrete(labels=c("2012 07" = "July 2012","2012 08" = "August 2012",
                            "2012 09" = "September 2012","2012 10" = "October 2012",
                            "2012 11" = "November 2012","2012 12" = "December 2012",
                            "2013 01" = "January 2013","2013 02" = "February 2013",
                            "2013 03" = "March 2013","2013 04" = "April 2013",
                            "2013 05" = "May 2013","2013 06" = "June 2013")) + 
  theme(text = element_text(size = 20)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


# View summarized data
head(grouped_data,36)

##############################################################


# Summarize data by month only
newdata1 <- group_by(.data = data, Month) %>% 
  summarize(sessions = sum(sessions),
            transactions = sum(transactions),
            QTY = sum(QTY),
            ECR = mean(transactions/sessions))

# Add the addsToCart data as a new column
newdata <- cbind(newdata1,cartdata)

month_over_month <- newdata %>%
mutate(
  momtrans = (transactions - lag(transactions)) / lag(transactions),
  momqty = (QTY - lag(QTY)) / lag(QTY),
  momecr = (ECR - lag(ECR))/lag(ECR),
  momadds = (addsToCart-lag(addsToCart))/lag(addsToCart),
  momsessions = (sessions-lag(sessions))/lag(sessions),
  pos = (momadds >= 0)
)

#month_over_month visualization
ggplot(month_over_month[2:12,], aes(x=Month, y=momadds, fill = pos)) + 
  geom_col(position="dodge") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("darkred", "darkgreen"), guide = 'none') + 
  theme(text = element_text(size = 20)) + 
  ylab("Percent Change\nMonthly Cart Adds") +  
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  scale_x_discrete(labels=c("2012 07" = "July 2012","2012 08" = "August 2012",
                            "2012 09" = "September 2012","2012 10" = "October 2012",
                            "2012 11" = "November 2012","2012 12" = "December 2012",
                            "2013 01" = "January 2013","2013 02" = "February 2013",
                            "2013 03" = "March 2013","2013 04" = "April 2013",
                            "2013 05" = "May 2013","2013 06" = "June 2013")) 

# Add left_in_cart column and qtysoldpersession columns
newmetrics <- mutate(newdata, left_in_cart = (addsToCart - QTY), 
               qtypersesh = QTY/sessions)

# Data visualiation qty per session with bar chart
ggplot(data=newmetrics, aes(x=Month, y=qtypersesh)) +
  geom_bar(stat="identity",fill = "darkgreen") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ylab("Quantity Sold Per Session") +   
  scale_fill_manual("Device",values=c("orange","red",'blue')) + 
  scale_x_discrete(labels=c("2012 07" = "July 2012","2012 08" = "August 2012",
                            "2012 09" = "September 2012","2012 10" = "October 2012",
                            "2012 11" = "November 2012","2012 12" = "December 2012",
                            "2013 01" = "January 2013","2013 02" = "February 2013",
                            "2013 03" = "March 2013","2013 04" = "April 2013",
                            "2013 05" = "May 2013","2013 06" = "June 2013")) + 
  theme(text = element_text(size = 20)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Visualize decrease in adds to cart by month, fit linear trendline
ggplot(newmetrics, aes(x=seq(1, 12, by=1), y = addsToCart) ) +
  geom_point(color = "black",size = 4) +
  geom_smooth(method = "lm", se = FALSE,color = "darkgreen")+
  coord_cartesian(ylim=c(0.00,250000)) + 
  theme_linedraw() + 
  xlab("Month") + 
  ylab("Cart Additions") +
  scale_x_discrete(limits=c("July 2012","August 2012","September 2012","October 2012",
                            "November 2012","December 2012",
                            "January 2013","February 2013",
                            "March 2013","April 2013",
                            "May 2013","June 2013"))+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+ 
  theme(text = element_text(size = 20))

# Extract only the last two months in the dataset for the May-June comparison
two_months_only <-newmetrics[11:12,]

# Take the transpose of the dataframe and label rows as old column names
# Courtesy of https://gist.github.com/csrvermaak/1825978b20a185b80f8c
two_months_named = setNames(data.frame(t(two_months_only)), two_months_only[,1])

# Get rid of data that is not useful (i.e. Month, dim_year, dim_month)
two_months_clean <- two_months_named[-c(1, 6, 7), ]

# Rename the columns so they are more readable
two_months <- setNames(two_months_clean,c("May","June"))

# Calculate absolute difference, relative difference, and percent change
# Have to change chr data types to numeric as well
totals <- mutate(two_months, Absolute_Diff = (as.numeric(June)-as.numeric(May)), 
                   Relative_Diff = (as.numeric(June)-as.numeric(May))/as.numeric(May),
                   Percent_Change = (as.numeric(June)-as.numeric(May))/as.numeric(May)*100,
                 June = as.numeric(June),
                 May = as.numeric(May),
                 pos = Relative_Diff >= 0)

# Visualize month over month relative difference data with barchart
ggplot(data=totals, aes(x=row.names(totals), y=Relative_Diff,fill = pos)) + 
  geom_bar(stat="identity", position=position_dodge()) + theme_linedraw() + 
  ylab("Percent Change\nMay to June 2013") + 
  scale_x_discrete(labels=c("addsToCart" = "Cart\nAdds", "ECR" = "Ecommerce\nConversion\nRatio",
                            "QTY" = "Quantity","qtypersesh"='QTY Per\nSession','left_in_cart' = "Left In Cart","sessions" = "Sessions",'transactions'='Transactions'))+
  scale_fill_manual(values = c("darkred", "darkgreen"), guide = 'none') + 
  theme(text = element_text(size = 20)) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.title.x = element_blank())

# Create workbook
work_book <- createWorkbook()

# Add two sheets to the workbook
addWorksheet(work_book, sheetName = "Sheet 1")
addWorksheet(work_book, sheetName = "Sheet 2")

# Write data to the workbooks
writeData(work_book, "Sheet 1", grouped_data,rowNames = TRUE)
writeData(work_book, "Sheet 2", totals,rowNames = TRUE)

# Save the workbook to the present working directory Desktop
saveWorkbook(work_book, paste(getwd(),"/Desktop/IXIS.xlsx", sep=""), 
             overwrite = TRUE, returnValue = TRUE)

















