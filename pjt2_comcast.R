library(stringi)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
comcast_data<- read.csv("Comcast Telecom Complaints data.csv",header = TRUE)
View(comcast_data)
#Manipulating column names
names(comcast_data)<- stri_replace_all(regex =  "\\.",replacement = "",names(comcast_data))
View(comcast_data)
comcast_data$Date<- dmy(comcast_data$Date)
monthly_count<- summarise(group_by(comcast_data,Month =as.integer(month(Date))),Count = n())
daily_count<- summarise(group_by(comcast_data,Date),Count =n())

monthly_count<-arrange(monthly_count,Month)

library(plotly)

# Create the ggplot objects for monthly and daily ticket counts
monthly_plot <- ggplot(data = monthly_count, aes(Month, Count, label = Count)) +
  geom_line() +
  geom_point(size = 0.8) +
  geom_text() +
  scale_x_continuous(breaks = monthly_count$Month) +
  labs(title = "Monthly Ticket Count", x = "Months", y = "No. of Tickets") +
  theme(plot.title = element_text(hjust = 0.5))

daily_plot <- ggplot(data = daily_count, aes(as.POSIXct(Date), Count)) +
  geom_line() +
  geom_point(size = 0.8) +
  scale_x_datetime(breaks = "1 weeks", date_labels = "%d/%m") +
  labs(title = "Daily Ticket Count", x = "Days", y = "No. of Tickets") +
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))

# Combine the ggplot plots into a single plotly plot
combined_plot <- subplot(monthly_plot, daily_plot, nrows = 2)
combined_plotly <- ggplotly(combined_plot)

# Display the plotly plot
combined_plotly
# Complaint Type Processing
network_tickets<- contains(comcast_data$CustomerComplaint,match = 'network',ignore.case = T)
internet_tickets<- contains(comcast_data$CustomerComplaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(comcast_data$CustomerComplaint,match = 'bill',ignore.case = T)
email_tickets<- contains(comcast_data$CustomerComplaint,match = 'email',ignore.case = T)
charges_ticket<- contains(comcast_data$CustomerComplaint,match = 'charge',ignore.case = T)

comcast_data$ComplaintType[internet_tickets]<- "Internet"
comcast_data$ComplaintType[network_tickets]<- "Network"
comcast_data$ComplaintType[billing_tickets]<- "Billing"
comcast_data$ComplaintType[email_tickets]<- "Email"
comcast_data$ComplaintType[charges_ticket]<- "Charges"

comcast_data$ComplaintType[-c(internet_tickets,network_tickets,
                              billing_tickets,charges_ticket,email_tickets)]<- "Others"

table(comcast_data$ComplaintType)
open_complaints<- (comcast_data$Status == "Open"| comcast_data$Status =="Pending")
closed_complaints<-(comcast_data$Status == "Closed"| comcast_data$Status =="Solved")
comcast_data$ComplaintStatus[ open_complaints]<-"Open" 
comcast_data$ComplaintStatus[closed_complaints]<- "Closed" 

library(plotly)

comcast_data <- group_by(comcast_data, State, ComplaintStatus)
chart_data <- summarise(comcast_data, Count = n())

ggplot_chart <- ggplot(as.data.frame(chart_data), mapping = aes(State, Count)) +
  geom_col(aes(fill = ComplaintStatus), width = 0.95) +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16, colour = "#0073C2FF"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Ticket Status Stacked Bar Chart",
       x = "States", y = "No of Tickets",
       fill = "Status")

# Convert ggplot chart to plotly
plotly_chart <- ggplotly(ggplot_chart)

# Display the plotly chart
plotly_chart

#State having max unresolved complaints
comcast_data %>% filter(ComplaintStatus=='Open') %>% group_by(State) %>% summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))


resolved_data <- group_by(comcast_data,ComplaintStatus)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 
resolved_data <- group_by(comcast_data,ReceivedVia,ComplaintStatus)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data)))

            
par(mfrow = c(1,2))
total<-ggplot(total_resloved,
              aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+  
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
    theme_classic()+theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
  
 
library(ggpubr)


# Pie Chart for Category wise Ticket Status
category <- ggplot(Category_resloved, aes(x = "", y = percentage, fill = ComplaintStatus)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(ReceivedVia, "-", round(percentage * 100), "%")), 
            position = position_stack(vjust = 0.5)) +
    theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Arrange and display the pie charts
ggarrange(total, category, nrow = 1, ncol = 2)

total
 category
 chart_arrange <- ggarrange(total, category, nrow = 1, ncol = 2)
 chart_arrange


