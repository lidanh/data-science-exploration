setwd("~/Projects/bgu/funnel_analysis")

# Download dataset
datasetFile <- "funnel_data.csv"
datasetUrl <- "https://raw.githubusercontent.com/lidanh/data-science-exploration/master/funnel_data.csv"

if (file.exists(datasetFile)) {
  file.remove(datasetFile)
}

download.file(datasetUrl, destfile = datasetFile, method = "curl")

# === Load Dataset ===
funnel <- read.csv(datasetFile)

# === Basic Exploration ===
summary(funnel)
head(funnel)

# === Graphs ===
library(ggplot2)
library(reshape2)

# 1. Basic funnel analysis
boxplot(funnel$new_items, 
        funnel$total_sent, 
        funnel$payment_started, 
        funnel$payment_finished, 
        funnel$paid_offline, 
        main="Funnel Steps Analysis",
        ylab ="Total", xlab ="Funnel Steps",
        names = c("New Items", "Total Sent", "Payment Started", "Payment Finished", "Paid Offline"),
        col = c("#459fed", "#32babc", "#32b76c", "#a0138e", "#faa030"))



# 2. Visualizing Funnel (group by date)
visualize_funnel <- function(title, steps) {
  total_by_day <- aggregate(steps, by = list(Date = funnel$date), FUN = sum)
  melted_totals <- melt(total_by_day, id = "Date", variable.name = "Step")
  ggplot(melted_totals, aes(x = Date, y = value, colour=Step, group = 1)) + 
    geom_smooth() +
    geom_line() +
    ylab(label="Total") + 
    xlab("Date") +
    ggtitle(title) +
    theme(
      axis.text.x=element_blank(), 
      plot.title = element_text(size=24, face="bold"))
}

# User end: new + sent
visualize_funnel("Users Funnel", list(New = funnel$new_items, Sent = funnel$total_sent))

# User of user end: start pay + finish pay
visualize_funnel("Users of Users Funnel", list(Start_Payment = funnel$payment_started, Finish_Payment = funnel$payment_finished))



# 3. Funnel Steps Correlations
library(corrplot)
M <- cor(subset(funnel, select = c("new_items", "total_sent", "payment_started", "payment_finished", "paid_offline")))
corrplot(M, method = "ellipse")



# 4. New-sent correlation
users_funnel_corr <- cor(funnel$new_items, funnel$total_sent)
ggplot(funnel, aes(x = new_items, y = total_sent, group = 1)) +
  geom_point(shape=16, color = "#0072B2") +
  geom_smooth(method=lm, se=FALSE, color="red", formula = y ~ x) +
  ylab(label="Sent Items") + 
  xlab("New Items") +
  ggtitle(paste("Create vs. Send Correlation: ", users_funnel_corr)) +
  theme(plot.title = element_text(size=24, face="bold"))



# 5. Histogram of starting payment by hour
avg_by_hour <- function(title, steps) {
  avg_by_hour_value <- aggregate(steps, by = list(Hour = funnel$hour), FUN = mean)
  melted_totals <- melt(avg_by_hour_value, id = "Hour", variable.name = "Step", stat="identity")
  ggplot(melted_totals, aes(x = Hour, y = value)) + 
    geom_bar(stat="identity", fill="#733ca6") +
    ylab(label="Avg.") + 
    xlab("Hour") +
    ggtitle(title) +
    stat_smooth(color = "#faa030", se=FALSE) + 
    theme(plot.title = element_text(size=24, face="bold"))
}

avg_by_hour("Avg. Payments by Hour", funnel$payment_started)
