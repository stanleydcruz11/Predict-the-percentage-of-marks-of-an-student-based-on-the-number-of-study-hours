df <- data.frame(Hours = c("2.5","5.1","3.2","8.5","3.5","1.5","9.2","5.5","8.3","2.7","7.7","5.9","4.5","3.3","1.1","8.9","2.5","1.9","6.1","7.4","2.7","4.8","3.8","6.9","7.8"),
                 Scores = c("21","47","27","75","30","20","88","60","81","25","85","62","41","42","17","95","30","24","67","69","30","54","35","76","86"))
print (df)
View(df)

write.csv(df,"I:/DS/TSP Grip\\TSP1.csv", row.names = FALSE)

df_TSP <- read.csv('TSP1.csv')
getwd()
View(df_TSP)
summary("df_TSP")
str(df_TSP)
install.packages("GGally")
install.packages("tidyverse")
library(GGally)
ggpairs(data=df_TSP, columns=1:2, title="df_TSP")

fit_1 <- lm(Scores ~ Hours, data = df_TSP)
summary(fit_1)

ggplot(data=df_TSP, aes(fit_1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

ggplot(data =df_TSP, aes(x = Hours, y = Scores)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")


predict(fit_1, data.frame(Hours = 9.25))