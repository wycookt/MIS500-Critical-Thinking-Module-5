install.packages("dplyr")


#Reading From .CSV
data <- read.csv("Restaurant_Scores.csv", header = TRUE, sep=",")

#Data Frame

scores_risk<-data.frame(
		scores=(data$inspection_score),
		risk=(data$risk_category)
		)
#Summary of Data
library(dplyr)

group_by(scores_risk, risk) %>%

  summarise(

    count = n(),

    mean = mean(scores, na.rm = TRUE),

    sd = sd(scores, na.rm = TRUE)

  )

summary(scores_risk$scores)

#ANOVA Test
res.aov <- aov(scores ~ risk, data = scores_risk)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)

# Box plot
boxplot(scores ~ risk, data = scores_risk,
        xlab = "Risk", ylab = "Scores",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07", "#2BCD47"))
