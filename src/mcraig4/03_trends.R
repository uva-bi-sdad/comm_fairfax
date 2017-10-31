library(rio)
library(ggplot2)
library(dplyr)

BMI <- import('data/comm_fairfax/working/ATUS_data_working/ATUS state year files/Avg_BMI.csv')
VA_BMI <- BMI[BMI$state == 'virginia', ]
head(BMI)
tail(BMI)


by_year <- BMI %>% group_by(year)

avg <- by_year %>% summarise(
    value = mean(value)
)

BMI_Trends <- ggplot() +
    geom_line(data = avg, aes(year, value, color = 'National'), size = 4) +
    geom_line(data = VA_BMI, aes(year, value, color = 'VA_BMI'), size = 4) +
    labs(title = 'VA vs National BMI', colour = "Geographical Level", x = "Year", y = "BMI Value") +
    theme(axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 12),
          title = element_text(size = 20),
          axis.title = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))


plot(BMI_Trends)

jpeg('output/mcraig4/BMI_Trends.jpeg', width = 1200, height = 800, quality = 100)
plot(BMI_Trends)
dev.off()
