Sys.setenv(JAVA_HOME="/Applications/Android Studio.app/Contents/jbr/Contents/Home")

install.packages("dplyr")
install.packages("ggpubr")
install.packages("haven")
install.packages("pastecs")
install.packages("xlsx")
install.packages("ggstatsplot")
install.packages("psych")
install.packages("DescTools")
install.packages("systemfonts")
install.packages("hrbrthemes")
install.packages("gginference")
install.packages("apa")

library(tidyr)
library(purrr)
library(haven)
library("xlsx")
library("dplyr")
library("ggpubr")
library("pastecs")
library("rstatix")
library("psych")
library("DescTools")
library(ggplot2)
library(hrbrthemes)
library(gginference)
library(apa)

######################################################
## Dataset
##

dataset <-
  read_sav("./dataset.sav") %>%
  transform(., LHtoFSH = LH / FSH) %>%
  transform(., VitDtoAMH = VIT.D / AMH)

dataset$mens[dataset$mens == 1] <- "Irregular"
dataset$mens[dataset$mens == 2] <- "Regular"

irregular_mens_dataset <-
  dataset %>%
  subset(., mens %in% c("Irregular"))

regular_mens_dataset <-
  dataset %>%
  subset(., mens %in% c("Regular"))

low_ratio_dataset <-
  dataset %>%
  subset(., VitDtoAMH < 5)

high_ratio_dataset <-
  dataset %>%
  subset(., VitDtoAMH >= 5)


######################################################
## Descriptive Stats
##


descriptive <- stat.desc(dataset, basic = TRUE, desc = TRUE, norm = TRUE)
irregular_mens_descriptive <- stat.desc(irregular_mens_dataset, basic = TRUE, desc = TRUE, norm = TRUE)
regular_mens_descriptive <- stat.desc(regular_mens_dataset, basic = TRUE, desc = TRUE, norm = TRUE)

write.xlsx(descriptive, "./descriptive.xlsx", row.names = TRUE, col.names = TRUE, append = FALSE)
write.xlsx(irregular_mens_descriptive, "./irregular-mens-descriptive.xlsx", row.names = TRUE, col.names = TRUE, append = FALSE)


Gmean(dataset$VitDtoAMH, conf.level = 0.95)
harmonic.mean(dataset$VitDtoAMH)
quantile(dataset$VitDtoAMH, na.rm = TRUE)
IQR(dataset$VitDtoAMH, na.rm = TRUE)

d <- function (x, n) {
  x <- na.omit(x)
  hist(x, main=n, freq=FALSE)
  lines(density(x), col='red', lwd=3)
  abline(v = c(mean(x),median(x)),  col=c("green", "blue"), lty=c(2,2), lwd=c(3, 3))
}

MeanCI(dataset$VIT.D, conf.level = 0.95, method = "classic")


f <- function (x, n) {
  qqnorm(x)
  qqline(x)
}

mean(dataset$VIT.D)

d(dataset$VitDtoAMH, expression("25(OH)D3" / "AMH"))
f(dataset$LHtoFSH, "AMH")

d(dataset$VIT.D, "25(OH)D3")
f(dataset$VIT.D, "AMH")

table(dataset$mens)
round(prop.table(table(dataset$mens)) * 100)

nSim <- 100000
n <- 1000

meanArray <- array(0, dim = nSim)

for (i in 1:nSim) {
  x <- sample(dataset$AMH, n, replace = TRUE)
  meanArray[i] <- mean(x)
}

mean(meanArray)
hist(meanArray)

######################################################
## 25(OH)D3 / AMH Correlation
##



main <- function () {
  result <- cor.test(dataset$VIT.D, log(dataset$AMH), method = "pearson")

  print(result)
  print(apa(result))

  ggplot(dataset, aes(x=VIT.D, y=log(AMH))) +
    geom_point(color="#606060") +
    geom_smooth(method=lm , color="black", se=TRUE) +
    annotate("text", x = 32, y = 2.8, label = paste0(
      "     Pearson's r ≈ ",
      round(result$estimate, 2),
      "\np-value = 0.002"
    )) +
    labs(x=expression(paste("25(OH)D"["3"], " (ng/mL)")), y=expression(log[e]("AMH (ng/mL)"))) +
    theme_ipsum(
      base_size = 12,
      axis_title_size = 14
    ) +
    theme(
      axis.line = element_line(colour = "gray"),
    )
}

5.225 - 14
round(8.43675655832796, 2)
main()


######################################################
## 25(OH)D3 / AMH Ratio below 7
##


AMH_UNL <- 4.5
VitD_LNL <- 30

critical_mean <- 7

main <- function () {
  shapiro.test(x = log(dataset$VitDtoAMH))

  print(paste("Critical mean = ", critical_mean))

  result <- t.test(log(dataset$VitDtoAMH), mu = log(critical_mean))

  print(apa(result))
}

main()
