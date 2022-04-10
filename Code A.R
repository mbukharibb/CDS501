
install.packages("e1071")
install.packages("ggplot2")
install.packages("vtable")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("plotly")
install.packages("tidyverse")

library(e1071)
library(ggplot2)
library(vtable)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(plotly)
library(tidyverse)


### DATA PROCESSING ###
cvd_1 <- read.csv2("cardio_train.csv", sep = ";")

#transforming the weight and height so we could calculate the bmi
cvd_1$weight <- as.numeric(levels(cvd_1$weight))[cvd_1$weight]
cols_to_change = c(1:4,6:13)
for (i in cols_to_change) {
  cvd_1[, i] <- as.numeric(cvd_1[, i])
}
cvd_1$height <- cvd_1$height/100

#calculate BMI
bmi <- format(round(cvd_1$weight/(cvd_1$height^2)), 0, nsmall = 0)
cvd_1$bmi <- bmi
cvd_1$bmi <- as.numeric(cvd_1$bmi)

#convert number format age from days to years
cvd_1$age <- format(round((cvd_1$age/365), 0), nsmall = 0)


#rearrange the columns
cvd_1 <- cvd_1[,c(1,2,3,4,5,14,6,7,8,9,10,11,12,13)]

sapply(cvd_1, class)
str(cvd_1)

#check for NULL values
sum(is.na(cvd_1))

summary(cvd_1)

vtable(cvd_1)

cvd_1$cardio <- as.factor(cvd_1$cardio)
str(cvd_1)

### DATA EXPLORATION ####

#show the distribution of cardio class
cvd_1 %>%
  ggplot(aes(x = cardio)) + 
  geom_histogram(stat = 'count', fill = "steelblue") +
  theme_bw()

#id column was dropped
cvd_2 <- cvd_1[-1] 

#the current theme is automatically applied to every plot we draw
theme_set(theme_bw()) 

cvd_2 <- cvd_2 %>%
  mutate(cardio = fct_recode(cardio, absent = "0", present = "1"))

chart <-
  cvd_2 %>%
  count(cardio) %>%
  mutate(pct = round(n / sum(n) * 100)) %>%
  ggplot(aes(x = cardio, y = n, fill = cardio)) +
  geom_bar(stat = "identity", width = 0.4, show.legend = FALSE) +
  labs(
    x = "Heart disease presence", y = "Number of patients",
    caption = "Source: Heart disease data"
  ) +
  scale_fill_manual(values = c("present" = "red", "absent" = "green"), aesthetics = "fill") +
  geom_text(aes(label = str_c(pct, "%")), vjust = 4.5, size = 2.5, colour = "black") +
  theme(
    legend.position = "top", axis.title.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 50, vjust = 0.3, face = "bold")
  )

ggplotly(chart, tooltip = c("x", "y"))

#categorical: cholesterol, gluc
#Visualize class separation by numeric features


#calculating the standard deviation 
cvd_sd <- sapply(cvd_1[,2:14], sd)
cvd_sd

### Stating Cumulative Distribution Function and Plotting the Quartile for 50% and 75% ###
## Since we're focusing on lifestyle attributes, we're only taking into accounts of bmi, ap_hi an ap_lo 
## to do the cumulative distribution function and plotting the quartile since the rest of the attributes are either ordinal variable or binary feature

#BMI 50%
pnorm_bmi50 <- pnorm(26, mean = 27.11, sd = 4.57334852)
pnorm_bmi50

x <- seq(from=0, to=50, length.out=100)
f <- dnorm(x, 27.11, 4.57334852)
nframe <- data.frame(x=x,y=f)
line <- qnorm(0.5, 27.11, 4.57334852)
xstr <- sprintf("qnorm(0.50) = %1.3f", line)
nframe75 <- subset(nframe, nframe$x < line)
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") +
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1) + ggtitle("BMI 50%")

#BMI 75%
pnorm_bmi75 <- pnorm(30, mean = 27.11, sd = 4.57334852)
pnorm_bmi75

x <- seq(from=0, to=50, length.out=100)
f <- dnorm(x, 27.11, 4.57334852)
nframe <- data.frame(x=x,y=f)
line <- qnorm(0.75, 27.11, 4.57334852)
xstr <- sprintf("qnorm(0.75) = %1.3f", line)
nframe75 <- subset(nframe, nframe$x < line)
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") +
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1) + ggtitle("BMI 75%")

#AP_HI 50%
pnorm_aphi50 <- pnorm(120, mean = 126.4, sd = 14.28983422)
pnorm_aphi50

x <- seq(from=50, to=200, length.out=100)
f <- dnorm(x, 126.4, 14.28983422)
nframe <- data.frame(x=x,y=f)
line <- qnorm(0.50, 126.4, 14.28983422)
xstr <- sprintf("qnorm(0.50) = %1.3f", line)
nframe75 <- subset(nframe, nframe$x < line)
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") +
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1) + ggtitle("AP_HI 50%")

#AP_HI 75%
pnorm_aphi50 <- pnorm(140, mean = 126.4, sd = 14.28983422)
pnorm_aphi50

x <- seq(from=50, to=200, length.out=100)
f <- dnorm(x, 126.4, 14.28983422)
nframe <- data.frame(x=x,y=f)
line <- qnorm(0.75, 126.4, 14.28983422)
xstr <- sprintf("qnorm(0.75) = %1.3f", line)
nframe75 <- subset(nframe, nframe$x < line)
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") +
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1) + ggtitle("AP_HI 75%")

#AP_LO 50%
pnorm_aplo50 <- pnorm(80, mean = 81.7, sd = 7.67336365)
pnorm_aplo50

x <- seq(from=50, to=120, length.out=100)
f <- dnorm(x, 81.7, 7.67336365)
nframe <- data.frame(x=x,y=f)
line <- qnorm(0.50, 81.7, 7.67336365)
xstr <- sprintf("qnorm(0.50) = %1.3f", line)
nframe75 <- subset(nframe, nframe$x < line)
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") +
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1) + ggtitle("AP_LO 50%")

#AP_LO 75%
pnorm_aplo75 <- pnorm(90, mean = 81.7, sd = 7.67336365)
pnorm_aplo75

x <- seq(from=50, to=120, length.out=100)
f <- dnorm(x, 81.7, 7.67336365)
nframe <- data.frame(x=x,y=f)
line <- qnorm(0.75, 81.7, 7.67336365)
xstr <- sprintf("qnorm(0.75) = %1.3f", line)
nframe75 <- subset(nframe, nframe$x < line)
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") +
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1) + ggtitle("AP_LO 75%")

#getting rid of outliers in height, weight, ap_hi, ap_lo
outliers1 <- boxplot(cvd_1$weight, plot=FALSE)$out
outliers2 <- boxplot(cvd_1$height, plot=FALSE)$out
outliers3 <- boxplot(cvd_1$ap_hi, plot=FALSE)$out
outliers4 <- boxplot(cvd_1$ap_lo, plot=FALSE)$out

cvd_1 <- cvd_1[-which(cvd_1$weight %in% outliers1),]
cvd_1 <- cvd_1[-which(cvd_1$height %in% outliers2),]
cvd_1 <- cvd_1[-which(cvd_1$ap_hi %in% outliers3),]
cvd_1 <- cvd_1[-which(cvd_1$ap_lo %in% outliers4),]
