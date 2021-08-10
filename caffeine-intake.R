dataset <- read.csv("/Users/ivantoskov/Desktop/R-project/caffeine-intake.csv")
colnames(dataset) <- paste("Q", 1:ncol(dataset), sep = "")
View(dataset)

age <- c(dataset$Q1)
caffeineConsumation <- c(dataset$Q2)
caffeineBeverages <- c(dataset$Q3)[dataset$Q3 != ""]
dailyCaffeineBeverages <- c(dataset$Q4)[!is.na(dataset$Q4)]
purpose <- c(dataset$Q5)[dataset$Q5 != ""]
drugOrSupp <- c(dataset$Q6)
hoursOfSleep <- c(dataset$Q7)

myColors <- c("coral2", "slateblue1", "gold1", "thistle2", "aquamarine")


# Q2: What is your age? - Categorical data
ageTable <- table(age)
ageTable
age_percentage <- round(100*ageTable/sum(ageTable), 1)
pie(ageTable,
    main="Age", 
    labels=age_percentage,
    col=c("coral2", "slateblue1", "gold1", "thistle2", "aquamarine"))
legend(x = "topright", legend = c("Under 18", "Over 60",
                             "18-29", "30-45",
                             "45-60"), cex = 0.7, text.width = 0.7, fill = myColors)

# Q3: Do you consume caffeinated drinks?
consumationTable <- round(prop.table(table(caffeineConsumation)) * 100)
factor(consumationTable)
barplot(consumationTable,
        main = "Do you consume caffeinated drinks?",
        col = myColors,
        ylim = c(0, 100))

# Q4: What type of caffeinated beverage do you usually consume? - Categorical data
caffeineBeverages[caffeineBeverages != ""]
beveragesTable <- table(caffeineBeverages[caffeineBeverages != ""])
beverages <- c("Coffee", "Energy drink", "Pre-workout", "Tea")
barplot(round(prop.table(beveragesTable)*100, 2), col = myColors,
        main = "What type of caffeinated beverage do you usually consume? ",
        ylim = c(0, 40), ylab = "Percentage", names.arg = beverages)
# Q5: About how many caffeinated beverages do you consume daily? - Numerical data
mean(dailyCaffeineBeverages, na.rm = TRUE)
median(dailyCaffeineBeverages, na.rm = TRUE)
dailyCaffeineTable <- table(dailyCaffeineBeverages)
names(dailyCaffeineTable)[dailyCaffeineTable == max(dailyCaffeineTable)]
summary(dailyCaffeineBeverages)
range(dailyCaffeineBeverages, na.rm = TRUE) # max and min value
var(dailyCaffeineBeverages, na.rm = TRUE)
sd(dailyCaffeineBeverages, na.rm = TRUE)
fivenum(dailyCaffeineBeverages)

normalDist <- rnorm(10^2, mean=mean(dailyCaffeineBeverages, na.rm = TRUE),
                        sd=sd(dailyCaffeineBeverages, na.rm = TRUE))
qqplot(dailyCaffeineBeverages, normalDist)
abline(a=0, b=1)

boxplot(dailyCaffeineBeverages,
        ylab="Caffeine beverages a day",
        col="aquamarine")

shapiro.test(dailyCaffeineBeverages)

# Q6: For what purpose would you consume a caffeinated beverage? - Categorical data
purposeTable <- table(purpose)
round(prop.table(purposeTable)*100, 2)
purposes <- c("To feel more awake", "To stay up late",
              "To be more focused and concentrated", "To improve physical performance",
              "No specific purpose")
row.names(purposeTable) <- paste(1:nrow(purposeTable))
barplot(round(prop.table(purposeTable)*100, 2), col = myColors,
        main = "For what purpose would you consume a caffeinated beverage?",
        ylim = c(0, 50), ylab = "Percentage")
legend(x = "top",
       legend = c("1. No specific purpose", "2. To be more focused and concentrated",
                                  "3. To feel more awake", "4. To improve physical performance",
                                   "5. To stay up late"),
       cex = 0.8, text.width = 2.5, fill = myColors)

# Q9: In your opinion, do you consider caffeine to be a drug or a food supplement? - Categorical data
drugOrSuppTable <- table(drugOrSupp)
drugOrSuppPercentage <- round(100*drugOrSuppTable/sum(drugOrSuppTable), 1)
pie(round(prop.table(drugOrSuppTable) * 100, 2) / length(drugOrSuppTable),
        labels = drugOrSuppPercentage,
        main = "Drug or supplement?",
        col = c("coral2", "slateblue1"))
legend(x = "topleft", legend = c("Drug", "Food supplement"),
       cex = 0.8, text.width = 1, fill = myColors)
# Q10: How many hours of sleep do you get each night? - Numerical data
mean(hoursOfSleep, na.rm = TRUE)
median(hoursOfSleep, na.rm = TRUE)
hoursOfSleepTable <- table(hoursOfSleep)
names(hoursOfSleepTable)[hoursOfSleepTable == max(hoursOfSleepTable)]
summary(hoursOfSleep)
range(hoursOfSleep, na.rm = TRUE) # max and min value
var(hoursOfSleep, na.rm = TRUE)
sd(hoursOfSleep, na.rm = TRUE)

normalDist2 <- rnorm(10^2, mean=mean(hoursOfSleep, na.rm = TRUE),
                    sd=sd(hoursOfSleep, na.rm = TRUE))
qqplot(hoursOfSleep, normalDist2)
abline(a=0, b=1)

boxplot(hoursOfSleep,
        ylab="Hours of sleep",
        col="aquamarine")

shapiro.test(hoursOfSleep)

# Hours of sleep related to caffeine consumption box plot
# Categorical data vs Numerical data
df1 <- data.frame(hoursOfSleep, caffeineConsumation)
consumers <- df1$hoursOfSleep[df1$caffeineConsumation == 'Yes']
median(consumers)
median(nonComsumers)
nonComsumers <- df1$hoursOfSleep[df1$caffeineConsumation == 'No']
shapiro.test(consumers) # isn't normal distribution
shapiro.test(nonComsumers) # is not normal dist
wilcox.test(hoursOfSleep ~ caffeineConsumation, data = df1,
            conf.int = TRUE, exact = FALSE)
tt <- boxplot(hoursOfSleep ~ caffeineConsumation,
              main="Hours of sleep related to caffeine consumption",
              col=myColors, ylim = c(3, 12),
              xlab="Caffeine consumption",
              ylab="Hours of sleep")

# Numerical vs Numerical data
df2 <- data.frame(dailyCaffeineBeverages, hoursOfSleep)
table(dailyCaffeineBeverages, hoursOfSleep)
plot(dailyCaffeineBeverages, hoursOfSleep,
     type="p",
     xlab="Caffeine products per day",
     ylab="Hours of sleep",
     col="coral2")
rho <- round(cor(dailyCaffeineBeverages, hoursOfSleep), 3)
model <- lm(hoursOfSleep ~ dailyCaffeineBeverages, data=df2)
rm(list = "model1")
summary(model)
abline(model)

