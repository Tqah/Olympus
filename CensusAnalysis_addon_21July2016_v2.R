#===POPULATION DATA_SINGAPORE 2010===

#In Excel, format Cell to number only
#setwd
setwd("/Users/lloyd/Documents/lloyd_2016/Services/Olympics/1stResponse/writeup/To\ send")

#read.data
Sg_popln <- read.csv("Population Data by Age Ethnics and Sex.csv", header = TRUE)

#To subset for age 10-19 years old in 2010
popln <- Sg_popln[12:21,]
popln$Age <- 10:19

library(dplyr)
library(tidyr)

ppln <- tbl_df(popln)
pplntidy <- ppln %>% select(Age,Males_Chinese,Females_Chinese,Males_Malays,
                            Females_Malays,Males_Indians,Females_Indians,
                            Males_Others,Females_Others)
pplntidy <- pplntidy %>% gather(sex_race,count,-Age) %>%
  separate(col = sex_race,into = c("Sex","Race"))
by_age <- pplntidy %>% group_by(Age) %>% summarise(total = sum(count)) %>%
  mutate(Probability = total/sum(total))

#===SAMPLE DATA_OLYMPIC====

#read in csv
olymp <- read.csv("data_syog2010impact_2016-06-13_17-00.csv", stringsAsFactors = FALSE)

#Pre-processing
#save the first two headers in olymp
head2 <- olymp[1,]

#remove 1st row, the double header
olymp2 <- olymp[2:nrow(olymp),]

#interviewees who finished
olymp2.done <- olymp2[olymp2$FINISHED == 1,]

#check for duplicates using NRIC
sum(duplicated(olymp2.done$PI07_01))

#Remove discorded response
olymp.done.nodiscord <- olymp2.done[!((olymp2.done$YP02_09 == 2 | olymp2.done$YP02_10 == 2 |
                                         olymp2.done$YP02_11 == 2 | olymp2.done$YP02_12 == 2) 
                                      & (olymp2.done$YP02_13 == 2)),]

colnames(olymp.done.nodiscord)[c(8,10,11)] <- c("Age","Sex","Race")

olympNEW <- tbl_df(olymp.done.nodiscord)
olympNEW <- select(olympNEW,Age,Sex,Race)

#Clean Age
#Age [PI06]
# 1 = 1991 ~ 25 yrs, 2 = 1992 ~ 24 yrs, 3 = 1993 ~ 23 yrs, 4 = 1994 ~ 22 yrs, 5 = 1995 ~ 21 yrs,
# 6 = 1996 ~ 20 yrs, 7 = 1997 ~ 19 yrs, 8 = 1998 ~ 18 yrs, 9 = 1999 ~ 17 yrs, 10 = 2000 ~ 16 yrs,
# -9 = Not answered
Age.vec <- 25:16
olympNEW$Age <- as.integer(olympNEW$Age)
for (i in 1:nrow(olympNEW)) {
  for (j in 1:10){
  olympNEW$Age[olympNEW$Age == j] <- Age.vec[j]
  }
}

#Clean Sex
# 1 = Male, 2 = Female, -9 = Not answered
#olympNEW$Sex <- as.character(olympNEW$Sex)
olympNEW$Sex[olympNEW$Sex == 1] <- "Males"
olympNEW$Sex[olympNEW$Sex == 2] <- "Females"
olympNEW$Sex[olympNEW$Sex == -9] <- "NA"

#Clean Race
# 1 = Chinese, 2 = Malay, 3 = Indian, 4 = Others, -9 = Not answered
olympNEW$Race <- as.character(olympNEW$Race)
olympNEW$Race[olympNEW$Race == 1] <- "Chinese"
olympNEW$Race[olympNEW$Race == 2] <- "Malays"
olympNEW$Race[olympNEW$Race == 3] <- "Indians"
olympNEW$Race[olympNEW$Race == 4] <- "Others"

by_age_olymp <- olympNEW %>% group_by(Age) %>% summarise(total = n()) %>% 
  mutate(Probability = total/sum(total))

#plotting a barplot of probability against age
age.prob.mx <- rbind(by_age$Probability,by_age_olymp$Probability)
png(filename = "AgePplnOlym.png", width = 650, height = 600)
barplot(age.prob.mx, beside = T, col = c("lightslateblue","turquoise2"), 
        ylim=c(0,0.5), names.arg = 16:25)
title(main = "Comparison of age distribution between population census 2010 and YOG 2010", 
      xlab = "Age (year)", ylab = "Probability")
legend("topright",legend = c("Population Census 2010","YOG 2010"), 
       col = c("lightslateblue","turquoise2"), pch = 15, bty="n")
dev.off()

#Consider Race
olympNEW <- olympNEW %>% group_by(Age,Sex,Race) %>% summarise(count = n()) %>% 
  arrange(Age,Sex,Race) %>% mutate(AgeNow = Age)
pplntidy <- pplntidy %>% arrange(Age,Sex,Race) %>% mutate(AgeNow = Age + 6)
pplntidy$AgeNow <- as.integer(pplntidy$AgeNow)

olympNEW.race <-  olympNEW %>% group_by(Race) %>% summarise(total=sum(count)) %>%
  mutate(Probability = total / sum(total))
pplntidy.race <- pplntidy %>% group_by(Race) %>% summarise(total=sum(count)) %>%
  mutate(Probability = total / sum(total))

#plotting a barplot of probability against race
race.mx <- rbind(pplntidy.race$Probability,olympNEW.race$Probability)  
png(filename = "RacePplnOlym.png", width = 650, height = 600)
barplot(race.mx, beside = T, col = c("lightslateblue","turquoise2"), 
        ylim=c(0,1), names.arg = c("Chinese","Indians","Malays","Others"))
title(main = "Comparison among ethic groups in population census 2010 and YOG 2010", 
      xlab = "Race", ylab = "Probability")
legend("topright",legend = c("Population Census 2010","YOG 2010"), 
       col = c("lightslateblue","turquoise2"), pch = 15, bty="n")
dev.off()

#Ppln 2010 16 yr old first value
#abline(h=by_age$Probability[1])
#Olym 16 yr old first value
#abline(h=by_age_olymp$Probability[1])

# olympNEW$Age <- as.integer(olympNEW$Age)
# olympNEW$Age[olympNEW$Age == 1] <- 25L
# olympNEW$Age[olympNEW$Age == 2] <- 24L
# olympNEW$Age[olympNEW$Age == 3] <- 23L
# olympNEW$Age[olympNEW$Age == 4] <- 22L
# olympNEW$Age[olympNEW$Age == 5] <- 21L
# olympNEW$Age[olympNEW$Age == 6] <- 20L
# olympNEW$Age[olympNEW$Age == 7] <- 19L
# olympNEW$Age[olympNEW$Age == 8] <- 18L
# olympNEW$Age[olympNEW$Age == 9] <- 17L
# olympNEW$Age[olympNEW$Age == 10] <- 16L


#Changing the codes to the actual ages
#Create a new column
olymp.done.nodiscord["Age"] <- NA     #Create a new column named "Age" and filled with NA
olymp.done.nodiscord$Age <- olymp.done.nodiscord$PI06     #Copy column olymp.done.nodiscord$PI06 to the new column
olymp.done.nodiscord$Age <- as.integer(olymp.done.nodiscord$Age)
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 1] <- 25
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 2] <- 24
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 3] <- 23
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 4] <- 22
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 5] <- 21
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 6] <- 20
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 7] <- 19
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 8] <- 18
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 9] <- 17
olymp.done.nodiscord$Age[olymp.done.nodiscord$Age == 10] <- 16
olymp.done.nodiscord$Age <- as.integer(olymp.done.nodiscord$Age)





#Age [PI06]
# 1 = 1991 ~ 25 yrs, 2 = 1992 ~ 24 yrs, 3 = 1993 ~ 23 yrs, 4 = 1994 ~ 22 yrs, 5 = 1995 ~ 21 yrs,
# 6 = 1996 ~ 20 yrs, 7 = 1997 ~ 19 yrs, 8 = 1998 ~ 18 yrs, 9 = 1999 ~ 17 yrs, 10 = 2000 ~ 16 yrs,
# -9 = Not answered
# Age groups are Younger (<20) and Older (>= 20)
olymp.young <- olymp.done.nodiscord[olymp.done.nodiscord$Age == 16 | olymp.done.nodiscord$Age == 17 | 
                                        olymp.done.nodiscord$Age == 18 | olymp.done.nodiscord$Age == 19,]
olymp.old <- olymp.done.nodiscord[olymp.done.nodiscord$Age == 20 | olymp.done.nodiscord$Age == 21 | 
                                      olymp.done.nodiscord$Age == 22 | olymp.done.nodiscord$Age == 23 | 
                                      olymp.done.nodiscord$Age == 24 | olymp.done.nodiscord$Age == 25,]
# birthYr <- c(nrow(birthYr.young), nrow(birthYr.old))
# #To put percentage calculated below on the bars
# birthYr.pcnt <- c(nrow(birthYr.young)/nrow(olymp.know), nrow(birthYr.old)/nrow(olymp.know))
# pcnt.birthYr <- round(100*birthYr.pcnt, 1)
# pcnt.birthYr <- paste(pcnt.birthYr,"%",sep="")

#TO PLOT HISTOGRAM

#Histogram for Age_Population
hist(allage, col = rgb(0,229/255,238/255,0.6), ylim=c(0,1),breaks = 9, freq = FALSE,
     xlim = c(14,26), xlab = "Age", 
     main = "Histogram of Age \n [n = 508,052] vs [n = 294]", axes = TRUE)

#to overlap two histogram or ..., add=T)
par(new = TRUE)

#Histogram for Age_Olympic
olymp.age <- as.numeric(olymp.done.nodiscord$Age)
hist(olymp.age, col = rgb(132/255,112/255,255/255,0.6), ylim=c(0,1), freq = FALSE,
     xlim = c(14,26), xlab = "Age", 
     main = "Histogram of Age \n [n = 508,052] vs [n = 294]", axes = TRUE)
data.lbls <- c("Population", "Sample - Olympic")
legend("topright", inset = .004, data.lbls, cex = 0.9, 
       fill = c(rgb(0,229/255,238/255,0.6), rgb(132/255,112/255,255/255,0.6)))

# #to find out RGB code for colours
# col2rgb("turquoise2") / rgb(0,229/255,238/255,0.6)
# col2rgb("lightslateblue")

#TO PLOT GROUPED BAR CHART - GENDER VS RACE

#Total Gender sorted by race
total.chinese.popln <- sum(popln$Total_Chinese)
total.malays.popln <- sum(popln$Total_Malays)
total.indians.popln <- sum(popln$Total_Indians)
total.others.popln <- sum(popln$Total_Others)


#barchart race vs gender - POPULATION
gender.race.popln <- c(total.chinese.popln,total.malays.popln,total.indians.popln,
                       total.others.popln)

#barchart race vs gender - OLYMPIC

#Race [PI08]
# 1 = Chinese, 2 = Malay, 3 = Indian, 4 = Others, -9 = Not answered
#for know
race.olymp <- olymp.done.nodiscord[olymp.done.nodiscord$PI08,]
chinese.olymp <- olymp.done.nodiscord[olymp.done.nodiscord$PI08 == 1,]
malays.olymp <- olymp.done.nodiscord[olymp.done.nodiscord$PI08 == 2,]
indians.olymp <- olymp.done.nodiscord[olymp.done.nodiscord$PI08 == 3,]
others.olymp <- olymp.done.nodiscord[olymp.done.nodiscord$PI08 == 4,]
race.olymp <- c(nrow(chinese.olymp), nrow(malays.olymp), nrow(indians.olymp), nrow(others.olymp))
total.race.olymp <- sum(c(nrow(chinese.olymp), nrow(malays.olymp), 
                          nrow(indians.olymp), nrow(others.olymp)))

#Total Gender sorted by race
total.chinese.olymp <- sum(nrow(chinese.olymp))
total.malays.olymp <- sum(nrow(malays.olymp))
total.indians.olymp <- sum(nrow(indians.olymp))
total.others.olymp <- sum(nrow(others.olymp))

gender.race.olymp <- c(total.chinese.olymp,total.malays.olymp,total.indians.olymp,
                       total.others.olymp)

#Race count for population
chinese.popln.count <- rep("Chinese", total.chinese.popln)
malays.popln.count <- rep("Malays", total.malays.popln)
indians.popln.count <- rep("Indians", total.indians.popln)
others.popln.count <- rep("Others", total.others.popln)
race.popln <- c(chinese.popln.count, malays.popln.count, 
                indians.popln.count, others.popln.count)

#Race count for olympic
chinese.olymp.count <- rep("Chinese", total.chinese.olymp)
malays.olymp.count <- rep("Malays", total.malays.olymp)
indians.olymp.count <- rep("Indians", total.indians.olymp)
others.olymp.count <- rep("Others", total.others.olymp)
race.olymp <- c(chinese.olymp.count, malays.olymp.count, 
                indians.olymp.count, others.olymp.count)

#GROUPING AND DATA MANIPULATION
Population <- table(race.popln)
Olympic <- table(race.olymp)
group <- rbind(Population, Olympic)
group.df <-  as.data.frame(group)
group.df2 <- apply(group.df, 1, function(x) x[1:4]/x[5])
group.df2 <- t(group.df2)

#TO PLOT GROUPED BAR CHART 
sample.lbls <- c("Population", "Olympic")
barplot(group.df2, ylim = c(0,1), col = c("turquoise2", "lightslateblue"), 
        main = "Distribution of race and gender \n [n = 508,052] vs [n = 294]", 
        ylab = "No. of respondents", xlab = "Race", names.arg = c("Chinese", 
                                                                  "Malays", "Indians", "Others"), beside = TRUE)
legend("topright", inset = .06, sample.lbls, cex = 0.9, fill = c("turquoise2", "lightslateblue"))




