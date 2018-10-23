#Pitcher Injury - Sumair Shah - December 2017
install.packages("pitchRx")
install.packages("dplyr")
install.packages('reshape')
library(pitchRx)
library(DBI)
library(dplyr)
library(reshape)

#####Function's for use later
MyMerge       <- function(x, y){
  df            <- merge(x, y, by= "row.names", all.x= F, all.y= F)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}
removeNAs <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
#####

#Create database
db <- src_sqlite("pitchRx.sqlite3", create = TRUE)
scrape(start = "2010-01-01", end = "2016-11-01", connect = db$con)
#average spin rate, movement, nasty factor, innings pitched, 
atbats <- tbl(db, 'atbat')
pitches <- tbl(db,'pitch')
atbats1 <- as_data_frame(atbats)
pitches1 <- as_data_frame(pitches)
#joining tables together by number of atbat pitches occured in and gameday_url with inner join
joined <- plyr::join(pitches1, atbats1, by = c('num', 'url'), type = 'inner')
names(joined)
rm(atbats1, pitches1)
#or joined using k13
k13 <- inner_join(pitches, atbats, by = c('num', 'gameday_link'))
joined <- collect(k13, n =Inf)
pitches_atbat_final <- c(joined$pitcher_name, joined$pitch_type, joined$start_speed, joined$end_speed, joined$nasty, joined$spin_rate, joined$break_length, joined$pfx_x, joined$pfx_z)
#delete rows with low percentage of pitches
#unidentified, pitch outs, ephis.
joined <- subset(joined, joined$pitch_type!="UN")
joined <- subset(joined, joined$pitch_type!="XX")
joined <- subset(joined, joined$pitch_type!="PO")
joined <- subset(joined, joined$pitch_type!="FO")
joined <- subset(joined, joined$pitch_type!="EP")
joined <- subset(joined, joined$pitch_type!="IN")
joined$pitch_type
#check to see that it worked 
count(joined$pitch_type == "UN")
count(joined$pitch_type == "XX")
count(joined$pitch_type == "PO")
count(joined$pitch_type == "EP")
count(joined$pitch_type == "CB")

#remove every row with NA in spin_rate, pitch_type, pitcher_name
joined$spin_rate
joined <- removeNAs(joined, "spin_rate")
joined <- removeNAs(joined, "pitch_type")
joined <- removeNAs(joined, "pitcher_name")
#5168582 appx 60,000 pitches missing for spin_rate
#check to see if NAs were removed
joined$spin_rate
joined$pitch_type
joined$pitcher_name

####RENAME PITCH VARIABLES
#FB = FA, FF, FT, FC, FS/SI/SF
#CB = CU, KC 
#!SF are hard on arms, leave variable
#convert FF-FT-FS-SI to FB
#convert CU-KC to CB 
joined$pitch_type <- as.character
joined$pitch_type[joined$pitch_type == "FA"] <- "FB"
joined$pitch_type[joined$pitch_type == "FF"] <- "FB"
joined$pitch_type[joined$pitch_type == "FT"] <- "FB"
joined$pitch_type[joined$pitch_type == "FS"] <- "FB"
joined$pitch_type[joined$pitch_type == "SI"] <- "FB"
joined$pitch_type[joined$pitch_type == "CU"] <- "CB"
joined$pitch_type[joined$pitch_type == "KC"] <- "CB"
#check to see that it worked
match('FF',joined$pitch_type)
match('FS',joined$pitch_type)
match('SI',joined$pitch_type)
match('KC',joined$pitch_type)
match('IN',joined$pitch_type)
match('CB', joined$pitch_type)
#####

#get the average spin rate for each pitch thrown by each pitcher
library(sqldf)
require(sqldf)
names(joined)
install.packages('reshape')
library(reshape)

#change names of variables so sql statement will work
colnames(pitchfxavg)[colnames(pitchfxavg)=="avg(pfx_x)"] <- "avrg_pfx_x"
colnames(pitchfxavg)[colnames(pitchfxavg)=="avg(pfx_z)"] <- "avrg_pfx_z"
#use sqldf to make new data table with averages. 
pitchfxavg <- sqldf("select pitcher_name, pitch_type, avg(spin_rate), avg(break_length), avg(pfx_x), avg(pfx_z), avg(start_speed), avg(end_speed) from joined group by pitcher_name, pitch_type")
match(joined$pitch_type == 'IN')

#movement is in + and negative corresponding with RHPvsLHP
#take the abs of movement and make a new column 
absmvmt <- sqldf("select pitcher_name, pitch_type, abs(avrg_pfx_x), abs(avrg_pfx_z) from pitchfxavg group by pitcher_name, pitch_type")

### bind new columns together
pitchfxavg <- cbind(pitchfxavg, absmvmt[1:8637,3:4])
#abs values added for pfx_x & pfx_z
#Remove avrg_pfx_x & avrg_pfx_z
pitchfxavg$avrg_pfx_x <- NULL
pitchfxavg$avrg_pfx_z <- NULL

#IN = Intentional Balls - Remove all rows 
pitchfxavg <- subset(pitchfxavg, pitchfxavg$pitch_type!="IN")
match('IN',pitchfxavg$pitch_type)

pitchfxavg1test <- pitchfxavg
colnames(pitchfxavg1test)[colnames(pitchfxavg1test)=="avg(spin_rate)"] <- "avrg_spin_rate"

#sql
sqldf("select pitcher_name, pitch_type, avrg_spin_rate from pitchfxavg1test pivot((avrg_spin_rate) for avrg_spin_rate in ([FB],[FC],[CH],[CB],[SL],[KN],[SF])) as eachpitch group by pitcher_name")
sqldf("select distinct pitch_type from pitchfxavg1test")

test <- reshape(pitchfxavg1test, avrg_spin_rate~pitch_type)

#################################################
#create binary variables for values based on pitchtype. Instead of using pivot in sql - Reshape package = more efficient.
avgspin <- cast(pitchfxavg1test, pitcher_name ~ pitch_type , value = 'avrg_spin_rate')
breaklength <- cast(pitchfxavg1test, pitcher_name ~ pitch_type , value = 'avg(break_length)')
avgstartspeed <- cast(pitchfxavg1test, pitcher_name ~ pitch_type , value = 'avg(start_speed)')
avgendspeed <- cast(pitchfxavg1test, pitcher_name ~ pitch_type , value = 'avg(end_speed)')
avrgpfx_x <- cast(pitchfxavg1test, pitcher_name ~ pitch_type , value = 'abs(avrg_pfx_x)')
avrgpfx_z <- cast(pitchfxavg1test, pitcher_name ~ pitch_type , value = 'abs(avrg_pfx_z)')
#DF = (avgspin , breaklength , avgstartspeed , avgendspeed , avrgpfx_x , avrgpfx_z)

write.csv(avgspin, file = "type&spin.csv")
write.csv(breaklength, file = "type&breaklength.csv")
write.csv(avgstartspeed, file = "type&startspeed.csv")
write.csv(avgendspeed, file = "type&endspeed.csv")
write.csv(avrgpfx_x, file = "type&avrgpfx_x.csv")
write.csv(avrgpfx_z, file = "type&avrgpfx_z.csv")

spinname <- colnames(avgspin , do.NULL=FALSE, prefix = "spin")
colnames(avgspin) <- spinname

colnames(pitchfxavg)[colnames(pitchfxavg)=="avg(pfx_x)"] <- "avrg_pfx_x"
colnames(pitchfxavg)[colnames(pitchfxavg)=="avg(pfx_z)"] <- "avrg_pfx_z"
colnames(pitchfxavg1)[colnames(pitchfxavg1)=="avg(start_speed)"] <- "avrg_start_speed"
colnames(pitchfxavg1)[colnames(pitchfxavg1)=="avg(end_speed)"] <- "avrg_end_speed"
colnames(pitchfxavg1)[colnames(pitchfxavg1)=="avg(spin_rate)"] <- "avrg_spin_rate"
pitchfxavg

pitchfxavg <- removeNAs(pitchfxavg, "pitcher_name")

############################
#dbSendQuery(db$con, "CREATE INDEX url_atbat ON atbat(url)")
#dbSendQuery(db$con, "CREATE INDEX url_pitch ON pitch(url)")
#dbSendQuery(db$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
#dbSendQuery(db$con, "CREATE INDEX des_index ON pitch(des)")
#pitch11 <- tbl(db, sql("SELECT * FROM pitch WHERE url LIKE ’%year_2011%’"))
#atbat11 <- tbl(db, sql("SELECT * FROM atbat WHERE atbat.url LIKE ’%year_2011%’"))
#########################
rm(joined)
rm(absmvmt)
rm(pitchfxavg)
#k13
k13 <- inner_join(pitches, atbats, by = c('num', 'gameday_link'))
joined <- collect(k13, n =Inf)
dat  <- Reduce(MyMerge, list(avgspin, breaklength, avgstartspeed, avgendspeed,avrgpfx_x,avrgpfx_z))
write.csv(dat, file = "merged.csv")
write.csv(test, file = "type&spin.csv")




####Machine Learning
#noinjcol1 = data without injuries and col variables 
#pitchdat1 = data with all variables except recent injuries
#pitchdatwght = data with a variable for average spin rate of all pitches and average break
###pitchdat1 = data with all variables except recent injuries
###noinjcol1 = data without injuries and col variables 

noinjury <- read.csv("noinjury.csv")
noinjury1 <- noinjury[1:1302, 2:67]
noinjury1$X1.35.days <- NULL
noinjury1$GS <- NULL
noinjury1$G <- NULL
noinjury1$IP <- NULL
noinjury1$BB.9 <- NULL

#Training and testing data set
train <- sample(1:1302, 651)
noinjury1.train <- noinjury1[train,]
noinjury1.test <- noinjury1[-train,]

# model without any injury variables
glm.no.injury <- glm(DL~.,data= noinjury1.train)
summary(glm.no.injury)
# Significant variables: 
# 3 stars: TBF
# 1 star: SL.avg_spin, SL.start_speed, SL.end_speed, CG, Age
# dot: CH.start_speed, , CH.end_speed, FC.avgfx_x, SL.avgfx_x, FC.avgfx_z, SL.avgfx_z

mod1.check <- rep(0,651)
mod1.predict <- predict(glm.no.injury, noinjury1.test, type = "response")
mod1.check[mod1.predict>.01]=1
tab1 <- table(noinjury1.test$DL, mod1.check)
tab1
# error rate = 51.61%
(335+1)/651

step(glm.no.injury)
step1 <- glm(DL ~ SL.avg_spin + SL.break_lngth + SL.start_speed +  SL.end_speed + FC.avgfx_x + SL.avgfx_x + CH.avgfx_z + FB.avgfx_z + FC.avgfx_z + SL.avgfx_z + CG + TBF + K. + FB. + FCv + Age, data = noinjury1.train)
summary(step1)

mod2.check <- rep(0,651)
mod2.predict <- predict(step1, noinjury1.test, type = "response")
mod2.check[mod1.predict>.01]=1
tab2 <- table(noinjury1.test$DL, mod1.check)
tab2
# error rate = 51.92% 
338/651


lda2 <- lda(DL~., data = noinjury1.train)
lda2.predict <- predict(lda2,noinjury1.test)$class
table(lda1.predict,pitchdat1.test$DL)
# error rate = 32.41%
(62+149)/651

### model without injury types and colinear variables  
train <- sample(1:1302, 651)
noinjcol1.train <- noinjcol1[train,]
noinjcol1.test <- noinjcol1[-train,]

glm.noinjcol1 <- glm(DL~., data = noinjcol1.train)
summary(glm.noinjcol1)
# Significant variables: 
# 3 stars : X1.35.days, X36.70.days, X71.105.days, X106.140.days, X141.175.days
# dots: SL.break_lngth, FC.start_speed, FC.end_speed

mod3.check <- rep(0,651)
mod3.predict <- predict(glm.noinjcol1, noinjcol1.test, type = "response")
mod3.check[mod1.predict>.01]=1
tab3 <- table(noinjcol1.test$DL, mod1.check)
tab3
# error rate = 52.38% 
(4+337)/651


step(glm.noinjcol1)
step2 <- glm(formula = DL ~ FB.avg_spin + FC.avg_spin + FC.break_lngth + FC.start_speed + FB.end_speed + FC.end_speed + X1.35.days + X36.70.days + X71.105.days + X106.140.days + X141.175.days + IP + GS + TBF + K.BB. + FB. + CH., data = noinjcol1.train)
summary(step2)


mod4.check <- rep(0,651)
mod4.predict <- predict(step2, noinjcol1.test, type = "response")
mod4.check[mod1.predict>.01]=1
tab4 <- table(noinjcol1.test$DL, mod1.check)
tab4
# error rate = 52.38% 

######
#######
########
#Final Model - over 70%... LDA does better than log regression/trees because it works better for very low n, defining boundaries for classification.
lda3 <- lda(DL~., data = noinjcol1.train)
lda3.predict <- predict(lda3,noinjcol1.test)$class
table(lda3.predict,noinjcol1.test$DL)
# error rate = 29.65%
(52+141)/651
#########
########
#######

### model with all variables except recent injuries
train <- sample(1:1302, 651)
pitchdat1.train <- noinjcol1[train,]
pitchdat1.test <- noinjcol1[-train,]

glm.pitchdat1 <- glm(DL~., data = pitchdat1.train)
summary(glm.pitchdat1)
# Significant variables are all ranges

mod5.check <- rep(0,651)
mod5.predict <- predict(glm.pitchdat1, pitchdat1.test, type = "response")
mod5.check[mod1.predict>.01]=1
tab5 <- table(pitchdat1.test$DL, mod1.check)
tab5
#error rate = 52.38%
(338+3)/651

step(glm.pitchdat1)
step3 <- glm(DL ~ FC.avg_spin + FB.end_speed + FB.avgfx_x + FC.avgfx_z + SL.avgfx_z + X1.35.days + X36.70.days + X71.105.days + X106.140.days + X141.175.days + IP + G + TBF + K.9 + BB.9 + total.strikes + Age, data = pitchdat1.train)

mod6.check <- rep(0,651)
mod6.predict <- predict(step3, pitchdat1.test, type = "response")
mod6.check[mod1.predict>.01]=1
tab6 <- table(pitchdat1.test$DL, mod1.check)
tab6
# error rate = 52.38%


lda1 <- lda(DL~., data = pitchdat1.train)
lda1.predict <- predict(lda1,pitchdat1.test)$class
table(lda1.predict,pitchdat1.test$DL)
# error rate = 32.41%
(62+149)/651

##for fun - trying to graph residuals of tree
library(tree)
tree.class <- tree(CB_avg_spin ~ ., data = pitchdat2)
plot(tree.class)
text(tree.class)
cv.tree.class <- cv.tree(tree.class)
plot(cv.tree.class$size, cv.tree.class$dev, type='b')
pruned.tree <- prune.tree(tree.class, best=4)
plot(pruned.tree)
text(pruned.tree, pretty = 0)
#mse 
summary(pruned.tree)
MSTreeSumTrain <- summary(pruned.tree)
MSEtreetrain <- mean(MSTreeSumTrain$residuals^2)
MSEtreetrain
#appx 5% 
predict.tree <- predict(pruned.tree, newdata = dat.valid)
plot(predict.tree)

#
######
#######
########
#Final Model - over 70%... LDA does better than log regression/trees because it works better for very low n, defining boundaries for classification.
lda3 <- lda(DL~., data = noinjcol1.train)
lda3.predict <- predict(lda3,noinjcol1.test)$class
table(lda3.predict,noinjcol1.test$DL)
# error rate = 29.65%
(52+141)/651
#########
########
#######