library('car')
movie=read.csv("C:/Users/workV/Downloads/IMDB_data.csv")
attach(movie)
summary(movie)


#0.1 actor1_starMeter  and its significance
lm.fit=lm(imdbScore~actor1_starMeter)
lm.fit
plot(actor1_starMeter, imdbScore)
abline(lm.fit)
boxplot(actor1_starMeter)
summary(lm.fit)
residualPlot(lm.fit)

#0.2 new column with value for actor1_starMeter (LOG NORMALIZED) and its significance
Logactor1_starMeter <- log(actor1_starMeter)
movie$Logactor1_starMeter <- Logactor1_starMeter 
lm.fit=lm(imdbScore~Logactor1_starMeter)
lm.fit
plot(Logactor1_starMeter, imdbScore)
abline(lm.fit)
boxplot(Logactor1_starMeter)
summary(lm.fit)
residualPlot(lm.fit)


#1.1 new column with avg value for StarMeter for all actors and its significance
avgStarMeter <- (actor1_starMeter+actor2_starMeter+actor3_starMeter)/3
movie$avgStarMeter <- avgStarMeter 
lm.fit=lm(imdbScore~avgStarMeter)
lm.fit
plot(avgStarMeter, imdbScore)
abline(lm.fit)
boxplot(avgStarMeter)
summary(lm.fit)
residualPlot(lm.fit)



#1.2 new column with avg value for StarMeter (LOG NORMALIZED) for all actors and its significance
LogavgStarMeter <- log(avgStarMeter)
movie$LogavgStarMeter <- LogavgStarMeter 
lm.fit=lm(imdbScore~LogavgStarMeter)
lm.fit
plot(LogavgStarMeter, imdbScore)
abline(lm.fit)
boxplot(LogavgStarMeter)
summary(lm.fit)
residualPlot(lm.fit)



#1.3 new column with avg value for StarMeter (Z score NORMALIZED) for all actors and its significance
m<-mean(avgStarMeter)
s<-sd(avgStarMeter)
ZavgStarMeter<- (avgStarMeter-m)/s
movie$ZavgStarMeter <- ZavgStarMeter 
lm.fit=lm(imdbScore~ZavgStarMeter)
lm.fit
plot(ZavgStarMeter, imdbScore)
abline(lm.fit)
boxplot(ZavgStarMeter)
summary(lm.fit)
residualPlot(lm.fit)


#2 new dataframe with only english movies and checking the significance on this new DF
EngMovies <- movie %>% filter(language =='English')

#2.1 New Column actor1_starMeter (LOG NORMALIZED) and its significance on only English Movies
ENGLogactor1_starMeter <- log(EngMovies$actor1_starMeter)
EngMovies$ENGLogactor1_starMeter <- ENGLogactor1_starMeter 
lm.fit=lm(EngMovies$imdbScore~ENGLogactor1_starMeter)
lm.fit
plot(ENGLogactor1_starMeter, EngMovies$imdbScore)
abline(lm.fit)
boxplot(ENGLogactor1_starMeter)
summary(lm.fit)
residualPlot(lm.fit)



#2.1 new column with avg value for StarMeter for all actors and its significance on only English Movies
ENGavgStarMeter <- (EngMovies$actor1_starMeter+EngMovies$actor2_starMeter+EngMovies$actor3_starMeter)/3
EngMovies$ENGavgStarMeter <- ENGavgStarMeter 
lm.fit=lm(EngMovies$imdbScore~ENGavgStarMeter)
lm.fit
plot(ENGavgStarMeter, EngMovies$imdbScore)
abline(lm.fit)
boxplot(ENGavgStarMeter)
summary(lm.fit)
residualPlot(lm.fit)

#2.2 new column with avg value for StarMeter ((LOG NORMALIZED)) for all actors and its significance on only English Movies
ENGLogavgStarMeter <- log(ENGavgStarMeter)
EngMovies$ENGLogavgStarMeter <- ENGLogavgStarMeter
lm.fit=lm(EngMovies$imdbScore~ENGLogavgStarMeter)
lm.fit
plot(ENGLogavgStarMeter, EngMovies$imdbScore)
abline(lm.fit)
boxplot(ENGLogavgStarMeter)
summary(lm.fit)
residualPlot(lm.fit)
