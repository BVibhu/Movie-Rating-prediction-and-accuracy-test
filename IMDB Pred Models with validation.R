
#Loading Required libraries
load.libraries <- c('gridExtra', 'corrplot', 'GGally', 'ggplot2', 'dplyr','car','boot','plm','lmtest','ggpubr','boot','caTools')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

imdb = read.csv("IMDB_data.csv")

head(imdb)

#confirming missing values -There are no missing values in the data

colSums(is.na(imdb))


# Separating categorical and numerical predictors into separate datasets
cat_var <- names(imdb)[which(sapply(imdb, is.character))]
numeric_var <- names(imdb)[which(sapply(imdb, is.numeric))]

imdb_cont <- imdb[numeric_var]
imdb_cat <- imdb[cat_var]



### DATASET PREPROCESSING ###

# Dropping columns with high cardinality or no patterns

newdf = subset(imdb, select = c('imdbScore',
                                'movieBudget',
                                'duration',
                                'maturityRating',
                                'nbNewsArticles',
                                'director',
                                'actor1_starMeter',
                                'actor2_starMeter',
                                'actor3_starMeter',
                                'nbFaces',
                                'action',
                                'adventure',
                                'scifi',
                                'thriller',
                                'musical',
                                'romance',
                                'western',
                                'sport',
                                'horror',
                                'drama',
                                'war',
                                'animation',
                                'crime',
                                'movieMeter_IMDBpro',
                                'colourFilm'
))

# calculating avg rating for each director

a = newdf %>%
  group_by(director) %>%
  summarise(dirimdbrating = mean(imdbScore))

# joined df with original to assign avg rating
imdbdf = inner_join(newdf, a, by = 'director')
#calculate rank and binning based on rank and filtering down to COLOUR movies only
imdbdf = imdbdf %>% mutate(directorRank = dense_rank(desc(dirimdbrating)))  %>%
  mutate(ranking_bin = ntile(directorRank, n=10)) %>%
  filter(colourFilm =='Color')

attach(imdbdf)
#converting star meter to avg of all 3
avgStarMeter <- (actor1_starMeter+actor2_starMeter+actor3_starMeter)/3
imdbdf$avgStarMeter <- avgStarMeter 

LogavgStarMeter <- log(avgStarMeter)
imdbdf$LogavgStarMeter <- LogavgStarMeter 

imdbdf$maturityRating

# Bucketing maturity rating into required buckets
levels(imdbdf$maturityRating) <- c(levels(imdbdf$maturityRating), "Others")
imdbdf$maturityRating[(imdbdf$maturityRating != 'R')&(imdbdf$maturityRating != 'PG-13')&(imdbdf$maturityRating != 'PG')] <- 'Others' 


#Dropping columns no longer required
imdbdf <- subset(imdbdf, select = -c(director, dirimdbrating, directorRank,colourFilm, actor1_starMeter,  actor2_starMeter, actor3_starMeter,LogavgStarMeter))



#Not Factors - "action","adventure","scifi","thriller","musical","romance","western","sport","horror","drama","war","animation","crime",
# convert to factors
cat_var <- c("maturityRating","ranking_bin")
for (a in cat_var)
  eval(parse(text=paste("imdbdf$", a, "=as.factor(imdbdf$", a, ")", sep="")))
sapply(imdbdf, is.factor)






#removing outliers
########CHANGE DATA SOURCE FOR 'imdbdf'
imdbdf=read.csv("C:/Users/workV/Downloads/imdbdf.csv")
attach(imdbdf)

## FINAL TRANSFORMED DATASET
attach(imdbdf)


model1 = glm(imdbScore~ movieBudget +poly(duration, 2) + maturityRating +poly(nbNewsArticles,2) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin + poly(movieMeter_IMDBpro,2)+avgStarMeter, data=imdbdf)
summary(model1)


#best one so far
model2 = glm(imdbScore~ movieBudget +poly(duration, 3) + maturityRating +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin + movieMeter_IMDBpro+poly(avgStarMeter,2), data=imdbdf)
summary(model2)

model3 = glm(imdbScore~ movieBudget +poly(duration, 2) + maturityRating +poly(nbNewsArticles,4) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin + movieMeter_IMDBpro+poly(avgStarMeter,3), data=imdbdf)
summary(model3)

model4 = glm(imdbScore~ movieBudget +poly(duration, 2) + maturityRating +poly(nbNewsArticles,4) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin +poly(avgStarMeter,3), data=imdbdf)
summary(model4)



model5 = glm(imdbScore~ movieBudget +poly(duration, 3) +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin +poly(avgStarMeter,2), data=imdbdf)
summary(model5)

model6 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin +poly(avgStarMeter,2), data=imdbdf)
summary(model6)

model7 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action + adventure+scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin, data=imdbdf)
summary(model7)

model8 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action +scifi +thriller + musical + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin, data=imdbdf)
summary(model8)

model9 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action +scifi +thriller + romance + western +sport +horror + drama + war + animation +
              crime +ranking_bin, data=imdbdf)
summary(model9)


model10 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + action +scifi +thriller + romance + western +sport +horror + drama + war + animation +
              crime, data=imdbdf)
summary(model10)

model11 = glm(imdbScore~ movieBudget +poly(duration, 2) +poly(nbNewsArticles,3) +nbFaces + western + drama + animation + ranking_bin, data=imdbdf)
summary(model11)


#Removing Heteroskedasticity

coeftest(model11, vcov=vcovHC(model2, type="HC1"))
summary(model11)

#VALIDATION TESTS
sample=sample.split(imdbdf$imdbScore, SplitRatio=0.5)
train_set=subset(imdbdf, sample==TRUE)
test_set=subset(imdbdf, sample==FALSE)

#VALIDATION APPROACH
#model 1

actual=test_set$imdbScore
prediction=predict(model1, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 2

actual=test_set$imdbScore
prediction=predict(model2, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 3

actual=test_set$imdbScore
prediction=predict(model3, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 4

actual=test_set$imdbScore
prediction=predict(model4, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 5

actual=test_set$imdbScore
prediction=predict(model5, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 6

actual=test_set$imdbScore
prediction=predict(model6, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 7

actual=test_set$imdbScore
prediction=predict(model7, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 8

actual=test_set$imdbScore
prediction=predict(model8, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 9

actual=test_set$imdbScore
prediction=predict(model9, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 10

actual=test_set$imdbScore
prediction=predict(model10, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#model 11

actual=test_set$imdbScore
prediction=predict(model11, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE

#LOOCV TEST

#MODEL1
mse=cv.glm(imdbdf, model1)$delta[1]
mse

#MODEL2
mse=cv.glm(imdbdf, model2)$delta[1]
mse

#MODEL3
mse=cv.glm(imdbdf, model3)$delta[1]
mse

#MODEL4
mse=cv.glm(imdbdf, model4)$delta[1]
mse

#MODEL5
mse=cv.glm(imdbdf, model5)$delta[1]
mse

#MODEL6
mse=cv.glm(imdbdf, model6)$delta[1]
mse

#MODEL7
mse=cv.glm(imdbdf, model7)$delta[1]
mse

#MODEL8
mse=cv.glm(imdbdf, model8)$delta[1]
mse

#MODEL9
mse=cv.glm(imdbdf, model9)$delta[1]
mse

#MODEL10
mse=cv.glm(imdbdf, model10)$delta[1]
mse

#MODEL11
mse=cv.glm(imdbdf, model11)$delta[1]
mse