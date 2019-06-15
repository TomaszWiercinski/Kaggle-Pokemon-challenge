library(dplyr)
library(purrr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(gridExtra)
library(randomForest)

# acquire data
pokemon <- read.csv('data/pokemon.csv')
combats <- read.csv('data/combats.csv')
multipliers <- read.csv('data/multipliers.csv')
tests <- read.csv('data/tests.csv')

# examine attributes
plot.HP <- ggplot(data=pokemon[,5:11], aes(x=HP)) + geom_histogram(fill='#33FF66') + labs(x=NULL, y=NULL, title='HP')
plot.Attack <- ggplot(data=pokemon[,5:11], aes(x=Attack)) + geom_histogram(fill='#33FF66') + labs(x=NULL, y=NULL, title='Attack')
plot.Defense <- ggplot(data=pokemon[,5:11], aes(x=Defense)) + geom_histogram(fill='#33FF66') + labs(x=NULL, y=NULL, title='Defense')

plot.SpAtk <- ggplot(data=pokemon[,5:11], aes(x=Sp..Atk)) + geom_histogram(fill='#33FF66') + labs(x=NULL, y=NULL, title='Sp.Atk')
plot.SpDef <- ggplot(data=pokemon[,5:11], aes(x=Sp..Def)) + geom_histogram(fill='#33FF66') + labs(x=NULL, y=NULL, title='Sp.Def')
plot.Speed <- ggplot(data=pokemon[,5:11], aes(x=Speed)) + geom_histogram(fill='#33FF66') + labs(x=NULL, y=NULL, title='Speed')

gridExtra::grid.arrange(plot.HP, plot.Attack, plot.Defense,
                        plot.SpAtk, plot.SpDef, plot.Speed, ncol=3)

# inspect differences between legendary and non-legendary pokemon
ggplot(pokemon, aes(x=Attack+Defense, y=Sp..Atk+Sp..Def, size=Speed, colour=Legendary)) + geom_point()

#clean multipliers table
temp <- multipliers[,1]
multipliers <- multipliers[,-1]
rownames(multipliers) <- temp
remove(temp)

# multipliers[Attacker type, Defender type]

# change empty type to 'None'
levels(pokemon$Type.2)[1] <- 'None'

# load prepare_data and simplify_winner functions
source('data_preparation.R')

# prepare training data
combats_full <- prepare_data(pokemon, combats)
combats_full <- simplify_winner(combats_full)

# prepare testing data
tests_full <- prepare_data(pokemon, tests)

# divide into training and testing sets
set.seed(1018)
sample <- sample.int(n = nrow(combats_full), size = floor(.75*nrow(combats_full)), replace = F)
combats_train <- combats_full[sample, ]
combats_test  <- combats_full[-sample, ]

# train decision tree model on full stats
model_A <- rpart(winner_xy ~ 
                   Type.1.x+Type.2.x+HP.x+Attack.x+Defense.x+Speed.x+Legendary.x+Generation.x+Sp..Atk.x+Sp..Def.x+
                   Type.1.y+Type.2.y+HP.y+Attack.y+Defense.y+Speed.y+Legendary.y+Generation.y+Sp..Atk.y+Sp..Def.y,
                 data=combats_train, control=rpart.control(cp=0.01))


# train decision tree model on simplified stats
model_B <- rpart(winner_xy ~ 
                   Mult+Legendary.y+Generation.y+Legendary.x+Generation.x+
                   HP.Diff+Attack.Diff+Defense.Diff+Speed.Diff+Sp.Atk.Diff+Sp.Def.Diff,
                 data=combats_train, control=rpart.control(cp=0.01))

# make predictions on model A
pred_A <- predict(model_A, combats_test, type='class')

# make predictions on model B
pred_B <- predict(model_B, combats_test, type='class')

# accuracy of model A
acc_A <- mean(pred_A == combats_test$winner_xy) * 100

# accuracy of model B
acc_B <- mean(pred_B == combats_test$winner_xy) * 100

# examine the models
par(mfrow=c(2,1))
rpart.plot(model_A, type=0, extra=6, main=paste('Model A - ', acc_A, '% accuracy', sep=''))
rpart.plot(model_B, type=0, extra=6, main=paste('Model B - ', acc_B, '% accuracy', sep=''))

printcp(model_A)
printcp(model_B)

par(mfrow=c(1,2))
plotcp(model_A, ylim=c(0, 1.1), main='Model A')
plotcp(model_B, ylim=c(0, 1.1), main='Model B')

# random forest model
set.seed(1010)
model_forest <- randomForest(formula=winner_xy ~ 
                               Type.1.x*Type.2.x*Type.1.y*Type.2.y+Mult+
                               HP.Diff+Attack.Diff+Defense.Diff+Speed.Diff+Sp.Atk.Diff+Sp.Def.Diff,
                             data=combats_train,
                             ntree = 35,
                             mtry = 6,
                             importance = T,
                             do.trace=F,
                             replace=F)
pred_forest <- predict(model_forest, combats_test, type='class', norm.votes = T)
acc_forest <- mean(pred_forest == combats_test$winner_xy)
acc_forest
caret::confusionMatrix(pred_forest, combats_test$winner_xy)
caret::confusionMatrix(predict(model_forest, combats_train, type='class', norm.votes = T), combats_train$winner_xy)
plot(model_forest)
lines(c(0,35), c(1-acc_forest, 1-acc_forest), col='blue')

# random forest model on entire set
model_forest <- randomForest(formula=winner_xy ~ 
                               Type.1.x*Type.2.x*Type.1.y*Type.2.y+Mult+
                               HP.Diff+Attack.Diff+Defense.Diff+Speed.Diff+Sp.Atk.Diff+Sp.Def.Diff,
                             data=combats_full,
                             ntree = 35,
                             mtry = 6,
                             importance = T,
                             do.trace=F,
                             replace=F)
plot(model_forest)
pred_forest <- predict(model_forest, combats_full, type='class', norm.votes = T)
acc_forest <- mean(pred_forest == combats_full$winner_xy)
acc_forest
lines(c(0,35), c(1-acc_forest, 1-acc_forest), col='blue')

# predict test responses
pred_tests <- predict(model_forest, tests_full, type='class', norm.votes = T)
tests_full$predicted <- pred_tests
tests_output <- tests_full %>% select(-c(Name.x, Type.1.x, Type.2.x, HP.x, Attack.x, Defense.x, Sp..Atk.x, Sp..Def.x, Speed.x, Generation.x,
                         Legendary.x, Name.y, Type.1.y, Type.2.y, HP.y, Attack.y, Defense.y, Sp..Atk.y, Sp..Def.y, Speed.y,
                         Generation.y, Legendary.y, Attack.Diff, HP.Diff, Defense.Diff, Sp.Atk.Diff, Sp.Def.Diff, Speed.Diff, 
                         Mult, Attack.Pow.x, Attack.Pow.y))
tests_output %>% head(10)
write.csv(tests_output, "data/tests_output.csv", row.names=F)