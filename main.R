library(dplyr)
library(purrr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(gridExtra)

# acquire data
pokemon <- read.csv('data/pokemon.csv')
combats <- read.csv('data/combats.csv')
multipliers <- read.csv('data/multipliers.csv')

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

# multipliers[Attacker type, Defender type]

# change empty type to 'None'
levels(pokemon$Type.2)[1] <- 'None'

# join pokemon and combats
pokemon <- pokemon %>% rename(First_pokemon = X.)
combats_full <- merge(combats, pokemon, by='First_pokemon')
pokemon <- pokemon %>% rename(Second_pokemon = First_pokemon)
combats_full <- merge(combats_full, pokemon, by='Second_pokemon')

# simplify winner
combats_full <- combats_full %>% mutate(winner_xy = ifelse(First_pokemon == Winner, 'x', 'y'))

# simplify stats
combats_full <- combats_full %>% mutate(Attack.Diff = Attack.x-Attack.y, 
                                        HP.Diff = HP.x-HP.y,
                                        Defense.Diff=Defense.x-Defense.y, 
                                        Sp.Atk.Diff = Sp..Atk.x-Sp..Atk.y, 
                                        Sp.Def.Diff = Sp..Def.x-Sp..Def.y,
                                        Speed.Diff = Speed.x-Speed.y)

# add type multipliers
combats_full <- combats_full %>% 
  rowwise() %>% 
  mutate(Mult = multipliers[Type.1.x, Type.1.y] *
           multipliers[Type.1.x, Type.2.y] *
           multipliers[Type.2.x, Type.1.y] *
           multipliers[Type.2.x, Type.2.y]) %>% 
  ungroup()

# add attacking power = Attack / HP
combats_full <- combats_full %>%
  mutate(Attack.Pow.x = Attack.x / (HP.y + Defense.y),
         Attack.Pow.y = Attack.y / (HP.x + Defense.x))

# divide into training and testing sets
set.seed(1018)
sample <- sample.int(n = nrow(combats_full), size = floor(.75*nrow(combats_full)), replace = F)
combats_train <- combats_full[sample, ]
combats_test  <- combats_full[-sample, ]

names(combats_train)

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


# plot the models
par(mfrow=c(2,1))
rpart.plot(model_A, type=0, extra=6, main=paste('Model A - ', acc_A, '%', sep=''))
rpart.plot(model_B, type=0, extra=6, main=paste('Model B - ', acc_B, '%', sep=''))

# generate association rules regarding types
type_rules <- arules::apriori(combats_train[c('Type.1.x', 'Type.2.x', 'Type.1.y', 'Type.2.y', 'winner_xy')], parameter = list(supp = 0.0001, conf = 1))
type_rules <- subset(type_rules, subset = rhs %pin% "winner_xy=")

inspect(head(sort(type_rules, by='count'), n=20))
