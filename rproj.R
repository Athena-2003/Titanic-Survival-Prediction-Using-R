# Load Titanic data for analysis. Open in spreadsheet view.
#titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)
#View(titanic)
library(ggplot2)

# Set up factors.
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)



#
# Survival by class
#
ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Class")


#Survival Rates by Embarked point
ggplot(titanic, aes(x = Embarked, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  coord_polar("y") +
  scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Embarked point")


#
#Survival Rates by Class and Sex
#
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Class and Sex")



#
# Survival Rates by Age, Class and Sex
#
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_density(alpha = 0.5) +
  scale_fill_discrete(name = "Status",labels = c("Deceased", "Survived"))+
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Class and Sex")



