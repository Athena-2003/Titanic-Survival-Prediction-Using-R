#setwd("~/Documents/R-Mini-Project")

titanic = read.csv("titanic.csv", stringsAsFactors = FALSE)

survived = subset(titanic, Survived == 1)
femalesurvived = subset(survived, Sex == "female")
malesurvived = subset(survived, Sex == "male")
female = subset(titanic, Sex == "female")
male1 = subset(titanic, Sex == "male" & Pclass == 1)
male2 = subset(titanic, Sex == "male" & Pclass == 2)
male3 = subset(titanic, Sex == "male" & Pclass == 3)

female1 = subset(titanic, Sex == "female" & Pclass == 1)
female2 = subset(titanic, Sex == "female" & Pclass == 2)
female3 = subset(titanic, Sex == "female" & Pclass == 3)

f1cs = subset(femalesurvived, Pclass == 1)
f2cs = subset(femalesurvived, Pclass == 2)
f3cs = subset(femalesurvived, Pclass == 3)

m1cs = subset(malesurvived, Pclass == 1)
m2cs = subset(malesurvived, Pclass == 2)
m3cs = subset(malesurvived, Pclass == 3)



#Total survival Probability on Titanic
# sp = tsnum / tnum

class1survived = subset(survived, Pclass == 1)
class2survived = subset(survived, Pclass == 2)
class3survived = subset(survived, Pclass == 3)

c1 = subset(titanic, Pclass == 1)
c2 = subset(titanic, Pclass == 2)
c3 = subset(titanic, Pclass == 3)

c1s = nrow(class1survived)
c2s = nrow(class2survived)
c3s = nrow(class3survived)

#Survived by classes
c1sp = c1s / nrow(c1)
c2sp = c2s / nrow(c2)
c3sp = c3s / nrow(c3)


probability = function() {
  print("What is your Gender ? ");
  print("Enter f for female and m for male: ");
  sex = readline();
  print("Which class ticket did you purchase ? ");
  print("Enter 1, 2 or 3: ");
  pclass = as.integer(readline());
  if(sex == "f" && pclass == 1) {
    prob = nrow(f1cs) / nrow (female1)
  }else if( sex == "f" && pclass == 2) {
    prob = nrow(f2cs) / nrow (female2)
  }else if( sex == "f" && pclass == 3) {
    prob = nrow(f3cs) / nrow (female3)
  }else if( sex == "m" && pclass == 1) {
    prob = nrow(m1cs) / nrow (male1)
  }else if( sex == "m" && pclass == 2) {
    prob = nrow(m2cs) / nrow (male2)
  }else if( sex == "m" && pclass == 3) {
    prob = nrow(m3cs) / nrow (male3)
  }else{
    print("Invalid Input")
  }
  sprintf("The probability of your survival aboard the Titanic is %f", (prob * 100))
  return(prob)
}

