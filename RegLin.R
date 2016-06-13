MAPE <- function(true, pred){
  return (1/length(true)*sum(abs(true-pred)/true)*100)
}

df = read.csv('LBC_cars_200_clean.csv') 
head(df)
summary(df)
dim(df)


### Suppression des NA
df = df[!is.na(df$Annee),]

df = df[!is.na(df$Marque),]

df = df[!is.na(df$Modèle),]

dim(df)

### Conservation des marques les plus représentées 

df = df[df$Marque %in% nom_marque, ]

dim(df)

### Change factor for categorical
df$Carburant <- relevel(df$Carburant, ref = "Essence")
df$Marque <- relevel(df$Marque, ref = "Renault")
df$Modèle <- relevel(df$Modèle, ref = "Clio")




### Second variables

df$Collection= sapply(df$Annee, function(x) ifelse(x<1986, 1,0))

df$Pro = factor(sapply(df$Référence,function(x) ifelse(is.na(x), 0, 1)))


# Date du véhicule + symétrie par rapport à 86 

df$Age = abs(1992-df$Annee)


RegLin1 <- lm(log(Prix) ~ Modèle+ (Annee)+Kilométrage +(Annee)*Kilométrage+  Carburant+ Transmission+ Collection+ Pro, data = df)
summary(RegLin1)
pred = exp((predict(RegLin1)))
MAPE(df$Prix, pred)

plot(df$Prix, pred)

hist(df$Prix - pred, breaks = 100)
