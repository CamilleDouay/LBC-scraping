MAPE <- function(true, pred){
  return (1/length(true)*sum(abs(true-pred)/true)*100)
}

df = read.csv('LBC_cars_200.csv') 
head(df)
summary(df)
dim(df)


### Suppression des NA
df = df[!is.na(df$Annee),]

df = df[!is.na(df$Marque),]

df = df[!is.na(df$Modèle),]

dim(df)

### Conservation des marques les plus représentées 
length(nom_marque)
df = df[df$Marque %in% nom_marque, ]

drop_marque = names(sort(table(df$Marque)))[!(names(sort(table(df$Marque))) %in% nom_marque)]
df$Marque <- droplevels(df$Marque, drop_names)

table_model = as.data.frame((table(df$Modèle)))
head(table_model)

nom_model = tableModele$Var1[table_model$Freq>40]
length(nom_model)

drop_model = names(sort(table(df$Modèle)))[!(names(sort(table(df$Modèle))) %in% nom_model)]
drop_model
df = df[df$Modèle %in% nom_model, ]
df$Modèle <- droplevels(df$Modèle, drop_names)


### Change factor for categorical
df$Carburant <- relevel(df$Carburant, ref = "Essence")
df$Marque <- relevel(df$Marque, ref = "Renault")
df$Modèle <- relevel(df$Modèle, ref = "Clio")

df$Marque = as.factor(df$Marque)



### Second variables

df$Collection= as.factor(sapply(df$Annee, function(x) ifelse(x<1986, 1,0)))

df$Pro = factor(sapply(df$Référence,function(x) ifelse(is.na(x), 0, 1)))


# Date du véhicule + symétrie par rapport à 86 

df$Age = abs(1991-df$Annee)

### MODELE 

library("randomForest")

RF = randomForest((Prix) ~ Modèle+ Age +Kilométrage+ Carburant+ Transmission + Carburant:Transmission:Pro+ Pro,
                  data = df, ntree = 100)
pred = (predict(RF))
MAPE(df$Prix, pred)

plot(df$Prix, pred)

hist(df$Prix - pred, breaks = 100)


pred[10]
df[10,]
