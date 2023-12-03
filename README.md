# Construcció de models multivariants

### Exercici 1:

Carreguem les dades i les visualitzem:

```{r, results=F}
socsupport <- read.csv("socsupport.csv", row.names = 1)
head(socsupport)
```

Seleccionem les columnes:

```{r}
dat0 = select(socsupport, c(10,11,12,13,14,15,16,17,18,19))
```

Fem un sapply per veure si hi ha valors NaN:

```{r, results=FALSE}
sapply(dat0, function(x) sum(is.na(x)))
```

Eliminem les files que tinguin valors NaN, posteriorment comprovem que ja no n'hi hagi:

```{r}
dat01 <- na.omit(dat0)
sapply(dat01, function(x) sum(is.na(x)))
```

Observem quantes files hem eliminat:

```{r, results="hold"}
dim(dat0) # Abans d'eliminar dades.
dim(dat01) # Després d'eliminar dades.
```

Observem que només hem eliminat 5 files, i que, per tant, no ens afectarà molt.

\newpage

Observem la correlació:

```{r}
R = cor(dat01)
cor.plot(R)
```

Quan tenim correlacions tan altes, com en aquest cas, algunes de 0.85, 0.86, 0.75, etc.
entrem en el problema de la correlació entre les variables independents, una
solució seria treure aquelles que estan molt interrelacionades o, per una altra
banda, aplicar PCA i reduir la dimensió.

Matriu de correlació diferent de la matriu identitat:

```{r}
cortest.bartlett(cor(dat01), n=dim(dat01))
```
Observem que el pvalor és menor a una alfa de l'1%, per tant, hi ha correlació entre variables.

\newpage

Normalitat multivariada:

```{r}
mshapiro.test(t(dat01))
```

Nombre de components:

```{r}
e = eigen(R)$val
plot(e, type="b", pch=20, col="blue", lwd=2, 
     main="Gràfic de sedimentació", xlab="Nº components")
abline(h=1, lwd=2, col="red", lty = 2)
```

Observem que a partir de la quarta component comença a sedimentar. Veiem que a partir de la tercera pca, obtenim un 80% de la variància.

\newpage

Generem la nova matriu de dades:

```{r}
facto = principal(dat01, nfactors = 3, rotate = "none")
facto$loadings

facto.rota=principal(dat01, nfactors = 3, rotate = "varimax")
facto.rota$loadings #
```

\newpage

Veiem com afecta cada variable a les components

```{r}
fa.diagram(facto.rota)
facto.rota$communality
```

Observem que les variables queden ordenats per ordre d'importància, en la PR1, són aquelles que el seu valor supera el 0,7, Pr2 és la segona i Pr3 la tercera, per tant, les variables que tenen més pes queden agrupades dins de Pr1.

\newpage

Comuns:

```{r}
facto.rota$communality
```

Puntuacions:

```{r}
head(facto.rota$scores)
```

\newpage

### Exercici 2:

Llegim i visualitzem les dades:

```{r, results=FALSE}
dat1 <- read.csv("spam7.csv", row.names = 1)
head(dat1)

summary(dat1)
```

Fem un arbre amb totes les variables:

```{r}
spam.rpart <- rpart(yesno ~ crl.tot + 
                      dollar + 
                      bang +
                      money + 
                      n000 + 
                      make, 
                    data=dat1)

prp(spam.rpart, extra=2)
```

\newpage

Fem una taula de sensibilitat i especificitat:

```{r}
tab1 <- table(predict(spam.rpart, type = "class"), dat1$yesno)
diag(tab1) / colSums( tab1 )
sum(diag(tab1)) / sum(tab1)
```

Observem que la sensibilitat, o el bé que detecti els casos que no són spam (n), veiem que és de 0.90.
En el cas de la (y), és el bé que detecta els casos que si són spam, i veiem que és de 0,79.
Finalment, ens dona "l'acuraci" que és de 0.86, és a dir, lo bé que encerta en general.

Fem el model logístic:

Abans de res, és necessari que "si i no", siguin 1 o 0, per tant, efectuem el canvi:

```{r, results="hold"}
table(dat1$yesno)
dat1$yn <- 1*( dat1$yesno == "y" )
head(dat1)

spam.logis <- glm(yn ~ crl.tot + 
                    dollar + 
                    bang +
                    money + 
                    n000 + 
                    make, 
                  data = dat1,
                  family = binomial)
```

```{r, results=FALSE}
summary(spam.logis)
```

Observem els resultats, i podem deduir que com més grans siguin els números de la columna "Estimate" relacionada amb les variables "ctr.tot, dollar, bang, money i n000", les probabilitats que siguin spam augmenten.
Veiem que "make" no surt marcada, i per tant es podria considerar treure-la del model.

\newpage

### Exercici 3:

Llegim i visualitzem les dades:

```{r, results="hold", results=FALSE}
dat3 <- read.csv("BEPS.csv", row.names = 1)
head(dat3)
summary(dat3)
table(dat3$vote)
```

Fem el dendrograma de les dades:

```{r}
fit <- rpart(vote~., data = dat3, method = 'class',)
rpart.plot(fit)
```

Hem fet un model classificador/arbre de clarificació el qual surt exposat en el dendrograma. Quan és de color roig, identifica que la predicció de les dades d'aquests individus amb aquests perfils seran que votarà conservadors.

Observem que en el cas dels votants conservadors, per la part esquerra del dendrograma, els votants conservadors consideren el "Hague" si és més gran o igual que 4. En el cas de "political knowledge" el consideren si és més gran o igual que 2.

Veiem que en el cas dels votants laboristes, per la part dreta del dendrograma, els votants conservadors consideren el "political knowledge" si és superior o igual que 1.
 
\newpage

### Exercici 4:

Llegim i visualitzem les dades:

```{r, results=FALSE}
dat4 <- read.csv("FirstYearGPA.csv", row.names = 1)
head(dat4)
```

```{r}
summary(dat4)
```

Observem, després de fer el summary, que el GPA no té molt de sentit, ja que dona per sobre de 4. Realment les notes del GPA van de 0 a 4.

\newpage

Eliminem dades que no pertoquen, en aquest cas, eliminarem només les dades que el GPA sigui superior a 4:  

```{r}
dat4_2 <- subset(dat4, GPA <= 4)
summary(dat4_2)
```
Observem que ara, efectivament, el GPA no està per sobre de 4.

Observem quantes dades hem eliminat:

```{r}
dim(dat4) # Abans d'eliminar dades.
dim(dat4_2) # Després d'eliminar dades.
```
Ens adonem que només hem eliminat un cas, per tant, podem pensar que era una dada equivocada.
També és molt positiu, ja que en estar eliminant només una dada i no moltes, no ens afectarà molt.

\newpage

Fem el model, en tenir les dades contínues, fem una regressió estàndard:

```{r}
fit4 <- glm(GPA ~ . , data = dat4_2)
summary(fit4)
```

Observem que les variables més significatives són HSGPA (Notes de l'institut), HU (quantitat d'hores que s'ha fet humanitats) i white.
M'ha sorprès negativament que surti t'han destacat la variable "white", et fa pensar sobre els privilegis que es poden tenir a EEUU, depenent de la teva ètnia.

En canvi, m'ha sorprès molt positivament que no sigui significatiu el sexe (que no siguis sempre significatiu, no vol dir que sigui dolent),
això vol dir, que les notes GPA no es veuen afectades per aquesta raó.

També és sorprenent que no siguin significatives algunes variables com sATV (Expressió oral i escrita) o SATM (Matemàtiques),
et dona a penar que l'itinerari no està tan marcat com aquí i que influeix molt que l'educació sigui privada.

Observem que, tot i no estar marcada com a significativa, la variable FirstGen, observem que es negativa, i això ens explica que penalitza
aquelles generacions que són les primeres que estudien (dins de la seva família).

Finalment, cal recalcar que totes les variables que estan al model ens aporten informació molt important (com sexe o FirstGen),
i no només les que estan marcades com a significatives, no ens hem de quedar només en les més significatives, sinó que totes les variables afecten.
