#Prosze ustawic odpowiednia sciezke
#setwd()
movies <- read.csv("movie_metadata.csv")

#usuniecie duplikatow
movies <- movies[!duplicated(movies$movie_title),]


#Usuwanie niepotrzebnych kolumn
movies_important <- movies[c(1,3,4,5,6,8,9,10,13,14,16,19,20,21,22,23,24,25,26,27,28)]



#Podzielenie cechy "genres" na pojedyncze gatunki
distinct_values <- unique(unlist(strsplit(movies_important$genres, "\\|")))
print(distinct_values)
for (genre in distinct_values){
  movies_important[genre] <- ifelse(grepl(genre, movies_important$genres),1,0)
}

#Usuwanie NA dla cech "gross" i "budget"
movies_important <- movies_important[complete.cases(movies_important[, c("gross","budget","num_critic_for_reviews","duration")]), ]


#Zastapienie pustych wartosci dla pojedynczych rekordow dla cechy "color"
unique(movies_important$color)
index_color <- which(movies_important$color=="")
#biorac pod uwage rok filmu, bedzie on zapewne kolorowy (zostalo to rowniez sprawdzone)
movies_important[index_color[1],]['color']='Color'
movies_important[index_color[2],]['color']='Color'
movies_important$color <- gsub(" ", "_", movies_important$color)



#Usuwanie NA dla cech "director_facebook_likes","actor_1_facebook_likes", "actor_2_facebook_likes", "actor_3_facebook_likes"
movies_important <- movies_important[complete.cases(movies_important[, c("director_facebook_likes","actor_1_facebook_likes", "actor_2_facebook_likes", "actor_3_facebook_likes")]), ]


#Zastapienie NA dla cechy "facenumber", czyli liczby twarzy na plakacie
index_facenumber <- which(is.na(movies_important$facenumber_in_poster))
movies_important[index_facenumber,]


#samodzielne sprawdzenie (na internecie) brakujacych plakatow
movies_important[index_facenumber[1],]['facenumber_in_poster']=15
movies_important[index_facenumber[2],]['facenumber_in_poster']=8
movies_important[index_facenumber[3],]['facenumber_in_poster']=2
movies_important[index_facenumber[4],]['facenumber_in_poster']=2
movies_important[index_facenumber[5],]['facenumber_in_poster']=1
movies_important[index_facenumber[6],]['facenumber_in_poster']=5


#Zastapienie pustych wartosci dla pojedynczych rekordow dla cechy "language"
index_language <- which(movies_important$language=="")
movies_important[index_language,]


#samodzielne sprawdzenie (na internecie) jezykow
movies_important[index_language[1],]['language']='English'
movies_important[index_language[2],]['language']='English'
movies_important[index_language[3],]['language']='English'

#Przygotowanie cechy "content rating" opisujacej kategorie wiekowa filmu
index_cont_rating <- which(movies_important$content_rating=="" | movies_important$content_rating=="Unrated" | movies_important$content_rating=="Passed" | movies_important$content_rating=="Approved"| movies_important$content_rating=="R"| movies_important$content_rating=="Not Rated")
#Wszelkie rozne kategorie wiekowe odpowiadajace za filmy nieocenione zastapione przez jedna wartosc 'Not Rated' dla spojnosci
for (i in 1:length(index_cont_rating)){
  movies_important[index_cont_rating[i],]['content_rating']='Not_Rated'
}

movies_important$content_rating[movies_important$content_rating == 'M']   <- 'PG' 
movies_important$content_rating[movies_important$content_rating == 'GP']  <- 'PG' 
movies_important$content_rating[movies_important$content_rating == 'X']   <- 'NC-17'

unique(movies_important$content_rating)


#usuniecie cechy "genres", jest ona nam juz niepotrzebna jako ze mamy podzial na pojedyncze gatunki
movies_final <- movies_important[-8]

#zamiana cech na zmienne typu factor
movies_final$color <- factor(movies_final$color)
movies_final$language <- factor(movies_final$language)
movies_final$country <- factor(movies_final$country)
movies_final$content_rating <- factor(movies_final$content_rating)

#Przygotowanie cechy 'country', zamienione zle wpisane kraje na odpowiednie
movies_important$country[movies_important$country=='Official site'] <- 'USA'
movies_important$country[movies_important$country=='New Line'] <- 'USA'
movies_important$country <- gsub(" ", "_", movies_important$country)
unique(movies_important$country)

#jako ze bedziemy mieli doczynienie z zadaniem klasyfikacji, podzial ocen na grupy (podzial zostal dokonany na podstawie wnioskow z internetu oraz naszego przekonania)
hist(movies_final$imdb_score, breaks = 10, main = 'Histogram ocen filmów', xlab = 'Ocena', ylab='Liczba ocen', col='azure3')
abline(v = 7, col = "darkblue", lwd = 5)
legend("topright", legend = "Ocena 7", col = "darkblue", lwd = 5)

for (j in 1:length(movies_final$director_facebook_likes)){
  if(movies_final$imdb_score[j] < 7){
    movies_final$imdb_score[j] = 'slabe'
  }
  else {
    movies_final$imdb_score[j] = 'dobre'
  }
}
#movies_final$imdb_score <- factor(movies_final$imdb_score)

table(movies_final$imdb_score)

set.seed(123)
#Podzial na zbior treningowy oraz testowy, stosunek 80/20
train_sample = sample(length(movies_final$director_facebook_likes), 4*trunc(length(movies_final$director_facebook_likes)/5))

movies_final$imdb_score <- factor(movies_final$imdb_score)

film_trening = movies_final[train_sample,]
film_test = movies_final[-train_sample,]

prop.table(table(film_trening$imdb_score))
prop.table(table(film_test$imdb_score))


#Budowa modelu kNN

#Przygotowanie zbioru danych pod kNN
movies_final_knn <- movies_final
movies_final_knn$color <- ifelse(movies_final_knn$color =='Color',1,0)
movies_final_knn$language <- ifelse(movies_final_knn$language =='English',1,0)
movies_final_knn$country <- ifelse(movies_final_knn$country =='USA',1,0)

for (rating in unique(movies_final_knn$content_rating)){
  movies_final_knn[rating] <- ifelse(movies_final_knn$content_rating == rating,1,0)
}
movies_final_knn_final <- movies_final_knn[c(-14,-19)]

#Skalowanie danych pod kNN - podejscie z normalizacja min-max
min_max_scale <- function(x) {
  scaled_x <- (x - min(x)) / (max(x) - min(x))
  return(scaled_x)
}
movies_final_knn_final_u <- movies_final_knn_final[complete.cases(movies_final_knn_final[, c(2,3)]), ]
movies_final_knn_final_norm1 <- as.data.frame(lapply(movies_final_knn_final_u[-17], min_max_scale))
movies_final_knn_final_norm1["IMDB score"] <-movies_final_knn_final_u[17]
movies_final_knn_final_norm1_final <-movies_final_knn_final_norm1[c(-39,-40,-41)]
train_sample2 = sample(length(movies_final_knn_final_norm1_final$duration), 4*trunc(length(movies_final_knn_final_norm1_final$duration)/5))

film_trening_knn1 = movies_final_knn_final_norm1_final[train_sample2,-46]
film_test_knn1 = movies_final_knn_final_norm1_final[-train_sample2,-46]

knn_labels_train1 <- movies_final_knn_final_norm1_final[train_sample2,46]
knn_labels_test1 <- movies_final_knn_final_norm1_final[-train_sample2,46]

prop.table(table(knn_labels_train1))
prop.table(table(knn_labels_test1))

#Implementacja algorytmu
install.packages("class")
library("class")

knn_model1 <- knn(train=film_trening_knn1, test = film_test_knn1, cl = knn_labels_train1, k=trunc(sqrt(length(knn_labels_test1))))

install.packages("gmodels")
library(gmodels)

z=CrossTable(x = knn_labels_test1, y = knn_model1, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('rzeczywiste dane', 'predykcja'))
((z$t[1]+z$t[4])/(z$t[1]+z$t[2]+z$t[3]+z$t[4]))*100

#optymalizacja 
temp = NULL
iteracja = NULL
for (i in 1:33){
  print(i*15)
  knn_model1 <- knn(train=film_trening_knn1, test = film_test_knn1, cl = knn_labels_train1, k=i*15)
  
  k=CrossTable(x = knn_labels_test1, y = knn_model1, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i*15
}
plot(iteracja, temp, type ='l', xlab = "Wartość parametru k", ylab = "Precyzja dla parametru k")
# widac ze od 90 skutecznosc spada
temp = NULL
iteracja = NULL
for (i in 1:33){
  print(i*3)
  knn_model1 <- knn(train=film_trening_knn1, test = film_test_knn1, cl = knn_labels_train1, k=i*3)
  
  k=CrossTable(x = knn_labels_test1, y = knn_model1, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i*3
}
plot(iteracja, temp, type ='l', xlab = "Wartość parametru k", ylab = "Precyzja dla parametru k")
# powyżej 35 bez sensu sprawdzać
temp = NULL
iteracja = NULL
for (i in 1:35){
  print(i)
  knn_model1 <- knn(train=film_trening_knn1, test = film_test_knn1, cl = knn_labels_train1, k=i)
  
  k=CrossTable(x = knn_labels_test1, y = knn_model1, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i
}
plot(iteracja, temp, type ='l', xlab = "Wartość parametru k", ylab = "Precyzja dla parametru k")

which(temp==max(temp))


# dla 5 i 11 dziala najlepiej

# pytanie czy to nie zbytnie dopasowanie do danych


temp = NULL
iteracja = NULL
losowanie = NULL
for (p in 1:50){
  train_sample1 = sample(length(movies_final_knn_final_norm1_final$duration), 4*trunc(length(movies_final_knn_final_norm1_final$duration)/5))
  
  
  film_trening_knn = movies_final_knn_final_norm1_final[train_sample1,-46]
  str(film_trening_knn)
  film_test_knn = movies_final_knn_final_norm1_final[-train_sample1,-46]
  
  knn_labels_train <- movies_final_knn_final_norm1_final[train_sample1,46]
  knn_labels_test <- movies_final_knn_final_norm1_final[-train_sample1,46]
  for (i in 1:35){
    knn_model <- knn(train=film_trening_knn, test = film_test_knn, cl = knn_labels_train, k=i)
    k=CrossTable(x = knn_labels_test, y = knn_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
    u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
    temp[i+(p-1)*35] = u
    iteracja[i+(p-1)*35] = i
    losowanie[i+(p-1)*35] = p
  }
}
srednia = NULL
suma = rep(0, 35)
for (c in 1:35){
  for (ur in 1:50){
    suma[c] = suma[c] + temp[35*(ur-1) + c]
  }
}
for (c1 in 1:35){
  srednia[c1] = suma[c1]/50
}
iteracja_help = seq(1:35)
plot(iteracja_help,srednia, type = 'l', xlab = 'Parametr k', ylab='Średnia wartość predykcji dla parametru')


wariancja = rep(0,35)
for (c in 1:35){
  for (ur in 1:50){
    wariancja[c] = wariancja[c] + (temp[35*(ur-1) + c] - srednia[c])**2
  }
}
plot(iteracja_help, wariancja/49, type ='l', xlab = 'Parametr k', ylab = 'Wariancja dla parametru')

#srednia
sorted_indices <- order(srednia, decreasing = TRUE)[1:5]
sorted_values <- srednia[sorted_indices]
print(sorted_indices)
print(sorted_values)

#wariancja
sorted_indices <- order(wariancja)[1:5]
sorted_values <- wariancja[sorted_indices]
print(sorted_indices)
print(sorted_values)






#inne podejscie za pomoca funkcji scale przeskalowanie danych
movies_final_knn_final_norm <- as.data.frame(scale(movies_final_knn_final[-17]))

movies_final_knn_final_norm["IMDB score"] <-movies_final_knn_final[17]
movies_final_knn_final_norm_final <-movies_final_knn_final_norm[c(-39,-40,-41)]
movies_final_knn_final_norm_final <- movies_final_knn_final_norm_final[complete.cases(movies_final_knn_final_norm_final[, c(2,3)]), ]


#podzial na zbior treningowy i testowy


train_sample1 = sample(length(movies_final_knn_final_norm_final$duration), 4*trunc(length(movies_final_knn_final_norm_final$duration)/5))


film_trening_knn = movies_final_knn_final_norm_final[train_sample1,-46]
film_test_knn = movies_final_knn_final_norm_final[-train_sample1,-46]

knn_labels_train <- movies_final_knn_final_norm_final[train_sample1,46]
knn_labels_test <- movies_final_knn_final_norm_final[-train_sample1,46]

prop.table(table(knn_labels_train))
prop.table(table(knn_labels_test))


#Implementacja algorytmu
knn_model <- knn(train=film_trening_knn, test = film_test_knn, cl = knn_labels_train, k=trunc(sqrt(length(knn_labels_test))))

z=CrossTable(x = knn_labels_test, y = knn_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('rzeczywiste dane', 'predykcja'))
((z$t[1]+z$t[4])/(z$t[1]+z$t[2]+z$t[3]+z$t[4]))*100


temp = NULL
iteracja = NULL
#optymalizacja 
for (i in 1:33){
  print(i*15)
  knn_model <- knn(train=film_trening_knn, test = film_test_knn, cl = knn_labels_train, k=i*15)
  
  k=CrossTable(x = knn_labels_test, y = knn_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i*15
}
plot(iteracja, temp, type ='l', xlab = "Wartość parametru k", ylab = "Precyzja dla parametru k")
# widac ze od 90 skutecznosc spada
temp = NULL
iteracja = NULL
for (i in 1:33){
  print(i*3)
  knn_model <- knn(train=film_trening_knn, test = film_test_knn, cl = knn_labels_train, k=i*3)
  
  k=CrossTable(x = knn_labels_test, y = knn_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i*3
}
plot(iteracja, temp, type ='l', xlab = "Wartość parametru k", ylab = "Precyzja dla parametru k")
# powyzej 35 bez sensu sprawdzac
temp = NULL
iteracja = NULL
for (i in 1:35){
  print(i)
  knn_model <- knn(train=film_trening_knn, test = film_test_knn, cl = knn_labels_train, k=i)
  
  k=CrossTable(x = knn_labels_test, y = knn_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i
}
plot(iteracja, temp, type ='l', xlab = "Wartość parametru k", ylab = "Precyzja dla parametru k")

which(temp==max(temp))

# dla 12 i 16 działa najlepiej

# pytanie czy to nie przypadek i zbytnie dopasowanie do danych
temp = NULL
iteracja = NULL
losowanie = NULL
for (p in 1:50){
  train_sample1 = sample(length(movies_final_knn_final_norm_final$duration), 4*trunc(length(movies_final_knn_final_norm_final$duration)/5))
  
  
  film_trening_knn = movies_final_knn_final_norm_final[train_sample1,-46]
  str(film_trening_knn)
  film_test_knn = movies_final_knn_final_norm_final[-train_sample1,-46]
  
  knn_labels_train <- movies_final_knn_final_norm_final[train_sample1,46]
  knn_labels_test <- movies_final_knn_final_norm_final[-train_sample1,46]
  for (i in 1:35){
    knn_model <- knn(train=film_trening_knn, test = film_test_knn, cl = knn_labels_train, k=i)
    k=CrossTable(x = knn_labels_test, y = knn_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
    u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
    temp[i+(p-1)*35] = u
    iteracja[i+(p-1)*35] = i
    losowanie[i+(p-1)*35] = p
  }
}
srednia = NULL
suma = rep(0, 35)
for (c in 1:35){
  for (ur in 1:50){
    suma[c] = suma[c] + temp[35*(ur-1) + c]
  }
}
for (c1 in 1:35){
  srednia[c1] = suma[c1]/50
}
iteracja_help = seq(1:35)
plot(iteracja_help,srednia, type = 'l', xlab = 'Parametr k', ylab='Średnia wartość predykcji dla parametru')

wariancja = rep(0,35)
for (c in 1:35){
  for (ur in 1:50){
    wariancja[c] = wariancja[c] + (temp[35*(ur-1) + c] - srednia[c])**2
  }
}
plot(iteracja_help, wariancja/49, type ='l', xlab = 'Parametr k', ylab = 'Wariancja dla parametru')

#srednia
sorted_indices <- order(srednia, decreasing = TRUE)[1:5]
sorted_values <- srednia[sorted_indices]
print(sorted_indices)
print(sorted_values)

#wariancja
sorted_indices <- order(wariancja)[1:5]
sorted_values <- wariancja[sorted_indices]
print(sorted_indices)
print(sorted_values/49)


# jak widać po dokladniejszym wglebieniu sie w dane najlepiej wybrac k=13









#Budowa modelu
install.packages("C50")

library("C50")

#MODEL PIERWSZY
model <- C5.0(film_trening[-18], film_trening$imdb_score)
model

#przewidywanie oceny filmu
movie_pred <- predict(model, film_test[-18])

CrossTable(x = film_test$imdb_score, y = movie_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('rzeczywiste dane', 'predykcja'))

#ADAboost, polepszanie drzewa

lepszy_model <- C5.0(film_trening[-18], film_trening$imdb_score, trials= 10)
lepszy_model

lepszy_movie_pred <- predict(lepszy_model, film_test[-18])

CrossTable(x = film_test$imdb_score, y = lepszy_movie_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('rzeczywiste dane', 'predykcja dla 10 prób'))

lepszy_model_100 <- C5.0(film_trening[-18], film_trening$imdb_score, trials= 100)
lepszy_movie_pred_100 <- predict(lepszy_model_100, film_test[-18])

CrossTable(x = film_test$imdb_score, y = lepszy_movie_pred_100, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('rzeczywiste dane', 'predykcja dla 100 prób'))

#dla wiecej trialsow lepiej
model2 <- C5.0(film_trening[-18], film_trening$imdb_score, control = C5.0Control(minCases = 80))
model2
movie_pred2 <- predict(model2, film_test[-18])
tabela = CrossTable(x = film_test$imdb_score, y = movie_pred2, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
((tabela$t[1]+tabela$t[4])/(tabela$t[1]+tabela$t[2]+tabela$t[3]+tabela$t[4]))*100

plot(model2)
#FINALNY MODEL
model3 <- C5.0(film_trening[-18], film_trening$imdb_score, trials = 100, control = C5.0Control(minCases = 80))
model3
movie_pred3 <- predict(model3, film_test[-18])
summary(model3)

CrossTable(x = film_test$imdb_score, y = movie_pred3, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, , dnn = c('rzeczywiste dane', 'predykcja dla 100 prób'))



#test
teste = movies_final_knn_final_norm1[c(1,2,3,7,8,11,12,13,14,15,31,37,49)]
train_sample4 = sample(length(teste$duration), 4*trunc(length(teste$duration)/5))


film_trening_knn4 = teste[train_sample4,-13]
film_test_knn4 = teste[-train_sample4,-13]

knn_labels_train4 <- teste[train_sample4,13]
knn_labels_test4 <- teste[-train_sample4,13]
prop.table(table(knn_labels_train4))
prop.table(table(knn_labels_test4))
knn_model4 <- knn(train=film_trening_knn4, test = film_test_knn4, cl = knn_labels_train4, k=trunc(sqrt(length(knn_labels_test))))

z=CrossTable(x = knn_labels_test1, y = knn_model1, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
#optymalizacja 
temp = NULL
iteracja = NULL
for (i in 1:33){
  print(i*15)
  knn_model5 <- knn(train=film_trening_knn4, test = film_test_knn4, cl = knn_labels_train4, k=i*15)
  
  k=CrossTable(x = knn_labels_test4, y = knn_model5, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i*15
}
plot(iteracja, temp, type ='l')
# widze że od 90 skuteczność spada
temp = NULL
iteracja = NULL
for (i in 1:33){
  print(i*3)
  knn_model6 <- knn(train=film_trening_knn4, test = film_test_knn4, cl = knn_labels_train4, k=i*3)
  
  k=CrossTable(x = knn_labels_test4, y = knn_model6, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i*3
}
plot(iteracja, temp, type ='l')
# powyżej 35 bez sensu sprawdzać
temp = NULL
iteracja = NULL
for (i in 1:35){
  print(i)
  knn_model7 <- knn(train=film_trening_knn4, test = film_test_knn4, cl = knn_labels_train4, k=i)
  
  k=CrossTable(x = knn_labels_test4, y = knn_model7, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
  print(u)
  temp[i] = u
  iteracja[i] = i
}
plot(iteracja, temp, type ='l')

which(temp==max(temp))


# dla 16 dziala najlpeiej

#sprawdzam czy nie przypadek, niekonieczne do modelu. Sprawdzamy tylko stabliność modelu.
temp = NULL
iteracja = NULL
losowanie = NULL
for (p in 1:50){
  train_sample1 = sample(length(teste$duration), 4*trunc(length(teste$duration)/5))
  
  
  film_trening_knn1 = teste[train_sample1,-13]
  film_test_knn1 = teste[-train_sample1,-13]
  
  knn_labels_train1 <- teste[train_sample1,13]
  knn_labels_test1 <- teste[-train_sample1,13]
  for (i in 1:35){
    knn_model <- knn(train=film_trening_knn1, test = film_test_knn1, cl = knn_labels_train1, k=i)
    k=CrossTable(x = knn_labels_test1, y = knn_model, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
    u = ((k$t[1]+k$t[4])/(k$t[1]+k$t[2]+k$t[3]+k$t[4]))*100
    temp[i+(p-1)*35] = u
    iteracja[i+(p-1)*35] = i
    losowanie[i+(p-1)*35] = p
  }
}
srednia = NULL
suma = rep(0, 35)
for (c in 1:35){
  for (ur in 1:50){
    suma[c] = suma[c] + temp[35*(ur-1) + c]
  }
}
for (c1 in 1:35){
  srednia[c1] = suma[c1]/50
}
iteracja_help = seq(1:35)
plot(iteracja_help,srednia, type = 'l')


wariancja = rep(0,35)
for (c in 1:35){
  for (ur in 1:50){
    wariancja[c] = wariancja[c] + (temp[35*(ur-1) + c] - srednia[c])**2
  }
}
#srednia
sorted_indices <- order(srednia, decreasing = TRUE)[1:5]
sorted_values <- srednia[sorted_indices]
print(sorted_indices)
print(sorted_values)

#wariancja
sorted_indices <- order(wariancja)[1:5]
sorted_values <- wariancja[sorted_indices]
print(sorted_indices)
print(sorted_values/49)

