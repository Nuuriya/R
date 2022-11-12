install.packages("ggplot")
library(rvest)  
con <- url(
  "https://www.avito.ru/kazan/kvartiry/prodam/novostroyka-ASgBAQICAUSSA8YQAUDmBxSOUg?q=%D0%BA%D1%83%D0%BF%D0%B8%D1%82%D1%8C+%D0%BA%D0%B2%D0%B0%D1%80%D1%82%D0%B8%D1%80%D1%83+%D0%B2+%D0%BD%D0%BE%D0%B2%D0%BE%D1%81%D1%82%D1%80%D0%BE%D0%B9%D0%BA%D0%B5+%D1%80%D1%8F%D0%B4%D0%BE%D0%BC+%D1%81+%D0%BC%D0%B5%D1%82%D1%80%D0%BE", "rb") 
  
bodyy <- read_html(con)%>%
  html_nodes("body")  
bodyy
descr <- bodyy%>%
  html_nodes("h3") %>%
  html_text()
descr
cost <- bodyy%>% 
  html_nodes("[class='price-text-_YGDY text-text-LurtD text-size-s-BxGpL']")%>%
  html_text2()  
cost

metro <- bodyy%>%
  html_nodes("[class='geo-georeferences-SEtee text-text-LurtD text-size-s-BxGpL']")%>%
  html_text2()  
metro

#split description
descr_split = strsplit(descr, ", ") 
df <- data.frame(matrix(unlist(descr_split), nrow=length(descr_split), byrow=TRUE))
df$metro = metro
df$cost = cost
df
write.csv(df , 'N:/documents/анализ интернет данных/avito_dirt.csv', row.names = FALSE)
################################################################################
#get count of rooms
df = read.csv(file = 'N:/documents/анализ интернет данных/avito_dirt.csv')
room = gsub("Квартира-студия", "0",  df$X1)
room = as.numeric(gsub("([0-9]+).*$", "\\1", room))
df_new <-  as.data.frame(room) 
df_new   

df 
#get square of flat  
square = as.numeric(gsub("([0-9]+).*$", "\\1", df$X2))
df_new$square = square

#get floor of flat and total floors in house
floor  <-   gsub("эт.", "",  df$X3)
floor_split = strsplit(floor, "/")
floor_df <- data.frame(matrix(unlist(floor_split), nrow=length(floor_split), byrow=TRUE))
floor_df$X2  <-  as.numeric(gsub("([0-9]+).*$", "\\1", floor_df$X2 ))
floor_df$X2
df_new$floor =  as.numeric(floor_df$X1) 
df_new$total_floor =   as.numeric(floor_df$X2)  

#get cost of flat
cost_num = gsub(" ", "",  df$cost)
cost_num = gsub("₽", "",  cost_num)
cost_num
df_new$cost = as.numeric(cost_num)

#get name of station and distance
dist  <- gsub("[^0-9–-]", "", df$metro) #remove all characters
dist = gsub("-", "60",  dist) #where there is no station set 60 min
dist =  as.numeric(gsub("([0-9]+).*$", "\\1", dist )) #get min dist
dist

station <- gsub("мин.", "",  df$metro) #remove bad words
station <-  gsub("от", "",  station)
station <-  gsub("до", "",  station)
station<- gsub('[[:digit:]]+', '', station)#remove digits
station <- gsub("–", "",  station)#remove bad characters
station <- trimws(station)#remove white space
station

df_new$dist = dist
df_new$station = station
df_new

#df_new$station = station
#one hot encoding
library(sjlabelled)
x <- to_factor(station)
oh_train <- model.matrix(~0+x) 
attr(oh_train, "dimnames")[[2]] <- levels(x)
df_itog <-cbind(df_new,oh_train)
df_itog

write.csv(df_itog , 'N:/documents/анализ интернет данных/avito.csv', row.names = FALSE)
df1 <-read.csv(file = 'N:/documents/анализ интернет данных/avito.csv')
df1 <- df_new
y<-df1$cost
x<-df1[ , -which(names(df1) %in% c("cost"))]
x
y
################################################################################
# all variables as regressors
lm.y <- lm(y ~ ., data = x)
summary(lm.y)

#R-squared — Коэффициент детерминации указывает насколько тесной является связь между факторами регрессии и зависимой переменной, 
#это соотношение объясненных сумм квадратов возмущений, к необъясненным. Чем ближе к 1, тем ярче выражена зависимость.
#Adjusted R-squared — Проблема с $R^2$ в том, что он по любому растет с числом факторов, поэтому высокое значение данного коэффициента может быть обманчивым, 
#когда в модели присутствует множество факторов. Для того, чтобы изъять из коэффициента корреляции данное свойство был придуман
#скорректированный коэффициент детерминации.
#F-statistic — Используется для оценки значимости модели регрессии в целом, является соотношением объяснимой дисперсии, к необъяснимой. Если модель линейной регрессии
#построена удачно, то она объясняет значительную часть дисперсии, оставляя в знаменателе малую часть. Чем больше значение параметра — тем лучше.

#t value — Критерий, основанный на t распределении Стьюдента. Значение параметра в линейной регрессии указывает на значимость фактора,
#принято считать, что при t > 2 фактор является значимым для модели.
#p value — Это вероятность истинности нуль гипотезы, которая гласит, что независимые переменные не объясняют динамику зависимой переменной. 
#Если значение p value ниже порогового уровня (.05 или .01 для самых взыскательных), то нуль гипотеза ложная. Чем ниже — тем лучше.

#R любезно обозначает звездочками значимые коэффициенты, для которых p-значение достаточно мало
y_pred = predict(lm.y, newdata=x)


library(ggplot) # вспомогательные функции для работы с моделями

library(broom) # преобразование результатов моделирования в табличный вид


ggplot(data = df1, aes(x = x$square, y = y_pred)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")
df1


xx=df1[ , x<-df1[ , -which(names(df1) %in% c("total_floor"))]]
xx
lm2.y <- lm(y ~ ., data = xx)
summary(lm2.y)

xx=df1[ ,  c("square", "room", "dist", "Аметьево", "Горки", "р.н.Советский","Северный.вокзал", "Суконная.слобода")]
xx
lm3.y <- lm(y ~ ., data = xx)
summary(lm3.y)

