# Loading packages
library(readr)
library(splitstackshape)
library(dplyr)
library(stringr)
library(tidyr)

# Reading data
df <- read_csv("data/data_30.06.2018.csv")

## Data cleaning

# There are some mistakes
df$articles[18] <- "300003; 1590003"
df$articles[19] <- "300003; 1590003"
df$articles[458] <- "2900003; 2860001"
df$articles[345] <- "2280001; 300001"
df$summary_crime[133] <- "Избиение потенциального наркопреступника"
df$summary_crime[134] <- "Подброс и хранение наркотиков"
df$summary_crime[135] <- "Фальсификация доказательств преступления"
df$summary_crime[136] <- "Фальсификация преступления"
df$drugs_seized_1[134] <- "амфетамин"
df$drugs_seized_weight_1[134] <- "5,5"
df$drugs_seized_2[134] <- "хлорфенилпиперазин"
df$drugs_seized_weight_2[134] <- "2,7"
df$drugs_seized_1[133] <- NA
df$drugs_seized_weight_1[133] <- NA
df$drugs_seized_2[133] <- NA

### Agencies ####
df$agency_crime[df$agency_crime %in% c("ОМВД","ОМОН")] <- "МВД"
df$agency_crime[df$agency_crime %in% c("УФСКН")] <- "ФСКН"

df$agency_catcher[df$agency_catcher %in% c("УСБ МВД","СБ МВД", "ОБНОН")] <- "МВД"
df$agency_catcher[df$agency_catcher %in% c("СКР")] <- "СК"
df$agency_catcher[df$agency_catcher %in% c("УФСКН")] <- "ФСКН"
df$agency_catcher[df$agency_catcher %in% c("УФСБ")] <- "ФСБ"

### Drugs ####
df$drugs_seized_1 <- gsub('"', '',df$drugs_seized_1)
df$drugs_seized_1 <- gsub('матамфетамин', 'метамфетамин',df$drugs_seized_1)
df$drugs_seized_1 <- gsub('героина', 'героин',df$drugs_seized_1)
df$drugs_seized_1 <- gsub('курительные смеси', 'курительная смесь',df$drugs_seized_1)
df$drugs_seized_2 <- gsub('"', '',df$drugs_seized_2)
df$drugs_seized_2 <- gsub('курительные смеси', 'курительная смесь',df$drugs_seized_2)
df$drugs_seized_1[df$drugs_seized_1 %in% c("марихуана","гашиш", "конопля","каннабис","мариухана",
                                           "каннабиноид", "каннабиноиды")] <- "каннабиноиды"
df$drugs_seized_2[df$drugs_seized_2 %in% c("марихуана","гашиш", "конопля","каннабис","мариухана",
                                           "каннабиноид", "каннабиноиды", "масло каннабиса")] <- "каннабиноиды"
df$drugs_seized_weight_1 <- as.numeric(format(df$drugs_seized_weight_1, decimal.mark=","))
df$drugs_seized_weight_2 <- as.numeric(format(df$drugs_seized_weight_2, decimal.mark=","))


### Regions ####
df$agency_geo_region[df$agency_geo_region == "Кабардино-Балкарская республика"] <- "Кабардино-Балкарская Республика"
df$agency_geo_region[df$agency_geo_region == "Респбулика Башкортостан"] <- "Республика Башкортостан"
df$agency_geo_region[df$agency_geo_region %in% c("Респубика Дагестан","Респбулика Дагестан")] <- "Республика Дагестан"
df$agency_geo_region[df$agency_geo_region == "Удмурстская Республика"] <- "Удмуртская Республика"
df$agency_geo_region[df$agency_geo_region == "Томбовская область"] <- "Тамбовская область"

### Dates ####
df$date_publication_first <- substr(df$date_publication,1,10)
df$year_publication_first <- format(as.Date(df$date_publication_first, format="%d.%m.%Y"),"%Y")
df$month_publication_first <- format(as.Date(df$date_publication_first, format="%d.%m.%Y"),"%m")
df$yday_publication_first <- as.numeric(strftime(as.Date(df$date_publication_first, format="%d.%m.%Y"), format = "%j"))
df$season_publication_first <- cut(df$yday_publication_first, breaks = c(0, 60, 152, 244, 335, 366),
                                   labels = c("Зима", "Весна", "Лето", "Осень", "Зима"),
                                   include.lowest = TRUE)

### Publishers ####

publishers <- cSplit_e(df, "publisher", sep = ";", type = "character", fill = 0, drop = TRUE) %>% 
  select(starts_with("publisher")) %>% 
  colSums(.) %>% 
  as.data.frame(.)

publishers$publisher <- substr(rownames(publishers),11,999)
rownames(publishers) <- NULL
publishers <- publishers[c(2,1)]
names(publishers) <- c("publisher","count")



# Remove leading/trailing whitespaces
publishers$publisher <- trimws(publishers$publisher)

# Remove ",<,>
publishers$publisher <- gsub('"|<|>', '', publishers$publisher)

# Remove round brackets and everything inside
publishers$publisher <- gsub("\\([^()]*\\)", "", publishers$publisher)

# Be careful - these are powerful regexes
###TODO Clean some more publishers here, please ###

publishers$publisher <- gsub("АиФ.+", "АиФ", publishers$publisher)

publishers$publisher[grepl("Коммерсант",publishers$publisher)] <- "Коммерсант"
publishers$publisher[grepl("РБК|RBC|РосБизнесКонсалтинг",publishers$publisher)] <- "РБК"
publishers$publisher[grepl("Московский комсомолец|Московский Комсомолец|МК", publishers$publisher)] <- "МК"

publishers$publisher[str_detect(publishers$publisher, "Вечерний Ставрополь")] <- "Вечерний Ставрополь"
publishers$publisher[str_detect(publishers$publisher, "Йошкар-Ола")] <- "Йошкар-Ола"
publishers$publisher[str_detect(publishers$publisher, "Невское время")] <- "Невское время"
publishers$publisher[str_detect(publishers$publisher, "ИНТЕР|Интер")] <- "Интер"
publishers$publisher[str_detect(publishers$publisher, "Слобода")] <- "Слобода"
publishers$publisher[str_detect(publishers$publisher, "Якутск вечерний|Якутск Вечерний")] <- "Якутск вечерний"
publishers$publisher[str_detect(publishers$publisher, "Трибуна")] <- "Трибуна"
publishers$publisher[str_detect(publishers$publisher, "Советская Молодежь|Советская молодежь")] <- "Советская молодежь"
publishers$publisher[str_detect(publishers$publisher, "Новая новгородская газета")] <- "Новая новгородская газета"
publishers$publisher[str_detect(publishers$publisher, "Нижегородские новости")] <- "Нижегородские новости"
publishers$publisher[str_detect(publishers$publisher, "Наше Время|Наше время")] <- "Наше время"
publishers$publisher[str_detect(publishers$publisher, "Молодой Коммунар|Молодой коммунар")] <- "Молодой коммунар"
publishers$publisher[str_detect(publishers$publisher, "Красный Север|Красный север")] <- "Красный север"
publishers$publisher[str_detect(publishers$publisher, "Краснодарские известия")] <- "Краснодарские известия"
publishers$publisher[str_detect(publishers$publisher, "Кабардино-Балкарская правда|Кабардино-балкарская правда")] <- "Кабардино-Балкарская правда"
publishers$publisher[str_detect(publishers$publisher, "Аресеньевские вести")] <- "Арсеньевские вести"
publishers$publisher[str_detect(publishers$publisher, "Вечерняя Казань")] <- "Вечерняя Казань"
publishers$publisher[str_detect(publishers$publisher, "Вятский наблюдатель")] <- "Вятский наблюдатель"
publishers$publisher[publishers$publisher == "Известия "] <- "Известия"
publishers$publisher[publishers$publisher == "Комсомольская правда "] <- "Комсомольская правда"
publishers$publisher[publishers$publisher == "Арсеньевские вести "] <- "Арсеньевские вести"

### ###
# Counts publishers

publishers <- publishers %>% 
  group_by(publisher) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count))

### Sanctions dummies

df$sanction[285] <- "уголовное дело"
df$sanction[286] <- "уголовное дело"
df$is_fired <- 0
df$is_wanted <- 0
df$is_case <- 0
df$is_seizure <- 0
df$is_justification <- 0
df$is_house_arrest <- 0
df$is_fired[str_detect(df$sanction, "увол|отстр")] <- 1
df$is_wanted[str_detect(df$sanction, "розыск")] <- 1
df$is_case[str_detect(df$sanction, "угол")] <- 1
df$is_seizure[str_detect(df$sanction, "пров|обыск")] <- 1
df$is_justification[str_detect(df$sanction, "оправд")] <- 1
df$is_house_arrest[str_detect(df$sanction, "подп")] <- 1
df$is_detention[str_detect(df$sanction, "страж|задер|арест")] <- 1

### Type of media ###

df_long_publishers <- unnest(df, publisher = strsplit(publisher, ";"))

df_long_publishers$publisher <- tolower(df_long_publishers$publisher)
df_long_publishers$agency_geo_city <- tolower(df_long_publishers$agency_geo_city)
df_long_publishers$agency_geo_region <- tolower(df_long_publishers$agency_geo_region)
df_long_publishers$publisher <- gsub(' (pdf-версия)', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('(московский выпуск, pdf)', '(Москва)', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('риа "росбизнесконсалтинг" казань и татарстан', 'рбк. татарстан (rt.rbc.ru)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('риа "росбизнесконсалтинг" нижний новгород', 'рбк. нижний новгород (nn.rbc.ru)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('риа "росбизнесконсалтинг" новосибирск и сибирь', 'рбк. новосибирск (nsk.rbc.ru)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('известия (московский выпуск)', 'известия', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('вечерка (томск)', 'вечерка thebest (томск)', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('московский комсомолец', 'мк', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('коммерсант', 'коммерсантъ', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('rbc news', 'рбк. rbc news', df_long_publishers$publisher, fixed = TRUE) 
df_long_publishers$publisher <- gsub('"', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('<', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub('>', '', df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("г.", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("(", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub(")", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub(".", "", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("ъъ", "ъ", df_long_publishers$publisher, fixed = TRUE)
df_long_publishers$publisher <- gsub("-", " ", df_long_publishers$publisher, fixed = TRUE)


allsources <- read_delim("data/allsources.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
allsources$name <- tolower(allsources$name)
allsources$name <- gsub(" (pdf-версия)", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" (pdf версия)", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" (архив)", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" архив", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" текст", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(" приложения", "", allsources$name, fixed = TRUE)
allsources$name <- gsub("г.", "", allsources$name, fixed = TRUE)
allsources$name <- gsub("(", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(")", "", allsources$name, fixed = TRUE)
allsources$name <- gsub(".", "", allsources$name, fixed = TRUE)
allsources$name <- gsub("-", " ", allsources$name, fixed = TRUE)

df_long_publishers$type <- NA

for(i in 1:nrow(allsources)){
  df_long_publishers$type[str_detect(df_long_publishers$publisher,allsources$name[i])] <- allsources$type[i]
}
rm(i)
for(i in 1:nrow(df_long_publishers)){
  df_long_publishers$type[str_detect(df_long_publishers$publisher,df_long_publishers$agency_geo_city[i])] <- "region"
}
rm(i)
for(i in 1:nrow(df_long_publishers)){
  df_long_publishers$type[str_detect(df_long_publishers$publisher,df_long_publishers$agency_geo_region[i])] <- "region"
}

df_long_publishers$type[str_detect(df_long_publishers$publisher,"ru")] <- "region int"

# There are 6 types of media: federal print ("fed"), regional print ("region"), federal Internet ("fed int"), regional Internet ("region int"), federal archive ("fed arch") and regional archive ("region arch").
# Archive types refer to the media that is no longer publishing.

df_long_publishers$type[df_long_publishers$publisher == " северо запад санкт петербург"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == "своими именами"] <- "fed arch"
df_long_publishers$type[df_long_publishers$publisher == "северо запад санкт петербург"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == " экстра реклама чита"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == "аиф   дон"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "аиф на оби"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "невское время"] <- "region arch"
df_long_publishers$type[df_long_publishers$publisher == "советская россия"] <- "fed"
df_long_publishers$type[df_long_publishers$publisher == "российская газета"] <- "fed"
df_long_publishers$type[df_long_publishers$publisher == "кабардино балкарская правда"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "российская газета неделя волга урал"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "столица плюс грозный"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "новая новгородская газета"] <- "region"
df_long_publishers$type[df_long_publishers$publisher == "комсомольская правда mskkpru"] <- "fed int"

df_long_publishers$fed_arch <- 0
df_long_publishers$fed <- 0
df_long_publishers$fed_int <- 0
df_long_publishers$region <- 0
df_long_publishers$region_arch <- 0
df_long_publishers$fed_arch[df_long_publishers$type == "fed arch"] <- 1
df_long_publishers$fed[df_long_publishers$type == "fed"] <- 1
df_long_publishers$fed_int[df_long_publishers$type == "fed int"] <- 1
df_long_publishers$region[df_long_publishers$type == "region"] <- 1
df_long_publishers$region_arch[df_long_publishers$type == "region arch"] <- 1
types <- select(df_long_publishers, N, fed, fed_int, fed_arch, region, region_arch)
types <- aggregate(. ~ N, types, sum)
df <- merge(df, types)

# Deleting unnecessary objects from memory:
rm(list = c("types","allsources", "i"))

### Articles from Criminal Code ####

df$articles <- gsub(',', ';', df$articles)
df_long_articles <- unnest(df, articles = strsplit(articles, ";"))
df_long_articles$articles <- gsub('\n| ', '', df_long_articles$articles)
df_long_articles$articles_short <- substr(df_long_articles$articles, 1,nchar(df_long_articles$articles)-4)

# Export data
save(df_long_articles, df, publishers, df_long_publishers, file = "data/data_clean.RData")
#write.csv2(df, "data/data_clean.csv", row.names = F)