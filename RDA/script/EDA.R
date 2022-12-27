install.packages("readr")
library(readr)
exam <- read_csv("exam.csv",
                 col_names = T,
                 col_types = cols("c", "f", "f", "i", "i", "i"),
                 na = "na",
                 locale = locale('ko', encoding = 'euc-kr'))

#데이터 탐색: glimpse, str, summary#
install.packages("dplyr")
library(dplyr)
glimpse(exam)
str(exam)
summary(exam)

#빈도수 구하기#
table(exam$address)
install.packages("descr")
library(descr)
freq(exam$address)
install.packages("ggplot2")
library(ggplot2)
qplot(data = exam, address)
qplot(data = exam, address, fill = gender)
qplot(data = exam, class, fill = gender)

# 기술통계량 구하기 #
summary(exam$math)
table(is.na(exam$math))
mean(exam$math,
     na.rm = T)
install.packages("psych")
library(psych)
describe(exam$math)
describe(exam$english)
describe(exam$history)
hist(exam$english, breaks = seq(0, 100, by = 5))
hist(exam$history, breaks = seq(0, 100, by = 10))

# 논리 연산자와 비교 연산자 #
library(descr)
freq(exam$address == "원효로")
table(exam$address == "원효로")
freq(exam$gender != "Female")
table(exam$gender != "Female")

freq(exam$math == 50)
freq(exam$math != 50)
freq(exam$math >= 50)
freq(exam$math < 50)

freq(exam$english <= 50 & exam$history >= 80)
freq(exam$math >= 90 | exam$history >= 90)
freq(exam$address == "효창동" | exam$address == "청파동" | exam$address == "서계동")
freq(exam$address %in% c("효창동", "청파동", "서계동"))

# imdb 데이터분석 실습 #
library(readr)
movie <- read_csv("imdb.csv", col_names = T)
library(dplyr)
glimpse(movie)
movie$Certificate <- as.factor(movie$Certificate)
movie$Genre <- as.factor(movie$Genre)
movie$Released_Year <- as.integer(movie$Released_Year)
install.packages("stringr")
library(stringr)
movie$Runtime <- str_replace_all(string = movie$Runtime,
                                 pattern = " min",
                                 replacement = "")
movie$Runtime <- as.integer(movie$Runtime)

# 데이터 탐색적 분석 #
summary(movie)
library(descr)
freq(movie$Certificate)
library(ggplot2)
qplot(data = movie, Released_Year, fill = Certificate)
library(psych)
describe(movie$IMDB_Rating)
#변동계수(CV: coefficient of variation): 표준편차/평균, MS = 12.38/77.97, IMDB = 0.28/7.95) 
describe(movie$Meta_score)#
12.38 / 77.97
0.28 / 12.38
hist(movie$Meta_score, breaks = seq(0, 100, 1))
hist(movie$IMDB_Rating, breaks = seq(0, 10, 0.1))
table(movie$Released_Year == 2019 & movie$Meta_score > 95)
table(is.na(movie$Gross))

# 변수명 바꾸기 #
library(dplyr)
movie <- movie %>% rename(Title = Series_Title, 
                          Year = Released_Year)
movie$var1 <- movie$Year
movie$var1 <- NULL

# 변수의 측정값 바꾸기 #
movie$Running <- movie$Runtime
movie$Running <- ifelse(movie$Running > 200, "Long", "Not Long")

table(movie$Running == "Long")
table(is.na(movie$Gross))
pairs(movie[15:16])
mean(movie$Gross / movie$No_of_Votes, na.rm = T)
movie$Gross <- ifelse(is.na(movie$Gross), 217.1, movie$Gross)
table(movie$Gross == 217.1)

# weather.csv 파일 실습하기 #
#문제1# 
library(readr)
weather <- read_csv("weather.csv", 
                    col_names = T, 
                    locale = locale('ko', encoding = 'euc-kr'))
#문제2 & 문제3#
library(dplyr)
glimpse(weather)
summary(weather)

weekdays(as.Date("2022-03-08"))
weather$요일 <- weekdays(weather$일시)
weather$일시 <- as.factor(weather$일시)

#문제4# 
weather$요일 <- as.factor(weather$요일)

#문제5#
var(weather$일강수량, na.rm = T)

#문제6&7#
library(psych)
descr_result <- describe(weather)
descr_result$cv <- descr_result$sd / descr_result$mean

#문제8#
library(descr)
freq(weather$요일)
str(weather)
weather$요일 <- factor(weather$요일, levels = c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))

#문제9#
library(ggplot2)
qplot(data = weather, 요일, fill = 요일구분)

#문제10#
hist(weather$평균기온, breaks = seq(-20, 50, 1))

#문제11#
library(descr)
freq(weather$평균기온 >= 10 & weather$평균기온 <= 20)

#문제12#
freq(is.na(weather$일강수량))

#문제13#
freq(weather$요일 %in% c("월요일", "화요일"))

#문제14#
freq(weather$최고기온 > 30 & weather$평균상대습도 > 80)

#문제15#
freq(weather$최고기온 < -10 | weather$합계일조시간 < 1)

#문제16#
library(dplyr)
weather <- weather %>% rename(평균기압 = 평균현지기압)

#문제17#
table(weather$요일구분)
weather$요일구분 <- factor(weather$요일구분, levels = c("휴일", "평일"))

#문제18#
table(weather$일강수량 == 0)
table(is.na(weather$일강수량))
weather$일강수량 <- ifelse(weather$일강수량 == 0, NA, weather$일강수량) # 문자형으로 NA가 아니라, 결측치를 의미하므로 큰 따옴표 없이 NA로 표기 #

#문제19#
library(descr)
freq(is.na(weather$평균기압))

#문제20#
mean(weather$평균기압, na.rm = T)

#문제21#
weather$평균기압 <- ifelse(is.na(weather$평균기압), 1006.264, weather$평균기압)