install.packages("readr")
library(readr)
rm(exam)
exam <- read_csv("exam.csv", col_names = T, na = "na", 
                 locale = locale('ko', encoding = 'euc-kr'))
write.csv(exam, "exam1.csv", na = "NA", row.names = F)

#데이터 탐색#
install.packages("dplyr")
library(dplyr)
glimpse(exam)
exam$address <- as.factor(exam$address)
exam$gender <- as.factor(exam$gender)
exam$class <- as.factor(exam$class)
str(exam)
str(exam$gender)
summary(exam)

#빈도수 확인하기#
table(exam$address)
install.packages("descr")
library(descr)
freq(exam$address)
install.packages("ggplot2")
library(ggplot2)
qplot(data = exam, address)
qplot(data = exam, address, fill = gender)

#기술통계량#
mean(exam$math)
freq(is.na(exam$math))
mean(exam$math, na.rm = T)
install.packages("psych")
library(psych)
descr <- describe(exam)

#히스토그램 그리기#
hist(exam$english, breaks = seq(0, 100, 10))
hist(exam$history, breaks = seq(0, 100, 5))
hist(exam$math, breaks = seq(0, 100, 5))

#비교 연산자#
library(descr)
table(exam$address == "원효로")
freq(exam$gender != "Female")
freq(exam$math == 50)
freq(exam$math != 50)
freq(exam$math <= 50)
freq(exam$math < 50)

#논리 연산자#
table(exam$english <= 50 & exam$history >= 80)
table(exam$math >= 90 | exam$history >= 90)
table(exam$address == "효창동" | exam$address == "청파동" | exam$address == "서계동")
table(exam$address %in% c("효창동", "청파동", "서계동"))

#weather.csv 실습#
#문제1#
weather <- read_csv("weather.csv", col_names = T,  
                locale = locale('ko', encoding = 'euc-kr'))

#문제2#
library(dplyr)
glimpse(weather)

#문제3#
summary(weather)

#문제4#
var(weather$일강수량, na.rm = T)

#요일변수 만들기와 척도 변경하기#
weekdays("2020-01-01")
weekdays(as.Date("2020-01-01"))
weather$요일 <- weekdays(weather$일시)

weather$요일1 <- weather$요일
weather$요일1 <- NULL

weather$요일구분 <- as.factor(weather$요일구분)
glimpse(weather$요일구분)

#문제5#
library(psych)
descr_w <- describe(weather)

#문제6#
table(weather$요일)

#문제7#
hist(weather$평균기온, breaks = seq(-20, 50, 1))

#문제8#
table(weather$최고기온 > 30 & weather$평균상대습도 > 80)

#문제9#
table(weather$최저기온 < -10 | weather$합계일조시간 < 1)

#문제10#
glimpse(weather$요일)
weather$요일 <- factor(weather$요일, levels = c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))

library(ggplot2)
qplot(data = weather, 요일, fill = 요일구분)

#문제11#
weather <- weather %>% rename(평균기압 = 평균현지기압)
weather <- weather %>% rename(평균습도 = 평균상대습도)

#문제12#
weather$일강수량 <- ifelse(is.na(weather$일강수량), 0, weather$일강수량)
mean(weather$평균기압, na.rm = T)
weather$평균기압 <- ifelse(is.na(weather$평균기압), 1006.26, weather$평균기압)
table(weather$평균기압 == 1006.26)