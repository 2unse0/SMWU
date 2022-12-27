### filter 함수 실습 ###
library(dplyr)
exam_c1 <- exam %>% filter(class == 1)

exam_male <- exam %>% filter(gender == "Male")
mean(exam_male$english)

### 문제1~3 ###
exam_123 <- exam %>% filter(class %in% c(1, 2, 3))
round(mean(exam_123$math), digits = 2)

exam_n4 <- exam %>% filter(class != 4) %>% 
  filter(math >= 90 | history >= 95)

quantile(exam$english, probs = c(0.9))
exam %>% filter(english >= quantile(exam$english, probs = c(0.9)))

### select 함수 실습 ###
exam %>% select(class, english, math) %>% print(n=Inf)
exam %>% select(-address) %>% print(n=Inf)
exam %>% select(contains("add"))

##문제4##
exam %>% filter(class == 1) %>% select(gender, math)

### arrange 함수 실습 ###
exam %>% arrange(math) %>% print(n=Inf)
exam %>% arrange(-math) %>% print(n=Inf)
exam %>% arrange(class, -math) %>% print(n=Inf)

### mutate 함수 실습 ###
exam <- exam %>% mutate(total = math + english + history, average = (math + english + history)/3)

## mutate 함수 & ifelse 함수 실습 ##
exam <- exam %>% mutate(test = ifelse(total >= 180, "pass", "fail"))
exam$test <- ifelse(is.na(exam$total), NA, exam$test) # 굳이 할 필요가 없지만, 만약 이와 같은 문제가 발생하면 사후에 처리 #
library(descr)
freq(exam$test)

exam <- exam %>% mutate(grade = ifelse(average < 60, "fail", ifelse(average < 75, "middle", ifelse(average < 90, "good", "excellent"))))

## mutate 함수 & case_when 함수 실습 ##
exam$test <- NULL
exam$grade <- NULL
exam <- exam %>% mutate(test = case_when(total < 180~"fail", total >= 180~"pass"))
exam <- exam %>% mutate(grade = case_when(average < 60~"fail", average < 75~"middle", average < 90~"good", average >= 90~"excellent"))

### relocate 함수 실습 ###
exam <- exam %>% relocate(total, .before = test)
exam <- exam %>% relocate(average, .after = test)
glimpse(exam)
exam$gender <- as.factor(exam$gender)
exam$class <- as.factor(exam$class)
exam$test <- as.factor(exam$test)
exam$grade <- as.factor(exam$grade)
exam <- exam %>% relocate(where(is.character))
exam <- exam %>% relocate(where(is.factor), .before = where(is.character))

### group_by 함수 and summarise 함수 실습 ###
exam %>% group_by(class) %>% summarise(n(), mean(math, na.rm = T), sd(math, na.rm =  T))
exam %>% group_by(class) %>% summarise(count = n(), mean_math = mean(math, na.rm = T), sd_math = sd(math, na.rm =  T))
exam_new <- exam %>% group_by(class, gender) %>% summarise(count = n(), mean_history = mean(history))

## 참조: sum 함수 ##
exam_new %>% mutate(perc = count / sum(count)) # 반별로 sum #
exam_new %>% mutate(perc = count / sum(exam_new$count)) # 전체 sum: 30명 #


## 문제1 ##
weather_new <- weather
library(dplyr)
weather_new <- weather_new %>% rename(최소습도 = 최소상대습도, 일조시간 = 합계일조시간, 일사량 = 합계일사량)

## 문제2 ##
weather_new2 <- weather_new %>% filter(요일구분 == "평일")
round(mean(weather_new2$최대풍속), digits = 2)

## 문제3 ##
weather_new3 <- weather_new %>% filter(평균습도 >= quantile(weather_new$평균습도, probs = c(0.95)))
mean(weather_new3$평균습도)

## 문제4 ##
weather_new4 <- weather_new %>% select(contains("기온") | contains("요일"))

## 문제5 ##
weather_new5 <- weather_new4 %>% filter(요일구분 == "휴일")
table(weather_new5$요일)

## 문제6 ##
weather_new <- weather_new %>% mutate(discomfort = 1.8*평균기온-0.55*(1-평균습도/100)*(1.8*평균기온-26)+32)

## 문제7 ##
weather_new <- weather_new %>% mutate(grade = case_when(discomfort <= 67~"very good", discomfort <= 73~"good", discomfort <=77~"not bad", discomfort > 77~"bad"))

## 문제8 ##
library(descr)
freq(weather_new$grade)
weather_new$grade <- factor(weather_new$grade, levels = c("very good", "good", "not bad", "bad"))

## 문제9 ##
weather_new %>% group_by(grade) %>% summarise(n(), mean(일사량))

n_distinct(weather_new$평균풍속)
n_distinct(weather_new$요일)