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


## movie 데이터프레임 실습 ##
names(movie) <- tolower(names(movie)) #대문자로 바꾸려면 toupper함수 사용#

# 문제1 #
library(dplyr)
movie1 <- movie %>% filter(year %in% c(2018:2020))
round(mean(movie1$runtime), digits = 2)

# 문제2 #
library(stringr)
movie2 <- movie %>% filter(certificate == "A") %>% filter(str_detect(genre, "Drama")) %>% arrange(-imdb_rating)

# 문제3 #
movie3 <- movie %>% filter(str_detect(genre, "Drama") & str_detect(overview, "crime"))

# 문제4 #
movie3 %>% filter(meta_score >= quantile(movie3$meta_score, probs = c(0.9), na.rm = T)) %>% select(title)

# 문제5 #
movie5 <- movie %>% select(contains("star") | contains("dir"))

# 문제6 #
library(descr)
movie6 <- freq(movie5$star1)

# 문제7 #
movie <- movie %>% mutate(score = 10*imdb_rating + meta_score)

# 문제8 #
movie <- movie %>% mutate(class = case_when(score <= 120~"D", score <= 130~"C", score <= 160~"B", score <= 180~"A", score > 180~"S"))

# 문제9 #
freq(movie$class)
movie$class <- factor(movie$class, levels = c("S", "A", "B", "C", "D"))

# 문제10 #
movie %>% group_by(class) %>% summarise(n(), mean(gross, na.rm = T))

# 문제11 #
n_distinct(movie$director)
movie %>% group_by(director) %>% summarise(number = n()) %>% arrange(-number) %>% head(10)

n_distinct(movie$imdb_rating)