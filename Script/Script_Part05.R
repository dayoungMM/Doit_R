#### 05-1 ####

## -------------------------------------------------------------------- ##

exam <- read.csv("Data/csv_exam.csv")

head(exam)      # 앞에서부터 6행까지 출력
head(exam, 10)  # 앞에서부터 10행까지 출력

tail(exam)      # 뒤에서부터 6행까지 출력
tail(exam, 10)  # 뒤에서부터 10행까지 출력

View(exam)      # 데이터 뷰어 창에서 exam 데이터 확인
dim(exam)       # 행, 열 출력
str(exam)       # 데이터 속성 확인
summary(exam)   # 요약 통계량 출력


## -------------------------------------------------------------------- ##
# ggplo2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg)

mpg

head(mpg)     # Raw 데이터 앞부분 확인
tail(mpg)     # Raw 데이터 뒷부분 확인
View(mpg)     # Raw 데이터 뷰어 창에서 확인
dim(mpg)      # 행, 열 출력
str(mpg)      # 데이터 속성 확인
summary(mpg)  # 요약 통계량 출력

df2 <- as.data.frame(table(mpg$model))
df2


#### 05-2 ####

## -------------------------------------------------------------------- ##
df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_raw

install.packages("dplyr")  # dplyr 설치
library(dplyr)             # dplyr 로드

df_new <- df_raw  # 복사본 생성
df_new            # 출력

df_new <- rename(df_new, v2 = var2)  # var2를 v2로 수정
df_new


#### 05-3 ####

## -------------------------------------------------------------------- ##
df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2       # var_sum 파생변수 생성
df

df$var_mean <- (df$var1 + df$var2)/2  # var_mean 파생변수 생성
df


## -------------------------------------------------------------------- ##
mpg$total <- (mpg$cty + mpg$hwy)/2  # 통합 연비 변수 생성
head(mpg)
mean(mpg$total)  # 통합 연비 변수 평균


## -------------------------------------------------------------------- ##
summary(mpg$total)  # 요약 통계량 산출
h<-hist(mpg$total)     # 히스토그램 생성
str(h)  #structure  #t6개의 리스트로 되어있는것 등 확인 가능, 등구간인지 아닌지 등 확인 가능
h
h$counts

qplot(total, data=mpg, bins=20)


h[["counts"]]
sum(h$count * h$mids)/ sum(h$count)  #total의 median, mean과 유사한 값이 나온다
median(mpg$total)
hist(mpg$total, nclass=20)
hist(mpg$total)

?hist
str(mpg$total)


sum(h$counts) #데이터 갯수는 234
sum(h$counts /234) #비율의 합은 1
h$counts/234 #빈도 비율

(h$density) # 빈도비율 바로 구하기


# 20이상이면 pass, 그렇지 않으면 fail 부여
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")

head(mpg, 20)     # 데이터 확인

table(mpg$test)   # 연비 합격 빈도표 생성

library(ggplot2)  # ggplot2 로드
qplot(mpg$test)   # 연비 합격 빈도 막대 그래프 생성


## -------------------------------------------------------------------- ##
# total을 기준으로 A, B, C 등급 부여
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20, "B", "C"))

head(mpg, 20)     # 데이터 확인

table(mpg$grade)  # 등급 빈도표 생성
qplot(mpg$grade)  # 등급 빈도 막대 그래프 생성

# A, B, C, D 등급 부여
mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >= 25, "B",
                            ifelse(mpg$total >= 20, "C", "D")))


## -------------------------------------------------------------------- ##
# 1.데이터 준비, 패키지 준비
mpg <- as.data.frame(ggplot2::mpg)  # 데이터 불러오기
library(dplyr)                      # dplyr 로드
library(ggplot2)                    # ggplot2 로드

# 2.데이터 파악
head(mpg)     # Raw 데이터 앞부분
tail(mpg)     # Raw 데이터 뒷부분
View(mpg)     # Raw 데이터 뷰어창에서 확인
dim(mpg)      # 차원
str(mpg)      # 속성
summary(mpg)  # 요약 통계량





###################################
## quiz p123
midwest <- as.data.frame(ggplot2::midwest) 
head(midwest[,c("poptotal","popasian")])

names(midwest[,c("poptotal","popasian")]) <- c("total","asian")
names(midwest[,c("poptotal","popasian")])

midwest <- rename(midwest, total = poptotal)

midwest <- rename(midwest, asian = popasian)
names(midwest)

#파생변수 생성
midwest$a_per <- (midwest$asian / midwest$total)*100
head(midwest)
hist(midwest$a_per)

# 평균 초과 이하 구분하기
mean_p=mean(midwest$a_per) #책에서 의도한바
mean_p=sum(midwest$asian)/ sum(midwest$total)*100 #논리적으로는 이렇게 해야한다
mean_p
(midwest$scale <- ifelse(midwest$a_per >= mean_p, "large", "small"))
head(midwest$scale)

qplot(total, a_per, data=midwest)
#scale을 지역이 얼마나 되는지 빈도표와 빈도 막대 그래프 그리기
table(midwest$scale)  # 빈도표 출력
prop.table(table(midwest$scale))
qplot(midwest$scale)  # 막대 그래프 생성


# 3.변수명 수정
mpg <- rename(mpg, company = manufacturer)

# 4.파생변수 생성
mpg$total <- (mpg$cty + mpg$hwy)/2                   # 변수 조합
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")  # 조건문 활용

# 5.빈도 확인
table(mpg$test)  # 빈도표 출력
qplot(mpg$test)  # 막대 그래프 생성

