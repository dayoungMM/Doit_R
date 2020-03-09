###
rm(list=ls())

english <- c(90, 80, 60, 70)  # 영어 점수 변수 생성
english

math <- c(50, 60, 100, 20)    # 수학 점수 변수 생성
math


df_midterm <- data.frame(english, math)
df_midterm
df_midterm[2]
df_midterm["math"]
df_midterm$math #행벡터
df_midterm[1:2] #열을 기준으로 indexing
df_midterm[2,1:2] #2행의 1~2열



class <- c(1, 1, 2, 2)
class

df_midterm <-data.frame(english,math, class)
df_midterm


#열 추가하는 방법
df_midterm$class <- class
df_midterm$class
df_midterm["class"] <- class
df_midterm[4] <- class # 4번째 컬럼에 추가
names(df_midterm)[4] <- "class2"   #컬럼 이름 설정
names(df_midterm)[3:4] <- c("class1", "class2") #컬럼 이름 설정(복수)
df_midterm

mean(df_midterm$english)
mean(df_midterm) #dataframe 전체에 대해서는 mean 불가

## apply 함수

apply(df_midterm, 2, mean) #두번째 파라미터: 축 => 1=행 , 2=열

class(apply(df_midterm, 2, mean))


summary(df_midterm[1:2])

rm(english, math, class)
df_midterm <- data.frame(english <- c(90, 80, 60, 70),      #<- 함수내에서도 변수를 생성
                         math <- c(50, 60, 100, 20),
                         class <- c(1, 1, 2, 2))
df_midterm



rm(english, math, class)

df_midterm <- data.frame(english = c(90, 80, 60, 70),   # = 함수내에서 변수 생성 X 인자명을 지정할뿐
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm


english


df<- df_midterm  #copy가 발생
df

df$math
df[,2]
mean(df["math"]) #df["math"]는 데이터 프레임이여서 mean 계산 불가
mean(df$math) #df$math는 벡터여서 mean 가능
mean(df[,2]) #가능
mean(df[,"math"]) #가능

df["math"] #dataframe
df[,"math"] # vector


## - 행 삽입하기.
df[5,] <- c(50, 60, 1)

head(df)
1:3 == c(1,2,3)

# - 순서와 이름에 의한 색인을 섞어 쓸 수 있다.
df[1:3, c('math', 'english')]
df[1:3, c(2,1)]
df[1,]
df[2,3]
df[c(1,3,4),]
df[c(1,3,4), c(1,3)]
df[c(1,3,4), c("english", "class")]

## Quiz p88
sales_fruit <- data.frame(product <- c("사과", "딸기", "수박"),
                            price <- c(1800,1500,3000),
                            sales <- c(24,38,13))
sales_fruit

sales_fruit$price

#apply(sales_fruit,2,mean)


install.packages("readxl")
library(readxl)

df_exam <- readxl::read_excel ("Data/excel_exam.xlsx")
df_exam

mean(df_exam$english)
mean(df_exam)

## quiz
#1. math와 science만을 선택하는 4가지 방법을 작성
df_exam
df_exam[c(3,5)]
df_exam[c("math","science")]
df_exam[,c(3,5)]
df_exam [,c("math","science")]

#2. math, english, science의 점수 합을 값으로 갖는 sum이라는 열을 생성하라
df_exam["sum"] <- apply(df_exam[c("math","english","science")], 1, sum )
df_exam["sum"] <- df_exam[3] + df_exam[4] + df_exam[5]
df_exam



# 3. sum이라는 열의 값이 150이상 180 이하인 행만 선택하라.
df_exam[(df_exam$sum >= 150) & (df_exam$sum <= 180),]

# 4. 각 반별로 science의 최고점과 최저점인 학생 id를 구하라.
s_min = min(df_exam$science)
s_max = max(df_exam$science)

df_exam[df_exam$science == s_min,]["id"]
df_exam[df_exam$science == s_max,]["id"]




classes <- unique(df_exam$class)
for (cls in classes){
  tmp <- df_exam[df_exam$class == cls, c(1,5)]
  min <- min(tmp$science)
  max <- max(tmp$science)
  print(cls)
  print("min")
  print(tmp[tmp$science == min,"id"])
  print("max")
  print(tmp[tmp$science == max,"id"])
}
which.min(df_exam$science)
which.max(df_exam$science)
df_exam$id[which.min(df_exam$science),]



minidx = tapply(df_exam$science, df_exam$class, which.min, simplify = F)
minidx


#read, write

# 엑셀 파일의 세 번째 시트에 있는 데이터 불러오기
df_exam_sheet <- read_excel("Data/excel_exam_sheet.xlsx", sheet=3)
df_exam_sheet


## -------------------------------------------------------------------- ##
df_csv_exam <- read.csv("Data/csv_exam.csv")
class(df_csv_exam)

df_csv_exam <- read.csv("Data/csv_exam.csv", stringsAsFactors = F)


## -------------------------------------------------------------------- ##
df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm

write.csv(df_midterm, file = "Data/df_midterm.csv")



# 2. 외부 데이터 이용하기

# 엑셀 파일
library(readxl)                                 # readxl 패키지 로드
df_exam <- read_excel("Data/excel_exam.xlsx")        # 엑셀 파일 불러오기

# CSV 파일
df_csv_exam <- read.csv("Data/csv_exam.csv")         # CSV 파일 불러오기
write.csv(df_midterm, file = "df_midterm.csv")  # CSV 파일로 저장하기

# Rda 파일
load("Data/df_midterm.rda")                          # Rda 파일 불러오기
save(df_midterm, file = "df_midterm.rda")       # Rda 파일로 저장하기
