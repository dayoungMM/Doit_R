### 변수만들기

a <- 1
a

b <- 2
a+b

### 여러값으로 구성된 변수 만들기
var1 <- c(1,2,5,7,8)
var1

var2 <- c(1:4)
var2

### 연속된 값으로 벡터 만들기
var3 <- seq(1,5)
var3

var4 <- seq(1,10,by=2)
var4

var4+2

### 브로드캐스팅
## R은 shape이 달라도 무조건 브로드 캐스팅 연산 가능
v1 <- c(1,3,5)
v2 <- c(0,4,6,8,9)
v1*v2
v1%*%v2 ##벡터의 내적(shape이 맞아야 연산가능)
v1%*%v1 == sum(v1*v1)
sum(v1,v2)
rep(v1,3) #repeat
seq(1,10,2)



### 문자로된 변수 만들기
str1 <- "Hello"

str2 <- c("a","b","c")
str2
# 단, 문자로된 변수로는 연산 불가


################################################
### 숫자를 다루는 함수




x <- c(1,2,3)
{mean(x)
max(x)
min(x)
median(x)
var(x)
sd(x)
}

avg <- function(v){
  len = length(v)
  sumv = sum(v)
  return (sumv/len)
}
avg(x)



n1<- 1:20
cut(n1, 5)

q<- c(8,7,3)
sort(q)
order(q, decreasing = T)

q[order(q, decreasing = T)][1:3]

which(1%%2==0)

q[q%%2 ==0]
q[which(q%%2==0)]

### 문자를 다루는 함수
str5 <- c("Hello","World","is","Good")

paste(str5, collapse = ",") #,를 구분자로 str5단어를 하나로 합침
                          #python) ",".join(str5)
### 함수의 옵션 설정하기 - 파라미터
paste(str5, collapse = "")

### 
set.seed(123)
sample(q,10,replace = T)  #random.choice  ## 중복추출 허용

(x=1:6)  #괄호하면 출력까지 해줌
(p = 1:6/sum(1:6))
sample(x,100,replace=T, p=p) ##주사위에서 랜덤추출하는데, probability 는 p의 확률로
#table :빈도를 세는 함수
table(sample(x,100,replace=T, p=p))
table(sample(x,100,replace=T, p=p))/10000

rm(list=ls(pattern = "v\\d"))


#################################################################
str1 <- "a"
str1

str2 <- "text"
str2

str3 <- "Hello World!"
str3

str4 <- c("a", "b", "c")
str4

str5 <- c("Hello!", "World", "is", "good!")
str5

str1+2

#데이터 타입
mode(str1)

#색인
grep('a',str5)
str5[grep('o',str5)]
str5[grepl('o',str5)]

#split

s1<-strsplit("I have a mail", sq <-" ")
s1  #list 형식인데, [[1]] 형식임
s1<- unlist(s1) #[1]형식으로 바꿔줌줌


library(ggplot2)
# 빈도 그래프 출력
qplot(x)


## -------------------------------------------------------------------- ##
# data에 mpg, x축에 hwy 변수 지정하여 그래프 생성
qplot(data = mpg, x = hwy)

# x축 cty
qplot(data = mpg, x = cty)

# x축 drv, y축 hwy
qplot(data = mpg, x = drv, y = hwy)

# x축 drv, y축 hwy, 선 그래프 형태
qplot(data = mpg, x = drv, y = hwy, geom = "line")

# x축 drv, y축 hwy, 상자 그림 형태
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot")

# x축 drv, y축 hwy, 상자 그림 형태, drv별 색 표현
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)

# qplot 함수 매뉴얼 출력
?qplot

mpg
qplot(data = mpg, x = hwy, bins = 20)
summary(mpg$hwy)
mpg[7:10] #열색인


qplot(data = mpg, x=drv, y=hwy, ggom = "line")

example(qplot)


qplot(cty, hwy, data=mpg)+
  geom_smooth(method="lm", se=T) #회귀선 추가, 오차 범위 추가

ggplot(mpg, aes(x=cty, y=hwy))+
  geom_point(size = 2)+    #산점도
  geom_smooth(method = "lm", se=F) #회귀선 

#-cy1을 cty와 hwy간 산점도를 그려라
qplot(cty, hwy, data= mpg, facets = ~cyl) #facet:입장을 달리해서/ layout 나누는 기준/ 원인변수



qplot(factor(cyl), hwy, data=mpg, geom="boxplot") #boxplot을 cyl에 따라

mpg

#제조사멸 모델별 바차트를 작성하라
qplot(manufacturer, data=mpg, geom="bar")
qplot(manufacturer, data=mpg, geom="bar")+coord_flip() #h-bar로 변경
qplot(model, data=mpg, geom="bar")+coord_flip()

manuf_mod = paste(mpg$manufacturer, mpg$model ,sep =" | ") #두개씩 결합합
manuf_mod
qplot(manuf_mod, data=mpg, geom="bar")+coord_flip()

## quiz p77
scores <- c(30,50,70,40,80)
mean(scores)
