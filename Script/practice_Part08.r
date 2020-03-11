## 8장 그래프 만들기
#### 08-2 산점도 - 수치변수 간 관계 ####
rm(list=ls())
## -------------------------------------------------------------------- ##
library(ggplot2)
mpg <- ggplot2::mpg

# x축 displ, y축 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y = hwy))

# 배경에 산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
ggplot(mpg, aes(displ, hwy)) + geom_point()

# x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) #범위지정

# x축 범위 3~6, y축 범위 10~30으로 지정 
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) + 
  ylim(10, 30)

### Quiz 실습하기 ###

ggplot(data = mpg, aes(x=cty, y=hwy)) +
  geom_point()


midwest <- as.data.frame(ggplot2::midwest)


ggplot(data=midwest, aes(x=poptotal, y=popasian))+
  geom_point() +
  xlim(0,3e+06)+
  ylim (0,40000)

ggplot(data=midwest, aes(x=log10(poptotal), y=log10(popasian)))+
  geom_point() 


#### 08-3 막대 그래프 - 집단 간 차이 표현 ####

## -------------------------------------------------------------------- ##
library(dplyr)

df_mpg <- mpg %>% 
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

df_mpg
# 데이터를 요약한 평균표를 그대로 이용할 때 geom_col()
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col() #빈도수가 이미 정해져있을때 


# - reorder(category, freq): freq 값에 따라 category를 재배열.
#-mean_hwy : 내림차순으로 정렬
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), 
                          y = mean_hwy)) + geom_col()

# 요약표를 이용하지 않고 원자료를 그대로 빈도체크해서 그릴 때 geom_bar()
table(mpg$drv)
ggplot(data = mpg, aes(x = drv)) + geom_bar()

ggplot(data = mpg, aes(x = hwy)) + geom_bar(width=1) #빈도수를 컴퓨터가 알아서 세줘야할 때 
ggplot(data = mpg, aes(x = hwy)) + geom_histogram(bins=30)

### Quiz 실습하기 ###
head(mpg)
suvs<-mpg%>% 
  (class=="suv") %>%
  group_by(manufacturer) %>%
  summarise(cty = mean(cty, na.rm=T)) %>%
  arrange(desc(cty)) %>% 
  head(5)

suvs

ggplot(data=suvs, aes(x = reorder(manufacturer,-cty), y= cty, fill=manufacturer))+
  geom_col() +
  scale_x_discrete(limits = suvs$manufacturer)

head(mpg)

ggplot(data=mpg, aes(x = mpg$class))+
  geom_bar()

#### 08-4 선 그래프 - 시계열 데이터 표현 ####

## ---- line graph를 위해서는 x, y가 모두 필요 ------- ##
ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line()

### Quiz 실습하기 ###
economics

ggplot(data = economics, aes(x=date, y=psavert))+
  geom_line()

#### 08-5 상자 그림 - 집단 간 분포 차이 표현 ####

## ------------ boxplot은 y를 필요로한다. ------------ ##
ggplot(data = mpg, aes(y = hwy)) + geom_boxplot()
boxplot(mpg$hwy)
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()
boxplot(mpg$hwy ~ mpg$drv)


### Quiz 실습하기 ###

mpg2 <- mpg%>% filter(class %in% c("compact","subcompact","suv"))
ggplot(data = mpg2, aes(x=class, y=cty)) +geom_boxplot()
boxplot(mpg2$cty ~ mpg2$class)

## ---- 8장 summary -------------------------- ##
## 1.산점도
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

# 축 설정 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)


## 2.평균 막대 그래프

# 1단계.평균표 만들기
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

# 2단계.그래프 생성하기, 크기순 정렬하기
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()


## 3.빈도 막대 그래프
ggplot(data = mpg, aes(x = drv)) + geom_bar()

## 4.선 그래프
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

## 5.상자 그림
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

