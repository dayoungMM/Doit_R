#### 06-2 ####

## -------------------------------------------------------------------- ##
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

#:: 왼쪽에 있는걸 오른쪽에 넣는다
dplyr::filter(.data = exam, class==1)
exam1 <- exam %>% dplyr::filter(class == 1)
exam1 <- filter(exam, exam$class ==1)
exam[exam$class==1,]
exam1

# exam에서 class가 1인 경우만 추출하여 출력
exam %>% filter(class == 1)  

# 2반인 경우만 추출
exam %>% filter(class == 2)  

# 1반이 아닌 경우
exam %>% filter(class != 1)

# 3반이 아닌 경우
exam %>% filter(class != 3)

# 1반과 3반이 아닌 경우 
exam %>% filter(class != 3 & (class != 1))

exam %>% filter(class != 3 %in% c(3,1))

## -------------------------------------------------------------------- ##
# 수학 점수가 50점을 초과한 경우
exam %>% filter(math > 50)

# 수학 점수가 50점 미만인 경우
exam %>% filter(math < 50)

# 영어 점수가 80점 이상인 경우
exam %>% filter(english >= 80)

# 영어 점수가 80점 이하인 경우
exam %>% filter(english <= 80)


## -------------------------------------------------------------------- ##
# 1반이면서 수학 점수가 50점 이상인 경우
exam %>% filter(class == 1 & math >= 50)

# 2반이면서 영어 점수가 80점 이상인 경우
exam %>% filter(class == 2 & english >= 80)


## -------------------------------------------------------------------- ##
# 수학 점수가 90점 이상이거나 영어 점수가 90점 이상인 경우
exam %>% filter(math >= 90 | english >= 90)

# 영어 점수가 90점 미만이거나 과학점수가 50점 미만인 경우
exam %>% filter(english < 90 | science < 50)


## -------------------------------------------------------------------- ##
# 1, 3, 5 반에 해당되면 추출
exam %>% filter(class == 1 | class == 3 | class == 5)

exam %>% filter(class %in% c(1,3,5))


## -------------------------------------------------------------------- ##
class1 <- exam %>% filter(class == 1)  # class가 1인 행 추출, class1에 할당
class2 <- exam %>% filter(class == 2)  # class가 2인 행 추출, class2에 할당

mean(class1$math)                      # 1반 수학 점수 평균 구하기
mean(class2$math)                      # 2반 수학 점수 평균 구하기


#과학점수가 50점 이상. 80점 이하인 3반이 아닌 학생중 영어점수가 가장 높은 id 는?
filt_stu <- exam %>% filter(class !=3) %>%
  filter(science >= 50 & science <=80) %>%
  select(id, english)

filt_stu

max_eng <- max(filt_stu$english)


filt_stu %>% filter(english == max_eng) %>%
  select(id)

#간단하게
filt_stu <- exam %>% filter(class !=3) %>%
  filter(science >= 50 & science <=80) %>%
  select(id, english) %>%
  filter(english == max(english))
filt_stu$id
  
#### 06-3 ####

## -------------------------------------------------------------------- ##
exam %>% select(math)                  # math 추출
exam %>% select(english)               # english 추출
exam %>% select(class, math, english)  # class, math, english 변수 추출
exam %>% select(-math)                 # math 제외
exam %>% select(-math, -english)       # math, english 제외


## -------------------------------------------------------------------- ##
# class가 1인 행만 추출한 다음 english 추출
exam %>% filter(class == 1) %>% select(english)

exam %>%
  filter(class == 1) %>%  # class가 1인 행 추출
  select(english)         # english 추출

exam %>% 
  select(id, math) %>%    # id, math 추출
  head                    # 앞부분 6행까지 추출

exam %>% 
  select(id, math) %>%  # id, math 추출
  head(10)              # 앞부분 10행까지 추출


### quiz p133

#### 둘 중 hwy평균 높은 자동차
s_car <- mpg %>% filter(displ <=4)
l_car <- mpg %>% filter(displ >=5)
print(mean(s_car$hwy))
print(mean(l_car$hwy))

#####두 제조사 도시연비 비교
audi <- mpg %>% filter(manufacturer == "audi")
toyota <- mpg %>% filter(manufacturer == "toyota")
print(mean(audi$cty))
print(mean(toyota$cty))

##### 세 제조사의 고속도로 연비 평균
rest_manu <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford","honda")) %>%
  summarise(mean_hwy = mean(hwy))
rest_manu

rest_manu2 <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford","honda"))
  
mean(rest_manu2$hwy)



### quiz p138

head(mpg)

mpg2 <- mpg %>% select(class, cty)
head(mpg2)

suv <- mpg2 %>% filter(class == "suv") %>% summarise(mean_cty = mean(cty))
suv
compact <- mpg2 %>% filter(class == "compact") %>% summarise(mean_cty = mean(cty))
compact

new <- data.frame(suv,compact)
new


### quiz 각 반별로 science의 최고점과 최저점인 학생 id를 구하라.
library(dplyr)
exam <- read.csv("Data/csv_exam.csv")

head(exam)

classes <- unique(exam$class)
max_id <- exam %>% group_by(class) %>%
  filter(science == max(science)) %>%
  summarise (max_id = id)


min_id <- exam %>% group_by(class) %>%
  filter(science == min(science)) %>%
  summarise (min_id = id)
data.frame(max_id,min_id)

minmax<-merge(max_id,min_id, by="class")
minmax


exam %>% group_by(class) %>%
  summarise(minId = id[which.min(science)],
            maxId = id[which.max(science)])




#### 06-4 ####

## -------------------------------------------------------------------- ##
exam %>% arrange(math)         # math 오름차순 정렬
exam %>% arrange(desc(math))   # math 내림차순 정렬
exam %>% arrange(class, math)  # class 및 math 오름차순 정렬

a<- exam %>% arrange(class, desc(math)) #클래스가 우선

b <-  exam %>% arrange(desc(math)) %>% arrange(class) #클래스가 우선

a == b

all(a==b)

sort(exam$math)

c <- sort(exam$math, index.return=T)$ix
d <- order(exam$math)
all(c==d)


### quiz p141
head(mpg)
audi <- mpg %>% filter(manufacturer == "audi") %>%
  arrange(desc(hwy))
audi[1:5,]

mpg %>% filter(manufacturer == "audi") %>%
  group_by(model) %>%
  summarise(mh = mean(hwy)) %>%
  arrange(desc(mh)) %>%
  head(5)


### p144 파생변수 추가하기

mpg2 <- mpg
mpg2<-mpg2 %>% mutate (total = cty + hwy)
mpg2["mean_total"] <- mpg2$total / 2
mpg2 %>%arrange(desc(mean_total)) %>%head(3)
mpg %>% mutate (total = cty + hwy, mean_total = (cty+hwy)/2) %>%
  arrange(desc(mean_total)) %>%
  head(3) 

#만약 모델별 평균 meanmpg가 높은 차종을 3개 출력하려면
mpg %>% mutate (total = cty + hwy, mean_total = (cty+hwy)/2) %>%
  group_by(model) %>%
  summarise(mean = mean(mean_total))%>%
  arrange(desc(mean))

mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean = mean(cty))%>%
  arrange(desc(mean)) %>%
  head(10)

head(mpg)



### quiz p150

mpg %>% group_by(class) %>%
  summarise(cty = mean(cty))%>%
  arrange(desc(cty))


mpg %>% group_by(manufacturer) %>%
  summarise(hwy = mean(hwy)) %>%
  arrange(desc(hwy))%>%
  head(5)

mpg %>% filter(class == "compact") %>%
  group_by(manufacturer) %>%
  summarise(count = length(model)) %>%
  arrange(desc(count))


mpg %>% filter(class == "compact") %>%
  group_by(manufacturer) %>%
  summarise(count = n(model)) %>%
  arrange(desc(count))



#### 06-5 ####

## -------------------------------------------------------------------- ##
exam %>%
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  head                                          # 일부 추출

exam %>%
  mutate(total = math + english + science,         # 총합 변수 추가
         mean = (math + english + science)/3) %>%  # 총평균 변수 추가
  head                                             # 일부 추출

exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head

exam %>%
  mutate(total = math + english + science) %>%  # 총합 변수 추가
  arrange(total) %>%                            # 총합 변수 기준 정렬
  head                                          # 일부 추출


#### 06-6 ####

## -------------------------------------------------------------------- ##
exam %>% summarise(mean_math = mean(math))  # math 평균 산출

exam %>% 
  group_by(class) %>%                   # class별로 분리
  summarise(mean_math = mean(math))     # math 평균 산출

exam %>% 
  group_by(class) %>%                   # class별로 분리
  summarise(mean_math = mean(math),     # math 평균
            sum_math = sum(math),       # math 합계
            median_math = median(math), # math 중앙값
            n = n())                    # 학생 수

mpg %>%
  group_by(manufacturer, drv) %>%       # 회사별, 구동방식별 분리
  summarise(mean_cty = mean(cty)) %>%   # cty 평균 산출
  head(10)                              # 일부 출력


## -------------------------------------------------------------------- ##
mpg %>%
  group_by(manufacturer) %>%           # 회사별로 분리
  filter(class == "suv") %>%           # suv 추출
  mutate(tot = (cty+hwy)/2) %>%        # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>%  # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>%          # 내림차순 정렬
  head(5)                              # 1~5위까지 출력


#### 06-7 ####

## -------------------------------------------------------------------- ##
# 중간고사 데이터 생성
test1 <- data.frame(id = c(1, 2, 3, 4, 5),           
                    midterm = c(60, 80, 70, 90, 85))

# 기말고사 데이터 생성
test2 <- data.frame(id = c(1, 2, 3, 4, 5),           
                    final = c(70, 83, 65, 95, 80))

test1  # test1 출력
test2  # test2 출력

total <- left_join(test1, test2, by = "id")  # id 기준으로 합쳐서 total에 할당
total                                        # total 출력


## -------------------------------------------------------------------- ##
name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name

exam_new <- left_join(exam, name, by = "class")
exam_new

exam_new <- left_join(exam, name[duplicated(name),],by = "class") #중복제거
exam_new


## -------------------------------------------------------------------- ##
# 학생 1~5번 시험 데이터 생성
group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                      test = c(60, 80, 70, 90, 85))

# 학생 6~10번 시험 데이터 생성
group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                      test = c(70, 83, 65, 95, 80))

group_a  # group_a 출력
group_b  # group_b 출력

group_all <- bind_rows(group_a, group_b)  # 데이터 합쳐서 group_all에 할당
group_all                                 # group_all 출력

#rbing, cbind 는 r의 기본함수로서 비슷한 기능을 한다.
# dply 패키지의 bind_cols, bind_rows, inner_join, left_join 함수를 사용하는 것이 더 빠르다

### quiz p156
# stringAsFactors: 문자를 factor타입으로 변환하지 않도록 설정하는 파라미터 
fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76 , 2.22),
                   stringsAsFactors = F)

fuel
head(mpg)

mpg_fuel <- left_join(mpg, fuel, by="fl")
mpg_fuel

mpg_fuel %>% select(model, fl, price_fl) %>% head(5)

View(mpg_fuel)


## -------------------------------------------------------------------- ##
## 1.조건에 맞는 데이터만 추출하기
exam %>% filter(english >= 80)

# 여러 조건 동시 충족
exam %>% filter(class == 1 & math >= 50)

# 여러 조건 중 하나 이상 충족
exam %>% filter(math >= 90 | english >= 90)
exam %>% filter(class %in% c(1,3,5))


## 2.필요한 변수만 추출하기
exam %>% select(math)
exam %>% select(class, math, english)

## 3.함수 조합하기, 일부만 출력하기
exam %>%
  select(id, math) %>%
  head(10)

## 4.순서대로 정렬하기
exam %>% arrange(math)         # 오름차순 정렬
exam %>% arrange(desc(math))   # 내림차순 정렬
exam %>% arrange(class, math)  # 여러 변수 기준 오름차순 정렬

## 5.파생변수 추가하기
exam %>% mutate(total = math + english + science)

# 여러 파생변수 한 번에 추가하기
exam %>%
  mutate(total = math + english + science,
         mean = (math + english + science)/3)

# mutate()에 ifelse() 적용하기
exam %>% mutate(test = ifelse(science >= 60, "pass", "fail"))

# 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>%
  mutate(total = math + english + science) %>%
  arrange(total)


## 6.집단별로 요약하기
exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math))

# 각 집단별로 다시 집단 나누기
mpg %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_cty = mean(cty))


## 7.데이터 합치기
# 가로로 합치기
total <- left_join(test1, test2, by = "id")

# 세로로 합치기
group_all <- bind_rows(group_a, group_b)

##########################################
### 분석도전

midwest <- ggplot2::midwest
midwest2 <- midwest
midwest2 <- midwest2 %>% mutate(child_rtio = 1- (popadults/poptotal))
names(midwest2)

midwest2 %>% arrange(desc(county)) %>% select(child_rtio) %>% head(5)
head(midwest2)

midwest2<-midwest2 %>% mutate(child_size = ifelse(child_rtio >=0.4, "large", ifelse(child_rtio >=0.3,"middle","small")))%>% 
  group_by(child_size) %>% summarise(count = n())

table(midwest2$child_size)



