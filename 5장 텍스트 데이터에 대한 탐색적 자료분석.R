####### 실제 텍스트 데이터의 전처리
RC <- scan("http://www.gutenberg.org/files/521/521-0.txt",
           what = "character", encoding = "UTF-8", sep = "\n")
RC

RC_Chpt <- grep(RC, pattern = "CHAPTER")   # 패턴 위치 찾아주는...  각 장이 시작되는 부분에 CHAPTER 단어 사용
RC_Chpt

RC_End <- grep(RC, pattern = "END OF THE PROJECT GUTENBERG EBOOK") - 1
RC_body <- RC[(RC_Chpt[1]):RC_End]
RC_body
# 각 장이 CHAPTER 단어 사용되고, END OF ~ 표현으로 본문이 마무리되는 규칙성 활용해서

## 본문에 대한 전처리 작업
# 각 행들이 구분자 "\n"로 나누어져 있으므로 이 행들을 하나로 연결하기 위해 paste() 함수를 사용
# paste() 함수에 collapse = " " 인수 입력... 각 행마다 공백(" ") 덧붙여 하나의 벡터로 연결
RC_all <- paste(RC_body, collapse = " ")
RC_all

# 소유격, is의 축약형 등에서 사용되는 "'s"를 삭제(""로 대체), '- 제외한 나머지 문장부호들 제거
# 대문자를 소문자로 변환
RC_all <- gsub(RC_all, pattern = "'s", replacement = "")
RC_all <- gsub(RC_all, pattern = "([^[:alnum:][:blank:]'-])",
               replacement = "")
RC_all <- tolower(RC_all)

# 불용어 삭제 : 이를 위해 연결되어 있는 문서를 단어 단위로 분리 -> 공백 기준
# n-그램 반영 분리가 바람직 -> n-그램 작성에 많은 노력 필요... -> 여기서는 편의상 공백 기준으로..
RC_all <- unlist(strsplit(RC_all, " "))
RC_all

library(stopwords)
RC_all <- RC_all[! RC_all %in% c(stopwords(), "")]         # 위에서 "'s"를 ""로 대체했기 때문에 이것도 제거
RC_all

# 문장부호 중 삭제되지 않은 아포스트로피(') 삭제, 단어들의 원형 복원
# 불용어 목록에 다수의 축약된 표현들이 포함되어 있기 때문
RC_all <- gsub(RC_all, pattern = "'", replacement = "")
library(textstem)
RC_all <- lemmatize_strings(RC_all)

RC_all_table <- sort(table(RC_all), decreasing = T)
RC_all_table

# 상대도수분포표
RC_all_proptable <- sort(prop.table(table(RC_all)),
                         decreasing = T)
RC_all_proptable

#############################  감성 어휘 사전...
library(tidytext)

bingsent <- get_sentiments("bing")
bingpos <- bingsent[bingsent$sentiment == "positive",]
bingneg <- bingsent[bingsent$sentiment == "negative",]

bingpos$word[1:30]
bingneg$word[1:30]

RC_all_table[names(RC_all_table) %in% bingpos$word]
RC_all_table[names(RC_all_table) %in% bingneg$word]

#### 막대그래프 이용 도수분포표 시각화
barplot(RC_all_table[1:32], las = 2)
RC_all_table

## 감성 어휘 사용하여 그려보기...
RC_sent <- ifelse(RC_all %in% bingpos$word, 1, ifelse(RC_all %in% bingneg$word, -1, 0))
RC_all
RC_sent

seq_along(RC_sent)
(seq_along(RC_sent) - 1) %/% 1000     # 0, 1, 2,.... 1000개씩 반복되는 벡터

barplot(tapply(RC_sent, (seq_along(RC_sent) - 1) %/% 1000, sum))
tapply(RC_sent, (seq_along(RC_sent) - 1) %/% 1000, sum)

####### 워드클라우드
library(wordcloud)
wordcloud(words = names(RC_all_table), freq = RC_all_table,
          max.words = 100)

### 색상으로...
wordcolor = rep("grey", length(RC_all_table))
wordcolor[names(RC_all_table) %in% bingpos$word] = "blue"
wordcolor[names(RC_all_table) %in% bingneg$word] = "black"
wordcloud(names(RC_all_table), RC_all_table, max.words = 100,
          colors = wordcolor, random.order = F, ordered.colors = T)

#### 출현 위치..
friday <- ifelse(RC_all == "friday", 1, 0)
plot(friday, type = "h", ylim = c(0, 1))

fear <- ifelse(RC_all == "fear", 1, 0)
plot(fear, type = "h", ylim = c(0, 1))


##### 두 문서의 단어 출현 빈도 비교
# RC_body는 전처리 되지 않은 원 문서가 저장되어 있는 문자열 벡터임
RC_body

# paste() 이용해서 RC_body 벡터 내의 원소들을 공백 " "을 덧붙여 가며 하나의 문자열로 묶어 주기
RC_body[1]
paste(RC_body, collapse = " ")

# strsplit() 이용해서 'CHAPTER' 등장하는 위치에서 나누어 주기
strsplit(paste(RC_body, collapse = " "), "CHAPTER")
unlist(strsplit(paste(RC_body, collapse = " "), "CHAPTER"))[-c(1:3)]

RC_by_Chpt <- unlist(strsplit(paste(RC_body, collapse = " "), "CHAPTER"))[-c(1:21)]    # 제목만 있는 부분 제거
RC_by_Chpt

## 문장부호 삭제, 대소문자 변환, 불용어 삭제, 원형복원
RC_by_Chpt <- gsub(pattern = "'s", replacement = "", x = RC_by_Chpt)
RC_by_Chpt <- gsub(RC_by_Chpt,
                   pattern = "([^[:alnum:][:blank:]'-])", replacement = "")
RC_by_Chpt <- tolower(RC_by_Chpt)
RC_by_Chpt

RC_by_Chpt <- strsplit(RC_by_Chpt, " ")
RC_by_Chpt

RC_by_Chpt <- lapply(RC_by_Chpt,
                     function(x) x[! x%in% c(stopwords(), "")])

RC_by_Chpt <- lapply(RC_by_Chpt, lemmatize_strings)
RC_by_Chpt
RC_by_Chpt[[1]]


####### 도수분포표와 막대그래프 이용 문서 비교
# 각 장의 내용, 즉 각 문자열 벡터로부터 생성한 단어주머니들을 비교하는 것이 목적이므로
# 해당 장에서는 등장하지 않았지만 다른 장에서 한 번이라도 등장한 적이 있는 단어들까지 고려

# 문서에 등장한 모든 단어들이 포함되어 있는 RC_all 벡터에서 단어들을 중복되지 않도록 unique() 함수로
# sort() 함수로 정렬
lev <- sort(unique(RC_all))
lev
RC_all

RC_Chpt3 <- table(factor(RC_by_Chpt[[3]], levels = lev, ordered = T))
RC_Chpt5 <- table(factor(RC_by_Chpt[[5]], levels = lev, ordered = T))
RC_Chpt3

## 2개 장의 도수분포표의 차이를 구하면 두 문서 사이의 단어 출현 빈도이 차이를 나타내는 벡터 생성
RC_Chpt5_3 <- RC_Chpt5 - RC_Chpt3
RC_Chpt5_3 <- sort(RC_Chpt5_3)
RC_Chpt5_3 <- RC_Chpt5_3[abs(RC_Chpt5_3) > 5]    # 출현 빈도 차이가 큰 단어들만 선택

RC_Chpt5_3


# 원래 차이 확인
RC_Chpt5_3_raw <- RC_Chpt5 - RC_Chpt3
sort(RC_Chpt5_3_raw)[1:20]   # 가장 많이 차이나는 상위 20개 단어 확인
max(abs(RC_Chpt5_3_raw))     # 최대 차이가 몇 개인지 확인


## 막대그래프
barplot(RC_Chpt5_3, las = 2)

## 워드클라우드 이용
# 공통된 단어들을 확인하기
RC_Chpt35 <- cbind(RC_Chpt3, RC_Chpt5)
commonality.cloud(RC_Chpt35, max.words = 200, random.order = FALSE)
# 단어-문서 행렬 형태로 입력해준 것 : 행에 단어들을, 열에 해당 단어들이 등장한 문서들을 배치하는 행렬

# 문서들 사이의 출현 빈도에 차이가 있는 단어들 이용
# 연한회색 -> 3장에서 더 많이 등장, 검은색 -> 5장에서 더 많이
comparison.cloud(RC_Chpt35, max.words = 200, random.order = FALSE)

## 산점도 이용하기
# 출현 빈도 차이 덧붙이기
RC_Chpt35 <- cbind(RC_Chpt35, RC_Chpt35[, 1] - RC_Chpt35[, 2])

# 색깔 구분 지정
# ifelse() 함수 사용, 3장에서 출현빈도가 5장보다 10번 넘게 높은 단어 --> 검은색, 반대는 파란색, 나머지 회색
RC_Chpt35col <- ifelse(RC_Chpt35[, 3] > 10, "black",
                       ifelse(RC_Chpt35[, 3] < -10, "blue", "grey"))

plot(RC_Chpt35[, 1], RC_Chpt35[, 2], type = "n")
text(RC_Chpt35[, 1], RC_Chpt35[, 2], row.names(RC_Chpt35),
     col = RC_Chpt35col)
