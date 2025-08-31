data("iris")

iris

############## 단어주머니
x <- c("Kim was loved by everybody.", "Everybody loved Kim",
       "Kim loved everybody")
x

strsplit(x, split = " ")     # "" 이렇게 해주면 안됨 --> 철자마다 구분이 되어버림

strsplit(x, split = " ")[[1]]

strsplit(x, split = " ")[[2]][3]

##########################################  토큰화
sub(pattern = "ouse", replacement = "ay", x = "mouse in the house")

gsub(pattern = "ouse", replacement = "ay", x = "mouse in the house")

### 축약된 표현의 토큰화
contraction_dict <- list(c("don't", "it's", "you're"),
                         c("do not", "it is", "you are"))
contraction_dict

dictlen <- length(contraction_dict[[1]])
dictlen

datstr <- "I don't think you're ready."
for (stri in 1:dictlen) {
  datstr <- gsub(pattern = contraction_dict[[1]][stri],
                 replacement = contraction_dict[[2]][stri], x = datstr)
}
datstr

strsplit(datstr, " ")

##### n-그램(n-gram)의 토큰화
ngram_dict <- list(c("bed and breakfast", "grab and go",
                     "New York"), c("bed_and_breakfast", "grab_and_go", "New_York"))
ngram_dict

ndictlen <- length(ngram_dict[[1]])

datstr <- "It's one of the best bed and breakfast in New York."

for (stri in 1:ndictlen) {
  datstr <- gsub(x = datstr, pattern = ngram_dict[[1]][stri],
                 replacement = ngram_dict[[2]][stri])
}

datstr

for (stri in 1:dictlen) {
  datstr <- gsub(pattern = contraction_dict[[1]][stri],
                 replacement = contraction_dict[[2]][stri],
                 x = datstr, ignore.case = T)
}

datstr

strsplit(datstr, " ")

########### 대소문자 변환
x <- c("Kim was a 12-year-old boy.", "It's 11 O'clock.",
       "Everybody loved Kim.")
tolower(x)

####### 문장부호 삭제
x <- c("Kim was a 12-year-old boy.", "It's 11 O'clock.",
       "Everybody loved Kim.")

# 문장부호 삭제
gsub(x, pattern = "([[:punct:]])", replacement = "")

# 문자와 숫자, 공백, 아포스트로피('), 하이픈(-) 에 해당되는 문자들은 삭제되지 않고 다른 문장부호만 삭제
gsub(x, pattern = "([^[:alnum:][:blank:]'-])", replacement = "")

# 정규표현식 사용해서 문장의 첫 글자로 사용된 대문자를 소문자로 변환
gsub(x, pattern = "([[:upper:]])", replacement = "\\L\\1",
     perl = T)

## 대소문자 변환 + 문장부호 삭제 --> 단어주머니 생성
x <- gsub(x, pattern = "([[:upper:]])", replacement = "\\L\\1",
          perl = T)
x

x <- gsub(x, pattern = "([^[:alnum:][:blank:]'-])",
          replacement = "")
x
strsplit(x, split = " ")

#### 어간추출
library(textstem)
txts <- c("The Williams sisters are leaving this tennis centre.")
stem_strings(txts, language = "porter")

#### 원형복원
lemmatize_strings(txts)

########## 불용어 삭제
library(stopwords)
stopwords()    # 불용어 목록

txt <- c("He decided to quit his job.", "I do not deny it.",
         "Can you hear me?")
txt <- tolower(txt)
txt <- gsub(txt, pattern = "([^[:alnum:][:blank:]'-])",
            replacement = "")
txt <- strsplit(txt, " ")
lapply(txt, setdiff, y = stopwords())   # setdiff() 함수는 차집합 구하는 함수 -> 불용어 목록에 속한 단어들 제거

## 불용어 목록 일부 수정
newstop <- c(stopwords(), "can")
newstop <- setdiff(newstop, c("no", "not"))

lapply(txt, setdiff, y = newstop)

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
