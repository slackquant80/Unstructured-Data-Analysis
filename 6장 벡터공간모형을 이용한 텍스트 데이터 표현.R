## 대소문자 변환, 문장부호 삭제, 원형복원
library(textstem)
x <- c("the best theater in New_York",
       "the best hotel in New_York", "the best gift for kids")
x <- tolower(x)
x <- gsub(x, pattern = "([^[:alnum:][:blank:]'-])",
          replacement = "")
x <- lemmatize_strings(x)
bows <- strsplit(x, " ")
bows

# 세 문장의 단어 출현 빈도를 하나의 행렬로
lev <- sort(unique(unlist(bows)))
lev

DTM <- lapply(bows, FUN = function(y, lev) {table(factor(y, lev,
                                                         order = T))}, lev = lev)
DTM
DTM <- matrix(unlist(DTM), nrow = length(DTM), byrow = TRUE)
DTM
colnames(DTM) <- lev
rownames(DTM) <- paste('doc', 1:dim(DTM)[1], sep = "")
DTM
paste('doc', 1:dim(DTM)[1], sep = "")

#### TF-IDF 방식
# TF 행렬과 IDF 행렬 작성하여 두 행렬의 원소들을 각각 곱하는 방식
TF <- 1 + log(DTM)
TF
TF[TF == -Inf] <- 0
TF

# 문서빈도 구하기
DTM[DTM > 0] <- 1
DTM
DF <- colSums(DTM)   # DTM 행렬의 각 열별로 0이 아닌 셀의 수를 셀 필요가 있음
DF

# 역문서빈도
IDF <- log(dim(DTM)[1] / DF)
dim(DTM)
dim(DTM)[1]
dim(DTM)[1] / DF

# TF-IDF 완성
TFIDF <- t(t(TF) * IDF)
TFIDF

TF
IDF

## 다른 방식으로(계산 방식의 차이일 뿐)
IDFmat <- matrix(IDF, nrow = dim(TF)[1], ncol = dim(TF)[2],
                 byrow = T)
IDFmat
TFIDF <- TF * IDFmat

####################################################################
