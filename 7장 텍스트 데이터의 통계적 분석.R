library(stopwords)
library(textstem)

### '로빈슨 크루소'와 '작은 아씨들' 각 장별 코사인 유사도 비교
RC <- scan("http://www.gutenberg.org/files/521/521-0.txt",
           what = "character", encoding = "UTF-8", sep = "\n")
LW <- scan("http://www.gutenberg.org/cache/epub/514/pg514.txt",
           what = "character", encoding = "UTF-8", sep = "\n")

## "CHAPTER" 단어 기준으로 나누어져 있으므로... 단어가 등장하는 행의 번호를 grep() 함수로 찾기
RC_Chpt <- grep(RC, pattern = "CHAPTER")
LW_Chpt <- grep(LW, pattern = "CHAPTER")

## 두 소설의 본문은 "end of the project...." 문구로 끝남... 바로 앞 행을 찾아서 저장
RC_End <- grep(tolower(RC), pattern = "end of the project gutenberg") - 1
LW_End <- grep(tolower(LW), pattern = "end of the project gutenberg") - 1

## 본문 저장
RC_body <- RC[(RC_Chpt[21]):RC_End]
LW_body <- LW[(LW_Chpt[48]):LW_End]
RCLW_body <- c(RC_body, LW_body)      # 하나의 벡터로 저장

RCLW_body
LW_body
## paste() 함수로 두 문서를 하나의 문자열로 연결..
# 각 장이 "CHAPTER"라는 단어로 구분될 수 있음을 이용 strsplit() 함수로 장별 구분
# 리스트 형태로 나온 결과를 벡터로 저장
# 첫 번째 원소는 분석 대상이 되는 내용 포함 x -> 삭제 (범위로 삭제... 수정사항)
RCLW_by_Chpt <- unlist(strsplit(paste(RCLW_body, collapse = " "), "CHAPTER"))[-1]
head(RCLW_by_Chpt, 1)

strsplit(paste(RCLW_body, collapse = " "), "CHAPTER")
RC_body
RC_Chpt

a <- "i love you"
strsplit(a, "love")

## 전처리 과정
RCLW_by_Chpt <- gsub(x = RCLW_by_Chpt, pattern = "'s",
                     replacement = "")
RCLW_by_Chpt <- gsub(RCLW_by_Chpt,
                     pattern = "([^[:alnum:][:blank:]'-])", replacement = "")
RCLW_by_Chpt <- tolower(RCLW_by_Chpt)
RCLW_by_Chpt <- strsplit(RCLW_by_Chpt, " ")
RCLW_by_Chpt <- lapply(RCLW_by_Chpt,
                       function(x) x[! x %in% c(stopwords(), "")])
RCLW_by_Chpt <- lapply(RCLW_by_Chpt, lemmatize_strings)

## 문서-단어 행렬 RCLW_DTM 작성
RCLW_lev <- sort(unique(unlist(RCLW_by_Chpt)))
RCLW_DTM <- lapply(RCLW_by_Chpt, FUN = function(x, lev)
{table(factor(x, lev, ordered = T))}, lev = RCLW_lev)
RCLW_DTM <- matrix(unlist(RCLW_DTM), nrow = length(RCLW_DTM),
                   byrow = TRUE)

# 차원 
dim(RCLW_DTM)

sum(RCLW_DTM>0)

sum(RCLW_DTM == 0)


a <- c(1, 2, 3, 0, 5, 0, 10)
sum(a)
sum(a>0)
sum(a == 0)

##### 문서-단어 행렬을 이용한 코사인 유사도 계산
## 각 문서 벡터의 내적 구하기
RCLW_DTMsqr <- RCLW_DTM %*% t(RCLW_DTM)

RCLW_CosSim <- RCLW_DTMsqr / sqrt(diag(RCLW_DTMsqr) %*% t(diag(RCLW_DTMsqr)))

## 샘플 구성
set.seed(1)
RCsample <- sort(sample.int(n = 20, size = 5))
RCsample

LWsample <- sort(sample.int(n = 47, size = 5))
LWsample

RCLW_CosSim_smpl <- RCLW_CosSim[c(RCsample, LWsample+20),         # 처음 20개 행,열이 로빈슨크루소이므로.. 
                                c(RCsample, LWsample+20)]

# 시각화
library(corrplot)
corrplot(RCLW_CosSim_smpl)

#### 군집분석
## 코사인 비유사성 행렬 기준
RCLW_clusters <- hclust(as.dist(1 - RCLW_CosSim))
RCLW_clusters$labels <- c(paste("Robinson", c(1:20)), paste("Women", c(1:47)))
plot(RCLW_clusters)

## 단일, 평균연결법으로
RCLW_single <- hclust(as.dist(1 - RCLW_CosSim), method = "single")
RCLW_single$labels <- c(paste("Robinson", c(1:20)),
                        paste("Women", c(1:47)))
plot(RCLW_single, main = "Cluster Dendrogram - Single", xlab = " ", sub = " ")

RCLW_average <- hclust(as.dist(1 - RCLW_CosSim), method = "average")
RCLW_average$labels <- c(paste("Robinson", c(1:20)),
                         paste("Women", c(1:47)))
plot(RCLW_average, main = "Cluster Dendrogram - Average", xlab = " ", sub = " ")

#### 유클리드 거리 기준
RCLW_euclidean <- hclust(dist(RCLW_DTM))
RCLW_euclidean$labels <- c(paste("Robinson", c(1:20)),
                           paste("Women", c(1:47)))
plot(RCLW_euclidean, main = "Cluster Dendrogram - Euclidean Distance", xlab = " ", sub = " ")

### 상대도수 유클리드 거리
RCLW_relfreq <- hclust(dist(RCLW_DTM / rowSums(RCLW_DTM)))
RCLW_relfreq$labels <- c(paste("Robinson", c(1:20)),
                         paste("Women", c(1:47)))
plot(RCLW_relfreq, xlab = " ", sub = " ")


## 분류분석
RCLW_DTMs <- rbind(colSums(RCLW_DTM[1:20,]),
                   colSums(RCLW_DTM[21:67,]))
sum(RCLW_DTMs[1,] > 0 & RCLW_DTMs[2,] > 0)
# 공통 사용된 단어들...

sample_words <- sample(which(RCLW_DTMs[1,] * RCLW_DTMs[2,]>0), 25)
RCLW_DTMs[1:2, sort(sample_words)]

RCLW_DTM[1:20, RCLW_lev == 'jo']
RCLW_DTM[21:67, RCLW_lev == 'jo']

str(RCLW_lev)
RCLW_lev[4810]
match('jo', RCLW_lev)

RCLW_DTM_smpl <- RCLW_DTM[, sample_words]
colnames(RCLW_DTM_smpl) <- RCLW_lev[sample_words]
RCLW_target <- c(rep("Robin", 20), rep("Women", 47))

RCLW_target

### rpart
library(rpart)
ctrl <- rpart.control(minsplit = 5, cp = -0.01, xval = 10)

fit_tree <- rpart(RCLW_target ~ ., data = data.frame(RCLW_DTM_smpl),
                  method = "class", control = ctrl)
fit_tree

plot(fit_tree)
text(fit_tree)

prune_tree <- prune(fit_tree, cp = 0)
prune_tree

plot(prune_tree, margin = 0.1)
text(prune_tree, cex = 2)
