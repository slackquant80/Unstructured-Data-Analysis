library(httr)

webpage = GET('http://press.knou.ac.kr/goods/textBookList.do?condLscValue=001&condMscValue=003&condSscValue=007&condScyr=4')

library(XML)
web = htmlParse(webpage)

gsub("[\r\n\t]", "", xpathSApply(web, '//*[@id="listForm"]/div/div[3]/div[4]/div[3]/table/tbody/tr[4]/td[2]/div/h5/a', xmlValue))
gsub("[\r\n\t]", "", xpathSApply(web, '//*[@id="listForm"]/div/div[3]/div[4]/div[3]/table/tbody/tr[3]/td[2]/div/h5/a', xmlValue))
# 전처리(줄바꿈 등에 대한 기호들이 같이 출력되는데 그것들 제거)

xpathSApply(web, '//*[@id="listForm"]/div/div[3]/div[4]/div[3]/table/tbody/tr[4]/td[2]/div/h5/a', xmlValue)

ls <- rep("", 5)
for (i in 1:5) {
  sub <- paste0('//*[@id="listForm"]/div/div[3]/div[4]/div[3]/table/tbody/tr[',i,']/td[2]/div/h5/a')
  ls[i] <- gsub("[\r\n\t]", "", xpathSApply(web, sub, xmlValue))
}

ls

#######################################################
library(rvest)
library(dplyr)
exurl <- "https://ko.wikipedia.org/wiki/%EB%B9%84%EC%A0%95%ED%98%95_%EB%8D%B0%EC%9D%B4%ED%84%B0"

html_ex <- read_html(exurl, encoding = "UTF-8")
html_ex %>% 
  html_nodes(".mw-parser-output p") %>% 
  html_text()

html_text(html_nodes(html_ex, ".mw-parser-output p"))

html_ex %>% 
  html_nodes("#mw-content-text p") %>% 
  html_text()


ex2url <- "https://www.index.go.kr/unity/potal/main/EachDtlPageDetail.do?idx_cd=2736"
html_ex2 <- read_html(ex2url, encoding = "UTF-8")
html_ex2 %>% 
  html_nodes(".table_style_2") %>% 
  html_table()

html_table(html_nodes(html_ex2, ".table_style_2"))
html_table(html_nodes(html_ex2, "#t_Table_273601"))

### 다른 테이블 가져오는 예(교재 주소는 변경으로 인해 실습결과 잘 안나옴)
library(rvest)
library(dplyr)
ex2url <- "http://en.wikipedia.org/wiki/Economy_of_South_Korea"
html_ex2 <- read_html(ex2url, encoding = "UTF-8")
html_ex2 %>% 
  html_nodes(".wikitable") %>% 
  html_table()


################### textstem 패키지
library(textstem)


library(stopwords)
stopwords("en")
stopwords("en", source = "snowball")
