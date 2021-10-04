library(httr)
library(rvest)
library(V8)

# 汇总榜
url <- "http://top.finance.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=finance_0_suda&top_time=20190429&top_show_num=20&top_order=DESC&js_var=all_1_data&get_new=1"
url <- "https://top.finance.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=finance_0_suda&top_time=20210928&top_show_num=20&top_order=DESC&js_var=all_1_data&get_new=1"

web <- httr::GET(url)
test <- web %>% read_html() %>% html_node("body") %>% html_text()

web %>% 
  read_html() %>% 
  html_node("body") %>% 
  html_text()

ct <- V8::v8()
ct$eval(test)
ct$eval(test)
page_data <- ct$get("all_1_data")
page_data <- page_data$data

url <- "http://www.cbirc.gov.cn/cn/view/pages/ItemDetail.html?docId=843851&itemId=928&generaltype=0&tdsourcetag=s_pctim_aiomsg"

web <- httr::GET(url)
article <- web %>% read_html() %>% html_elements(xpath = "div[class='Section0']") %>% html_text()

web %>% 
  read_html() %>% 
  html_node("body") %>% 
  html_text()

ct <- V8::v8()
ct$eval(article)
article_text <- ct$get("section0")
article_text <- page_data$data




# 新闻榜
url <- "http://top.finance.sina.com.cn/ws/GetTopDataList.php?top_type=day&top_cat=finance_news_0_suda&top_time=20190429&get_new=1&top_show_num=20&top_order=DESC&js_var=all_1_data"

web <- GET(url)
test <- web %>% read_html() %>% html_node("body") %>% html_text()

ct <- v8()
ct$eval(test)
page_data2 <- ct$get("all_1_data")
page_data2 <- page_data2$data