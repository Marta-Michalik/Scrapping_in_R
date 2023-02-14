#install.packages("rvest")
#install.packages("dplyr")
#install.packages('xml2')
#install.packages("tidyverse")
#install.packages("stringr")
library("stringr")
library("tidyverse")
library('xml2')
library("rvest")
library("dplyr")

#create a frame for data
books <- data.frame()

#function for getting info from subpage
i = 1
get_year <- function(book_link) {
  book_page <- read_html(book_link)
  year <- book_page %>% html_nodes(".row+ .row") %>% html_text() %>% paste(collapse = ",")
  print("doing sth")
  print(i)
  i = i + 1
  return(year)
}


for (page_nr in seq(from = 1, to = 2, by = 1)) {
  #get page link and load
  link <- paste0("https://www.goodreads.com/list/show/264.Books_That_Everyone_Should_Read_At_Least_Once?page=", page_nr, "")
  page <- read_html(link)
  
  #scrap title, author and score
  title <- page %>% html_nodes(".bookTitle") %>% html_text()
  author <- page %>% html_nodes(".authorName span") %>% html_text()
  score <- page %>% html_nodes(".uitext a:nth-child(1)") %>% html_text()
  
  #get info from subpages and get the info about publishing
  book_links <- page %>% html_nodes(".bookTitle") %>%
    html_attr("href") %>% paste("https://www.goodreads.com", ., sep="")
  published_year <- sapply(book_links, get_year)
  
  #save to a data frame
  books <- rbind(books, data.frame(title, author, published_year, score, stringsAsFactors = FALSE))
  print(paste("Page:", page_nr))
}

#save to a file
write.csv(books, "books.csv")

#extract year of first publishing
books$published_year <-  str_extract(books$published_year, "(?<=first published).+" ) %>%
  str_extract(.,"(1|2)[0-9]{3}")

#tidy score column, remove ',', change to numeric
books$score <- str_extract(books$score, "(?<=score: ).+") %>%
  gsub(",", "", .) %>%
  as.numeric()


#read tidied data
books <- read.csv("bookss.csv")

#remove rows with missing values
books[complete.cases(books), ]


#plot year vs score ("A book’s total score is based on multiple factors, including the number of people who have voted for it and how highly those voters ranked the book.")
library(ggplot2)
library(scales)
ggplot(books, aes(published_year, score)) + geom_point() + scale_x_continuous(breaks = seq(1300, 2020, 100)) + scale_y_continuous(breaks = seq(0, 2500000,  300000), labels = comma)




