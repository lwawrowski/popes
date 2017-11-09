library(tidyverse)
library(rvest)

wiki <- "https://pl.wikipedia.org/wiki/Poczet_papie%C5%BCy"

tmp <- wiki %>%
  read_html %>%
  html_nodes("table")

listPopes <- list()

for(i in c(1:19,21:22)){
  name <- paste0("tab",i)
  a <- html_table(tmp[[i]])[-1,]
  listPopes[[name]] <- a
}

popesNames <- NULL

for(i in 1:21){
  if(i < 12){
    names <- listPopes[[i]][,2]
    popesNames <- c(popesNames, names)
  } else{
    names <- listPopes[[i]][,3]
    popesNames <- c(popesNames, names)
  }
}

popesNames.df <- as.data.frame(popesNames)

popesNames_filter <- popesNames.df %>%
  # wyrzucenie antypapieży
  filter(!grepl("Antypapież",popesNames)) %>%
  # usunięcie OSB
  mutate(popesNames=gsub("OSB", "", popesNames),
         # usunięcie przypisów
         popesNames=gsub("\\[.*", "", popesNames),
         # usunięcie wyrażenia "po raz ..."
         popesNames=gsub("Po raz.*", "", popesNames),
         # Jan Paweł jako jedno imię
         popesNames=gsub("Jan Paweł", "JanPaweł", popesNames)) %>%
  distinct() %>%
  separate(popesNames, into = c("name", "number", "adds"), remove = F)

namesFreq <- popesNames_filter %>%
  count(name)

filter(namesFreq, n > 4) %>%
  ggplot(., aes(x=reorder(name, -n), y=n)) + geom_col() + coord_flip() +
  xlab("Wybrane imię") + ylab("")

  
