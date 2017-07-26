# R-analyst-template R template
# library(dplyr)
# library(ggplot2)
# library(plotly)

movie <- read.csv('../input/movie_metadata.csv',header=T,stringsAsFactors = F)
str(movie)
dim(movie)

#折线图
temp <- movie %>% select(imdb_score,title_year)
temp <- temp %>% group_by(title_year)%>% summarise(score=mean(imdb_score))
temp <- na.omit(temp)
p <- plot_ly(temp, x = title_year, y = score, name = "Avg Score by Year")
p %>%
  add_trace(y = fitted(loess(score ~ as.numeric(title_year))), x = title_year) %>%
  layout(title = "Year and Score",
         showlegend = FALSE) %>%
  dplyr::filter(score == max(score)) %>%
  layout(annotations = list(x = title_year, y = score, text = "Peak", showarrow = T))
  
  #柱状分布图
temp <- movie %>% select(content_rating,imdb_score)
temp <- temp %>% group_by(content_rating)%>% summarise(score = mean(imdb_score))
p <- plot_ly(
  x = temp$content_rating,
  y = temp$score,
  name = "Avg score by Rating",
  type = "bar")
p

# leaf tree
temp <- movie %>% select(imdb_score,content_rating)
temp <- na.omit(temp)
plot_ly(temp, x = imdb_score, color = content_rating, type = "box")

#rating
temp <- movie %>% select(director_name,imdb_score)
temp <- temp %>% group_by(director_name) %>% summarise(avg=mean(imdb_score))
temp <- temp %>% arrange(desc(avg))
temp <- temp[1:20,]
temp %>%
  formattable(list(avg = color_bar("orange")), align = 'l')
  
#commone plot
plot_ly(temp, x = director_facebook_likes, y = imdb_score,
        color =content_rating , mode = "markers",text=paste('Movie:',movie_title))
