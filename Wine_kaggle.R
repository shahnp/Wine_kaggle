
# Load data
wine <- read.csv('wine_kaggle.csv', stringsAsFactors = FALSE, encoding = 'UTF-8')

# library
library(dplyr)
library(ggplot2)

# Remove the first and third column
wine <- wine[ , -c(1,3)]
View(wine)

# Arrange
# top 10 countries
wine %>%
  group_by(country) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# top_n func and grouping top 10 countries 

selected_countries <- wine %>%
  group_by(country) %>%
  summarize (count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10) %>%
  select(country)

selected_countries

# created as dataframe
class(selected_countries)

# overwriting the dataaframe
selected_countries <- as.character(selected_countries$country)
class(selected_countries)

# 
select_point <- wine %>%
  filter(country %in% selected_countries) %>%
  select(country, points) %>%
  arrange(country)
select_point

# To see if expensive wines are above the point.

ggplot(wine, aes(points, price)) +
  geom_point() + # scatterplot
  geom_smooth()  # Add a smooth line

# series of box-plot
ggplot(select_point, aes( x= reorder(country, points, median),
                          y = points)) +
  geom_boxplot(aes(fill= country))+ # boxplot filing color by country
xlab("Country")+
  ylab("Prices")+
  ggtitle("Distribution of top Ten Wine Producing Countries") +
  theme(plot.title = element_text(hjust = .5))

# not top rated wine producing countries but have high points.

wine %>%
  filter(!(country %in% selected_countries)) %>%
  group_by(country) %>%
  summarise(median = median(points)) %>%
  arrange(desc(median)) # we want top rated wine to come first

# Intresect function

top <- wine %>%
  group_by(country) %>%
  summarise(median = median(points)) %>%
  arrange(desc(median))
top
class(top)
top <- as.character(top$country)
class(top)

# Part 3 video is about intersect  and setdiff

# Intersect func

both = intersect(top, selected_countries)
both


top = top[1:10]

both = intersect(top, selected_countries)
both

# set difference

not = setdiff(top, selected_countries)
not

topwine <- wine%>%
  group_by(variety) %>%
  summarise(number = n()) %>%
  arrange(desc(number)) %>%
  top_n(10)
topwine

topwine <- as.character(topwine$variety)
class(topwine)
topwine

wine%>%
  filter(variety %in% topwine) %>%
  group_by(variety) %>%
  summarise(median = median(points)) %>%
  ggplot(aes(reorder(variety, median), median)) +
  geom_col(aes(fill = variety)) +
  xlab("Variety") +
  ylab("Media Points") +
  scale_x_discrete(labels = abbreviate)

# Top 15 percent of wine
top15p <- wine %>%
  arrange(desc(points)) %>%
  filter(points > quantile(points, prob = .85))

top15p
class(top15p)

glimpse(top15p)

# cheap 15 Percent
# do intersect and see if top 15 % wine are any cheapest 15 %

cheapest15p <- wine %>%
  arrange(price) %>%
  head(nrow(top15p))

glimpse(cheapest15_p)

cheapest15p
class(cheapest15p)

goodvalue <- intersect(top15p, cheapest15p)
goodvalue
#



top15_p <- wine %>% arrange(desc(points)) %>% filter(points > quantile(points, prob= .85))
cheapest15_p <- wine %>% arrange(price) %>% head(nrow(top15_p))

goodvalue_wine <- intersect(top15_p,cheapest15_p)
goodvalue_wine

# Video 4


# Feature Engineering


#points to price  rational

wine %>% mutate(PPratio = points/price)
wine


