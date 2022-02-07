# Introduction to dplyr
library(dyplyr)
library(ggplot2)

# Usamos un dataset de paises productores de vinos para analizar
# Iniciamos con los paises con mas registros(que mas producen).
setwd("~/data_R/dplyr/wine.csv")
wine <- read.csv("wine.csv", stringAsFactors = FALSE, encoding = "UT-8")
wine <- wine[, -c(1, 3)]	
wine %>% 
	group_by(country) %>% 
	summarize(count = n()) %>% 
	arrange(desc(count))
selected_countries <- wine %>% 
	group_by(country) %>% 
	summarize(count = n()) %>%   
	s
	arrange(desc(count)) %>% 
	top_n(10) %>% 
	select(country)
selected_countries
selected_countries <- as.character(select_countries$country)

# Visualizamos los puntos(points) de los paises con mas registros
select_point <- wine %>% 
	filter(country %in% selected_countries) %>% 
	select(country, points) %>% 
	arrange(country)
select_point
ggplot(wine, aes(points, price)) + geom_point() + geom_smooth()
ggplot(select_point, aes(x = reorder(country, points, median), y = points)) + 
	geom_boxplot(aes(fill = country), show.legend = FALSE) + xlab("country") + ylab("points") +
	ggtitle("Distribution of Top Ten Wine Producing Countries") + 
	theme(plot.title = element_text(hjust = 0.5))

# Paises con mayores medianas, que no estan dentro de las 10 con mejores registros.
wine %>% 
	filter(!(country %in% selected_countries)) %>% 
	group_by(country) %>% 
	summarise(median = median(points)) %>% 
	arrange(desc(median))

# Countries with most median
top <- wine %>% 
	group_by(country) %>% 
	summarize(median = median(points)) %>% 
	arrange(desc(median))
top <- as.character(top$country)
top
top <- top[1:10]

# Introduction to dplyr. Intersect & Setdiff fuction
# Combining datasets
both <- intersect(top, selected_countries)
both
not <- setdiff(top, selected_countries)
not

# Agrupamos por Variedad de vino encontrando los mejores 10.
topwine <- wine %>%
	group_by(variety) %>%
	summarise(number = n()) %>%
	arrange(desc(number)) %>%
	top_n(10)
topwine <- as.character(topwine$variety)

wine %>%
	filter(variety %in% topwine) %>%
	group_by(variety) %>%
	summarise(mediana = median(points)) %>%
	ggplot(aes(reorder(variety, mediana), mediana)) +
	geom_col(aes(fill = variety)) + xlab("Variety") + ylab("Median Points") +
	scale_x_discrete(labels = abbreviate)

top15p <- wine %>%
	arrange(desc(points) %>%
	filter(points > quantile(points, prob = 0.85))