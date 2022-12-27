crime <- USArrests
library(tibble)
crime <- rownames_to_column(crime, var = "state")
crime$state <- tolower(crime$state)

install.packages("maps")
library(maps)

library(ggplot2)
states_map <- map_data("state")

install.packages("mapproj")
library(mapproj)
install.packages("ggiraphExtra")
library(ggiraphExtra)
ggChoropleth(data = crime, aes(fill = Assault, map_id = state), map = states_map)
ggChoropleth(data = crime, aes(fill = Assault, map_id = state), map = states_map, interactive = T)

install.packages("plotly")
library(plotly)
library(ggplot2)

p1 <- ggplot(data = mpg, aes(displ, highway, col = drv)) + geom_point()
ggplotly(p1)

p2 <- ggplot(data = mpg, aes(class, fill = class)) + geom_bar() + coord_flip()
ggplotly(p2)

p3 <- ggplot(data = mpg, aes(class, fill = fuel)) + geom_bar(position = "dodge")
ggplotly(p3)

head(diamonds)
str(diamonds)

p4 <- ggplot(data = diamonds, aes(cut, fill = clarity)) + geom_bar(position = "dodge")
ggplotly(p4)

p5 <- ggplot(data = diamonds, aes(cut, fill = color)) + geom_bar(position = "dodge")
ggplotly(p5)

install.packages("dygraphs")
library(dygraphs)
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
dygraph(eco) %>% dyRangeSelector()

eco_a <- xts(corona19$total_cases, order.by = corona19$date)
eco_b <- xts(corona19$total_vaccinations/100, order.by = corona19$date)
eco_c <- cbind(eco_a, eco_b)
colnames(eco_c) <- c("total_cases", "total_vaccinations")
dygraph(eco_c) %>% dyRangeSelector()
