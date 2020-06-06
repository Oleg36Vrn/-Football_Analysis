Regions <- readShapePoly("gadm36_BLR_1.shp")
plot(Regions)
slotNames(Regions)
Regions@data
Regions@data$NAME_1
spplot(Regions,
       "NAME_1", # отображаемая переменная
       scales = list(draw = T), # опция, включающая отображение координатных осей
       col.regions = terrain.colors(n = 6) ) # опция, задающая заливку цветом
spplot(Regions, "NAME_1",
       col.regions = brewer.pal(6, "Set3"), 
       par.settings = list(axis.line = list(col = NA)))
Regions@data$Population = c(1188, 1814, 1352,
                            2309, 6913, 1700)
mypalette <- colorRampPalette(c("seagreen", "whitesmoke"))
spplot(Regions, "Population",
       col.regions = mypalette(20), # определение цветовой шкалы
       col = "transparent", # отключение контурных линий на карте
       par.settings = list(axis.line = list(col = NA)))
spplot(Regions,
       "Population", # отображаемая переменная
       scales = list(draw = T), # опция, включающая отображение координатных осей
       col.regions = terrain.colors(n = 20) ) # опция, задающая заливку цветом
plotcol <- brewer.pal(6,"RdBu")
spplot(Regions, "Population",
       col.regions = plotcol, # определение цветовой шкалы
       at = brks.eq$brks, # задает границы классов
       par.settings = list(axis.line = list(col = NA)))
require(classInt)
brks.eq = classIntervals(Regions$Population, n = 6, style = "equal")
brks.eq
spplot(Regions, "Population",
       main = 'Забито голов на стадионах субъектов',
       col.regions = plotcol,
       at = brks.eq$brks, # задает границы классов
       par.settings = list(axis.line = list(col = NA)))
install.packages("ggplot2", "readxl", "scales", "dplyr", "Cairo")
Sys.setlocale("LC_ALL", "ru_RU.UTF-8")
df_logging <- read_excel("Belarus.xlsx", sheet ="Dynamic")
df_logging@value
df_logging$value
df_logging$value*100
df_logging
df_logging <- df_logging$value*100
df_logging$value <- df_logging$value*100
ggplot(data=df_logging, aes(x=year, y=value)) +
  geom_line(aes(linetype=location))
y
ggplot(data=df_logging, aes(x=year, y=value)) +
  geom_line(aes(linetype=location)) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(legend.title = element_blank(), legend.position="bottom", legend.spacing.x = unit(0.5, "lines")) +
  labs(x = "", y = "Удельный вес голов в Высшей лиге, %", color="red")
ggplot(data=df_logging, aes(x=year, y=value)) +
  geom_line(aes(linetype=location)) +
  geom_point(size=1) +
  theme_classic(base_family = "PT Sans", base_size = 12) +
  theme(legend.title = element_blank(), legend.position="bottom", legend.spacing.x = unit(0.5, "lines")) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "solid")) +
  labs(x = "", y = "Удельный вес голов в Высшей лиге, %", color="")
hist(df_logging$value)
barplot(1185:1811:1312:2305:6949:1708)
goals <- read_excel("Belarus.xlsx", sheet ="All Goals")
barplot(goals$value)
barplot(goals$value, names.arg = goals$location, horiz = TRUE, las = 1)
margins.default = par("mar")
par(mar = c(5, 7, 4, 2)) # увеличим поле left до 10 условных единиц
barplot(goals$value, names.arg = goals$location, horiz = TRUE, las = 1)
barplot(goals$value, 
        names.arg = goals$location, 
        main = "Забито голов в Высшей лиге", 
        xlab = "", 
        horiz = TRUE, 
        las = 1,
        xlim = c(0,7000), 
        col = green.transp)
green.transp = rgb(0, 1, 0, 0.5) # появился четвертый параметр
geolocation <- read_excel("Belarus.xlsx", sheet ="Coordinates city")
sf_geolocation = st_as_sf(geolocation, coords = c("lon", "lat"), crs = 4326)
plot(st_geometry(sf_geolocation), pch = 19, col = 'red', cex = 0.25)
plot(st_geometry(Regions), border = 'grey', add = TRUE)
city = read_excel("Belarus.xlsx", sheet ="Coordinates city", 
                                                col_names = c('City', 'lat', 'lon')),
                     locale = locale(encoding = 'CP1251')
minsk.sfg = st_point(c(27.340, 53.536))
brest.sfg = st_point(c(23.411, 52.055))
cities.sfc = st_sfc(minsk.sfg, brest.sfg)
print(cities.sfc)
st_crs(cities.sfc) = st_crs(4326) # WGS84
print(cities.sfc)
plot(cities.sfc, pch = 19)
  filter(sovereignt == 'Belarus') %>% 
  st_geometry() %>%
  plot(add = TRUE)
Regions1 <- as.vector(Regions)
library(tidyverse)
library(gganimate)
Dynamic <- read_excel("Belarus.xlsx", sheet ="Dynamic")
Dynamic_formatted <- Dynamic %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  group_by(location) %>%
  filter(rank <=6) %>%
  ungroup()
staticplot = ggplot(Dynamic_formatted, aes(rank, group = location,
                                       fill = as.factor(location), color = as.factor(location))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(location, " ")), vjust = 0.2, hjust = "top", size = 9) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0, size = 60)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=32, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=24, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=24, hjust=0.5, face="bold.italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(4,4, 4, 6, "cm"))
anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Удельный вес голов субъекта за год,% : {closest_state}', 
       subtitle  =  "Высшая лига Беларуси",
       caption  = "Равновесие Нэша | Data Source: Sports.ru")
# For GIF
animate(anim, 200, fps = 10,  width = 1200, height = 1000, duration = 30,
        renderer = gifski_renderer("gganim4.gif"))
animate(anim, 200, fps = 20,  width = 1200, height = 1000,
        renderer = ffmpeg_renderer()) -> for_mp4
anim_save("animation.mp4", animation = for_mp4 )
Dynamic_formatted$Value_lbl
Dynamic_formatted$Value_lbl <- Dynamic_formatted$value*100
format(round(1.292, 2), nsmall = 2)
Dynamic_formatted$Value_lbl <- formatC(Dynamic_formatted$Value_lbl, digits = 2, format = "f")
Value_lbl
margin()
cumul <- read_excel("Belarus.xlsx", sheet ="Cumulate")
ggplot(cumul, mapping = aes(x = year, y = sum)) + # а теперь и с точками
ggplot(data = cumul) +
geom_line(mapping = aes(x = year, y = sum) +
geom_point(mapping = aes(x = year, y = sum) +          
geom_text(aes(label = floor(sum))) +
ggtitle('Голов нарастающим итогом',
subtitle = 'Высшая лига Беларуси, 1992-2019')
abline(h = 1000, col = "blue", lwd = 3, lty = 2)
plot(cum1$year, 
     cum1$Брестская)
     col="red3")
cum1 <- read_excel("Belarus.xlsx", sheet ="Cum1")
plot(cum1$year, cum1$Brest, type="l")
plot(cum1$year, 
     cum1$Brest, 
     pch=20, 
     type="o", 
     ylim = c(0,2500), 
     col="red",
     main="Голов нарастающим итогом в Высшей лиге Беларуси, 1992-2019", 
     xlab="Год", 
     ylab="Голов")
points(cum1$year, cum1$Vitebsk, pch=20, col="forestgreen")
lines(cum1$year, cum1$Vitebsk, pch=20, col="forestgreen")
points(cum1$year, cum1$Homel, pch=20, col="orange")
lines(cum1$year, cum1$Homel, pch=20, col="orange")
points(cum1$year, cum1$Hrodno, pch=20, col="black")
lines(cum1$year, cum1$Hrodno, pch=20, col="black")
points(cum1$year, cum1$Mogilev, pch=20, col="purple")
lines(cum1$year, cum1$Mogilev, pch=20, col="purple")
xlines = seq(min(cum1$year), max(cum1$year), 1)
ylines = seq(ceiling(min(cum1$Brest)),
             floor(max(cum1$Brest)), 1)
abline(h = 1000, col = "blue", lwd = 3, lty = 2)
location <-Brest 
labels <- c('Brest','Vitebsk', 'Homel', 'Hrodno', 'Minsk', 'Mogilev')
location = "topright"
pts = c(20, 20, 20) # каждый элемент показывается точкой типа 20
lns = c(1, 1, 1) # каждый элемент показывается линией толщиной 1
main = 'Субъект'
cumul %>% 
  dplyr::filter(title == 'Голов накопленным итогом') %>% 
  ggplot(mapping = aes(x = location, y = value)) +
  geom_col() +
  ggtitle('Забито голов на стадионах субъекта',
          subtitle = 'Высшая лига Беларуси, 1992-2019') +
  theme_wsj(title_family = 'Times New Roman') +
  xlab('Субъект') +
  ylab('Число голов') +
  ylim (0, 400) +
  coord_flip() +
  facet_wrap(~year)
brks = c(0, 50, 100, 150, 200)
scale_y_continuous(breaks = brks * 1e3, labels = brks)
spplot(Regions, "Population",
        main = 'Забито голов на стадионах субъектов',
       col.regions = brewer.pal(6, "Set3"), # определение цветовой шкалы
       at = brks.eq$brks, # задает границы классов
       par.settings = list(axis.line = list(col = NA)))
