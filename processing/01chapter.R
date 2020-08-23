### Graficos para Entrada 1 Pagina web

#######0. Cargar elementos básicos

#Cargar paquetes 

if (!require("pacman"))install.packages("pacman")

#Manipulacion general

pacman::p_load("tidyverse",
               "dplyr",
               "ggplot2",
               "haven",
               "forcats",
               "ggsci",
               "ggpubr",#corr
               "ggpol", #piramide
               "gdata",
               "hrbrthemes",
               "gganimate", "broom", "plotly", "magick",
               "png",
               "gifski",
               "dygraphs") #interactive 

#Manipulacion de mapas
pacman::p_load("sf","raster","spData","spDataLarge",
               "rgdal","leaflet", "htmlwidgets", "tmap", "mapview", "shiny", "maps")

#Series temporales
pacman::p_load("ggfortify", #series temporales
    "changepoint",#Identify shifts in mean and/or variance in a time series
    "strucchange", #Detect jumps in a data
    "ggpmisc") # Detect peaks stat_peaks() and valleys stat_valleys()
 

#Cargar base de datos
df <- readxl::read_excel("Base.xlsx", sheet = 1)

###1. Graficos requeridos
#En funcion de los graficos producidos(sintaxis "Graficos Sindicalizacion.R"), se modificarán 
# Mapa 1 (linea 243) --> versión mapa pero también líneas e interactivo
# Grafico 1 (linea 160, grafico 20) --> gift, pero versión separada entre feminizados y no
#Grafico 2 (linea 415, grafico 22.1) --> solo con Chile e interactivo

# #Argentina
# Australia
# Austria
# Belgium
# Brazil
# Canada
# Chile
# Croatia
# Cyprus
# Czech Republic
# Denmark
# Estonia
# Finland
# France
# Germany
# Greece
# Hungary
# Iceland
# Ireland
# Israel
# Italy
# Japan
# Latvia
# Lithuania
# Luxembourg
# Malaysia
# Malta
# Mexico
# Netherlands
# New Zealand
# Norway
# Poland
# Portugal
# Russian Federation
# Slovakia
# Slovenia
# South Africa
# South Korea
# Spain
# Sweden
# Switzerland
# Turkey
# United Kingdom
# United States
# Uruguay

#2. Filtrar datos

names(df)

df <- df %>% filter(country %in% c("Argentina",
                                "Australia",
                                "Austria",
                                "Belgium",
                                "Brazil",
                                "Canada",
                                "Chile",
                                "Croatia",
                                "Cyprus",
                                "Czech Republic",
                                "Denmark",
                                "Estonia",
                                "Finland",
                                "France",
                                "Germany",
                                "Greece",
                                "Hungary",
                                "Iceland",
                                "Ireland",
                                "Israel",
                                "Italy",
                                "Japan",
                                "Latvia",
                                "Lithuania",
                                "Luxembourg",
                                "Malaysia",
                                "Malta",
                                "Mexico",
                                "Netherlands",
                                "New Zealand",
                                "Norway",
                                "Poland",
                                "Portugal",
                                "Russian Federation",
                                "Slovakia",
                                "Slovenia",
                                "South Africa",
                                "South Korea",
                                "Spain",
                                "Sweden",
                                "Switzerland",
                                "Turkey",
                                "United Kingdom",
                                "United States",
                                "Uruguay")) 

df <- df %>% dplyr::select(country, year,ud, ud_fem, ud_male, gender_wage_gap, labor_force_participation_rate_fem, part_time_employ_fem)

##### Mapa 1 ######
#1. Manipulacion datos
#Seleccionar último dato de sindicalización por país
#Me di cuenta que tendré que imputar datos antes (paquete tidyr, funcion fill)
df1 <- tidyr::fill(df, ud)

df1<- df1 %>% filter(!is.na(ud_fem))%>% 
  group_by(country) %>%
  filter(year == max(year)) %>% dplyr::select(country,year,ud,ud_fem, ud_male)

#Crear indicador ratio
df1 <-  mutate(df1, ratioudsex =  
               ud_fem / ud_male)

#2. Cargar mapa
#Make a Spdf object (spatial polygon data frame).

my_map <- shapefile("C:/Users/Valentina Andrade/Documents/2. Trabajo/FONDECYT/Analisis Cuantitativo/Densidad sindical/mapa/TM_WORLD_BORDERS_SIMPL-0.3.shp")

names(my_map)

#Rename Korea, Republic of
table(my_map$NAME)
table(df1$country)
my_map$NAME <- gsub("Korea, Republic of", "South Korea", my_map$NAME)
my_map$NAME <- gsub("United States Minor Outlying Islands", "United States", my_map$NAME)
my_map$NAME <- gsub("United States Virgin Islands", "United States", my_map$NAME)

df1$country <- gsub("Russian Federation", "Russia", df1$country)

#Join map and data
map <- merge(my_map, df1, by.x = "NAME", by.y = "country")

table(map@data$ud_fem)

#map is the new data

# 3. Create a color palette with handmade bins (RColorBrewer)

mybins <- c(0,0.5,0.7,0.9, 1, 1.3,1.4, Inf)

mypalette <- colorBin( palette=c("#0099CC", "white","magenta4"), domain=map@data$ratioudsex, na.color="transparent", bins=mybins)

summary(map@data$ratioudsex)

# Prepare the text for tooltips:
mytext <- paste(
  "<b>", map@data$NAME,"</b>","<br/>", 
  "Union Density: ", round(map@data$ud,2),"%", "<br/>", 
  "Female union density: ", map@data$ud_fem,"%", "<br/>",
  "Male union density: ", map@data$ud_male,"%", "<br/>",
  "Feminization: ", round(map@data$ratioudsex, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

##Para el futuro
#Si quiero linkear algo:
# content <- paste(sep = "<br/>",
#                  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
#                  "606 5th Ave. S",
#                  "Seattle, WA 98138")


# Final Map
map2 <- leaflet(map) %>% 
  addTiles(providers$Stamen.TonerLite)  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(ratioudsex), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="black", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~ratioudsex, opacity=0.9, title = "Feminization", position = "bottomleft" )

map2

#Guardar
saveWidget(map2, file=paste0(getwd(), "Andrade.uniondensity.html"))  


############ Grafico 1 ###############
#Gif 1 feminizadas y gif 2 masc

#1. Manipular datos 
#usar df

df2 <- df %>% filter(!is.na(ud_fem)) %>%
  dplyr::select(year, country, ud, ud_fem, ud_male)

# df2 <- df2 %>% group_by(country, max(year)) %>% 
#   mutate(femin=ifelse(test = (ud_fem>ud_male),yes = 1,no = 0))

df2 <- df2 %>% gather(key = "sexud", value = "rate", -ud, -country, -year)

df2  <- mutate(df2, sex = ifelse(sexud %in% c("ud_fem"), "Female", "Male"))


df2$femin <- NA

df2  <- mutate(df2, femin = ifelse(country %in% c("Australia",
                                                  "Austria",
                                                  "Brazil",
                                                  "Canada",
                                                  "Chile",
                                                  "Croatia",
                                                  "Czech Republic",
                                                  "Denmark",
                                                  "Estonia",
                                                  "Finland",
                                                  "Hungary",
                                                  "Iceland",
                                                  "Ireland",
                                                  "Israel",
                                                  "Latvia",
                                                  "Lithuania",
                                                  "Malaysia",
                                                  "Mexico",
                                                  "New Zealand",
                                                  "Norway",
                                                  "Poland",
                                                  "Russian Federation",
                                                  "Slovenia",
                                                  "South Africa",
                                                  "Sweden",
                                                  "United Kingdom",
                                                  "United States",
                                                  "Uruguay"), 1, 0))

df2$country <- gsub("Russian Federation", "Russia", df2$country)

feminizado <- df2 %>% filter(femin == 1)
masculinizado <- df2 %>% filter(femin == 0)

table(df2$year)

# Grafico 1.1

grafico1.1  <- ggplot(feminizado, aes( x= round(year, digits= 0), y = rate, group = sex, colour = sex)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="Densidad sindical", breaks=c("Female", "Male"),
                      labels=c("Femenina", "Masculina"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Año", y = "Densidad sindical") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0), axis.text.x = element_text(size = 5))

grafico1.1

ggsave(
  plot = grafico1.1,
  filename = "grafico1.1.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

grafico1.1gif <- grafico1.1 + gganimate::transition_reveal(year) + labs(title = "", caption = "source: Valentina Andrade's elaboration")
grafico1.1gif

#Guardar
anim_save(plot = grafico1.1gif, filename = "gif1.1.gif", animation = last_animation(), path = NULL, width = 5000, height = 1000)


# Grafico 1.2

grafico1.2  <- ggplot(masculinizado, aes( x= round(year, digits= 0), y = rate, group = sex, colour = sex)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="Densidad sindical", breaks=c("Female", "Male"),
                      labels=c("Femenina", "Masculina"), values = c(Female = "magenta4", Male = "#0099CC")) + 
  theme_classic()+
  labs(x = "Año", y = "Densidad sindical") + 
  facet_wrap(~country, scales = "free") + 
  theme(legend.justification=c(1,0), legend.position=c(1,0), axis.text.x = element_text(size = 5))

grafico1.2

ggsave(
  plot = grafico1.2,
  filename = "grafico1.2.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

gif1.2 <- grafico1.2 + gganimate::transition_reveal(year) + labs(title = "", caption = "source: Valentina Andrade's elaboration")
gif1.2

#Guardar
anim_save(plot = gif1.2, filename = "gif1.2.gif", animation = last_animation(), path = NULL, width = 5000, height = 1000)


##################### Grafico 2 #################################
#### Grafico brecha salarial, ud

#0. Manipular y filtrar
df3 <- df %>% tidyr::fill(ud_fem) %>% tidyr::fill(ud_male) %>% tidyr::fill(gender_wage_gap)%>% tidyr::fill(labor_force_participation_rate_fem) 
df3 <- df3 %>% filter( country ==  "Chile", year >= 1998, year <=2018 ) %>% dplyr::select(country, year, ud_fem, ud_male, labor_force_participation_rate_fem, gender_wage_gap)  %>% 
  gather(key = "employmentkey", value = "value", -year, -country)
  # filter(employmentkey %in% c("gender_wage_gap","labor_force_participation_rate_fem", "ud_fem", "ud_male" ), !is.na(value)) 

# Grafico 
grafico2A <- ggplot(df3, aes(x= year, y = value, color = employmentkey,
                       group = employmentkey)) + geom_line(size = 1) +
  scale_color_manual(values = c("brown1","seagreen4","magenta4", "#0099CC"), name = "",
                     breaks = c("gender_wage_gap","labor_force_participation_rate_fem", "ud_fem", "ud_male"),
                     labels = c("Brecha salarial de género","Participación laboral femenina","Sindicalización femenina", "Sindicalización masculina")) + theme_classic()+
  geom_vline(data = filter(df3, country=="Chile"), aes(xintercept = 2016), linetype = "dashed") + 
  labs( y = "%", x = "Año" ) + theme(legend.position = "bottom") + scale_x_continuous(limits = c(1998,2018))

grafico2A

#Guardar grafico no iinteractivo

ggsave(
  plot = grafico2A,
  filename = "grafico2A.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 30,
  height = 20
)

#Gráfico interactivo básico

ggplotly(grafico2A, type = 'scatter', mode = 'lines', tooltip = c("value"))


###Manipulacion para interactivo

df4 <- df %>% tidyr::fill(ud_fem) %>% tidyr::fill(ud_male) %>% tidyr::fill(gender_wage_gap)%>% tidyr::fill(labor_force_participation_rate_fem) 
df4 <- df4 %>% filter( country ==  "Chile", year >= 1998, year <=2018 ) %>% 
  dplyr::select(country, year, ud_fem, ud_male, labor_force_participation_rate_fem, gender_wage_gap)  
#%>%filter(employmentkey %in% c("gender_wage_gap","labor_force_participation_rate_fem", "ud_fem", "ud_male" ), !is.na(value)) 

##Grafico interactivo con paqueteplotly
  
grafico2B <- plot_ly(df4, x = ~year, y = ~ud_fem, name = 'Sindicalización femenina', type = 'scatter', mode = 'lines', line = list(color = 'rgb(204, 0, 204)'),
             hoverinfo = 'text',
             text = ~paste(
               "<b>", round(df4$ud_fem,2),"%","</b>", 
               "(", df4$year,")",
               sep="")) %>% 
  add_trace(y = ~ud_male, name = 'Sindicalización masculina', mode = 'lines', line = list(color = 'rgb(0, 153, 204)'), 
            hoverinfo = 'text',
            text = ~paste(
              "<b>", round(df4$ud_male,2),"%","</b>", 
              "(", df4$year,")",
              sep="")) %>%
  add_trace(y = ~labor_force_participation_rate_fem, name = 'Participación laboral femenina', mode = 'lines',  line = list(color = 'rgb(0, 205, 143)'),
            hoverinfo = 'text',
            text = ~paste(
              "<b>", round(df4$labor_force_participation_rate_fem,2),"%","</b>", 
              "(", df4$year,")",
              sep="")) %>%
  add_trace(y = ~gender_wage_gap, name = 'Brecha salarial de género', mode = 'lines',  line = list(color = 'rgb(255, 102, 102)'),
            hoverinfo = 'text',
            text = ~paste(
              "<b>", round(df4$gender_wage_gap,2),"%","</b>", 
              "(", df4$year,")",
              sep="")) %>%
  layout(title = "",
         xaxis = list(title = "Año"),
         yaxis = list (title = "%"), legend = list(y = -50, x = 2000, orientation = 'h'), shapes = change) 

grafico2B
