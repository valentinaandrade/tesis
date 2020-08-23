# Chapter 2 ----
# Valentina Andrade

#1. Packages
pacman::p_load(dplyr, ggplot2,
               ggsci, tidyr, WDI, plotly,
               RColorBrewer, htmlwidgets,
               kableExtra)

#0. Theme
theme_set(theme_classic() + theme(axis.title =  element_text(size = 14),
                                  axis.text = element_text(size =10),
                                  legend.text = element_text(size =14)))

#lead() next and lag() past; first() el primero
options(kableExtra.html.bsTable = T)
options(knitr.kable.NA = '')

#2. Cargar bases de datos
load(file = "../data/db-proc.RData")

# 3. Explorar base
names(db)

# 4. Figures

# Figure 2.1  ------------
# Densidad sindical en países OCDE ultimo año
g <- db %>% filter(!is.na(UD), ocde =="OCDE") %>% group_by(country) %>% filter(year == max(year)) %>% 
  mutate(n_ud = ifelse(UD <=30, "Bajo","Alto")) %>% 
  ggplot(aes(x= reorder(country, UD), y = UD, fill =UD,
             text = paste("País:", country, "</br>Año:", year))) + 
  geom_bar(stat="identity", color = "black") + scale_fill_viridis_c(name = "") + geom_hline(aes(yintercept = 35.6), linetype = "dashed", color = "gray40") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  labs(x= "", y = "Densidad sindical", caption = "Fuente: Elaboración propia en base a ICTWSS (2019), con UD último año. Linea indica promedio") + theme(legend.position ="none",axis.text.x= element_text(angle = 40))

subplot(
  with_options(list(digits = 2), ggplotly(g, tooltip = c("text", "y"), titleX = T)))


# Figure 2.2-------------
#Densidad sindical por contiente 1960 a 2020
g  <- db %>% select(UD, year, country, continent) %>% filter(!is.na(UD), continent  !="África") %>% 
  ggplot(aes( x= year, y=  UD, color = country))  + geom_line() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  facet_wrap(.~continent) +
  theme(legend.position='none') +  
  labs(y =" Densidad sindical", x ="", caption= "Fuente: Elaboración propia en base a ICTWSS (2019)(*)")

p <- ggplotly(g) 
p
htmlwidgets::saveWidget(as_widget(p), file = "figura2.0.html")

#Save
ggsave(plot = g,
       filename = "codes/images/figure2.0.png",device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)


# Figure 2.3 -------------

# Figure 2.4 -----------------

# Figure 2.5 ----------------------


# Figure 2.6 -------
# Union density and sectors
new_wdi_cache <- WDIcache() 
WDIsearch("industry.*employment.", cache = new_wdi_cache)

wdi_dat <- WDI(indicator = c("SL.SRV.EMPL.ZS
", "SL.AGR.EMPL.ZS", "SL.IND.EMPL.ZS"), start = 1960, end = 2019, extra = TRUE)

names(wdi_dat)

# WDI() returns a data frame in wide format. Rows are per year and country/region, columns are the different indicators.
# wb() returns a data frame in long format. Each row is one observation

wdi_dat <- wdi_dat %>% select(country, year, iso3c, iso2c,
                              region, ser_empl="SL.SRV.EMPL.ZS\n",
                              ind_empl="SL.IND.EMPL.ZS",
                              agr_empl="SL.AGR.EMPL.ZS", income)

fig2 <- wdi_dat %>% filter(country %in% c("World", "Latin America & Caribbean", "Europe & Central Asia", "East Asia & Pacific", "Middle East & North Africa","North America")) %>% 
  gather(key = "sector", value = "percent", -country, -year, -iso3c, -iso2c, -region, -income) %>%  filter(!is.na(percent))

fig2$country <- as.factor(fig2$country)
levels(fig2$country) <- c("Asia del Este y Pacífico", "Europa y Asia Central","Latinoamérica", "África", "Norteamérica",  "Mundo")

ggplot(fig2, aes(x = year, y = percent)) + 
  geom_line(aes(color = sector), size = 1) + 
  scale_color_brewer(palette = "Dark2",name = "", labels =c("Agricultura", "Industria", "Servicios")) +
  theme(legend.position ="bottom") + facet_wrap(.~country, scales = "free")+ 
  labs(y ="%", x ="", caption= "Fuente: Elaboración propia en base a Banco Mundial (2020) en base a estimaciones de ILO (2019)")

ggsave(plot = last_plot(),
       filename = "codes/images/figure2.5.png",device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)


# Union density by sex and
# Wage and salaried workers (employees) are those workers who hold the type of jobs defined as "paid employment jobs," where the incumbents hold explicit (written or oral) or implicit employment contracts that give them a basic remuneration that is not directly dependent upon the revenue of the unit for which they work.

WDIsearch("part.time*", cache = new_wdi_cache)

wdi_dat <- WDI(indicator = c("SL.EMP.WORK.FE.ZS
", "SL.EMP.WORK.MA.ZS", "SL.TLF.PART.ZS", "SL.TLF.PART.FE.ZS", "SL.TLF.PART.MA.ZS"), start = 1960, end = 2019, extra = TRUE)


db %>% select(year, LFPR_WOMEN, LFPR_MEN,LFPR_MW) %>%   
  group_by(year) %>% do(add_row(., year = unique(.$year),
                                LFPR_MW = mean(.$LFPR_MW, na.rm = T),
                                LFPR_WOMEN = mean(.$LFPR_WOMEN, na.rm = T),
                                LFPR_MEN = mean(.$LFPR_MEN, na.rm = T))) %>% filter(is.na(country)) %>%
  gather(key = "lf", value = "percent", -year, -country) %>%
  filter(!is.na(percent)) %>% 
  ggplot(aes(x = year, y = percent)) + 
  geom_line(aes(color = lf), size = 1) + 
  geom_hline(aes(yintercept = 50), linetype = "dashed", color = "gray40")+
  scale_color_brewer(palette = "Dark2",name = "", labels = c("Hombres", "Total", "Mujeres")) +
  theme(legend.position ="bottom") + 
  labs(y ="%", x ="", caption= "Fuente: Elaboración propia en base a OCDE (2020).Wage and salaried workers by sex")

ggsave(plot = last_plot(),
       filename = "codes/images/figure2.6.png",device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)

# Figura 2.7 ----------------

db %>% select(year, PT_MW, PT_MEN,PT_WOMEN) %>%   
  group_by(year) %>% do(add_row(., year = unique(.$year),
                                PT_MW = mean(.$PT_MW, na.rm = T),
                                PT_MEN = mean(.$PT_MEN, na.rm = T),
                                PT_WOMEN = mean(.$PT_WOMEN, na.rm = T))) %>% filter(is.na(country)) %>%
  gather(key = "pt", value = "percent", -year, -country) %>%
  filter(!is.na(percent)) %>% 
  ggplot(aes(x = year, y = percent)) + 
  geom_line(aes(color = pt), size = 1) + scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_brewer(palette = "Dark2",name = "", breaks = c("PT_MW", "PT_MEN","PT_WOMEN"),labels = c( "Total","Hombres", "Mujeres")) +
  theme(legend.position ="bottom") + 
  labs(y ="Empleo parcial", x ="", caption= "Fuente: Elaboración propia en base a OCDE (2020).Wage and salaried workers by sex")

ggsave(plot = last_plot(),
       filename = "codes/images/figure2.7.png",device = "png",
       dpi = "retina",units = "cm",
       width = 25,height = 15)


## Table2.1 -----------
table <- db %>% group_by(country, year) %>% 
  do(add_row(., country = unique(.$country),
             year = unique(.$year),
             growth_ud = mean(.$growth_ud, na.rm = T))) %>%
  mutate(growth = ifelse(growth_ud < 0, "Decenso","Aumento")) %>% select(country, growth_ud, growth) %>% filter(!is.na(growth_ud), !is.na(growth)) %>%  unique()

# Paises que ha disminuido y aumentado sindicalizacion (ultimos 20 anos)
table %>% filter(year > 2000)%>% group_by(country) %>% summarise(mean = mean(growth_ud)) %>%
  mutate(growth = ifelse(mean < 0, "Decenso","Aumento")) %>% unique() %>% group_by(growth) %>% summarise(n())


table %>% group_by(country)  %>%
  filter(n()>56)%>% summarise(mean = mean(growth_ud)) %>%
  mutate(growth = ifelse(mean < 0, "Decenso","Aumento")) %>% unique() %>% group_by(growth) %>% summarise(n())

# Manipular y crear tabla
table1 <- table %>% 
  mutate(periodo=case_when(year %in% c(1961:1979)~1,
                           year %in% c(1980:1999)~2,
                           year %in% c(2000:2020)~3)) %>% 
  group_by(periodo,country) %>% summarise(growth_ud=mean(growth_ud,na.rm = TRUE)) %>% 
  spread(periodo,growth_ud)


table1 <-knitr::kable(table1, digits = 3,  booktabs = T,
                      col.names = c("País", "60-70", "80-90", "2000-hoy")) %>%
  add_header_above(c(" ", "Décadas" = 3)) %>% 
  kableExtra::kable_styling(latex_options = "hold_position")%>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)%>%
  kableExtra::column_spec(1, width = "9cm") %>%
  kableExtra::footnote(general = "Fuente: Elaboración propia en base a ICTWSS (2019)",general_title = "")

save_kable(table1, file ="codes/images/tabla2.1.png")


## Figure 2.9 -----------------------

g <- db  %>% fill(rmw, .direction = "downup") %>% filter(!is.na(UD)) %>% 
  group_by(country)  %>% filter(year == max(year)) %>% arrange(desc(UD))%>% 
  ggplot(aes(rmw, UD, size = UD, fill= voc, 
             text = paste("País:", country, "</br>Año:", year))) +
  geom_point(alpha=0.5, shape=21, color="black")  +
  scale_fill_viridis_d(guide=FALSE, option="A") + 
  scale_size(range = c(.1, 10))  + labs(x = "US $ Purchasing Power Parities (PPPs)") + scale_x_continuous(labels = function(x) paste0(x, "USD")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,55)) 

subplot(with_options(list(digits = 2), ggplotly(g, tooltip = c("text", "y"), titleX = T)))

##  Nota: RMW are in constant prices at last year USD PPPs

