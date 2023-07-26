library(tidyverse)
library(extrafont)
library(scales)
library(showtext)
library(tint)
library(miniUI)
library(units)
library(reactable)
library(zoo)
library(lubridate)
library(wesanderson)
library(ggsci)
library(RColorBrewer)
library(directlabels)
#library('Cairo')
library(rcartocolor)
library(ggtext)
library(viridis)

setwd("C:/Users/luism/OneDrive/R/ENIGH")
concentrado <- read_csv("01Datos/SON_ING_DECIL.csv", 
                        locale = locale(encoding = "ISO-8859-1"))

concentrado <- concentrado %>% mutate(DECILES=factor(DECILES, ordered=TRUE, levels = c("I","II","III","IV", "V", "VI", "VII", "VIII", "IX", "X")))
ingresos <- ggplot(concentrado, aes(x= as.factor(DECILES), y= p2020)) +
  geom_col(aes (fill= as.factor(CL), color=as.factor(CL)), alpha=0.8, linetype= "solid", width = 0.8)+
  geom_text(data=subset(concentrado, CL=="A"),aes(label=scales::percent(p2020,0.1L)),position = position_dodge(width = 1),
            vjust = -0.5, size = 5, family="Lato Light") + 
  geom_text(data=subset(concentrado, CL=="B"),aes(label=scales::percent(p2020,0.1L)),position = position_dodge(width = 1),
            vjust = -0.5, size = 5, family="Lato Black") + 
  annotate("text", x="X", y=0.3, label="Una quinta parte de los hogares de Sonora \nconcentra 48.7% de los ingresos   ", size = 5, family="Lato", fontface="italic",
           hjust = 1.12, color= "#01A2AC") +
  
  scale_fill_manual( values=c("gray90", "#01A2AC"))  +
  scale_color_manual(values=c( "gray70", "#01787E"))  +
  #scale_x_continuous(breaks = seq(from = 2015, to = 2020, by = 1)) +
  scale_y_continuous (expand = c(0,0), limits = c(0,.4)) +
  theme_void() +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        plot.title = element_markdown(family = "Lato Black", size = 20,color = "black"), 
        plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"), axis.line.x = element_line(linetype = "solid"),
        axis.text.y = element_blank(), axis.text.x = element_text(family = "Lato", size =10,margin=margin(5,0,0,0)),
        axis.title.x = element_text(family = "Lato Light", size = 6, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), legend.position = "none",
        plot.background = element_rect(fill = "white", color = "black", size = 2.5),
        plot.caption = element_text(family = "Lato", size = 6, color = "black")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-weight: bold;'>Sonora en la ENIGH:</span><span style = 'color:#01A2AC;'> estructura de ingresos por decil de hogares</span>", 
       subtitle= "Ingreso del decil de hogares respecto al total estatal en 2020", caption ="Cada decil se refiere a una d?cima parte de los hogares de Sonora ordenados por ingreso.\nElaboraci?n Luis Armando Moreno (@dogomoreno) con informaci?n de ENIGH 2020, INEGI.\nwww.luisarmandomoreno.com")

ingresos
ggsave("ingresos2022.png",ingresos, bg = "transparent", height = 5, width = 5*(16/9), device = png)

ingresospesos <- ggplot(concentrado, aes(x= as.factor(DECILES), y= m2020)) +
  geom_col(aes (fill= as.factor(CL2), color=as.factor(CL2)), alpha=0.8, linetype= "solid", width = 0.8)+
  geom_text(data=subset(concentrado, CL2=="A"),aes(label=scales::comma(m2020,1L)),position = position_dodge(width = 1),
            vjust = -0.5, size =4, family="Lato Black") + 
  geom_text(data=subset(concentrado, CL2=="B"),aes(label=scales::comma(m2020,1L)),position = position_dodge(width = 1),
            vjust = -0.5, size = 4, family="Lato Black") + 
  annotate("text", x="I", y=50000, label="El ingreso mensual promedio de los hogares en Sonora es de $20,452", size = 5, family="Lato",
           hjust = 0) +
  annotate("text", x="I", y=45000, label="La mitad de hogares de menores ingresos percibe $8,870,", size = 4, family="Lato", fontface="italic", color="gray50",
           hjust = 0) +
  annotate("text", x="I", y=41000, label="mientras que la de mayores ingresos $32,035.", size = 4, family="Lato", fontface="italic", color= "#01A2AC",
           hjust = 0) +
  scale_fill_manual( values=c("gray90", "#01A2AC"))  +
  scale_color_manual(values=c( "gray70", "#01787E"))  +
  #scale_x_continuous(breaks = seq(from = 2015, to = 2020, by = 1)) +
  scale_y_continuous (expand = c(0,0), limits = c(0,71000)) +
  theme_void() +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        plot.title = element_markdown(family = "Lato Black", size = 20,color = "black"), 
        plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"), axis.line.x = element_line(linetype = "solid"),
        axis.text.y = element_blank(), axis.text.x = element_text(family = "Lato", size =10,margin=margin(5,0,0,0)),
        axis.title.x = element_text(family = "Lato Light", size = 6, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), legend.position = "none",
        plot.background = element_rect(fill = "white", color = "black", size = 2.5),
        plot.caption = element_text(family = "Lato", size = 6, color = "black")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-weight: bold;'>Sonora en la ENIGH:</span><span style = 'color:#01A2AC;'> ingreso mensual promedio de los hogares</span>", 
       subtitle= "Ingreso mensual promedio en pesos por decil de hogares en 2020.\nCada decil se refiere a una décima parte de los hogares de Sonora ordenados por ingreso.", caption ="Elaboración Luis Armando Moreno (@dogomoreno) con información de ENIGH 2020, INEGI.\nwww.luisarmandomoreno.com")

ingresospesos
ggsave("ingresospesos2.png",ingresospesos, bg = "transparent", height = 5, width = 5*(16/9), device = png)


gasto <- read_csv("01Datos/SON_EGR_DECIL.csv", 
                        locale = locale(encoding = "ISO-8859-1"))

gasto <- gasto %>% mutate(DECILES=factor(DECILES, ordered=TRUE, levels = c("I","II","III","IV", "V", "VI", "VII", "VIII", "IX", "X", "ESTATAL")),
                          Rubro=factor(Rubro, ordered=TRUE, levels = c(   "Vestido y\ncalzado","Salud","Transferencias",
                                                                          "Limpieza, cuidados de la casa,\nenseres, muebles",
                                                                          "Cuidados personales\ny otros" ,
                                                                          "Educaci?n y\nesparcimiento",
                                                                          "Vivienda, combustibles,\nenerg?a el?ctrica",
                                                                          "Transporte, veh?culos,\ncomunicaciones",
                                                                          "Alimentos, bebidas\ny tabaco")))
filtro <- c("Vestido y\ncalzado","Salud")
gastoestr <- ggplot(gasto, aes(x= DECILES, y= pctjGasto, group=Rubro)) +
  geom_col(aes (fill= Rubro), position="fill", color="white", alpha=0.8, linetype= "solid", width = 0.8)+
  geom_text(aes(label=scales::percent(ifelse(Rubro%in%filtro, NA, pctjGasto),1L)),position = position_fill(vjust = 0.5), size = 4, family="Lato Black", color="white") +
  scale_fill_viridis( option= "mako",discrete = T, direction=-1)  +
  geom_text(aes(label=scales::percent(ifelse(!(Rubro%in%filtro), NA, pctjGasto),1L)),position = position_fill(vjust = 0.5), size = 4, family="Lato Black", color="#01A2AC") +
  # guides(fill = guide_legend(reverse=TRUE)) +
  #scale_color_manual(values=c( "gray70", "#01787E"))  +
  #scale_x_continuous(breaks = seq(from = 2015, to = 2020, by = 1)) +
  scale_y_continuous (expand=c(0,0), limits = c(-0.01,1.01), labels = scales::percent_format(accuracy = 1)) +
  #coord_flip() +
  theme_void() +
  theme(plot.margin = margin(0.5, 0.8, 0.5, 0.8, "cm"), 
        plot.title = element_markdown(family = "Lato Black", size = 28,color = "black"), 
        plot.subtitle = element_text(family = "Lato Light", size = 14, color = "black"), axis.line.x = element_line(linetype = "solid"),
        axis.text.y = element_blank(), axis.text.x = element_text(family = "Lato", size =10,margin=margin(5,0,0,0)),
        axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 6, hjust=1), legend.position = "right",
        plot.background = element_rect(fill = "white", color = "black", size = 2.5), legend.title = element_blank(),
        plot.caption = element_text(family = "Lato", size = 10, color = "black"),plot.title.position = 'plot', plot.caption.position = 'plot',
        legend.direction = "vertical", legend.text = element_text(family = "Lato Light", size =12), legend.key.height = unit(2.7, "cm")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-weight: bold;'>ENIGH en Sonora:</span><span style = 'color:#01A2AC';> estructura del gasto en el hogar por rubro</span>", 
       subtitle= "Estructura porcentual del gasto por rubro en los hogares por decil de ingreso en 2020", caption ="Cada decil se refiere a una d?cima parte de los hogares de Sonora ordenados por ingreso.\nElaboraci?n Luis Armando Moreno (@dogomoreno) con informaci?n de ENIGH 2020, INEGI.\nwww.luisarmandomoreno.com")

gastoestr
ggsave("egresospctj.png",gastoestr, bg = "transparent", height = 12, width =12, type= 'cairo')



gasto <- read_csv("01Datos/SON_EGR_DECIL 2.csv", 
                  locale = locale(encoding = "ISO-8859-1"))

gasto <- gasto %>% mutate(DECILES=factor(DECILES, ordered=TRUE, levels = c("I","II","III","IV", "V", "VI", "VII", "VIII", "IX", "X", "ESTATAL")),
                          Rubro=factor(Rubro, ordered=TRUE, levels = c(   "Vestido y calzado","Salud","Transferencias",
                                                                          "Limpieza, cuidados de la casa, muebles",
                                                                          "Cuidados personales" ,
                                                                          "Educaci?n y esparcimiento",
                                                                          "Vivienda, combustibles, energ?a",
                                                                          "Transporte, veh?culos, comunicaciones",
                                                                          "Alimentos y similares")))
filtro <- c("Vestido y calzado","Salud")

gastoestr <- ggplot(gasto, aes(x= DECILES, y= pctjGasto, group=Rubro)) +
  geom_col(aes (fill= Rubro), position="fill", color="white", alpha=0.8, linetype= "solid", width = 0.8)+
  geom_text(aes(label=scales::percent(ifelse(Rubro%in%filtro, NA, pctjGasto),1L)),position = position_fill(vjust = 0.5), size = 4, family="Lato Black", color="white") +
  scale_fill_viridis( option= "mako",discrete = T, direction=-1)  +
  geom_text(aes(label=scales::percent(ifelse(!(Rubro%in%filtro), NA, pctjGasto),1L)),position = position_fill(vjust = 0.5), size = 4, family="Lato Black", color="#01A2AC") +
  guides(fill=guide_legend(title.position = "top", 
         hjust = 0.5, #centres the title horizontally
         title.hjust = 0.5,
         label.position = "bottom", reverse=TRUE, nrow = 1)) +
  #scale_color_manual(values=c( "gray70", "#01787E"))  +
  #scale_x_continuous(breaks = seq(from = 2015, to = 2020, by = 1)) +
  scale_y_continuous (expand=c(0,0), limits = c(-0.01,1.01), labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_void() +
  theme(plot.margin = margin(0.5, 0.8, 0.5, 0.8, "cm"), 
        plot.title = element_markdown(family = "Lato Black", size = 28,color = "black"), 
        plot.subtitle = element_text(family = "Lato Light", size = 14, color = "black"), axis.line.x = element_line(linetype = "solid"),
        axis.text.x = element_blank(), axis.text.y = element_text(family = "Lato", size =12,margin=margin(5,0,0,0)),
        axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 6, hjust=1), legend.position = "top",
        plot.background = element_rect(fill = "white", color = "black", size = 2.5), legend.title = element_blank(),
        plot.caption = element_text(family = "Lato", size = 8, color = "black"),plot.title.position = 'plot', plot.caption.position = 'plot',
        legend.direction = "horizontal", legend.text = element_text(family = "Lato", size =9), legend.key.width = unit(1.8, "cm"), 
        plot.tag= element_text(family = "Lato", size =15, angle=90 ), plot.tag.position = c(0,0.5)) +
  labs(y = NULL, 
       x = NULL,legend= NULL, tag="Deciles de ingreso", title  = "<span style = 'font-weight: bold;'>Sonora en la ENIGH:</span><span style = 'color:#01A2AC';> estructura del gasto en el hogar por rubro</span>", 
       subtitle= "Estructura porcentual del gasto por rubro y  decil de ingreso en los hogares en 2020", caption ="Cada decil se refiere a una d?cima parte de los hogares de Sonora ordenados por ingreso.\nElaboraci?n Luis Armando Moreno (@dogomoreno) con informaci?n de ENIGH 2020, INEGI.\nwww.luisarmandomoreno.com")

gastoestr
ggsave("egresospctj.png",gastoestr, bg = "transparent", height = 8, width = 8*(16/9), type= 'cairo')

           
gini <- read_csv("01Datos/gini.csv", 
                        locale = locale(encoding = "ISO-8859-1"))

ginig <- ggplot(gini, aes(x= enigh, y= gini, group= tiempo)) +
  geom_line(aes(group = enigh), linetype= "dashed",
            color="grey") +
  geom_line(aes (color=tiempo), alpha=0.5, linetype= "solid", size=1)+
  geom_point(aes (fill=tiempo), color="white", shape=21, size=3, stroke=2)+
  geom_dl(aes(label = tiempo, color=tiempo),  method = list(dl.trans(x = x + 0.8), "last.bumpup", cex = 1, fontfamily= "Lato Black")) +
  geom_text(data= subset(gini, tiempo=="Antes de\ntransferencias"), aes(y=gini+.016, label=gini, color=tiempo), family="Lato Light", size=4) +
  geom_text(data= subset(gini, tiempo=="Antes de\ntransferencias"), aes(y=gini+.04, label=enigh), color="black", family="Lato Black", size=4) +
  geom_text(data= subset(gini, tiempo=="Despu?s de\ntransferencias"), aes(y=gini-.02, label=gini, color=tiempo), family="Lato Light", size=4) +
  annotate("text", x=2015, y=0.35, label= "El coeficiente de Gini es un indicador de concentraci?n del ingreso.\nToma valores entre 0 y 1, entre m?s cercano a 1 mayor concentraci?n de ingreso.", 
           size = 4, family="Lato", fontface="italic", color="gray50", hjust = 0) +
  scale_fill_manual( values=c("#F79646", "#01A2AC" ))  +
  scale_color_manual(values=c( "#F79646", "#01A2AC"))  +
  scale_x_continuous(limits=c(2016-1,2020+1),breaks = seq(from = 2016, to = 2020, by = 2)) +
  scale_y_continuous (expand = c(0,0), limits = c(.3,.6)) +
  theme_void() +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        plot.title = element_markdown(family = "Lato Black", size = 20,color = "black"), 
        plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"), axis.line.x = element_line(linetype = "solid"),
        axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(family = "Lato Light", size = 6, hjust=1),
        axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), legend.position = "none",
        plot.background = element_rect(fill = "white", color = "black", size = 2.5),
        plot.caption = element_text(family = "Lato", size = 6, color = "black")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-weight: bold;'>Sonora en la ENIGH:</span><span style = 'color:#01A2AC;'> coeficiente de Gini</span>", 
       subtitle= "Coeficiente de Gini antes y despu?s de transferencias.", caption ="Elaboraci?n Luis Armando Moreno (@dogomoreno) con informaci?n de ENIGH 2020, INEGI.\nwww.luisarmandomoreno.com")


#ggsave("gini2.png",ginig, height = 5, width = 5*(16/9), device=png, )

           
           
pngfile <- here::here("img", "gini.png")
# Initialize device
ragg::agg_png(
  pngfile, height = 5, width = 5*(16/9), units = "in",
  res = 400
)
ginig
# Close device
invisible(dev.off())
            
  