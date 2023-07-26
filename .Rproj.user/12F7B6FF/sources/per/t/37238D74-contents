rm(list=ls())

# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(zoo)
library(lubridate)
library("Cairo")
library(directlabels)
library(ggtext)
library(geofacet)

temaejes <- theme(plot.margin = margin(10, 40, 10, 40),
                  plot.title = element_markdown(family = "Lato Black", size =  30), 
                  panel.grid=element_blank(), panel.border=element_blank(), 
                  axis.line= element_line(color = "black", size = 0.3), 
                  plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                  strip.text = element_text(family = "Lato Black", size = 8),
                  axis.text = element_text(family = "Lato", size =6),
                  plot.background = element_rect(fill = "transparent", color = "transparent", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 8),
                  legend.text = element_blank(),
                  legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')


Electricidad <- read_csv("01Datos/Electricidad.csv") %>% rename(code=entidad_res) %>% mutate(Sonora=if_else(code==26,"1", "2"), group="1") %>% 
  mutate(color_texto = if_else(porcentaje>0.05, "1","2"), size=porcentaje*500)

Electricidadnal <- ggplot(Electricidad) +
  geom_col(aes(y=porcentaje, x=group, fill=porcentaje)) +
  geom_text(data=subset(Electricidad, porcentaje<=0.055),aes(y=.05, x=group, label=paste0(round(porcentaje*100,1), "%")), color="#453400ff", family="Lato Black", size=4.5) +
  geom_text(data=subset(Electricidad, porcentaje>0.055),aes(y=porcentaje/2, x=group, label=paste0(round(porcentaje*100,1), "%")), color="white", family="Lato Black", size=4.5) +
  scale_y_continuous(limits=c(0,0.10), expand=c(0.01,0)) +
  scale_x_discrete(expand=c(0.233,0.233)) +
  coord_flip() +
    facet_geo(~ code, 
            grid = mx_edos_nk, 
            label = "code",
            scales = "fixed",
            strip.position = "bottom") +
  scale_fill_gradient2(low= "#fbdb76ff", mid = "#fabf11ff", high="#554002ff", midpoint = .045) +
  theme_void()+ temaejes +
  theme(legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        plot.title = element_markdown(family = "Lato Black", size =  40), 
        plot.caption = element_text(family = "Lato", size = 8, color="gray65"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_markdown(family = "Lato Light", size = 14, color = "black"),
        plot.tag = element_markdown(family = "Lato", size = 16, color = "black", hjust=0),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.background = element_rect(fill="gray95", color="gray40"),
        axis.line = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none", legend.text=element_markdown(family = "Lato Black", size = 24, hjust=0),
        legend.title=element_markdown(family = "Lato", size = 22, hjust=0),
        strip.background = element_rect(fill="transparent", color="transparent"),
        strip.text = element_text(family = "Lato Black", size =12, color="black", hjust=0.5), 
        legend.key.size = unit(1.5, 'cm'),
        plot.tag.position = c(0.67, 0.65)) +
  
labs(y = NULL, 
     #tag="<span style = 'font-size:10pt;color:#000000';>Respecto al máximo semanal<br>previo a octubre de 2021</span>",
     x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>ENIGH 2020</span><br><span style = 'color:#fabf11ff';>Gasto en electricidad y combustibles</span>"),
     subtitle= "Gasto por concepto de energía eléctrica y combustibles respecto al gasto total de los hogares por entidad federativa.<br>",
     caption ="Elaboración Luis Armando Moreno (@dogomoreno) con información de la ENIGH 2020, INEGI.\nwww.luisarmandomoreno.com")
Electricidadnal
ggsave("electricidad.png",Electricidadnal , width =6*(16/9), height = 7, type = "cairo", dpi = 400)
 

library(ggnewscale)
Electricidadnal <- ggplot(Electricidad) +
  geom_point(aes(y=0.05, x=group, fill=porcentaje, size=porcentaje), shape=21, color="#fabf11ff") +
  scale_size(range = c(8, 18)) +
  new_scale("size")+
  geom_text(aes(y=.05, x=group, label=paste0(round(porcentaje*100,1), "%"), color=color_texto, size=porcentaje),family="Lato Black") +
  scale_size(range = c(3.5, 4.5)) +
  #geom_text(data=subset(Electricidad, porcentaje<=0.055),aes(y=.05, x=group, label=paste0(round(porcentaje*100,1), "%")), color="#453400ff", family="Lato Black", size=4.5) +
  #geom_text(data=subset(Electricidad, porcentaje>0.055),aes(y=.05, x=group, label=paste0(round(porcentaje*100,1), "%")), color="white", family="Lato Black", size=4.5) +
  scale_y_continuous(limits=c(0,0.10), expand=c(0.01,0)) +
  scale_x_discrete(expand=c(0.233,0.233)) +
    coord_flip() +
  facet_geo(~ code, 
            grid = mx_edos_nk, 
            label = "code",
            scales = "fixed",
            strip.position = "bottom") +
  scale_fill_gradient2(low= "#fbdb76ff", mid = "#fabf11ff", high="#554002ff", midpoint = .045) +
  scale_color_manual(values=c("1"="white","2"="#554002ff")) +
  theme_void()+ temaejes +
  theme(legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        plot.title = element_markdown(family = "Lato Black", size =  36), 
        plot.caption = element_text(family = "Lato", size = 8, color="gray65"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_markdown(family = "Lato Light", size = 14, color = "black"),
        plot.tag = element_markdown(family = "Lato", size = 16, color = "black", hjust=0),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none", legend.text=element_markdown(family = "Lato Black", size = 24, hjust=0),
        legend.title=element_markdown(family = "Lato", size = 22, hjust=0),
        strip.background = element_rect(fill="transparent", color="transparent"),
        strip.text = element_text(family = "Lato Black", size =12, color="black", hjust=0.5), 
        legend.key.size = unit(1.5, 'cm'),
        plot.tag.position = c(0.67, 0.65)) +
  
  labs(y = NULL, 
       #tag="<span style = 'font-size:10pt;color:#000000';>Respecto al máximo semanal<br>previo a octubre de 2021</span>",
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>ENIGH 2020</span><br><span style = 'color:#fabf11ff';>El gasto en electricidad y combustibles</span>"),
       subtitle= "Gasto por concepto de energía eléctrica y combustibles respecto al gasto total de los hogares por entidad federativa.<br>",
       caption ="Elaboración Luis Armando Moreno (@dogomoreno) con información de la ENIGH 2020, INEGI.\nwww.luisarmandomoreno.com")
Electricidadnal
ggsave("electricidad3.png",Electricidadnal , width =6*(16/9), height = 7, type = "cairo", dpi = 600, bg="transparent")

