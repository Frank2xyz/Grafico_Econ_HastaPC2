#realizado por :Edwar Frank Carrasco Castañeda
#Menciòn : Tema usado del grupo "paqueteRos"

library(tidyverse)
library(remotes)
library(extrafont)
install_version("Rttf2pt1", version = "1.3.8")


theme_propio2 <- function(){
  theme(
    plot.title        = element_text(size = 20,
                                     color = "black",
                                     hjust = 0, 
                                     vjust = 0.1,
                                     face = "bold"), 
    plot.subtitle     = element_text(color = "black",
                                     size = 11),
    
    plot.caption      = element_text(face = "bold", color = "black"),
    
    axis.title.x      = element_text(size = 10, color = "black", face = "bold"),
    axis.text.x       = element_text(color = "black", size = 8, face = "bold"),
    axis.title.y      = element_text(size = 10, color = "black", face = "bold" ),
    axis.text.y       = element_text(color = "black", size = 8, face = "bold"),
    
    axis.ticks        = element_line(color = "white"),
    
    panel.border      = element_rect(color = "#ffffff", fill = NA, size = 1,linetype = 1),
    
    panel.background  = element_rect(fill = "#595757"),
    
    plot.background   = element_rect(fill = "#dbdbdb"),
    
    panel.grid        = element_line(color = "#211e1e", linetype = 1, size = 0.1),
    
    legend.background = element_rect(color = "black", fill = "#D5D8DC", linetype = 0.5),
    legend.title      = element_text(face = "bold", color = "black", size = 11),
    legend.text       = element_text(face = "bold", color = "black", size = 9),
    legend.position   = "right"
  )
}




x1 <- data.frame(
   MATRICULA = c(20211028L,20210928L,20210936L,20210672L,
                 20210674L,20210827L,20210828L,20211039L,20210832L,
                 20200438L,20171027L,20200442L,20210835L,20200221L,20210948L,
                 20211003L,20200053L,20210906L,20200185L,20210646L,20211013L,
                 20210647L,20210706L,20171365L,20210651L,20200467L,20211068L,
                 20211070L,20210920L,20210927L),
         PC1 = c(NA,2,9,12,15,15.5,17.5,15,19,17.5,3,
                 10.5,12.5,18,NA,20,19,15,5,9,NA,15.5,11.5,18,10,18,
                 19,10,19,6.5),
         PC2 = c(NA,2,1,10,10.5,15,14,17,9,14.5,6,7,
                 7.5,6,NA,13.5,13.5,10,5.5,10,NA,10,15.5,13.5,19,14.5,
                 16,11,13.5,14)
)
x1 <- na.omit(x1)

x1$MATRICULA = as.character(x1$MATRICULA)

x1 |> mutate(Promedio = (x1$PC1 + x1$PC2)/2)->x1

x1 |>  filter( Promedio >=10.5) ->x2

x2 |> ggplot( aes( x=MATRICULA , y = Promedio  , color=MATRICULA)) + geom_lollipop() + 
  geom_text(aes(label=Promedio, hjust=-0.1)) + coord_flip() + theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,18,2)) +theme_propio2() + theme(legend.position = "none") 




