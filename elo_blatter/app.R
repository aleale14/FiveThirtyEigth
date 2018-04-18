library(DT)
library(shiny)
library(fivethirtyeight)
library(ggplot2)
library(dplyr)
library(tidyr)

# load color palette
source("C:/Users/the1iz/Documents/R/palette.R")

# set working directory
setwd("C:/Users/the1iz/Documents/R/538/Elo_Blatter")

df<-elo_blatter
df<-df%>%
  gather(elo,`ELO_rating`,elo98:elo15)%>%
  select(country,confederation,elo,ELO_rating)

df2<-df%>%
  mutate(`ELO_rebalanced`=round(100*ELO_rating/df$ELO_rating[df$elo=="elo98"],2))%>%
  mutate(elo=recode(elo,"elo98"="'98","elo15"="'15"))%>%
  mutate(elo=factor(elo,levels = c("'98","'15")))%>%
  mutate(country=as.factor(country),confederation=as.factor(confederation))%>%
  rename(year=elo)

server <- function(input, output, session) {
  
  
  
  
  # I'm setting paging = FALSE so all rows are shown all the time
  # scrollX adds a scrollbar, filter allows column filtering
  output$mytable <- DT::renderDataTable(df2,
                                        options = list(paging=FALSE, scrollX = TRUE),
                                        rownames=F,
                                        filter = "top")
  
  
  output$plot2<- renderPlot({
    filtered_data <- input$mytable_rows_all
    data_filt <- df2[filtered_data,]%>%
      gather(ELO_Class,`ELO Values`,ELO_rating:ELO_rebalanced)
    if(nrow(data_filt)>0){
      ggplot(data_filt)+
        geom_point(aes(x=factor(year),y=`ELO Values`,
                       color=confederation),alpha=0.2)+
        geom_line(aes(x=factor(year),y=`ELO Values`,
                      color=confederation,group=country),alpha=0.2)+
        geom_point(data=data_filt%>%
                     filter(year==("'15"))%>%
                     group_by(confederation,ELO_Class)%>%
                     summarise(mean=mean(`ELO Values`,na.rm=TRUE)),
                   aes(x=factor("'15"),y=mean),
                   shape=25,size=3,stroke=1.3,color="darkred")+
        geom_text(data=data_filt%>%
                    filter(year==("'15"))%>%
                    group_by(confederation,ELO_Class)%>%
                    summarise(mean=mean(`ELO Values`,na.rm=TRUE)),
                  aes(x= 2.05, y=mean,
                      label = paste("mean"),
                      color=confederation), size=3,hjust=0)+
        labs(color='Confederation',
             x="Year",
             y="ELO rating [absolute / rebalanced]",
             title="ELO evolution over Blatter's reign at FIFA",
             subtitle="Evolution in ELO scores between 1998 and 2015")+
        theme_gray()+
        theme(plot.title = element_text(size=22),
              plot.subtitle = element_text(size=16),
              axis.title = element_text(size=16),
              axis.text = element_text(size=12),
              legend.title = element_text(size=16),
              legend.text = element_text(size=12),
              legend.position = "none",
              strip.text.x = element_text(size = 12),
              strip.text.y = element_text(size = 12))+
        coord_cartesian(xlim = c(.9,2.4), ylim = NULL, expand = F)+
        facet_grid(ELO_Class~confederation,scales = "free")+
        scale_color_aa()#+
        #geom_hline(aes(yintercept = 1),color="darkred",alpha=0.4,linetype="dashed")
    }
  })
  
}

ui <- basicPage(
  
  h1("Interactive visualization ELO ratings under Sepp Blatter's tenure at FIFA"),
  
  h5("The Elo rating system is a method for calculating the relative skill 
     levels of players in zero-sum games such as chess. It is named after its 
     creator Arpad Elo, a Hungarian-American physics professor."),
  h5("The World Football Elo Ratings is a ranking system for men's national 
     association football teams that is published by the website eloratings.net. 
     It is based on the Elo rating system but includes modifications to take 
     various football-specific variables into account, like the margin of 
     victory, importance of a match, and home field advantage."),

  plotOutput("plot2"),
  DT::dataTableOutput("mytable")
  
)

shinyApp(ui = ui, server = server)

