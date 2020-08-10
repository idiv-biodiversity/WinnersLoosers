library(shiny)
library(ggplot2)
library(plotly)
library(DT)

winnersloosers<- readRDS("./datasets/winnersloosers.rds")
winnersloosers$TaxonName<-as.character(winnersloosers$TaxonName)
Encoding(winnersloosers$TaxonName)<- c("UTF-8")
winnersloosers$TaxonName<- as.factor(as.character(winnersloosers$TaxonName))
orderspecs<- winnersloosers$TaxonName[order(winnersloosers$perc_3_1,decreasing=T)]




winnersloosers$Species<- factor(winnersloosers$TaxonName,
                                    levels=orderspecs)


names(winnersloosers)[2]<- c("Change")
winnersloosers<- winnersloosers[,c(4,2)]
winloosecols<- readRDS("./datasets/winloosecols.rds")

info_winnersloosers<- readRDS("./datasets/info_winnersloosers.rds")
info_winnersloosers$Species<- as.character(info_winnersloosers$Species)

Encoding(info_winnersloosers$Species)<- c("UTF-8")
info_winnersloosers<- info_winnersloosers[which(info_winnersloosers$Species %in% winnersloosers$Species),]
info_winnersloosers<-info_winnersloosers[order(info_winnersloosers$`Delta Period 3-1`,decreasing=T),]


ui <- fluidPage(
  fluidRow(
    column(width = 12, class = "well",
           h4(div(HTML("Plot of changes in SOP<sub>Spec</sub> as presented in Eichenberg <i>et al.</i> (2020)"
              ))),
           h5(div(HTML("Plot shows the difference in species occurrence [%] in Germany between 1960-1987 and 1996-2017. <br>
                       Hover mouse over a bar to see species name and the respective Change; drag a rectagle to zoom in if needed"))),
           plotlyOutput("plot", height = 400)
           ),
    column(width=12, 
           h5(div(HTML("The table shows the occurrence values of the selected species in all three study periods and their changes between the periods [%]."))),
           dataTableOutput("table")
           )
  )
)



server <- function(input, output) {
   
  output$plot <- renderPlotly({
  
    p<- ggplot(data=winnersloosers, aes(x=Species, y=Change, col=Change, text=paste0("Change [%]: ",Change))) + 
        geom_hline(yintercept=0, col="black") + geom_bar(stat="identity",width = 1)+
        scale_color_gradientn(colors=winloosecols(2145),name="Change [%]")+
        theme(axis.text.x = element_text(angle=45, size=3, vjust = 0.5, hjust=0),
              panel.background = element_rect(fill="gray90")) + 
        xlab("Species") + ylab("Change in SOP [%]")
    #plotly:: style(p, text=winnersloosers$Change)
    ggplotly(p, tooltip = c("x","text")) 
    }
  )
  
  species_choice<- reactive({
    d<- event_data(event="plotly_hover")$x
    }
    )
  
  output$table<-renderDataTable({
    if(is.null(species_choice())){ info_winnersloosers} else{
    info_winnersloosers[species_choice(),]}
    }
    )
               
}


shinyApp(ui,server)
