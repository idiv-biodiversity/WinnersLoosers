library(shiny)
library(ggplot2)
library(plotly)
library(DT)

winnersloosers<- readRDS("./datasets/winnersloosers.rds")
winnersloosers$TaxonName<-as.character(winnersloosers$TaxonName)
Encoding(winnersloosers$TaxonName)<- c("UTF-8")
winnersloosers$TaxonName<- as.factor(as.character(winnersloosers$TaxonName))
orderspecs<- winnersloosers$TaxonName[order(winnersloosers[,2],decreasing=T)]


winnersloosers_abs<- readRDS("./datasets/winnersloosers_abs.rds")
winnersloosers_abs$TaxonName<-as.character(winnersloosers_abs$TaxonName)
Encoding(winnersloosers_abs$TaxonName)<- c("UTF-8")
winnersloosers_abs$TaxonName<- as.factor(as.character(winnersloosers_abs$TaxonName))
orderspecs_abs<- winnersloosers_abs$TaxonName[order(winnersloosers_abs[,2],decreasing=T)]


winnersloosers$Species<- factor(winnersloosers$TaxonName,
                                levels=orderspecs)


winnersloosers_abs$Species<- factor(winnersloosers_abs$TaxonName,
                                    levels=orderspecs_abs)


row.names(winnersloosers_abs)<- seq(from=1,to=2140, by=1)

row.names(winnersloosers)<- seq(from=1,to=2140, by=1)


names(winnersloosers)[2]<- c("Change")
winnersloosers<- winnersloosers[,c(4,2)]

names(winnersloosers_abs)[2]<- c("Change")
winnersloosers_abs<- winnersloosers_abs[,c(3,2)]



winloosecols<- readRDS("./datasets/winloosecols.rds")
winloosecols_abs<- readRDS("./datasets/winloosecols_abs.rds")



info_winnersloosers<- readRDS("./datasets/info_winnersloosers.rds")
info_winnersloosers$Species<- as.character(info_winnersloosers$Species)


info_winnersloosers_abs<- readRDS("./datasets/info_winnersloosers_abs.rds")
info_winnersloosers_abs$Species<- as.character(info_winnersloosers_abs$Species)


Encoding(info_winnersloosers$Species)<- c("UTF-8")
info_winnersloosers<- info_winnersloosers[which(info_winnersloosers$Species %in% winnersloosers$Species),]
info_winnersloosers<-info_winnersloosers[order(info_winnersloosers$`Delta Period 3-1`,decreasing=T),]


Encoding(info_winnersloosers_abs$Species)<- c("UTF-8")
info_winnersloosers_abs<- info_winnersloosers_abs[which(info_winnersloosers_abs$Species %in% winnersloosers_abs$Species),]
info_winnersloosers_abs<-info_winnersloosers_abs[order(info_winnersloosers_abs$`Delta Period 3-1`,decreasing=T),]



ui <- fluidPage(
  fluidRow(
    tabsetPanel(type = "tabs",
                id = "mean_or_sd_maps",
                tabPanel("Winners and Losers in distributional change (relative)",
                         column(width = 12, class = "well",
                                h4(div(HTML("Plot of relative changes in SOP<sub>Spec</sub> [%] 
                                    as presented in Eichenberg <i>et al.</i> (2020)")
                                )
                                ),
                                h5(div(HTML("Plot shows the difference in species occurrence [%] in Germany between 1960-1987 and 1996-2017. <br>
                                    Hover mouse over a bar to see species name and the respective Change; drag a rectagle to zoom in if needed")
                                )
                                ),
                                plotlyOutput("plot", height = 400)
                         ),
                         column(width=12, 
                                h5(div(HTML("The table shows the occurrence values 
                                              of the selected species in all three study 
                                              periods and their relative changes between the periods [%]."))),
                                dataTableOutput("table")
                         )
                ),
                tabPanel("Winners and Losers in distributional change (absolute)",
                         column(width = 12, class = "well",
                                h4(div(HTML("Plot of absolute changes in SOP<sub>Spec</sub>  
                                    as presented in Eichenberg <i>et al.</i> (2020)")
                                )
                                ),
                                h5(div(HTML("Plot shows the absolute differences in species occurrence in Germany between 1960-1987 and 1996-2017. <br>
                                    Hover mouse over a bar to see species name and the respective Change; drag a rectagle to zoom in if needed")
                                )
                                ),
                                plotlyOutput("plot2", height = 400)
                         ),
                         column(width=12, 
                                h5(div(HTML("The table shows the occurrence values 
                                              of the selected species in all three study 
                                              periods and their absolute changes between the periods."))),
                                dataTableOutput("table2")
                         )
                ),
                tabPanel("Methodological information",
                         h3("Data compilation"),
                         br(),
                         HTML("We compiled an extensive dataset of approx. 29
                              million occurrence records in Germany between 
                              1960 and 2017 from 23 different data 
                              sources (Table S1 in Eichenberg et al., 2020). 
                              The full dataset comprises the non-aggregated data
                              underlying the German Distribution Atlas of 
                              Ferns and Flowering Plants, restricted to 
                              observations between 1960 and 2013. We extended 
                              this dataset to the year 2017 by integrating data 
                              from more recent habitat mapping projects of 
                              federal states, vegetation relevés provided in 
                              two major German databases, GVRD (
                              <a href='http://vegetation-db.biologie.uni-halle.de'>
                              http://vegetation-db.biologie.uni-halle.de</a>) 
                              and vegetweb 2.0 (<a href='https://www.vegetweb.de'>
                              https://www.vegetweb.de</a>) 
                              and from universities and private collections 
                              (see Eichenberg et al. 2020 for further details)."),
                         br(),
                         br(),
                         h3("Determination of study periods"),
                         br(),
                         HTML("For analyzing trends, the dataset was binned into three periods 
                              (1960-1987, 1988-1996, 1997-2017), each of them with similar 
                              number of total records and covering all 12024 German grid-cells. 
                              The temporal extent of these periods was determined by the need 
                              to find periods of similar coverage (spatial and taxonomically) 
                              of the whole nation (see Appendix I in Eichenberg et al. 2020 
                              for further details)."),
                         br(),
                         br(),
                         h3("Species selection and Nomenclature"),
                         br(),
                         HTML("Taxa were harmonized using an accepted common taxonomic reference 
                              list for Germany (GermanSL; <a href='https://germansl.infinitenature.org/'>
                              https://germansl.infinitenature.org/</a>). 
                              Subspecies, variants etc. were raised to the species or, if necessary, to the 
                              aggregate level. We excluded species that were recorded in only one of the 
                              three periods or had fewer than 23 records in total. 
                              Species were characterized according to their floristic status either as natives, 
                              archaeophytes (non-natives introduced before 1492) or 
                              neophytes (non-natives introduced after 1492,) using 
                              information available from the database BiolFlor (<a href='https://www.ufz.de/biolflor/index.jsp'>
                              https://www.ufz.de/biolflor/index.jsp</a>) 
                              and FloraWeb (<a href='http://www.floraweb.de'>
                              http://www.floraweb.de/</a>). 
                              Species with an unknown floristic status were excluded. This left 
                              us with a total of 2245 species for analysis, 
                              equaling 58% of all German vascular plants 
                              (if raised to a similar taxonomic level as used for the analyses 
                              in Eichenberg et al., 2020)."),
                         br(),
                         br(),
                         h3("Correction for false absences"),
                         HTML("Where occurrence records do not originate from a project focusing 
                              on the complete floristic inventory or does not use complete 
                              check-lists, false absences (i.e. not reporting a species that was 
                              present, but was either not detected or detected but not reported) 
                              are an issue. In addition, atlas projects, which aim at taxonomic 
                              completeness may not be finished in a federal state completely 
                              within one of the defined study periods, leading to taxonomic or 
                              spatial gaps in the data of a single study period. 
                              To correct for this so-called reporting bias, we used the Frescalo 
                              algorithm available in the R package ‘sparta’ 
                              (<a href='https://github.com/BiologicalRecordsCentre/sparta'>
                              https://github.com/BiologicalRecordsCentre/sparta</a>). 
                              Briefly, the Frescalo algorithm calculates the occurrence probability (OP) 
                              of a species not detected or reported in a focal grid-cell, 
                              based on the frequency of this species in the local neighborhood 
                              (here: 100 grid-cells) of this cell, while accounting for the 
                              ecological similarity of the neighborhood. Ecological similarity of 
                              the neighboring grid-cells was calculated based on a set of 76 variables, 
                              comprising climatic, topographic and edaphic measures. A detailed 
                              description of the specifications for the Frescalo algorithm is 
                              given in Appendix I (c.f. Eichenberg et al. 2020)."),
                         br(),
                         br(),
                         h3("Definition of 'Occurrence' in the context of Eichenberg et al. (2020)"),
                         br(),
                         HTML("Occurrence is defined as the sum of occurrence probabilities over all grid cells
                              in which a species occurred in a certain time step. 
                              Maps of the spatial distribution of a given species across 
                              the study region at a given time are not a direct outcome 
                              of the Frescalo algorithm as available in ’sparta’. 
                              The spatial distribution of the probability of a species 
                              being present at the focal grid-cell of neighborhood in a 
                              certain period can be readily calculated from the available 
                              output using the Equations given in Eichenberg et al. (2020). 
                              Nationwide occurrence is approximated by summing up the values of 
                              grid-cell occurrence probability across all grid cells in which a 
                              species is detected in a certain timestep."),
                         br(),
                         br(),
                         h3("Definition of 'species-richness' in the context of Eichenberg et al. (2020)"),
                         br(),
                         HTML("We summed up the occurrence probabilities of all species within 
                              a grid-cell as an estimate of species-richness (SOP<sub>Grid</sub>), 
                              while acknowledging that it is not species-richness per-se, since 
                              our analysis does not include the very rare species (see 'Species selection and Nomenclature').
                              Hence, our species-richness values are underestimating actual grid-cell 
                              species-richness. However, SOP<sub>Grid</sub> was found to be highly significantly 
                              correlated (r= 0.39, p< 0.001) to species numbers in grid-cells of the 
                              FlorKart dataset that were identified to be well-sampled by previous research. 
                              Therefore, changes in SOP<sub>Grid</sub> can be interpreted as meaningfully 
                              representing relative changes between grid-cells and timesteps.")
                )
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
  
  output$plot2 <- renderPlotly({
    
    p<- ggplot(data=winnersloosers_abs, aes(x=Species, y=Change, col=Change, text=paste0("Change [abs]: ",Change))) + 
      geom_hline(yintercept=0, col="black") + geom_bar(stat="identity",width = 1)+
      scale_color_gradientn(colors=rev(winloosecols_abs(2140)),name="Change [abs]")+
      theme(axis.text.x = element_text(angle=45, size=3, vjust = 0.5, hjust=0),
            panel.background = element_rect(fill="gray90")) + 
      xlab("Species") + ylab("Change in SOP [absolute]")
    #plotly:: style(p, text=winnersloosers$Change)
    ggplotly(p, tooltip = c("x","text")) 
  }
  )
  
  species_choice<- reactive({
    d<- event_data(event="plotly_hover")$x
  }
  )
  
  output$table2<-renderDataTable({
    if(is.null(species_choice())){ info_winnersloosers_abs} else{
      info_winnersloosers_abs[species_choice(),]}
  }
  )          
}


shinyApp(ui,server)