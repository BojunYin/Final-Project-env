
library(shiny)
library(ggplot2)
library(ggplot2)
library(ggmap)
library(proj4)
library(StatDA)
library(dplyr)
library(scales)

data(moss)
data(ohorizon)
data(chorizon)
data(topsoil)
data(bhorizon)

proj4string <- "+proj=utm +zone=35 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

#Trasfor UTM to longlat
COO_1 = moss[c("XCOO","YCOO")]
pj_1 <- project(COO_1, proj4string, inverse=TRUE)
latlon_1 <- data.frame(lat=pj_1$y, lon=pj_1$x)
moss=moss%>%mutate(lat=latlon_1$lat,long=latlon_1$lon)

COO_2 = ohorizon[c("XCOO","YCOO")]
pj_2 <- project(COO_2, proj4string, inverse=TRUE)
latlon_2 <- data.frame(lat=pj_2$y, lon=pj_2$x)
ohorizon=ohorizon%>%mutate(lat=latlon_2$lat,long=latlon_2$lon)

COO_3 = bhorizon[c("XCOO","YCOO")]
pj_3 <- project(COO_3, proj4string, inverse=TRUE)
latlon_3 <- data.frame(lat=pj_3$y, lon=pj_3$x)
bhorizon=bhorizon%>%mutate(lat=latlon_3$lat,long=latlon_3$lon)

COO_4 = chorizon[c("XCOO","YCOO")]
pj_4 <- project(COO_4, proj4string, inverse=TRUE)
latlon_4 <- data.frame(lat=pj_4$y, lon=pj_4$x)
chorizon=chorizon%>%mutate(lat=latlon_4$lat,long=latlon_4$lon)

COO_5 = topsoil[c("XCOO","YCOO")]
pj_5 <- project(COO_5, proj4string, inverse=TRUE)
latlon_5 <- data.frame(lat=pj_5$y, lon=pj_5$x)
topsoil=topsoil%>%mutate(lat=latlon_5$lat,long=latlon_5$lon)

#get base map
register_google(key = "AIzaSyDKqz868onrYFKnvGv6UVUrXfSIwq4e-Zg")
map <- get_googlemap(c(30, 69),zoom = 6,scale = 4)

source("helpers.R")


ui <- fluidPage(
  titlePanel("Kola Project"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from 1998 Kola Project survey area.
               Show elements level in in different regions"),
      
      selectInput("elements", 
                  label = "Choose a chemical elements to display",
                  choices = c("Ni","Ag", "Al","As", "Au","B","Ba","Be","Bi","Ca","Cd","Co",
                              "Cr","Cu","Fe","Hg","K","La","Mg","Mn","Mo","Na","Ni","p",
                              "Pb","Pd","Pt","Rb","S","Sb","Sc","Se","Si","Sr","Th","Tl","U","V","Y","Zn"
                              ),
                  selected = "Ni"),
     selectInput("Measurements", 
                  label = "choose a way of measurements to display",
                  choices = c("Moss","O-horizon","B-horizon","C-horizon","Topsoil"),
                  selected = "Moss"),
     selectInput("Display", 
                label = "choose the display ways of plot:\nLinear: evenly scaled point\nExponential: focus on lagre numbers",
                choices = c("Linear","Exponential"),
                selected = "Moss")
    ),
      mainPanel(plotOutput("map"))
  )
)


server <- function(input, output) {
  output$map <- renderPlot({
    
    data_set <- switch(input$Measurements, 
                   "Moss" = moss,
                   "O-horizon"= ohorizon,
                   "B-horizon"= bhorizon,
                   "C-horizon"= chorizon,
                   "Topsoil"= topsoil)
    
    if (input$Measurements == "Moss"){
    elemets <- switch(input$elements,
                      "Ni"= moss$Ni,
                      "Ag"= moss$Ag,
                      "Al"=moss$Al,
                      "As"=moss$Au,
                      "Au"=moss$Au,
                      "B"=moss$B,
                      "Ba"=moss$Ba,
                      "Be"=moss$Be,
                      "Bi"=moss$Bi,
                      "Ca"=moss$Ca,
                      "Cd"=moss$Cd,
                      "Co"=moss$Co,
                      "Cr"=moss$Cr,
                      "Cu"=moss$Cu,
                      "Fe"=moss$Fe,
                      "Hg"=moss$Hg,
                      "K"=moss$K,
                      "La"=moss$La,
                      "Mg"=moss$Mg,
                      "Mn"=moss$Mn,
                      "Mo"=moss$Mo,
                      "Na"=moss$Na,
                      "Ni"=moss$Ni,
                      "P"=moss$P,
                      "Pb"=moss$Pb,
                      "Pd"=moss$Pd,
                      "Pt"=moss$Pt,
                      "Rb"=moss$Rb,
                      "S"=moss$S,
                      "Sb"=moss$Sb,
                      "Sc"=moss$Sc,
                      "Se"=moss$Se,
                      "Si"=moss$Si,
                      "Sr"=moss$Sr,
                      "Th"=moss$Th,
                      "Tl"=moss$Tl,
                      "U"=moss$U,
                      "V"=moss$V,
                      "Y"=moss$Y,
                      "Zn"=moss$Zn)
    }
    if (input$Measurements == "O-horizon"){
      elemets <- switch(input$elements,
                        "Ni"= ohorizon$Ni,
                        "Ag"= ohorizon$Ag,
                        "Al"=ohorizon$Al,
                        "As"=ohorizon$Au,
                        "Au"=ohorizon$Au,
                        "B"=ohorizon$B,
                        "Ba"=ohorizon$Ba,
                        "Be"=ohorizon$Be,
                        "Bi"=ohorizon$Bi,
                        "Ca"=ohorizon$Ca,
                        "Cd"=ohorizon$Cd,
                        "Co"=ohorizon$Co,
                        "Cr"=ohorizon$Cr,
                        "Cu"=ohorizon$Cu,
                        "Fe"=ohorizon$Fe,
                        "Hg"=ohorizon$Hg,
                        "K"=ohorizon$K,
                        "La"=ohorizon$La,
                        "Mg"=ohorizon$Mg,
                        "Mn"=ohorizon$Mn,
                        "Mo"=ohorizon$Mo,
                        "Na"=ohorizon$Na,
                        "Ni"=ohorizon$Ni,
                        "P"=ohorizon$P,
                        "Pb"=ohorizon$Pb,
                        "Pd"=ohorizon$Pd,
                        "Pt"=ohorizon$Pt,
                        "Rb"=ohorizon$Rb,
                        "S"=ohorizon$S,
                        "Sb"=ohorizon$Sb,
                        "Sc"=ohorizon$Sc,
                        "Se"=ohorizon$Se,
                        "Si"=ohorizon$Si,
                        "Sr"=ohorizon$Sr,
                        "Th"=ohorizon$Th,
                        "Tl"=ohorizon$Tl,
                        "U"=ohorizon$U,
                        "V"=ohorizon$V,
                        "Y"=ohorizon$Y,
                        "Zn"=ohorizon$Zn)
    }
    if (input$Measurements == "B-horizon"){
      elemets <- switch(input$elements,
                        "Ni"= bhorizon$Ni,
                        "Ag"= bhorizon$Ag,
                        "Al"=bhorizon$Al,
                        "As"=bhorizon$Au,
                        "Au"=bhorizon$Au,
                        "B"=bhorizon$B,
                        "Ba"=bhorizon$Ba,
                        "Be"=bhorizon$Be,
                        "Bi"=bhorizon$Bi,
                        "Ca"=bhorizon$Ca,
                        "Cd"=bhorizon$Cd,
                        "Co"=bhorizon$Co,
                        "Cr"=bhorizon$Cr,
                        "Cu"=bhorizon$Cu,
                        "Fe"=bhorizon$Fe,
                        "Hg"=bhorizon$Hg,
                        "K"=bhorizon$K,
                        "La"=bhorizon$La,
                        "Mg"=bhorizon$Mg,
                        "Mn"=bhorizon$Mn,
                        "Mo"=bhorizon$Mo,
                        "Na"=bhorizon$Na,
                        "Ni"=bhorizon$Ni,
                        "P"=bhorizon$P,
                        "Pb"=bhorizon$Pb,
                        "Pd"=bhorizon$Pd,
                        "Pt"=bhorizon$Pt,
                        "Rb"=bhorizon$Rb,
                        "S"=bhorizon$S,
                        "Sb"=bhorizon$Sb,
                        "Sc"=bhorizon$Sc,
                        "Se"=bhorizon$Se,
                        "Si"=bhorizon$Si,
                        "Sr"=bhorizon$Sr,
                        "Th"=bhorizon$Th,
                        "Tl"=bhorizon$Tl,
                        "U"=bhorizon$U,
                        "V"=bhorizon$V,
                        "Y"=bhorizon$Y,
                        "Zn"=bhorizon$Zn)
    }
    if (input$Measurements == "C-horizon"){
      elemets <- switch(input$elements,
                        "Ni"= chorizon$Ni,
                        "Ag"= chorizon$Ag,
                        "Al"=chorizon$Al,
                        "As"=chorizon$Au,
                        "Au"=chorizon$Au,
                        "B"=chorizon$B,
                        "Ba"=chorizon$Ba,
                        "Be"=chorizon$Be,
                        "Bi"=chorizon$Bi,
                        "Ca"=chorizon$Ca,
                        "Cd"=chorizon$Cd,
                        "Co"=chorizon$Co,
                        "Cr"=chorizon$Cr,
                        "Cu"=chorizon$Cu,
                        "Fe"=chorizon$Fe,
                        "Hg"=chorizon$Hg,
                        "K"=chorizon$K,
                        "La"=chorizon$La,
                        "Mg"=chorizon$Mg,
                        "Mn"=chorizon$Mn,
                        "Mo"=chorizon$Mo,
                        "Na"=chorizon$Na,
                        "Ni"=chorizon$Ni,
                        "P"=chorizon$P,
                        "Pb"=chorizon$Pb,
                        "Pd"=chorizon$Pd,
                        "Pt"=chorizon$Pt,
                        "Rb"=chorizon$Rb,
                        "S"=chorizon$S,
                        "Sb"=chorizon$Sb,
                        "Sc"=chorizon$Sc,
                        "Se"=chorizon$Se,
                        "Si"=chorizon$Si,
                        "Sr"=chorizon$Sr,
                        "Th"=chorizon$Th,
                        "Tl"=chorizon$Tl,
                        "U"=chorizon$U,
                        "V"=chorizon$V,
                        "Y"=chorizon$Y,
                        "Zn"=chorizon$Zn)
    }
    if (input$Measurements == "Topsoil"){
      elemets <- switch(input$elements,
                        "Ni"= topsoil$Ni,
                        "Ag"= topsoil$Ag,
                        "Al"=topsoil$Al,
                        "As"=topsoil$Au,
                        "Au"=topsoil$Au,
                        "B"=topsoil$B,
                        "Ba"=topsoil$Ba,
                        "Be"=topsoil$Be,
                        "Bi"=topsoil$Bi,
                        "Ca"=topsoil$Ca,
                        "Cd"=topsoil$Cd,
                        "Co"=topsoil$Co,
                        "Cr"=topsoil$Cr,
                        "Cu"=topsoil$Cu,
                        "Fe"=topsoil$Fe,
                        "Hg"=topsoil$Hg,
                        "K"=topsoil$K,
                        "La"=topsoil$La,
                        "Mg"=topsoil$Mg,
                        "Mn"=topsoil$Mn,
                        "Mo"=topsoil$Mo,
                        "Na"=topsoil$Na,
                        "Ni"=topsoil$Ni,
                        "P"=topsoil$P,
                        "Pb"=topsoil$Pb,
                        "Pd"=topsoil$Pd,
                        "Pt"=topsoil$Pt,
                        "Rb"=topsoil$Rb,
                        "S"=topsoil$S,
                        "Sb"=topsoil$Sb,
                        "Sc"=topsoil$Sc,
                        "Se"=topsoil$Se,
                        "Si"=topsoil$Si,
                        "Sr"=topsoil$Sr,
                        "Th"=topsoil$Th,
                        "Tl"=topsoil$Tl,
                        "U"=topsoil$U,
                        "V"=topsoil$V,
                        "Y"=topsoil$Y,
                        "Zn"=topsoil$Zn)
    }
    
    ggmap(map)+geom_point(aes(x=long, y = lat, size = elemets,fill = elemets), data = data_set,shape=21, alpha=0.8)+
      scale_fill_gradient(low = "white", high = "blue")+
      if(input$Display == "Linear"){
      scale_size_continuous(range = c(1, 7), breaks=pretty_breaks(5)) 
      }else{
      scale_size_continuous(range = c(1, 15), breaks=pretty_breaks(7))
      }
})
}
shinyApp(ui,server)
