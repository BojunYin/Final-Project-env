

library(shiny)
library(ggplot2)
library(ggplot2)
library(ggmap)
library(proj4)
library(StatDA)
library(dplyr)
library(scales)
library(gridExtra)
library(ggthemes)
source("helpers.R")

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

ewY = c(7500000,7600000)
ewX = c(200000,200000)
ew = data.frame(ewX,ewY)
pj_6 <- project(ew, proj4string, inverse=TRUE)
print(pj_6)

loc = list(x=c(766824.2,755182.3,708614.7,667868.0,354991.9,353536.7,592195.7),
           y=c(7751607,7679137,7635654,7611937,7614572,7908407,7903136))
locin_moss = in.polygon(moss$XCOO,moss$YCOO,loc$x,loc$y) 
locin_ohorizon = in.polygon(ohorizon$XCOO,ohorizon$YCOO,loc$x,loc$y)
locin_bhorizon = in.polygon(bhorizon$XCOO,bhorizon$YCOO,loc$x,loc$y)
locin_chorizon = in.polygon(chorizon$XCOO,chorizon$YCOO,loc$x,loc$y)
locin_topsoil = in.polygon(topsoil$XCOO,topsoil$YCOO,loc$x,loc$y)

ref=c(616307.2, 7716277)
moss_locin = moss%>%filter(locin_moss)%>%mutate(distance = sqrt((XCOO-ref[1])^2+(YCOO-ref[2])^2)/1000)
ohorizon_locin = ohorizon%>%filter(locin_ohorizon)%>%mutate(distance = sqrt((XCOO-ref[1])^2+(YCOO-ref[2])^2)/1000)
bhorizon_locin = bhorizon%>%filter(locin_bhorizon)%>%mutate(distance = sqrt((XCOO-ref[1])^2+(YCOO-ref[2])^2)/1000)
chorizon_locin = chorizon%>%filter(locin_chorizon)%>%mutate(distance = sqrt((XCOO-ref[1])^2+(YCOO-ref[2])^2)/1000)
topsoil_locin = topsoil%>%filter(locin_topsoil)%>%mutate(distance = sqrt((XCOO-ref[1])^2+(YCOO-ref[2])^2)/1000)

loc = as.data.frame(loc)
pj_7 <- project(loc, proj4string, inverse=TRUE)
print(pj_7)

#location of the sites 
lat = c(69.579275,67.58325 )
lon = c(30.686939,32.53156)
latlon_location = data.frame(lon = lon,lat = lat)

#get base map
register_google(key = "AIzaSyDKqz868onrYFKnvGv6UVUrXfSIwq4e-Zg")
map <- get_googlemap(c(30, 69),zoom = 6,scale = 4)




ui <- fluidPage(
  titlePanel("Kola Project"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from 1998 Kola Project survey area.
               Show elements level in in different regions.
               The red dots on the picture are the locations of 
               two main pollution sources:Monchegorsk and Nikel/Zapoljarnij"),
      
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
     
     selectInput("elements2", 
                 label = "Choose the second chemical elements to compare ",
                 choices = c("Ni","Ag", "Al","As", "Au","B","Ba","Be","Bi","Ca","Cd","Co",
                             "Cr","Cu","Fe","Hg","K","La","Mg","Mn","Mo","Na","Ni","p",
                             "Pb","Pd","Pt","Rb","S","Sb","Sc","Se","Si","Sr","Th","Tl","U","V","Y","Zn"
                 ),
                 selected = "Ni"),
     width = 3
     ),
     mainPanel(plotOutput("gmplot"))
    
  )
)


server <- function(input, output) {
  output$gmplot  <- renderPlot({
    
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
    elements2 <- switch(input$elements2,
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
    
    X_range = moss$long[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Y_range = moss$lat[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Ni_M= moss$Ni[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Ag_M= moss$Ag[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Al_M=moss$Al[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    As_M=moss$Au[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Au_M=moss$Au[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    B_M=moss$B[(moss$lat>67.46288) & (moss$lat < 68.35282)]                     
    Ba_M=moss$Ba[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Be_M=moss$Be[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Bi_M=moss$Bi[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Ca_M=moss$Ca[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Cd_M=moss$Cd[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Co_M=moss$Co[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Cr_M=moss$Cr[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Cu_M=moss$Cu[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Fe_M=moss$Fe[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Hg_M=moss$Hg[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    K_M=moss$K[(moss$lat>67.46288) & (moss$lat < 68.35282)]                      
    La_M=moss$La[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Mg_M=moss$Mg[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Mn_M=moss$Mn[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Mo_M=moss$Mo[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Na_M=moss$Na[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Ni_M=moss$Ni[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    P_M=moss$P[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Pb_M=moss$Pb[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Pd_M=moss$Pd[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Pt_M=moss$Pt[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Rb_M=moss$Rb[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    S_M=moss$S[(moss$lat>67.46288) & (moss$lat < 68.35282)]                      
    Sb_M=moss$Sb[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Sc_M=moss$Sc[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Se_M=moss$Se[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Si_M=moss$Si[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Sr_M=moss$Sr[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Th_M=moss$Th[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Tl_M=moss$Tl[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    U_M=moss$U[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    V_M=moss$V[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Y_M=moss$Y[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    Zn_M=moss$Zn[(moss$lat>67.46288) & (moss$lat < 68.35282)]
    distance = (moss$XCOO[(moss$lat>67.46288) & (moss$lat < 68.35282)]-753970)/1000
    data_plot1 = data.frame(X_range,Y_range,distance,Ni_M,Ag_M, Al_M,As_M, Au_M,B_M,Ba_M,Be_M,Bi_M,Ca_M,Cd_M,Co_M,
                            Cr_M,Cu_M,Fe_M,Hg_M,K_M,La_M,Mg_M,Mn_M,Mo_M,Na_M,Ni_M,P_M,
                            Pb_M,Pd_M,Pt_M,Rb_M,S_M,Sb_M,Sc_M,Se_M,Si_M,Sr_M,Th_M,Tl_M,U_M,V_M,Y_M,Zn_M)
    distance_M = data_plot1$distance
    elements_M = switch(input$elements,
                        "Ni"= data_plot1$Ni_M,
                        "Ag"= data_plot1$Ag_M,
                        "Al"=data_plot1$Al_M,
                        "As"=data_plot1$Au_M,
                        "Au"=data_plot1$Au_M,
                        "B"=data_plot1$B_M,
                        "Ba"=data_plot1$Ba_M,
                        "Be"=data_plot1$Be_M,
                        "Bi"=data_plot1$Bi_M,
                        "Ca"=data_plot1$Ca_M,
                        "Cd"=data_plot1$Cd_M,
                        "Co"=data_plot1$Co_M,
                        "Cr"=data_plot1$Cr_M,
                        "Cu"=data_plot1$Cu_M,
                        "Fe"=data_plot1$Fe_M,
                        "Hg"=data_plot1$Hg_M,
                        "K"=data_plot1$K_M,
                        "La"=data_plot1$La_M,
                        "Mg"=data_plot1$Mg_M,
                        "Mn"=data_plot1$Mn_M,
                        "Mo"=data_plot1$Mo_M,
                        "Na"=data_plot1$Na_M,
                        "Ni"=data_plot1$Ni_M,
                        "P"=data_plot1$P_M,
                        "Pb"=data_plot1$Pb_M,
                        "Pd"=data_plot1$Pd_M,
                        "Pt"=data_plot1$Pt_M,
                        "Rb"=data_plot1$Rb_M,
                        "S"=data_plot1$S_M,
                        "Sb"=data_plot1$Sb_M,
                        "Sc"=data_plot1$Sc_M,
                        "Se"=data_plot1$Se_M,
                        "Si"=data_plot1$Si_M,
                        "Sr"=data_plot1$Sr_M,
                        "Th"=data_plot1$Th_M,
                        "Tl"=data_plot1$Tl_M,
                        "U"=data_plot1$U_M,
                        "V"=data_plot1$V_M,
                        "Y"=data_plot1$Y_M,
                        "Zn"=data_plot1$Zn)
    data_plot2 = moss_locin
    distance_Z = moss_locin$distance
    elements_Z <- switch(input$elements,
                         "Ni"= moss_locin$Ni,
                         "Ag"= moss_locin$Ag,
                         "Al"=moss_locin$Al,
                         "As"=moss_locin$Au,
                         "Au"=moss_locin$Au,
                         "B"=moss_locin$B,
                         "Ba"=moss_locin$Ba,
                         "Be"=moss_locin$Be,
                         "Bi"=moss_locin$Bi,
                         "Ca"=moss_locin$Ca,
                         "Cd"=moss_locin$Cd,
                         "Co"=moss_locin$Co,
                         "Cr"=moss_locin$Cr,
                         "Cu"=moss_locin$Cu,
                         "Fe"=moss_locin$Fe,
                         "Hg"=moss_locin$Hg,
                         "K"=moss_locin$K,
                         "La"=moss_locin$La,
                         "Mg"=moss_locin$Mg,
                         "Mn"=moss_locin$Mn,
                         "Mo"=moss_locin$Mo,
                         "Na"=moss_locin$Na,
                         "Ni"=moss_locin$Ni,
                         "P"=moss_locin$P,
                         "Pb"=moss_locin$Pb,
                         "Pd"=moss_locin$Pd,
                         "Pt"=moss_locin$Pt,
                         "Rb"=moss_locin$Rb,
                         "S"=moss_locin$S,
                         "Sb"=moss_locin$Sb,
                         "Sc"=moss_locin$Sc,
                         "Se"=moss_locin$Se,
                         "Si"=moss_locin$Si,
                         "Sr"=moss_locin$Sr,
                         "Th"=moss_locin$Th,
                         "Tl"=moss_locin$Tl,
                         "U"=moss_locin$U,
                         "V"=moss_locin$V,
                         "Y"=moss_locin$Y,
                         "Zn"=moss_locin$Zn)
    
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
      elements2 <- switch(input$elements2,
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
      X_range = ohorizon$long[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Y_range = ohorizon$lat[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Ni_M= ohorizon$Ni[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Ag_M= ohorizon$Ag[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Al_M=ohorizon$Al[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      As_M=ohorizon$Au[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Au_M=ohorizon$Au[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      B_M=ohorizon$B[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]                     
      Ba_M=ohorizon$Ba[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Be_M=ohorizon$Be[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Bi_M=ohorizon$Bi[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Ca_M=ohorizon$Ca[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Cd_M=ohorizon$Cd[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Co_M=ohorizon$Co[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Cr_M=ohorizon$Cr[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Cu_M=ohorizon$Cu[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Fe_M=ohorizon$Fe[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Hg_M=ohorizon$Hg[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      K_M=ohorizon$K[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]                      
      La_M=ohorizon$La[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Mg_M=ohorizon$Mg[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Mn_M=ohorizon$Mn[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Mo_M=ohorizon$Mo[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Na_M=ohorizon$Na[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Ni_M=ohorizon$Ni[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      P_M=ohorizon$P[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Pb_M=ohorizon$Pb[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Pd_M=ohorizon$Pd[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Pt_M=ohorizon$Pt[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Rb_M=ohorizon$Rb[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      S_M=ohorizon$S[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]                      
      Sb_M=ohorizon$Sb[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Sc_M=ohorizon$Sc[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Se_M=ohorizon$Se[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Si_M=ohorizon$Si[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Sr_M=ohorizon$Sr[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Th_M=ohorizon$Th[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Tl_M=ohorizon$Tl[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      U_M=ohorizon$U[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      V_M=ohorizon$V[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Y_M=ohorizon$Y[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      Zn_M=ohorizon$Zn[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]
      distance = (ohorizon$XCOO[(ohorizon$lat>67.46288) & (ohorizon$lat < 68.35282)]-753970)/1000
      data_plot1 = data.frame(X_range,Y_range,distance,Ni_M,Ag_M, Al_M,As_M, Au_M,B_M,Ba_M,Be_M,Bi_M,Ca_M,Cd_M,Co_M,
                              Cr_M,Cu_M,Fe_M,Hg_M,K_M,La_M,Mg_M,Mn_M,Mo_M,Na_M,Ni_M,P_M,
                              Pb_M,Pd_M,Pt_M,Rb_M,S_M,Sb_M,Sc_M,Se_M,Si_M,Sr_M,Th_M,Tl_M,U_M,V_M,Y_M,Zn_M)
      distance_M = data_plot1$distance
      elements_M = switch(input$elements,
                          "Ni"= data_plot1$Ni_M,
                          "Ag"= data_plot1$Ag_M,
                          "Al"=data_plot1$Al_M,
                          "As"=data_plot1$Au_M,
                          "Au"=data_plot1$Au_M,
                          "B"=data_plot1$B_M,
                          "Ba"=data_plot1$Ba_M,
                          "Be"=data_plot1$Be_M,
                          "Bi"=data_plot1$Bi_M,
                          "Ca"=data_plot1$Ca_M,
                          "Cd"=data_plot1$Cd_M,
                          "Co"=data_plot1$Co_M,
                          "Cr"=data_plot1$Cr_M,
                          "Cu"=data_plot1$Cu_M,
                          "Fe"=data_plot1$Fe_M,
                          "Hg"=data_plot1$Hg_M,
                          "K"=data_plot1$K_M,
                          "La"=data_plot1$La_M,
                          "Mg"=data_plot1$Mg_M,
                          "Mn"=data_plot1$Mn_M,
                          "Mo"=data_plot1$Mo_M,
                          "Na"=data_plot1$Na_M,
                          "Ni"=data_plot1$Ni_M,
                          "P"=data_plot1$P_M,
                          "Pb"=data_plot1$Pb_M,
                          "Pd"=data_plot1$Pd_M,
                          "Pt"=data_plot1$Pt_M,
                          "Rb"=data_plot1$Rb_M,
                          "S"=data_plot1$S_M,
                          "Sb"=data_plot1$Sb_M,
                          "Sc"=data_plot1$Sc_M,
                          "Se"=data_plot1$Se_M,
                          "Si"=data_plot1$Si_M,
                          "Sr"=data_plot1$Sr_M,
                          "Th"=data_plot1$Th_M,
                          "Tl"=data_plot1$Tl_M,
                          "U"=data_plot1$U_M,
                          "V"=data_plot1$V_M,
                          "Y"=data_plot1$Y_M,
                          "Zn"=data_plot1$Zn)
      data_plot2 = ohorizon_locin
      distance_Z = ohorizon_locin$distance
      elements_Z <- switch(input$elements,
                           "Ni"= ohorizon_locin$Ni,
                           "Ag"= ohorizon_locin$Ag,
                           "Al"=ohorizon_locin$Al,
                           "As"=ohorizon_locin$Au,
                           "Au"=ohorizon_locin$Au,
                           "B"=ohorizon_locin$B,
                           "Ba"=ohorizon_locin$Ba,
                           "Be"=ohorizon_locin$Be,
                           "Bi"=ohorizon_locin$Bi,
                           "Ca"=ohorizon_locin$Ca,
                           "Cd"=ohorizon_locin$Cd,
                           "Co"=ohorizon_locin$Co,
                           "Cr"=ohorizon_locin$Cr,
                           "Cu"=ohorizon_locin$Cu,
                           "Fe"=ohorizon_locin$Fe,
                           "Hg"=ohorizon_locin$Hg,
                           "K"=ohorizon_locin$K,
                           "La"=ohorizon_locin$La,
                           "Mg"=ohorizon_locin$Mg,
                           "Mn"=ohorizon_locin$Mn,
                           "Mo"=ohorizon_locin$Mo,
                           "Na"=ohorizon_locin$Na,
                           "Ni"=ohorizon_locin$Ni,
                           "P"=ohorizon_locin$P,
                           "Pb"=ohorizon_locin$Pb,
                           "Pd"=ohorizon_locin$Pd,
                           "Pt"=ohorizon_locin$Pt,
                           "Rb"=ohorizon_locin$Rb,
                           "S"=ohorizon_locin$S,
                           "Sb"=ohorizon_locin$Sb,
                           "Sc"=ohorizon_locin$Sc,
                           "Se"=ohorizon_locin$Se,
                           "Si"=ohorizon_locin$Si,
                           "Sr"=ohorizon_locin$Sr,
                           "Th"=ohorizon_locin$Th,
                           "Tl"=ohorizon_locin$Tl,
                           "U"=ohorizon_locin$U,
                           "V"=ohorizon_locin$V,
                           "Y"=ohorizon_locin$Y,
                           "Zn"=ohorizon_locin$Zn)
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
      elements2 <- switch(input$elements2,
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
      
      X_range = bhorizon$long[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Y_range = bhorizon$lat[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Ni_M= bhorizon$Ni[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Ag_M= bhorizon$Ag[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Al_M=bhorizon$Al[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      As_M=bhorizon$Au[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Au_M=bhorizon$Au[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      B_M=bhorizon$B[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]                     
      Ba_M=bhorizon$Ba[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Be_M=bhorizon$Be[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Bi_M=bhorizon$Bi[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Ca_M=bhorizon$Ca[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Cd_M=bhorizon$Cd[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Co_M=bhorizon$Co[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Cr_M=bhorizon$Cr[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Cu_M=bhorizon$Cu[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Fe_M=bhorizon$Fe[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Hg_M=bhorizon$Hg[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      K_M=bhorizon$K[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]                      
      La_M=bhorizon$La[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Mg_M=bhorizon$Mg[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Mn_M=bhorizon$Mn[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Mo_M=bhorizon$Mo[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Na_M=bhorizon$Na[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Ni_M=bhorizon$Ni[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      P_M=bhorizon$P[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Pb_M=bhorizon$Pb[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Pd_M=bhorizon$Pd[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Pt_M=bhorizon$Pt[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Rb_M=bhorizon$Rb[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      S_M=bhorizon$S[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]                      
      Sb_M=bhorizon$Sb[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Sc_M=bhorizon$Sc[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Se_M=bhorizon$Se[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Si_M=bhorizon$Si[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Sr_M=bhorizon$Sr[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Th_M=bhorizon$Th[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Tl_M=bhorizon$Tl[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      U_M=bhorizon$U[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      V_M=bhorizon$V[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Y_M=bhorizon$Y[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      Zn_M=bhorizon$Zn[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]
      distance = (bhorizon$XCOO[(bhorizon$lat>67.46288) & (bhorizon$lat < 68.35282)]-753970)/1000
      data_plot1 = data.frame(X_range,Y_range,distance,Ni_M,Ag_M, Al_M,As_M, Au_M,B_M,Ba_M,Be_M,Bi_M,Ca_M,Cd_M,Co_M,
                              Cr_M,Cu_M,Fe_M,Hg_M,K_M,La_M,Mg_M,Mn_M,Mo_M,Na_M,Ni_M,P_M,
                              Pb_M,Pd_M,Pt_M,Rb_M,S_M,Sb_M,Sc_M,Se_M,Si_M,Sr_M,Th_M,Tl_M,U_M,V_M,Y_M,Zn_M)
      distance_M = data_plot1$distance
      elements_M = switch(input$elements,
                          "Ni"= data_plot1$Ni_M,
                          "Ag"= data_plot1$Ag_M,
                          "Al"=data_plot1$Al_M,
                          "As"=data_plot1$Au_M,
                          "Au"=data_plot1$Au_M,
                          "B"=data_plot1$B_M,
                          "Ba"=data_plot1$Ba_M,
                          "Be"=data_plot1$Be_M,
                          "Bi"=data_plot1$Bi_M,
                          "Ca"=data_plot1$Ca_M,
                          "Cd"=data_plot1$Cd_M,
                          "Co"=data_plot1$Co_M,
                          "Cr"=data_plot1$Cr_M,
                          "Cu"=data_plot1$Cu_M,
                          "Fe"=data_plot1$Fe_M,
                          "Hg"=data_plot1$Hg_M,
                          "K"=data_plot1$K_M,
                          "La"=data_plot1$La_M,
                          "Mg"=data_plot1$Mg_M,
                          "Mn"=data_plot1$Mn_M,
                          "Mo"=data_plot1$Mo_M,
                          "Na"=data_plot1$Na_M,
                          "Ni"=data_plot1$Ni_M,
                          "P"=data_plot1$P_M,
                          "Pb"=data_plot1$Pb_M,
                          "Pd"=data_plot1$Pd_M,
                          "Pt"=data_plot1$Pt_M,
                          "Rb"=data_plot1$Rb_M,
                          "S"=data_plot1$S_M,
                          "Sb"=data_plot1$Sb_M,
                          "Sc"=data_plot1$Sc_M,
                          "Se"=data_plot1$Se_M,
                          "Si"=data_plot1$Si_M,
                          "Sr"=data_plot1$Sr_M,
                          "Th"=data_plot1$Th_M,
                          "Tl"=data_plot1$Tl_M,
                          "U"=data_plot1$U_M,
                          "V"=data_plot1$V_M,
                          "Y"=data_plot1$Y_M,
                          "Zn"=data_plot1$Zn)
      data_plot2 = bhorizon_locin
      distance_Z = bhorizon_locin$distance
      elements_Z <- switch(input$elements,
                           "Ni"= bhorizon_locin$Ni,
                           "Ag"= bhorizon_locin$Ag,
                           "Al"=bhorizon_locin$Al,
                           "As"=bhorizon_locin$Au,
                           "Au"=bhorizon_locin$Au,
                           "B"=bhorizon_locin$B,
                           "Ba"=bhorizon_locin$Ba,
                           "Be"=bhorizon_locin$Be,
                           "Bi"=bhorizon_locin$Bi,
                           "Ca"=bhorizon_locin$Ca,
                           "Cd"=bhorizon_locin$Cd,
                           "Co"=bhorizon_locin$Co,
                           "Cr"=bhorizon_locin$Cr,
                           "Cu"=bhorizon_locin$Cu,
                           "Fe"=bhorizon_locin$Fe,
                           "Hg"=bhorizon_locin$Hg,
                           "K"=bhorizon_locin$K,
                           "La"=bhorizon_locin$La,
                           "Mg"=bhorizon_locin$Mg,
                           "Mn"=bhorizon_locin$Mn,
                           "Mo"=bhorizon_locin$Mo,
                           "Na"=bhorizon_locin$Na,
                           "Ni"=bhorizon_locin$Ni,
                           "P"=bhorizon_locin$P,
                           "Pb"=bhorizon_locin$Pb,
                           "Pd"=bhorizon_locin$Pd,
                           "Pt"=bhorizon_locin$Pt,
                           "Rb"=bhorizon_locin$Rb,
                           "S"=bhorizon_locin$S,
                           "Sb"=bhorizon_locin$Sb,
                           "Sc"=bhorizon_locin$Sc,
                           "Se"=bhorizon_locin$Se,
                           "Si"=bhorizon_locin$Si,
                           "Sr"=bhorizon_locin$Sr,
                           "Th"=bhorizon_locin$Th,
                           "Tl"=bhorizon_locin$Tl,
                           "U"=bhorizon_locin$U,
                           "V"=bhorizon_locin$V,
                           "Y"=bhorizon_locin$Y,
                           "Zn"=bhorizon_locin$Zn)
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
      elements2 <- switch(input$elements2,
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
      X_range = chorizon$long[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Y_range = chorizon$lat[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Ni_M= chorizon$Ni[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Ag_M= chorizon$Ag[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Al_M=chorizon$Al[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      As_M=chorizon$Au[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Au_M=chorizon$Au[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      B_M=chorizon$B[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]                     
      Ba_M=chorizon$Ba[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Be_M=chorizon$Be[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Bi_M=chorizon$Bi[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Ca_M=chorizon$Ca[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Cd_M=chorizon$Cd[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Co_M=chorizon$Co[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Cr_M=chorizon$Cr[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Cu_M=chorizon$Cu[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Fe_M=chorizon$Fe[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Hg_M=chorizon$Hg[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      K_M=chorizon$K[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]                      
      La_M=chorizon$La[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Mg_M=chorizon$Mg[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Mn_M=chorizon$Mn[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Mo_M=chorizon$Mo[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Na_M=chorizon$Na[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Ni_M=chorizon$Ni[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      P_M=chorizon$P[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Pb_M=chorizon$Pb[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Pd_M=chorizon$Pd[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Pt_M=chorizon$Pt[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Rb_M=chorizon$Rb[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      S_M=chorizon$S[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]                      
      Sb_M=chorizon$Sb[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Sc_M=chorizon$Sc[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Se_M=chorizon$Se[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Si_M=chorizon$Si[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Sr_M=chorizon$Sr[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Th_M=chorizon$Th[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Tl_M=chorizon$Tl[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      U_M=chorizon$U[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      V_M=chorizon$V[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Y_M=chorizon$Y[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      Zn_M=chorizon$Zn[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]
      distance = (chorizon$XCOO[(chorizon$lat>67.46288) & (chorizon$lat < 68.35282)]-753970)/1000
      data_plot1 = data.frame(X_range,Y_range,distance,Ni_M,Ag_M, Al_M,As_M, Au_M,B_M,Ba_M,Be_M,Bi_M,Ca_M,Cd_M,Co_M,
                              Cr_M,Cu_M,Fe_M,Hg_M,K_M,La_M,Mg_M,Mn_M,Mo_M,Na_M,Ni_M,P_M,
                              Pb_M,Pd_M,Pt_M,Rb_M,S_M,Sb_M,Sc_M,Se_M,Si_M,Sr_M,Th_M,Tl_M,U_M,V_M,Y_M,Zn_M)
      distance_M = data_plot1$distance
      elements_M = switch(input$elements,
                          "Ni"= data_plot1$Ni_M,
                          "Ag"= data_plot1$Ag_M,
                          "Al"=data_plot1$Al_M,
                          "As"=data_plot1$Au_M,
                          "Au"=data_plot1$Au_M,
                          "B"=data_plot1$B_M,
                          "Ba"=data_plot1$Ba_M,
                          "Be"=data_plot1$Be_M,
                          "Bi"=data_plot1$Bi_M,
                          "Ca"=data_plot1$Ca_M,
                          "Cd"=data_plot1$Cd_M,
                          "Co"=data_plot1$Co_M,
                          "Cr"=data_plot1$Cr_M,
                          "Cu"=data_plot1$Cu_M,
                          "Fe"=data_plot1$Fe_M,
                          "Hg"=data_plot1$Hg_M,
                          "K"=data_plot1$K_M,
                          "La"=data_plot1$La_M,
                          "Mg"=data_plot1$Mg_M,
                          "Mn"=data_plot1$Mn_M,
                          "Mo"=data_plot1$Mo_M,
                          "Na"=data_plot1$Na_M,
                          "Ni"=data_plot1$Ni_M,
                          "P"=data_plot1$P_M,
                          "Pb"=data_plot1$Pb_M,
                          "Pd"=data_plot1$Pd_M,
                          "Pt"=data_plot1$Pt_M,
                          "Rb"=data_plot1$Rb_M,
                          "S"=data_plot1$S_M,
                          "Sb"=data_plot1$Sb_M,
                          "Sc"=data_plot1$Sc_M,
                          "Se"=data_plot1$Se_M,
                          "Si"=data_plot1$Si_M,
                          "Sr"=data_plot1$Sr_M,
                          "Th"=data_plot1$Th_M,
                          "Tl"=data_plot1$Tl_M,
                          "U"=data_plot1$U_M,
                          "V"=data_plot1$V_M,
                          "Y"=data_plot1$Y_M,
                          "Zn"=data_plot1$Zn)
      data_plot2 = chorizon_locin
      distance_Z = chorizon_locin$distance
      elements_Z <- switch(input$elements,
                           "Ni"= chorizon_locin$Ni,
                           "Ag"= chorizon_locin$Ag,
                           "Al"=chorizon_locin$Al,
                           "As"=chorizon_locin$Au,
                           "Au"=chorizon_locin$Au,
                           "B"=chorizon_locin$B,
                           "Ba"=chorizon_locin$Ba,
                           "Be"=chorizon_locin$Be,
                           "Bi"=chorizon_locin$Bi,
                           "Ca"=chorizon_locin$Ca,
                           "Cd"=chorizon_locin$Cd,
                           "Co"=chorizon_locin$Co,
                           "Cr"=chorizon_locin$Cr,
                           "Cu"=chorizon_locin$Cu,
                           "Fe"=chorizon_locin$Fe,
                           "Hg"=chorizon_locin$Hg,
                           "K"=chorizon_locin$K,
                           "La"=chorizon_locin$La,
                           "Mg"=chorizon_locin$Mg,
                           "Mn"=chorizon_locin$Mn,
                           "Mo"=chorizon_locin$Mo,
                           "Na"=chorizon_locin$Na,
                           "Ni"=chorizon_locin$Ni,
                           "P"=chorizon_locin$P,
                           "Pb"=chorizon_locin$Pb,
                           "Pd"=chorizon_locin$Pd,
                           "Pt"=chorizon_locin$Pt,
                           "Rb"=chorizon_locin$Rb,
                           "S"=chorizon_locin$S,
                           "Sb"=chorizon_locin$Sb,
                           "Sc"=chorizon_locin$Sc,
                           "Se"=chorizon_locin$Se,
                           "Si"=chorizon_locin$Si,
                           "Sr"=chorizon_locin$Sr,
                           "Th"=chorizon_locin$Th,
                           "Tl"=chorizon_locin$Tl,
                           "U"=chorizon_locin$U,
                           "V"=chorizon_locin$V,
                           "Y"=chorizon_locin$Y,
                           "Zn"=chorizon_locin$Zn)
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
      elements2 <- switch(input$elements2,
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
      
      X_range = topsoil$long[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Y_range = topsoil$lat[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Ni_M= topsoil$Ni[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Ag_M= topsoil$Ag[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Al_M=topsoil$Al[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      As_M=topsoil$Au[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Au_M=topsoil$Au[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      B_M=topsoil$B[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]                     
      Ba_M=topsoil$Ba[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Be_M=topsoil$Be[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Bi_M=topsoil$Bi[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Ca_M=topsoil$Ca[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Cd_M=topsoil$Cd[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Co_M=topsoil$Co[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Cr_M=topsoil$Cr[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Cu_M=topsoil$Cu[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Fe_M=topsoil$Fe[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Hg_M=topsoil$Hg[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      K_M=topsoil$K[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]                      
      La_M=topsoil$La[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Mg_M=topsoil$Mg[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Mn_M=topsoil$Mn[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Mo_M=topsoil$Mo[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Na_M=topsoil$Na[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Ni_M=topsoil$Ni[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      P_M=topsoil$P[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Pb_M=topsoil$Pb[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Pd_M=topsoil$Pd[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Pt_M=topsoil$Pt[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Rb_M=topsoil$Rb[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      S_M=topsoil$S[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]                      
      Sb_M=topsoil$Sb[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Sc_M=topsoil$Sc[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Se_M=topsoil$Se[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Si_M=topsoil$Si[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Sr_M=topsoil$Sr[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Th_M=topsoil$Th[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Tl_M=topsoil$Tl[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      U_M=topsoil$U[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      V_M=topsoil$V[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Y_M=topsoil$Y[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      Zn_M=topsoil$Zn[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]
      distance = (topsoil$XCOO[(topsoil$lat>67.46288) & (topsoil$lat < 68.35282)]-753970)/1000
      data_plot1 = data.frame(X_range,Y_range,distance,Ni_M,Ag_M, Al_M,As_M, Au_M,B_M,Ba_M,Be_M,Bi_M,Ca_M,Cd_M,Co_M,
                              Cr_M,Cu_M,Fe_M,Hg_M,K_M,La_M,Mg_M,Mn_M,Mo_M,Na_M,Ni_M,P_M,
                              Pb_M,Pd_M,Pt_M,Rb_M,S_M,Sb_M,Sc_M,Se_M,Si_M,Sr_M,Th_M,Tl_M,U_M,V_M,Y_M,Zn_M)
      distance_M = data_plot1$distance
      elements_M = switch(input$elements,
                          "Ni"= data_plot1$Ni_M,
                          "Ag"= data_plot1$Ag_M,
                          "Al"=data_plot1$Al_M,
                          "As"=data_plot1$Au_M,
                          "Au"=data_plot1$Au_M,
                          "B"=data_plot1$B_M,
                          "Ba"=data_plot1$Ba_M,
                          "Be"=data_plot1$Be_M,
                          "Bi"=data_plot1$Bi_M,
                          "Ca"=data_plot1$Ca_M,
                          "Cd"=data_plot1$Cd_M,
                          "Co"=data_plot1$Co_M,
                          "Cr"=data_plot1$Cr_M,
                          "Cu"=data_plot1$Cu_M,
                          "Fe"=data_plot1$Fe_M,
                          "Hg"=data_plot1$Hg_M,
                          "K"=data_plot1$K_M,
                          "La"=data_plot1$La_M,
                          "Mg"=data_plot1$Mg_M,
                          "Mn"=data_plot1$Mn_M,
                          "Mo"=data_plot1$Mo_M,
                          "Na"=data_plot1$Na_M,
                          "Ni"=data_plot1$Ni_M,
                          "P"=data_plot1$P_M,
                          "Pb"=data_plot1$Pb_M,
                          "Pd"=data_plot1$Pd_M,
                          "Pt"=data_plot1$Pt_M,
                          "Rb"=data_plot1$Rb_M,
                          "S"=data_plot1$S_M,
                          "Sb"=data_plot1$Sb_M,
                          "Sc"=data_plot1$Sc_M,
                          "Se"=data_plot1$Se_M,
                          "Si"=data_plot1$Si_M,
                          "Sr"=data_plot1$Sr_M,
                          "Th"=data_plot1$Th_M,
                          "Tl"=data_plot1$Tl_M,
                          "U"=data_plot1$U_M,
                          "V"=data_plot1$V_M,
                          "Y"=data_plot1$Y_M,
                          "Zn"=data_plot1$Zn)
      data_plot2 = topsoil_locin
      distance_Z = topsoil_locin$distance
      elements_Z <- switch(input$elements,
                           "Ni"= topsoil_locin$Ni,
                           "Ag"= topsoil_locin$Ag,
                           "Al"=topsoil_locin$Al,
                           "As"=topsoil_locin$Au,
                           "Au"=topsoil_locin$Au,
                           "B"=topsoil_locin$B,
                           "Ba"=topsoil_locin$Ba,
                           "Be"=topsoil_locin$Be,
                           "Bi"=topsoil_locin$Bi,
                           "Ca"=topsoil_locin$Ca,
                           "Cd"=topsoil_locin$Cd,
                           "Co"=topsoil_locin$Co,
                           "Cr"=topsoil_locin$Cr,
                           "Cu"=topsoil_locin$Cu,
                           "Fe"=topsoil_locin$Fe,
                           "Hg"=topsoil_locin$Hg,
                           "K"=topsoil_locin$K,
                           "La"=topsoil_locin$La,
                           "Mg"=topsoil_locin$Mg,
                           "Mn"=topsoil_locin$Mn,
                           "Mo"=topsoil_locin$Mo,
                           "Na"=topsoil_locin$Na,
                           "Ni"=topsoil_locin$Ni,
                           "P"=topsoil_locin$P,
                           "Pb"=topsoil_locin$Pb,
                           "Pd"=topsoil_locin$Pd,
                           "Pt"=topsoil_locin$Pt,
                           "Rb"=topsoil_locin$Rb,
                           "S"=topsoil_locin$S,
                           "Sb"=topsoil_locin$Sb,
                           "Sc"=topsoil_locin$Sc,
                           "Se"=topsoil_locin$Se,
                           "Si"=topsoil_locin$Si,
                           "Sr"=topsoil_locin$Sr,
                           "Th"=topsoil_locin$Th,
                           "Tl"=topsoil_locin$Tl,
                           "U"=topsoil_locin$U,
                           "V"=topsoil_locin$V,
                           "Y"=topsoil_locin$Y,
                           "Zn"=topsoil_locin$Zn)
    }
      p1 <- ggmap(map)+geom_point(aes(x=long, y = lat, size = elemets,fill = elemets), data = data_set,shape=21, alpha=0.8)+
        geom_point(data=latlon_location, aes(x=lon,y=lat), color='red',size=3)+
        scale_fill_gradient(low = "white", high = "blue")+
        scale_size_continuous(range = c(1, 7), breaks=pretty_breaks(5))+
        labs(title = paste("the distribution of",input$elements,"in",input$Measurements,"soils of the Kola Project area"),
             x = "longitude", y = "latitude",
             caption ="displayed in a linear relationship between analytical value and proportional dot size") 
      
      p2 <-  ggmap(map)+geom_point(aes(x=long, y = lat, size = elemets,fill = elemets), data = data_set,shape=21, alpha=0.8)+
        geom_point(data=latlon_location, aes(x=lon,y=lat), color='red',size=3)+
        scale_fill_gradient(low = "white", high = "blue")+
        scale_size_continuous(range = c(1, 15), breaks=pretty_breaks(7))+
        labs(title = paste("the distribution of",input$elements,"in",input$Measurements,"soils of the Kola Project area"),
             x = "longitude", y = "latitude",
             caption ="displayed in a exponential relationship which focus on large value") 
      
      p3 <- ggplot(data_set,aes(x=elemets,y=elements2))+geom_point()+
        labs(title = paste("the relationship between",input$elements,"and",input$elements2),
             x = paste(input$elements,"in",input$Measurements,"[mg/kg]"),
             y = paste(input$elements2,"in",input$Measurements,"[mg/kg]"),
             caption ="displayed in real value")+
        theme_gdocs()+ scale_color_gdocs()
      
      
      p4 <- ggplot(data_set,aes(x=log10(elemets),y=log10(elements2))) + geom_point()+
        labs(title = paste("the relationship between",input$elements,"and",input$elements2),
             x = paste(input$elements,"in",input$Measurements,"[mg/kg]"),
             y = paste(input$elements2,"in",input$Measurements,"[mg/kg]"),
             caption ="displayed in log10(x) value")+
        theme_gdocs()+ scale_color_gdocs()
        
      
      
      
      p5<- ggmap(map)+geom_point(aes(x=X_range,y=Y_range), data = data_plot1, shape=21, alpha=0.8)+
        geom_point(data=latlon_location, aes(x=lon[2],y=lat[2]), color='red',size=3)+
        geom_hline(yintercept=67.46288)+
        geom_hline(yintercept=68.35282)+
        labs(title = "The sampling area of the samples in the figure on the right",x = "longitude", y = "latitude") 
      
      p6<- ggplot(data_plot1,aes(x=distance_M,y=log10(elements_M)))+geom_point()+
        geom_smooth()+
        labs(title = "The relationship between chemical elements level and the distance",
             x = "Distance from Monchegorsk [km]", y = paste(input$elements,"in",input$Measurements,"[mg/kg]"))+
        theme_gdocs()+ scale_color_gdocs()
    
        
      p7<-ggmap(map)+geom_point(aes(x=long,y=lat), data = data_plot2, shape=21, alpha=0.8)+
        geom_segment(aes(x=24, y=68.5, xend=24, yend=71.3))+
        geom_segment(aes(x=24, y=71.3, xend=28, yend=71.3))+
        geom_segment(aes(x=28, y=71.3, xend=34, yend=69.4))+
        geom_segment(aes(x=34, y=69.4, xend=32, yend=68.4))+
        geom_segment(aes(x=24, y=68.5, xend=32, yend=68.4))+
        geom_point(data=latlon_location, aes(x=lon[1],y=lat[1]), color='red',size=3)+
        labs(title = "The sampling area of the samples in the figure on the right",x = "longitude", y = "latitude")
      
      p8<-ggplot(data_plot2,aes(x=distance_Z,y=log10(elements_Z)))+geom_point()+
        geom_smooth(method='lm', formula= y~x)+
        labs(title = "The relationship between chemical elements level and the distance",
             x = "Distance from Nikel/Zapoljarnij [km]", y = paste(input$elements,"in",input$Measurements,"[mg/kg]"))+
        theme_gdocs()+ scale_color_gdocs()
     
      
      grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
      
    },height = 1500, width = 1500)
}
  
shinyApp(ui,server)
