#Aplicacion Web Shiny para ¿Quien nos representa?
#Creada en 2019 por Daniel Alvarez Hernandez

require(shiny)
library(png)
library(magick)
library(ggplot2)


diputados<-read.csv2("diputados.txt",encoding = "latin1")

coord_psoe<-c("+359+611","+384+636","+209+286","+209+311","+209+336","+209+361","+209+386","+209+411","+209+436","+209+461","+184+286","+184+311","+185+336","+184+361","+184+386","+184+411","+184+436","+184+461","+159+261","+159+286","+159+311","+159+336","+159+361","+159+386","+159+411","+159+436","+159+461","+159+486","+134+261","+134+286","+134+311","+134+336","+134+361","+134+386","+134+411","+134+436","+134+461","+134+486","+109+261","+109+286","+109+311","+109+336","+109+361","+109+386","+109+411","+109+436","+109+461","+109+486","+109+511","+109+536","+109+561","+109+586","+109+611","+84+261","+84+286","+84+311","+84+336","+84+361","+84+386","+84+411","+84+436","+84+461","+84+486","+84+511","+84+536","+84+561","+84+586","+84+611","+59+261","+59+286","+59+311","+59+336","+59+361","+59+386","+59+411","+59+436","+59+461","+59+486","+59+511","+59+536","+59+561","+59+586","+59+611","+59+636")

coord_podemos<-c("+359+636","+483+611","+359+211","+334+186","+334+211","+309+136","+309+161","+309+186","+309+211","+284+136","+284+161","+284+186","+284+211","+284+236","+259+86","+259+111","+259+136","+259+161","+259+186","+259+211","+259+236","+259+261","+234+86","+234+111","+234+136","+234+161","+234+186","+234+211","+234+236","+234+261","+209+86","+209+111","+209+136","+209+161","+209+186","+209+211","+209+236","+209+261","+184+111","+184+136","+184+161","+184+186","+184+211","+184+236","+184+261","+159+111","+159+136","+159+161","+159+186","+159+211","+159+236","+134+136","+134+161","+134+186","+134+211","+134+236","+109+161","+109+186","+109+211","+109+236","+84+161","+84+186","+84+211","+84+236","+59+186","+59+211","+59+236")

coord_ciudadanos<-c("+332+611","+431+636","+534+86","+509+86","+509+111","+509+136","+509+161","+509+186","+509+211","+484+86","+484+111","+484+136","+484+161","+484+186","+484+211","+459+86","+459+111","+459+136","+459+161","+459+186","+459+211","+434+86","+434+111","+434+136","+434+161","+434+186","+434+211","+409+111","+409+136","+409+161","+409+186","+409+211")

coord_pp<-c("+406+636","+456+636","+456+611","+509+236","+734+186","+734+211","+734+236","+734+261","+734+286","+734+311","+734+336","+734+361","+734+386","+734+411","+734+436","+734+461","+734+486","+734+511","+709+161","+709+186","+709+211","+709+236","+709+261","+709+286","+709+311","+709+336","+709+361","+709+386","+709+411","+709+436","+709+461","+709+486","+709+511","+709+536","+709+561","+709+586","+709+611","+684+161","+684+186","+684+211","+684+236","+684+261","+684+286","+684+311","+684+336","+684+361","+684+386","+684+411","+684+436","+684+461","+684+486","+684+511","+684+536","+684+561","+684+586","+684+611","+659+136","+659+161","+659+186","+659+211","+659+236","+659+261","+659+286","+659+311","+659+336","+659+361","+659+386","+659+411","+659+436","+659+461","+659+486","+634+111","+634+136","+634+161","+634+186","+634+211","+634+236","+634+261","+634+286","+634+311","+634+336","+634+361","+634+386","+634+411","+634+436","+634+461","+634+486","+609+111","+609+136","+609+161","+609+186","+609+211","+609+236","+609+261","+609+286","+609+311","+609+336","+609+361","+609+386","+609+411","+609+436","+609+461","+584+86","+584+111","+584+136","+584+161","+584+186","+584+211","+584+236","+584+261","+584+286","+584+311","+584+336","+584+361","+584+386","+584+411","+584+436","+584+461","+559+86","+559+111","+559+136","+559+161","+559+186","+559+211","+559+236","+559+261","+559+286","+534+111","+534+136","+534+161","+534+186","+534+211","+534+236","+534+261")

coord_pnv<-c("+734+636","+734+611","+734+586","+734+561","+734+536")

coord_bildu<-c("+484+61","459+61")

coord_esquerra<-c("+434+61","+409+61","+409+86","+384+86","+384+111","+384+136","+384+161","+384+186","+384+211")

coord_pdecat<-c("+384+61","+334+61","+359+61","+359+86","+359+111","+359+136","+359+161","+359+186")

coord_upv<-c("+334+86","+334+111","+334+136","+334+161")

coord_upn<-c("+284+86","+284+111")

coord_ccanaria<-c("+309+61")

coord_ncanarias<-c("+309+86")

coord_foro<-c("+309+111")




congreso<- image_read("congreso.png")

# imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "pink",
#                degrees = 0, location = "+483+636")
# image_write(imagen,"hola.png")


# Define UI for application that draws a histogram
ui <- fluidPage(
   fluidRow(
     titlePanel("••••••••••••••••••••••••••••••••••••••• ¿QUIÉN NOS REPRESENTA?••••••••••••••••••••••••••••••••••••••")
   ),
   fluidRow(
    
     column(3,
           
            radioButtons("sexo", label = "Seleccione su sexo",
                         choices = list("All"="All" ,"Hombre" = "Hombre", "Mujer" = "Mujer"), 
                         selected = "All")
                
              ),
     column(2,
            
             radioButtons("estado_civil", label = "Seleccione su estado civil",
                         choices = list("All"="All","Soltero/a" = "Soltero/a","Pareja de Hecho"="Pareja de hecho" ,"Casado/a" = "Casado/a", "Divorciado/a (Separado/a)" = "Divorciado/a","Viudo/a"="Viudo/a"),inline = TRUE ,
                         selected = "All")
            
     ),
     column(2,
            
            selectInput("edad", label = "Seleccione su edad", 
                        choices = list("All"="All","25-29" = "25-29", "30-34" = "30-34", "35-39" = "35-39","40-44"="40-44","45-49"="45-49","50-54"="50-54","55-59"="55-59","60-64"="60-64","65-69"="65-69","70-74"="70-74","75-79"="75-79"), 
                        selected = "All")
            
     ),
     column(2,
           
            selectInput("origen", label = "Seleccione su Provincia", 
                        choices = list("All"="All","A Coruña"="A Coruña","Álava"="Álava","Albacete"="Albacete","Alicante"="Alicante","Almería"="Almería","Asturias"="Asturias","Ávila"="Ávila","Badajoz"="Badajoz","Barcelona"="Barcelona","Bizkaia"="Bizkaia","Burgos"="Burgos","Cáceres"="Cáceres","Cádiz"="Cádiz","Cantabria"="Cantabria","Castellón"="Castellón","Ceuta"="Ceuta","Ciudad Real"="Ciudad Real","Córdoba"="Córdoba","Cuenca"="Cuenca","Gipuzkoa"="Gipuzkoa","Girona"="Girona","Granada"="Granada","Guadalajara"="Guadalajara","Huelva"="Huelva","Huesca"="Huesca","Islas Baleares"="Islas Baleares","Jaén"="Jaén","La Rioja"="La Rioja","Las Palmas"="Las Palmas","León"="León","Lleida"="Lleida","Lugo"="Lugo","Madrid"="Madrid","Málaga"="Málaga","Melilla"="Melilla","Murcia"="Murcia","Navarra"="Navarra","Ourense"="Ourense","Palencia"="Palencia","Pontevedra"="Pontevedra","Salamanca"="Salamanca","Santa Cruz de Tenerife"="Santa Cruz de Tenerife","Segovia"="Segovia","Sevilla"="Sevilla","Soria"="Soria","Tarragona"="Tarragona","Teruel"="Teruel","Toledo"="Toledo","Valencia"="Valencia","Valladolid"="Valladolid","Zamora"="Zamora","Zaragoza"="Zaragoza"), 
                        selected = "All")
            
       
     ),
     column(2,
            
            radioButtons("nivel_academico", label = "Seleccione su nivel academico",
                         choices = list("All"="All","Cursando estudios"="Cursando estudios","Educación Superior"= "Educación Superior", "2ª Etapa E.Secundaria con Orientacion Profesional" = "2ª Etapa E.Secundaria con O.Profesional"), inline = TRUE,
                         selected = "All")
            
       
       
     )
   ),
   fluidRow(
     column(6,
     
            
            # In a imageOutput, passing values for click, dblclick, hover, or brush
            # will enable those interactions.
            imageOutput("image1", height = 700,
                        # Equivalent to: click = clickOpts(id = "image_click")
                        click = "image_click",
                        dblclick = dblclickOpts(
                          id = "image_dblclick"
                        ),
                        hover = hoverOpts(
                          id = "image_hover"
                        ),
                        brush = brushOpts(
                          id = "image_brush"
                        )
            )
            
            
     ),
     column(width = 6,
            
           dataTableOutput("table")
            
       
     )
   ),
   fluidRow(
     column(width = 3,
            verbatimTextOutput("click_info")
     ),
     column(width = 3,
            verbatimTextOutput("dblclick_info")
     ),
     column(width = 3,
            verbatimTextOutput("hover_info")
     ),
     column(width = 3,
            verbatimTextOutput("brush_info")
     )
     
   )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
    
  output$table <- renderDataTable({
    data <- diputados
    contador_pp<-0
    contador_psoe<-0
    contador_podemos<-0
    contador_ciudadanos<-0
    contador_bildu<-0
    contador_eupv<-0
    contador_ccanaria<-0
    contador_pnv<-0
    contador_esquerra<-0
    contador_foro<-0
    contador_ncanarias<-0
    contador_pdecat<-0
    contador_upn<-0
    
    
    if (input$sexo != "All") {
      data <- data[data$SEXO == input$sexo,]
    }
    if (input$estado_civil != "All") {
      data <- data[data$ECIVIL == input$estado_civil,]
    }
    if (input$edad != "All") {
      data <- data[data$RANGO.EDAD == input$edad,]
    }
    if (input$origen != "All") {
      data <- data[data$ORIGEN == input$origen,]
    }
    if (input$nivel_academico != "All") {
      data <- data[data$NIVEL.ACADEMICO == input$nivel_academico,]
    }
    
    
    
    for(i in 1:length(data$G.PARLAMENTARIO)){
      local({ 
      if(data$G.PARLAMENTARIO[i]=="PP"){
        contador_pp=contador_pp+1
      
      }else
      if(data$G.PARLAMENTARIO[i]=="PSOE"){
        contador_psoe=contador_psoe+1
      }else
      if(data$G.PARLAMENTARIO[i]=="Podemos"){
        contador_podemos=contador_podemos+1
      }else
      if(data$G.PARLAMENTARIO[i]=="Ciudadanos"){
        contador_ciudadanos=contador_ciudadanos+1
      }else
      if(data$G.PARLAMENTARIO[i]=="Bildu"){
        contador_bildu=contador_bildu
      }else
      if(data$G.PARLAMENTARIO[i]=="C-P-EUPV"){
        contador_eupv=contador_eupv+1
      }else
      if(data$G.PARLAMENTARIO[i]=="Coalicion Canaria"){
        contador_ccanaria=contador_ccanaria+1
      }else
      if(data$G.PARLAMENTARIO[i]=="EAJ-PNV"){
        contador_pnv=contador_pnv+1
      }else
      if(data$G.PARLAMENTARIO[i]=="Esquerra Republicana"){
        contador_esquerra=contador_esquerra+1
      }else
      if(data$G.PARLAMENTARIO[i]=="Foro(G.Mixto)"){
        contador_foro=contador_foro+1
      }else
      if(data$G.PARLAMENTARIO[i]=="Nueva Canarias"){
        contador_ncanarias=contador_ncanarias+1
      }else
      if(data$G.PARLAMENTARIO[i]=="PdeCat"){
        contador_pdecat=contador_pdecat+1

      }else
      if(data$G.PARLAMENTARIO[i]=="UPN-PP"){
        contador_upn=contador_upn+1
      }
      
        }) #fin local()

    }
    
    
    if(contador_pp<134){
      
      
     for(q in 1:(134-contador_pp)){
       local({ 
       imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                              degrees = 0, location =coord_pp[q] )
       
      
      
       
       })
     }
     
    }
    if(contador_psoe<84){
    
      for(w in 1:(84-contador_psoe)){
        
        imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                               degrees = 0, location =coord_psoe[w] )
        
        image_write(imagen,"hola.png")
        
        
        }
     
    }
    if(contador_podemos<67){
     for(e in 1:(67-contador_podemos)){
       local({ 
       imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                              degrees = 0, location =coord_podemos[e] )
       })
      }
    }
    if(contador_ciudadanos<32){
     for(r in 1:(32-contador_ciudadanos)){
       local({ 
       imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                              degrees = 0, location =coord_ciudadanos[r] )
       })
      }
    }
    if(contador_bildu<2){
     
      for(t in 1:(2-contador_bildu)){
        local({ 
        imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                               degrees = 0, location =coord_bildu[t] )
      })
     }
    }
    if(contador_eupv<4){
      for(u in 1:(4-contador_eupv)){
        local({ 
        imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                               degrees = 0, location =coord_upv[u] )
        })
     }
    }
    if(contador_pnv<5){
     for(o in 1:(5-contador_pnv)){
       local({ 
       imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                              degrees = 0, location =coord_pnv[o] )
       })
      }
    }
    if(contador_esquerra<9){
      for(p in 1:(9-contador_esquerra)){
        local({ 
        imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                               degrees = 0, location ="+734+586" )
        })
        
      }
    }
    if(contador_pdecat<8){
     for(a in 1:(8-contador_pdecat)){
       local({ 
       imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                              degrees = 0, location =coord_pdecat[a] )
       })
      }
    }
    if(contador_upn<2){
      for(s in 1:(2-contador_upn)){
        local({ 
        imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                               degrees = 0, location = coord_upn[s])
        })
      }
    }
    if(contador_foro==0){
    
      imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                             degrees = 0, location = coord_foro[1])
    }
    if(contador_ncanarias==0){
      imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                             degrees = 0, location = coord_ncanarias[1])
    }
    if(contador_ccanaria==0){
      imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
                             degrees = 0, location =coord_ccanaria[1] )
    }
    
    # imagen<-image_annotate(congreso,"...." ,size = 20,color="white", boxcolor = "white",
    #                        degrees = 0, location =coord_psoe[17] )
    # image_write(imagen,"hola.png")
    image_write(imagen,"hola.png")
    data
  },options =list(lengthMenu = c(1,2, 5,25, 50), pageLength = 1,scrollX = TRUE))
  
  
  ##################################
  
  output$image1 <- renderImage({
    
   
    
    
    
     list(
      src = "hola.png",
      height = 600,
      width=800,
      contentType = "image/png",
      alt = "Congreso"
    )
    
  }, deleteFile = FALSE)
  
  output$click_info <- renderPrint({
    cat("input$image_click:\n")
    str(input$image_click)
  })
  output$hover_info <- renderPrint({
    cat("input$image_hover:\n")
    str(input$image_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$image_dblclick:\n")
    str(input$image_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$image_brush:\n")
    str(input$image_brush)
  })
  
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)

