

library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)
library(readxl)
library(reshape)
library(plotly)
library(tidyr)
library(readxl)
library(sf)
library(plotly)
library(shinydashboardPlus)
library(dashboardthemes)


df1 <- read_excel("daabase.xlsx", sheet="4.2.9")
df1 = as.data.frame(df1)
shp <- sf::read_sf("IDN_adm1.shp")
shp["NAME_1"] #mengambil nama dengan titik geografi yang ada
shp1 <- shp %>% 
  left_join(df1, by = c("NAME_1" = "Provinsi")) #mengisi sesuai dengan provinsinya
head(shp1)



df2 <- read_excel("daabase.xlsx", sheet="4.2.4")
shp2 <- shp %>% 
  left_join(df2, by = c("NAME_1" = "Provinsi")) #mengisi sesuai dengan provinsinya
head(shp2)
df2 = as.data.frame(df2)
df3a <- read_excel("daabase.xlsx", sheet="4.2.7")
df3a <- df3a[1:70,1:6]
df3a = as.data.frame(df3a)
df1.3 <- read_excel("daabase.xlsx", sheet="4.2.1")
df1.3 = as.data.frame(df1.3)
df4a <- read_excel("daabase.xlsx", sheet ="4.2.3")

df4a = as.data.frame(df4a)
df3 <- read_excel("daabase.xlsx", sheet="4.2.12")
df3 = as.data.frame(df3)
colnames(df3)=c("provinsi","limabelas","dualima","tigalima","empatlima","limalima","enamlima")
df3 = df3[,-1]
df4 = read_excel("daabase.xlsx", sheet="4.2.3a")
df4 = df4[,-1]
df4 = as.data.frame(df4)
df5 = read_excel("daabase.xlsx", sheet="4.2.1a")
df5=as.data.frame(df5)
df6 = read_excel("daabase.xlsx", sheet="Regresi")
df6 = df6[c(1:34),]
df6 = as.data.frame(df6)

df7<- read_excel("daabase.xlsx", sheet ="4.2.12a")
df8<- read_excel("daabase.xlsx", sheet ="4.2.6")
df7 = as.data.frame(df7)
df8 = as.data.frame(df8)

ui <- dashboardPage(
  skin = "purple-light",
  dashboardHeader(title = "SehatiRakyat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("house")),
      menuItem("Dashboard", tabName = "dashboard",icon = icon("table-columns"),
               menuSubItem("Fasilitas Kesehatan", tabName = "fasilitas"),
               menuSubItem("Epidemiologi", tabName = "epidemiologi"),
               menuSubItem("Kebiasaan Hidup", tabName = "kebiasaan"),
               menuSubItem("Inferensia", tabName="inf")
               
      ),
      menuItem("Database", tabName = "database",icon = icon("database"))
      
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem(tabName = "beranda",
              titlePanel(
                h1(strong("SELAMAT DATANG DI SehatiRakyat!"),
                   style="text-align:center;")),
              br(),
              carousel(width = 12,
                       id = "mycarousel",
                       carouselItem(
                         tags$img(src = "https://1.bp.blogspot.com/-NME67SybCjc/VozlBXmcVPI/AAAAAAAAANg/TDhGcNe3YYw/s1600/edit-100_4072.jpg")
                       ),
                       carouselItem(
                         
                         tags$img(src = "https://orbitdigitaldaily.com/wp-content/uploads/2020/07/PELAYANAN-BPJS-KESEHATAN-OK.jpg")
                       ),
                       carouselItem(
                         
                         tags$img(src = "https://cegahstunting.id/wp-content/uploads/2020/05/Pelayanan-Gizi-dan-Kesehatan-Copyright-UNICEF-Indonesia.jpg")
                       )
              )),
      tabItem(tabName = "dashboard",
              
      ),
      tabItem(tabName = "fasilitas",
              tabsetPanel(
                tabPanel("Rumah Sakit dan Puskesmas",icon=icon("hospital"),
                         fluidRow(box(title=h1(strong("Rumah Sakit dan Puskesmas")), status = "primary", solidHeader = FALSE,
                                      width =12)
                                 
                         ), fluidRow(box(title = strong("Filter Provinsi"),
                                         selectInput("PROVINCEDF1", " ",
                                                     choices = unique(df1$Provinsi))
                         ),box(title = strong("Filter Tahun"),
                               selectInput("TahunDF1", " ",
                                           choices = unique(df1$Tahun))
                         )),
                         fluidRow(
                           infoBoxOutput(outputId = "RSU", width=3),
                           infoBoxOutput(outputId = "RSK", width=3),
                           infoBoxOutput(outputId = "PRI", width=3),
                           infoBoxOutput(outputId= "PRNI", width=3)
                         ),
                         fluidRow(box(title = "Mapchart", width = 8, status = "primary", solidHeader = T,
                                      plotOutput("MAPDF1",height=300)),
                                  box(title=strong("Filter Sarana"), width=4,
                                      radioButtons("SARANADF1", " ",
                                                   choices = c("Rumah Sakit Umum"="RSU", "Rumah Sakit Khusus"="RSK", "Puskesmas Rawat Inap"= "PRI", "Puskesmas Non Rawat Inap" = "PNRI"))
                                  )), 
                         fluidRow(box(title = "TOP", width = 4, status = "primary", solidHeader = T,
                                      plotOutput("BAR1DF1",height=300)),box(title = "BOTTOM", width = 4, status = "primary", solidHeader = T,
                                                                            plotOutput("BAR2DF1",height=300)),box(title = "HISTOGRAM", width = 4, status = "primary", solidHeader = T,
                                                                                                                  plotOutput("HISTDF1",height=300))),
                ),
                
                tabPanel("Tenaga Kerja", icon=icon("user"),
                         fluidRow(box(title=h1(strong("Tenaga Kerja Kesehatan")), status = "primary", solidHeader = FALSE,
                                      width =12)
                                  # Add content for "Rumah Sakit dan Puskesmas" tab here
                         ), fluidRow(box(title = strong("Filter Provinsi"),
                                         selectInput("PROVINCEDF2", " ",
                                                     choices = unique(df2$Provinsi))
                         ),box(title = strong("Filter Tahun"),
                               selectInput("TahunDF2", " ",
                                           choices = unique(df2$Tahun))
                         )),
                         fluidRow(
                           infoBoxOutput(outputId = "TM", width=3),
                           infoBoxOutput(outputId = "TK", width=3),
                           infoBoxOutput(outputId = "PK", width=3),
                           infoBoxOutput(outputId= "TKF", width=3)
                         ),
                         fluidRow(box(title = "Mapchart", width = 8, status = "primary", solidHeader = T,
                                      plotOutput("MAPDF2",height=300)),
                                  box(title = strong("Filter Tenaga Kerja"), width=4,
                                      radioButtons("SARANADF2", " ",
                                                   choices = c("Tenaga Medis"= "Tenaga.Medis","Tenaga Keperawatan"= "Tenaga.Keperawatan", "Tenaga Psikologi Klinis" = "psikologi.klinis", "Tenaga Kefarmasian" = "Tenaga.Kefarmasian"))
                                  )), 
                         fluidRow(box(title = "TOP", width = 4, status = "primary", solidHeader = T,
                                      plotOutput("BAR1DF2",height=300)),
                                  box(title = "BOTTOM", width = 4, status = "primary", solidHeader = T,
                                      plotOutput("BAR2DF2",height=300)),
                                  box(title = "HISTOGRAM", width = 4, status = "primary", solidHeader = T,
                                      plotOutput("HISTDF2",height=300))),
                         
                         
                      
                ),
                tabPanel("Sebaran Sarana Kesehatan", icon = icon("map"),
                         fluidRow(box(title=h1(strong("Sebaran Sarana Kesehatan")), status = "primary", solidHeader = FALSE, width = 12),
                         ),
                         fluidRow(box(title=strong("Filter Tahun"), selectInput("TAHUNDF1.3", " ",
                                                                                choices = unique(df1.3$Tahun)), width = 3),
                                  box(title=strong("Filter Provinsi"), selectInput("PROVINCEDF1.3", " ",
                                                                                   choices = unique(df1.3$Provinsi)), width = 3),
                                  box(title=strong("Filter Sarana"),selectInput("SARANADF1.3", "",
                                                                                choices = unique(df1.3$Sarana)), width = 6)
                         ),
                         fluidRow(box(title="Jumlah Desa yang Memiliki Sarana",width=6,height = 612, status="primary", solidHeader = T,
                                      plotOutput("BARDF1.3")), 
                                  box(width = 6,title = "Perbandingan Jumlah Desa dengan Sarana dan Total Desa", status="primary", solidHeader = T, 
                                      infoBoxOutput(outputId = "Desa",width = 6),
                                      infoBoxOutput(outputId = "Sarana", width = 6),
                                      fluidRow(box(plotOutput("PIEDF1.3"),width=12)
                                      )
                                  )
                         ),
                )
              )
      ),
      tabItem(tabName = "epidemiologi",
              tabsetPanel(
                tabPanel("Penemuan Kasus",icon=icon("virus"),
                         fluidRow(box(title=h1(strong("Penemuan Kasus")), status = "primary", solidHeader = FALSE,
                                      width =12)
                         ),
                         fluidRow(
                           box(title = strong("Filter Provinsi"),
                               selectInput("PROVINCEdf3a", " ",
                                           choices = unique(df3a$Provinsi))
                           ),box(title = strong("Filter Tahun"),
                                 selectInput("TAHUNdf3a", " ",
                                             choices = unique(df3a$Tahun))
                           )
                         ),
                         fluidRow(box(width = 8,height = 300,
                                      fluidRow(
                                        infoBoxOutput(outputId = "AIDS", width = 6),
                                        infoBoxOutput(outputId = "DBD", width = 6)),
                                      hr(),
                                      fluidRow(
                                        infoBoxOutput(outputId = "Malaria", width = 6),
                                        infoBoxOutput(outputId = "KUSTA", width = 6))
                         ),
                         box(title=strong("Filter Penyakit"), selectInput("JENISdf3a", " ",
                                                                          choices=c("AIDS"="Aids", "Kusta"="kusta","Malaria"="malaria","DBD"="dbd")),width = 4),
                         box(title=strong("Rangking"), sliderInput('slider', " ",
                                                                   min =1 , max = 34, value = 15, step = 1), width=4)),
                         fluidRow(box(title = "Skala", width = 12, status = "primary", solidHeader =  T,
                                      plotOutput("BARdf3a", height = 500))
                         )
                
                ),
                tabPanel("Imunisasi Balita", icon=icon("heart"),
                         fluidRow(box(title=h1(strong("Imunitas Balita")), status = "primary", solidHeader = FALSE,
                                      width =12)
                         ),
                         fluidRow(box(title = "Persebaran Distribusi Imunitas Balita", width = 6, height=680, status = "primary", solidHeader = T,
                                      fluidRow(box(title=strong("Filter Tahun"), selectInput("TAHUNdf4a", " ",
                                                                                             choices = unique(df4a$Tahun)),width=6),
                                               box(title=strong("Skala"), sliderInput('slider1',' ',
                                                                                               min=1, max=34,value=10,step=1)),width=6),
                                      plotOutput("HISTdf4a")),
                                  box(title = "Ranking Provinsi dengan Imunisasi Dasar Balita", width = 6, height = 680, status = "primary", solidHeader = T,
                                      fluidRow(box(title=strong("Urutkan Berdasar"), radioButtons("urutan", " ",
                                                                                                  choices = c("Tertinggi", "Terendah"), selected = "Tertinggi"),width=6),
                                               box(title=strong("Banyaknya Provinsi yang Ditampilkan"),sliderInput('slider2', " ",
                                                                                                                   min=1, max=35, value = 5,step=1),width=6),
                                               box(plotOutput("BARdf4a"),width = 12, height = 680))),
                                  
                         ),
                        
                ),
              )
      ),
      tabItem(tabName = "kebiasaan",
              tabPanel("Kebiasaan Hidup",icon=icon("virus"),
                       fluidRow(box(title=h1(strong("Kebiasaan Hidup")), status = "primary", solidHeader = FALSE,
                                    width =12)
                       ),
                       fluidRow(
                         box(title = strong("Filter Provinsi"),
                             selectInput("PROVINCEDF7", " ",
                                         choices = unique(df8$Provinsi))
                         )
                       ),
                       fluidRow(box(title=strong("Perkembangan Keluhan Penyakit pada Masyarakat dari tahun 2018-2022 (%)"), status="primary", solidHeader = T, width = 12,
                                    plotOutput("linedf8"))),
                       fluidRow(box(title=strong("Presentase Perokok Menurut Kelompok Umur pada Tahun 2021 (%)"),status = "primary",solidHeader = TRUE, width=12,
                                    plotOutput("bardf7")))
                       
              )
              # Add content for "Kebiasaan Hidup" tab here
      ),
      tabItem(tabName = "inf",
              tabsetPanel(
                tabPanel("Hipotesis",
                         box(width = 6,
                             fluidRow(
                               box(width = 12,
                                   radioButtons("slctes","Pilih Pengujian",choices = c("Apakah Rata-rata Persentase Penduduk Perokok Berbeda di Setiap Kategori Usia?", "Apakah Telah Terjadi Peningkatan Jumlah Balita yang diimunisasi?", "Bagaimana Perbandingan Proporsi Jumlah Desa yang Memiliki Sarana Kesehatan di Tiap Provinsi?"))
                               )
                             ),
                             fluidRow(
                               box(width = 12,
                                   conditionalPanel(condition = "input.slctes =='Apakah Rata-rata Persentase Penduduk Perokok Berbeda di Setiap Kategori Usia?'",
                                                    
                                                    selectInput("slct1","Pilih variabel 1 Kategori Usia",choices = colnames(df3),multiple = F),
                                                    selectInput("slct2","Pilih Variabel 2 Kategori Usia",choices = colnames(df3),multiple = F),
                                                    sprintf("\\( H_0 : \\mu_1 - \\mu_2 = \\)"),
                                                    numericInput("h0",label = NULL, value = 0, step = 0.1),
                                                    radioButtons("asm","Pilih Asumsi" , choices=c(
                                                      "\\( \\sigma^2_1 = \\sigma^2_2 \\)" = TRUE,
                                                      "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = FALSE
                                                    )),
                                                    sliderInput("alpa","Tingkat Signifikansi \\(\\alpha = \\)",
                                                                min = 0.01,
                                                                max = 0.20,
                                                                value = 0.05)
                                                    
                                   ),
                                   
                                   conditionalPanel(condition = "input.slctes == 'Apakah Telah Terjadi Peningkatan Jumlah Balita yang diimunisasi?'",
                                                    
                                                    selectInput("slct3","Pilih Variabel Tahun yang akan diuji",choices = colnames(df4),multiple = F),
                                                    selectInput("slct4","Ingin dibandingkan dengan tahun berapa?",choices = colnames(df4),multiple = F),
                                                    sprintf("\\( H_0 : \\mu_1 - \\mu_2 > \\)"),
                                                    numericInput("h01",label = NULL, value = 0, step = 0.1),
                                                    radioButtons("asm1","Pilih Asumsi" , choices=c(
                                                      "\\( \\sigma^2_1 = \\sigma^2_2 \\)" = TRUE,
                                                      "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = FALSE
                                                    )),
                                                    sliderInput("alpa1","Tingkat Signifikansi \\(\\alpha = \\)",
                                                                min = 0.01,
                                                                max = 0.20,
                                                                value = 0.05)
                                                    
                                   ),
                                   
                                   conditionalPanel(condition = "input.slctes == 'Bagaimana Perbandingan Proporsi Jumlah Desa yang Memiliki Sarana Kesehatan di Tiap Provinsi?'",
                                                    
                                                    selectInput("slct5","Pilih Provinsi 1",choices = ifelse(colnames(df5)=="Sarana","",colnames(df5)),multiple = F,selected = "Aceh"),
                                                    selectInput("slct51","Pilih Provinsi 2",choices = ifelse(colnames(df5)=="Sarana","",colnames(df5)),multiple = F,selected = "Sumatera Utara"),
                                                    selectInput("slct6","Pilih Sarana",choices = c("Rumah Sakit"="RS","Rumah Sakit Bersalin"="RS_Bersalin","Poliklinik"="Poliklinik","Puskesmas"="Puskesmas","Puskesmas Pembantu"="Puskesmas_Pembantu","Apotek"="Apotek"),multiple = F),
                                                    sprintf("\\( H_0 : p_1 - p_2 > \\)"),
                                                    numericInput("h02",label = NULL, value = 0, step = 0.1),
                                                    checkboxInput("pooled","Gunakan Pooled Standar Error",F),
                                                    sliderInput("alpa2","Tingkat Signifikansi \\(\\alpha = \\)",
                                                                min = 0.01,
                                                                max = 0.20,
                                                                value = 0.05)
                                                    
                                   )
                               )
                             )
                         ),
                         box(width = 6,
                             conditionalPanel(condition = "input.slctes =='Apakah Rata-rata Persentase Penduduk Perokok Berbeda di Setiap Kategori Usia?'",
                                              uiOutput("result_twomean")),
                             conditionalPanel(condition = "input.slctes == 'Apakah Telah Terjadi Peningkatan Jumlah Balita yang diimunisasi?'",
                                              uiOutput("result_twomean1")),
                             conditionalPanel(condition = "input.slctes =='Bagaimana Perbandingan Proporsi Jumlah Desa yang Memiliki Sarana Kesehatan di Tiap Provinsi?'",
                                              uiOutput("result_twoprop"))
                         ),
                         fluidRow(
                           box(width = 12,
                               plotOutput("plot"))
                         )
                ),
                tabPanel("Regresi",
                         box(width = 6,
                             fluidRow(
                               box(width = 12,
                                   radioButtons("check","Pilih Metode Regresi yang Ingin Digunakan",choices = c("Sederhana","Berganda"))
                               )
                             ),
                             fluidRow(
                               box(width = 12,
                                   conditionalPanel(condition = "input.check == 'Sederhana'",
                                                    selectInput("sdh","Pilih Analisis",choices =c(
                                                      "Akses Sanitasi Layak terhadap Imunisasi Balita", "Jumlah Tenaga Kesehatan terhadap Jumlah Desa yang Memiliki Sarana Rumah Sakit","Akses Sanitasi Layak terhadap Keluhan Kesehatan"
                                                    ))
                                   ),
                                   conditionalPanel(condition = "input.check == 'Berganda'",
                                                    selectInput("sdh1","Pilih Analisis",choices = c(
                                                      "Penyakit Menular terhadap Keluhan Kesehatan", "Kelompok Umur yang Merokok terhadap Keluhan Kesehatan","Sarana dan Tenaga Medis terhadap Keluhan Kesehatan","Sarana Mata Air terhadap Air Minum"
                                                    )))
                               )
                             ),
                             fluidRow(
                               box(title = "Residual Analisis" ,width = 12,
                                   plotOutput("plotreg"))
                             )),
                         box(width=6, height = 760,
                             paste("Data : "),
                             br(),
                             br(),
                             DT::dataTableOutput("tbl")),
                         fluidRow(
                           box(width = 12, title = "Analisis Regresi",
                               
                               uiOutput("sdrh")
                               
                               
                           )
                         ),
                         fluidRow(
                           box(width = 12, title = "Output R Studio ",
                               verbatimTextOutput("summary"))
                         )
                )
                
                
                
              )
              
      ),
      tabItem(tabName = "lingkungan",
              tabBox(id = "lingkunganTabs",width = 12,
                     tabPanel("Sanitasi",
                              # Add content for "Sanitasi" tab here
                     ),
                     tabPanel("Sumber Air",
                              # Add content for "Sumber Air" tab here
                     )
              )
      ),
      tabItem(tabName = "database",
              tabBox(id="t2",width = 12,
                     tabPanel("Data",icon = icon("address-card"),dataTableOutput("dfb")),
                     tabPanel("Struktur",icon = icon("address-card"),verbatimTextOutput("structure")),
                     tabPanel("Summary",icon = icon("address-card"),verbatimTextOutput("sumari"))
              )
      )

    )
  )
)


server <- function(input, output) {
  prop.z.test2 <- function(x1, x2, n1, n2, p0 = 0, pooled.stderr = T, conf.level = 0.95) {
    ts.z <- NULL
    cint <- NULL
    p.val <- NULL
    phat1 <- x1 / n1
    qhat1 <- 1 - phat1
    phat2 <- x2 / n2
    qhat2 <- 1 - phat2
    pooled.phat <- ((n1 * phat1) + (n2 * phat2)) / (n1 + n2)
    pooled.qhat <- 1 - pooled.phat
    if (pooled.stderr == FALSE) {
      SE.phat <- sqrt((phat1 * qhat1) / n1 + (phat2 * qhat2) / n2)
    } else {
      SE.phat <- sqrt(pooled.phat * pooled.qhat * (1 / n1 + 1 / n2))
    }
    ts.z <- (phat1 - phat2 - p0) / SE.phat
    p.val <- pnorm(ts.z, lower.tail = FALSE)
    cint <- (phat1 - phat2) + c(
      -1 * ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat),
      ((qnorm(((1 - conf.level) / 2) + conf.level)) * SE.phat)
    )
    return(list(x1 = x1, x2 = x2, n1 = n1, n2 = n2, estimate1 = phat1, estimate2 = phat2, null.value = p0, stderr = SE.phat, pooled.phat = pooled.phat, statistic = ts.z, p.value = p.val, conf.int = cint))
  }
  
  
  output$result_twomean = renderUI({
    if(input$asm == FALSE){
      t = t.test(x=df3[,input$slct1],y=df3[,input$slct2],alternative = "two.sided",mu=input$h0,paired=F,conf.level = 1 - input$alpa, var.equal = F)
      withMathJax(
        paste("Data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(df3[,input$slct1], collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(df3[,input$slct2], collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ",t$null.value," and \\(H_1 : \\mu_1 - \\mu_2 \\)", " \\( \\neq \\)",t$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
          "(",round(t$estimate[1],3),ifelse(t$estimate[2] >= 0, paste0(" - ",round(t$estimate[2],3)),paste0(" + ",round(abs(t$estimate[2]),3))),ifelse(t$null.value >= 0,paste0(" - ",t$null.value), paste0(" + ", abs(t$null.value))),") / ",round(t$stderr,3), " \\( = \\) ",
          round(t$statistic,3)
        ),
        br(),
        paste0(
          "3. Nilai Kritis :", " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)",
          input$alpa / 2, ", ", round(t$parameter, 3), "\\()\\)", " \\( = \\) ",
          "\\( \\pm \\)", round(qt(input$alpa / 2, df = t$parameter, lower.tail = FALSE), 3)
        ),
        br(),
        paste0("4. Keputusan : ", ifelse(t$p.value < input$alpa, "Tolak \\(H_0\\)", "Gagal Tolak \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intrepretasi"),
        br(),
        paste0("Pada tingkat signifikansi ", input$alpa * 100, "%, ", ifelse(t$p.value < input$alpa, "Kita menolak hipotesis nol bahwa perbedaan rata-rata adalah ", "kita gagal menolak hipotesis nol bahwa perbedaan rata-rata adalah "), t$null.value, " \\((p\\)-value ", ifelse(t$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t$p.value, 3))), ")", ".")
      )
    }else{
      t = t.test(x=df3[,input$slct1],y=df3[,input$slct2],alternative = "two.sided",mu=input$h0,paired=F,conf.level = 1 - input$alpa, var.equal = T)
      s_p <- sqrt(((length(df3[,input$slct1]) - 1) * var(df3[,input$slct1]) + (length(df3[,input$slct2]) - 1) * var(df3[,input$slct2])) / t$parameter)
      withMathJax(
        paste("Data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(df3[,input$slct1], collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(df3[,input$slct2], collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ",t$null.value," and \\(H_1 : \\mu_1 - \\mu_2 \\)", " \\( \\neq \\)",t$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}} = \\) ",
          "(",round(t$estimate[1],3),ifelse(t$estimate[2] >= 0, paste0(" - ",round(t$estimate[2],3)),paste0(" + ",round(abs(t$estimate[2]),3))), ifelse(t$null.value >= 0, paste0(" - ", t$null.value), paste0(" + ", abs(t$null.value))), ") / (",round(s_p,3), " * ", round(sqrt((1 / length(df3[,input$slct1])) + (1/length(df3[,input$slct2]))),3), ") \\( = \\) ",
          round(t$statistic,3)
        ),
        br(),
        paste0(
          "3. Nilai Kritis :", " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)",
          input$alpa / 2, ", ", round(t$parameter, 3), "\\()\\)", " \\( = \\) ",
          "\\( \\pm \\)", round(qt(input$alpa / 2, df = t$parameter, lower.tail = FALSE), 3)
        ),
        br(),
        paste0("4. Keputusan : ", ifelse(t$p.value < input$alpa, "Tolak \\(H_0\\)", "Gagal Tolak \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intrepretasi"),
        br(),
        paste0("Pada tingkat signifikansi ", input$alpa * 100, "%, ", ifelse(t$p.value < input$alpa, "Kita menolak hipotesis nol bahwa perbedaan rata-rata adalah ", "kita gagal menolak hipotesis nol bahwa perbedaan rata-rata adalah "), t$null.value, " \\((p\\)-value ", ifelse(t$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t$p.value, 3))), ")", ".")
      )
    }
    
  })
  
  output$result_twomean1 = renderUI({
    if(input$asm1 == FALSE){
      t = t.test(x=df4[,input$slct3],y=df4[,input$slct4],alternative = "two.sided",mu=input$h01,paired=F,conf.level = 1 - input$alpa1, var.equal = F)
      withMathJax(
        paste("Data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(df4[,input$slct3], collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(df4[,input$slct4], collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ",t$null.value," and \\(H_1 : \\mu_1 - \\mu_2 \\)", " \\( > \\)",t$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
          "(",round(t$estimate[1],3),ifelse(t$estimate[2] >= 0, paste0(" - ",round(t$estimate[2],3)),paste0(" + ",round(abs(t$estimate[2]),3))),ifelse(t$null.value >= 0,paste0(" - ",t$null.value), paste0(" + ", abs(t$null.value))),") / ",round(t$stderr,3), " \\( = \\) ",
          round(t$statistic,3)
        ),
        br(),
        paste0(
          "3. Nilai Kritis :", " \\( t_{\\alpha, \\nu} = t(\\)",
          input$alpa1, ", ", round(t$parameter, 3), "\\()\\)", " \\( = \\) ",
          "", round(qt(input$alpa1, df = t$parameter, lower.tail = FALSE), 3)
        ),
        br(),
        paste0("4. Keputusan : ", ifelse(t$p.value < input$alpa1, "Tolak \\(H_0\\)", "Gagal Tolak \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intrepretasi"),
        br(),
        paste0("Pada tingkat signifikansi ", input$alpa1 * 100, "%, ", ifelse(t$p.value < input$alpa1, "Kita menolak hipotesis nol bahwa perbedaan rata-rata adalah ", "kita gagal menolak hipotesis nol bahwa perbedaan rata-rata adalah "), t$null.value, " \\((p\\)-value ", ifelse(t$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t$p.value, 3))), ")", ".")
      )
    }else{
      t = t.test(x=df4[,input$slct3],y=df4[,input$slct4],alternative = "two.sided",mu=input$h01,paired=F,conf.level = 1 - input$alpa1, var.equal = T)
      s_p <- sqrt(((length(df4[,input$slct3]) - 1) * var(df4[,input$slct3]) + (length(df4[,input$slct4]) - 1) * var(df4[,input$slct4])) / t$parameter)
      withMathJax(
        paste("Data:"),
        br(),
        paste(c("\\(Sample_1=\\)", paste(df4[,input$slct3], collapse = ", ")), collapse = " "),
        br(),
        paste(c("\\(Sample_2=\\)", paste(df4[,input$slct4], collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ",t$null.value," and \\(H_1 : \\mu_1 - \\mu_2 \\)", " \\( > \\)",t$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}} = \\) ",
          "(",round(t$estimate[1],3),ifelse(t$estimate[2] >= 0, paste0(" - ",round(t$estimate[2],3)),paste0(" + ",round(abs(t$estimate[2]),3))), ifelse(t$null.value >= 0, paste0(" - ", t$null.value), paste0(" + ", abs(t$null.value))), ") / (",round(s_p,3), " * ", round(sqrt((1 / length(df4[,input$slct3])) + (1/length(df4[,input$slct4]))),3), ") \\( = \\) ",
          round(t$statistic,3)
        ),
        br(),
        paste0(
          "3. Nilai Kritis :", " \\( t_{\\alpha, n_1 + n_2 - 2} = t(\\)",
          input$alpa1, ", ", round(t$parameter, 3), "\\()\\)", " \\( = \\) ",
          "", round(qt(input$alpa1, df = t$parameter, lower.tail = FALSE), 3)
        ),
        br(),
        paste0("4. Keputusan : ", ifelse(t$p.value < input$alpa1, "Tolak \\(H_0\\)", "Gagal Tolak \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intrepretasi"),
        br(),
        paste0("Pada tingkat signifikansi ", input$alpa1 * 100, "%, ", ifelse(t$p.value < input$alpa1, "Kita menolak hipotesis nol bahwa perbedaan rata-rata adalah ", "kita gagal menolak hipotesis nol bahwa perbedaan rata-rata adalah "), t$null.value, " \\((p\\)-value ", ifelse(t$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t$p.value, 3))), ")", ".")
      )
    }
  })
  
  output$result_twoprop = renderUI({
    
    if(input$pooled==F){
      t = prop.z.test2(x1=df5[,input$slct5][df5$Sarana==input$slct6],x2=df5[,input$slct51][df5$Sarana==input$slct6],n1=df5[,input$slct5][df5$Sarana=="Desa"],n2=df5[,input$slct51][df5$Sarana=="Desa"],p0 = input$h02, pooled.stderr=F, conf.level = 1-input$alpa2)
      withMathJax(
        paste("Data : "),
        br(),
        paste0("Jumlah Desa yang memiliki sarana ",input$slct6," di Provinsi ",input$slct5," (x1) : ",t$x1),
        br(),
        paste0("Jumlah Desa yang memiliki sarana ",input$slct6," di Provinsi ",input$slct51," (x2) : ",t$x2),
        br(),
        paste0("Jumlah Desa di Provinsi ",input$slct5," (n1) : ",t$n1),
        br(),
        paste0("Jumlah Desa di Provinsi ",input$slct51," (n2) : ",t$n2),
        br(),
        paste0("\\(\\hat{p}_1 =\\) ", round(t$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 =\\) ", round(t$estimate2, 3)),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", t$null.value, " and \\(H_1 : p_1 - p_2 \\) ", "\\( > \\) ", t$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}} = \\) ",
          "(", round(t$estimate1, 3), ifelse(t$estimate2 >= 0, paste0(" - ", round(t$estimate2, 3)), paste0(" + ", round(abs(t$estimate2), 3))), ifelse(t$null.value >= 0, paste0(" - ", t$null.value), paste0(" + ", abs(t$null.value))), ") / ", round(t$stderr, 3), " \\( = \\) ",
          ifelse(t$null.value >= -1 & t$null.value <= 1, round(t$statistic, 3), "Error: \\( p_1 - p_2 \\) harus \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", " \\( z_{\\alpha} = z(\\)",
          input$alpa2, "\\()\\)", " \\( = \\) ",
          "",round(qnorm(input$alpa2, lower.tail = FALSE), 3)),
        br(),
        paste0("4. Keputusan : ", ifelse(t$p.value < input$alpa2, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intrepretasi"),
        br(),
        paste0("Pada tingkat signifikansi ", input$alpa2 * 100, "%, ", ifelse(t$p.value < input$alpa2, "Kita menolak hipotesis nol bahwa perbedaan proporsi adalah  ", "Kita gagal menolak hipotesis nol bahwa perbedaan proporsi adalah "), t$null.value, " \\((p\\)-value ", ifelse(t$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t$p.value, 3))), ")", ".")
        
      )
    }else{
      t = prop.z.test2(x1=df5[,input$slct5][df5$Sarana==input$slct6],x2=df5[,input$slct51][df5$Sarana==input$slct6],n1=df5[,input$slct5][df5$Sarana=="Desa"],n2=df5[,input$slct51][df5$Sarana=="Desa"],p0 = input$h02, pooled.stderr=T, conf.level = 1-input$alpa2)
      withMathJax(
        paste("Data : "),
        br(),
        paste0("Jumlah Desa yang memiliki sarana ",input$slct6," di Provinsi ",input$slct5," (x1) : ",t$x1),
        br(),
        paste0("Jumlah Desa yang memiliki sarana ",input$slct6," di Provinsi ",input$slct51," (x2) : ",t$x2),
        br(),
        paste0("Jumlah Desa di Provinsi ",input$slct5," (n1) : ",t$n1),
        br(),
        paste0("Jumlah Desa di Provinsi ",input$slct51," (n2) : ",t$n2),
        br(),
        paste0("\\(\\hat{p}_1 =\\) ", round(t$estimate1, 3)),
        br(),
        paste0("\\(\\hat{p}_2 =\\) ", round(t$estimate2, 3)),
        br(),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1 - p_2 = \\) ", t$null.value, " and \\(H_1 : p_1 - p_2 \\) ", "\\( > \\) ", t$null.value),
        br(),
        paste0("2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} \\) "),
        br(),
        paste0("dimana ", "\\( \\hat{p} = \\dfrac{n_1 \\hat{p}_1 + n_2 + \\hat{p}_2}{n_1 + n_2} = \\) ", "(", t$n1, " * ", round(t$estimate1, 3), " + ", t$n2, " * ", round(t$estimate2, 3), ") / (", t$n1, " + ", t$n2, ") = ", round(t$pooled.phat, 3)),
        br(),
        paste0(
          "\\( \\Rightarrow z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - (p_1 - p_2)}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1} + \\dfrac{1}{n_2}\\Big)}} = \\) ",
          "(", round(t$estimate1, 3), ifelse(t$estimate2 >= 0, paste0(" - ", round(t$estimate2, 3)), paste0(" + ", round(abs(t$estimate2), 3))), ifelse(t$null.value >= 0, paste0(" - ", t$null.value), paste0(" + ", abs(t$null.value))), ") / ", round(t$stderr, 3), " \\( = \\) ",
          ifelse(t$null.value >= -1 & t$null.value <= 1, round(t$statistic, 3), "Error: \\( p_1 - p_2 \\) harus \\( -1 \\leq p_1 - p_2 \\leq 1\\)")
        ),
        br(),
        paste0(
          "3. Critical value :", " \\( z_{\\alpha} = z(\\)",
          input$alpa2, "\\()\\)", " \\( = \\) ",
          "",round(qnorm(input$alpa2, lower.tail = FALSE), 3)),
        br(),
        paste0("4. Keputusan : ", ifelse(t$p.value < input$alpa2, "Tolak \\(H_0\\)", "Gagal tolak \\(H_0\\)")),
        br(),
        br(),
        tags$b("Intrepretasi"),
        br(),
        paste0("Pada tingkat signifikansi ", input$alpa2 * 100, "%, ", ifelse(t$p.value < input$alpa2, "Kita menolak hipotesis nol bahwa perbedaan proporsi adalah  ", "Kita gagal menolak hipotesis nol bahwa perbedaan proporsi adalah "), t$null.value, " \\((p\\)-value ", ifelse(t$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(t$p.value, 3))), ")", ".")
      ) 
    }
    
  })
  
  output$plot = renderPlot({
    if(input$slctes == "Apakah Rata-rata Persentase Penduduk Perokok Berbeda di Setiap Kategori Usia?" & input$asm == T){
      t = t.test(x=df3[,input$slct1],y=df3[,input$slct2],alternative = "two.sided",mu=input$h0,paired=F,conf.level = 1 - input$alpa, var.equal = T)
      funcshaded = function(x){
        y = dt(x,df=t$parameter)
        y[x < qt(input$alpa / 2, df=t$parameter, lower.tail = F) & x > qt(input$alpa / 2, df = t$parameter)] = NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = t$parameter, lower.tail = FALSE), qt(0.999, df = t$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = t$parameter)) +
        stat_function(fun = funcshaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = t$statistic, color = "steelblue") +
        geom_text(aes(x = t$statistic, label = paste0("Test statistic = ", round(t$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(t$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")+ theme(rect = element_blank())
      p
    }else if(input$slctes == "Apakah Rata-rata Persentase Penduduk Perokok Berbeda di Setiap Kategori Usia?" & input$asm == F){
      t = t.test(x=df3[,input$slct1],y=df3[,input$slct2],alternative = "two.sided",mu=input$h0,paired=F,conf.level = 1 - input$alpa, var.equal = F)
      funcshaded = function(x){
        y = dt(x,df=t$parameter)
        y[x < qt(input$alpa / 2, df=t$parameter, lower.tail = F) & x > qt(input$alpa / 2, df = t$parameter)] = NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = t$parameter, lower.tail = FALSE), qt(0.999, df = t$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = t$parameter)) +
        stat_function(fun = funcshaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = t$statistic, color = "steelblue") +
        geom_text(aes(x = t$statistic, label = paste0("Test statistic = ", round(t$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(t$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")+ theme(rect = element_blank())
      p
    }else if(input$slctes == "Apakah Telah Terjadi Peningkatan Jumlah Balita yang diimunisasi?" & input$asm1 == T){
      t = t.test(x=df4[,input$slct3],y=df4[,input$slct4],alternative = "two.sided",mu=input$h01,paired=F,conf.level = 1 - input$alpa1, var.equal = T)
      funcshaded = function(x){
        y = dt(x,df=t$parameter)
        y[x < qt(input$alpa1, df=t$parameter, lower.tail = F)] =NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = t$parameter, lower.tail = FALSE), qt(0.999, df = t$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = t$parameter)) +
        stat_function(fun = funcshaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = t$statistic, color = "steelblue") +
        geom_text(aes(x = t$statistic, label = paste0("Test statistic = ", round(t$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(t$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")+ theme(rect = element_blank())
      p
    }else if(input$slctes == "Apakah Telah Terjadi Peningkatan Jumlah Balita yang diimunisasi?" & input$asm1 == F){
      t = t.test(x=df4[,input$slct3],y=df4[,input$slct4],alternative = "two.sided",mu=input$h01,paired=F,conf.level = 1 - input$alpa1, var.equal = F)
      funcshaded = function(x){
        y = dt(x,df=t$parameter)
        y[x < qt(input$alpa1, df=t$parameter, lower.tail = F)] =NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qt(0.999, df = t$parameter, lower.tail = FALSE), qt(0.999, df = t$parameter, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dt, args = list(df = t$parameter)) +
        stat_function(fun = funcshaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = t$statistic, color = "steelblue") +
        geom_text(aes(x = t$statistic, label = paste0("Test statistic = ", round(t$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Student distribution", " t(", round(t$parameter, 3), ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")+ theme(rect = element_blank())
      p
    }else if(input$slctes == "Bagaimana Perbandingan Proporsi Jumlah Desa yang Memiliki Sarana Kesehatan di Tiap Provinsi?"){
      if(input$pooled == T){
        t = prop.z.test2(x1=df5[,input$slct5][df5$Sarana==input$slct6],x2=df5[,input$slct51][df5$Sarana==input$slct6],n1=df5[,input$slct5][df5$Sarana=="Desa"],n2=df5[,input$slct51][df5$Sarana=="Desa"],p0 = input$h02, pooled.stderr=T, conf.level = 1-input$alpa2)
      }else{
        t = prop.z.test2(x1=df5[,input$slct5][df5$Sarana==input$slct6],x2=df5[,input$slct51][df5$Sarana==input$slct6],n1=df5[,input$slct5][df5$Sarana=="Desa"],n2=df5[,input$slct51][df5$Sarana=="Desa"],p0 = input$h02, pooled.stderr=F, conf.level = 1-input$alpa2)
      }
      funcshaded <- function(x) {
        y <- dnorm(x, mean = 0, sd = 1)
        y[x < qnorm(input$alpa2, mean = 0, sd = 1, lower.tail = FALSE) ] <- NA
        return(y)
      }
      p <- ggplot(data.frame(x = c(qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE), qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE))), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
        stat_function(fun = funcshaded, geom = "area", alpha = 0.8) +
        theme_minimal() +
        geom_vline(xintercept = t$statistic, color = "steelblue") +
        geom_text(aes(x = t$statistic, label = paste0("Test statistic = ", round(t$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
        ggtitle(paste0("Normal distribution N(0,1)")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
    }
  })
  
  output$tbl = renderDataTable({
    if(input$check=="Sederhana"){
      if(input$sdh=="Akses Sanitasi Layak terhadap Imunisasi Balita"){
        Imunisasi = df6$Imunisasi_Balita
        Sanitasi = df6$Sanitasi
        DT::datatable(data.frame(Sanitasi,Imunisasi),
                      extensions = "Buttons",
                      options = list(
                        lengthChange = FALSE,
                        dom = "Blfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
      }else if(input$sdh=="Jumlah Tenaga Kesehatan terhadap Jumlah Desa yang Memiliki Sarana Rumah Sakit"){
        Tenaga_Kesehatan = df6$Tenaga
        Jumlah_Desa = df6$Desa_Rs
        DT::datatable(data.frame(Tenaga_Kesehatan,Jumlah_Desa),
                      extensions = "Buttons",
                      options = list(
                        lengthChange = FALSE,
                        dom = "Blfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
      }else{
        Sanitasi = df6$Sanitasi
        Keluhan = df6$Keluhan
        DT::datatable(data.frame(Sanitasi,Keluhan),
                      extensions = "Buttons",
                      options = list(
                        lengthChange = FALSE,
                        dom = "Blfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
      }
    }else{
      if(input$sdh1=="Penyakit Menular terhadap Keluhan Kesehatan"){
        x = data.frame("AIDS"=df6$aids,"Kusta"=df6$kusta,"Malaria"=df6$malaria,"DBD"=df6$dbd)
        DT::datatable(data.frame(x,"Keluhan"=df6$Keluhan),
                      extensions = "Buttons",
                      options = list(
                        lengthChange = FALSE,
                        dom = "Blfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
      }else if(input$sdh1=="Kelompok Umur yang Merokok terhadap Keluhan Kesehatan"){
        x = data.frame("15_24"=df6$limabelas,"25_34"=df6$dualima,"35_44"=df6$tigalima,"45_54"=df6$empatlima,"55_64"=df6$limalima,"65+"=df6$enalima)
        DT::datatable(data.frame(x,"Keluhan"=df6$Keluhan),
                      extensions = "Buttons",
                      options = list(
                        lengthChange = FALSE,
                        dom = "Blfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
      }else if(input$sdh1=="Sarana dan Tenaga Medis terhadap Keluhan Kesehatan"){
        x = data.frame("RSU"=df6$RSU,"RSK"=df6$RSK,"PRI"=df6$PRI,"PNRI"=df6$PNRI,"Tenaga Kesehatan"=df6$Tenaga)
        DT::datatable(data.frame(x,"Keluhan"=df6$Keluhan),
                      extensions = "Buttons",
                      options = list(
                        lengthChange = FALSE,
                        dom = "Blfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
      }else{
        x = data.frame("Leading"=df6$Leding,"Pompa"=df6$Pompa,"Sumur"=df6$Sumur_lin,"Mata Air"=df6$Mataair_lin)
        DT::datatable(data.frame(x,"Air Minum Layak"=df6$Air_minum),
                      extensions = "Buttons",
                      options = list(
                        lengthChange = FALSE,
                        dom = "Blfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
      }
    }
  })
  
  output$sdrh = renderUI({
    if(input$check=="Sederhana"){
      if(input$sdh=="Akses Sanitasi Layak terhadap Imunisasi Balita"){
        model = lm(df6$Imunisasi_Balita~df6$Sanitasi)
        withMathJax(
          paste0("\\(\\bar{x} =\\) ", round(mean(df6$Sanitasi), 3)),
          br(),
          paste0("\\(\\bar{y} =\\) ", round(mean(df6$Imunisasi_Balita), 3)),
          br(),
          paste0("\\(n =\\) ", length(df6$Imunisasi_Balita)),
          br(),
          paste0("\\(\\hat{\\beta}1 = \\dfrac{\\big(\\sum^n{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(model$coef[[2]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(model$coef[[1]], 3)),
          br(),
          br(),
          paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(model$coef[[1]], 3), " + ", round(model$coef[[2]], 3), "\\( x \\)"),
          br(),
          paste0(
            "Adj. \\( R^2 = \\) ", round(summary(model)$adj.r.squared, 3),
            ", \\( \\beta_0 = \\) ", round(model$coef[[1]], 3),
            ", \\( \\beta_1 = \\) ", round(model$coef[[2]], 3),
            ", P-value ", "\\( = \\) ", signif(summary(model)$coef[2, 4], 3)
          )
        )
        
      }else if(input$sdh=="Jumlah Tenaga Kesehatan terhadap Jumlah Desa yang Memiliki Sarana Rumah Sakit"){
        model = lm(df6$Desa_Rs~df6$Tenaga)
        withMathJax(
          paste0("\\(\\bar{x} =\\) ", round(mean(df6$Tenaga), 3)),
          br(),
          paste0("\\(\\bar{y} =\\) ", round(mean(df6$Desa_Rs), 3)),
          br(),
          paste0("\\(n =\\) ", length(df6$Desa_Rs)),
          br(),
          paste0("\\(\\hat{\\beta}1 = \\dfrac{\\big(\\sum^n{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(model$coef[[2]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(model$coef[[1]], 3)),
          br(),
          br(),
          paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(model$coef[[1]], 3), " + ", round(model$coef[[2]], 3), "\\( x \\)"),
          br(),
          paste0(
            "Adj. \\( R^2 = \\) ", round(summary(model)$adj.r.squared, 3),
            ", \\( \\beta_0 = \\) ", round(model$coef[[1]], 3),
            ", \\( \\beta_1 = \\) ", round(model$coef[[2]], 3),
            ", P-value ", "\\( = \\) ", signif(summary(model)$coef[2, 4], 3)
          )
        )
        
      }else{
        model = lm(df6$Keluhan~df6$Sanitasi)
        withMathJax(
          paste0("\\(\\bar{x} =\\) ", round(mean(df6$Sanitasi), 3)),
          br(),
          paste0("\\(\\bar{y} =\\) ", round(mean(df6$Keluhan), 3)),
          br(),
          paste0("\\(n =\\) ", length(df6$Keluhan)),
          br(),
          paste0("\\(\\hat{\\beta}1 = \\dfrac{\\big(\\sum^n{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(model$coef[[2]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(model$coef[[1]], 3)),
          br(),
          br(),
          paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(model$coef[[1]], 3), " + ", round(model$coef[[2]], 3), "\\( x \\)"),
          br(),
          paste0(
            "Adj. \\( R^2 = \\) ", round(summary(model)$adj.r.squared, 3),
            ", \\( \\beta_0 = \\) ", round(model$coef[[1]], 3),
            ", \\( \\beta_1 = \\) ", round(model$coef[[2]], 3),
            ", P-value ", "\\( = \\) ", signif(summary(model)$coef[2, 4], 3)
          )
        )
      }
    }else{
      if(input$sdh1=="Penyakit Menular terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$aids+df6$kusta+df6$malaria+df6$dbd)
        withMathJax(
          paste0("\\(\\bar{x}  AIDS =\\) ", round(mean(df6$aids), 3)),
          br(),
          paste0("\\(\\bar{x}  Kusta =\\) ", round(mean(df6$kusta), 3)),
          br(),
          paste0("\\(\\bar{x}  Malaria =\\) ", round(mean(df6$malaria), 3)),
          br(),
          paste0("\\(\\bar{x}  DBD =\\) ", round(mean(df6$dbd), 3)),
          br(),
          paste0("\\(\\bar{y}  Keluhan =\\) ", round(mean(df6$Keluhan), 3)),
          br(),
          paste0("\\(n =\\) ", length(df6$Keluhan)),
          br(),
          paste0("\\(\\hat{\\beta} = (X^TX)^-1 X^TY \\)"),
          br(),
          paste0("\\(\\hat{\\beta}_0 = \\) ", round(model$coef[[1]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_1 = \\)", round(model$coef[[2]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_2 = \\) ", round(model$coef[[3]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_3 = \\) ", round(model$coef[[4]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_4 = \\)", round(model$coef[[5]], 3)),
          br(),
          paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x1 + \\hat{\\beta}_2 x2 + \\hat{\\beta}_3 x3 + \\hat{\\beta}_4 x4= \\) ", round(model$coef[[1]], 3), " + ", round(model$coef[[2]], 3), "\\( x1 \\)", " + ", round(model$coef[[3]], 3), "\\( x2 \\)", " + ", round(model$coef[[4]], 3), "\\( x3 \\)", " + ", round(model$coef[[5]], 3), "\\( x4 \\)")
        )
        
      }else if(input$sdh1=="Kelompok Umur yang Merokok terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$limabelas+df6$dualima+df6$tigalima+df6$empatlima+df6$limalima+df6$enalima)
        withMathJax(
          paste0("\\(\\bar{x}  15-24 =\\) ", round(mean(df6$limabelas), 3)),
          br(),
          paste0("\\(\\bar{x}  25-34 =\\) ", round(mean(df6$dualima), 3)),
          br(),
          paste0("\\(\\bar{x}  35-44 =\\) ", round(mean(df6$tigalima), 3)),
          br(),
          paste0("\\(\\bar{x}  45-54 =\\) ", round(mean(df6$empatlima), 3)),
          br(),
          paste0("\\(\\bar{x}  55-64 =\\) ", round(mean(df6$limalima), 3)),
          br(),
          paste0("\\(\\bar{x}  65+ =\\) ", round(mean(df6$enalima), 3)),
          br(),
          paste0("\\(\\bar{y}  Keluhan =\\) ", round(mean(df6$Keluhan), 3)),
          br(),
          paste0("\\(n =\\) ", length(df6$Keluhan)),
          br(),
          paste0("\\(\\hat{\\beta} = (X^TX)^-1 X^TY \\)"),
          br(),
          paste0("\\(\\hat{\\beta}_0 = \\) ", round(model$coef[[1]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_1 = \\)", round(model$coef[[2]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_2 = \\) ", round(model$coef[[3]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_3 = \\) ", round(model$coef[[4]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_4 = \\)", round(model$coef[[5]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_5 = \\)", round(model$coef[[6]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_6 = \\)", round(model$coef[[7]], 3)),
          br(),
          paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x1 + \\hat{\\beta}_2 x2 + \\hat{\\beta}_3 x3 + \\hat{\\beta}_4 x4 + \\hat{\\beta}_5 x5 + \\hat{\\beta}_6 x6 =\\) ", round(model$coef[[1]], 3), " + ", round(model$coef[[2]], 3), "\\( x1 \\)", " + ", round(model$coef[[3]], 3), "\\( x2 \\)", " + ", round(model$coef[[4]], 3), "\\( x3 \\)", " + ", round(model$coef[[5]], 3), "\\( x4 \\)", " + ", round(model$coef[[6]], 3), "\\( x5 \\)", " + ", round(model$coef[[7]], 3), "\\( x6 \\)")
        )
      }else if(input$sdh1=="Sarana dan Tenaga Medis terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$RSU+df6$RSK+df6$PRI+df6$PNRI+df6$Tenaga)
        withMathJax(
          paste0("\\(\\bar{x} RSU =\\) ", round(mean(df6$RSU), 3)),
          br(),
          paste0("\\(\\bar{x} RSK =\\) ", round(mean(df6$RSK), 3)),
          br(),
          paste0("\\(\\bar{x} PRI =\\) ", round(mean(df6$PRI), 3)),
          br(),
          paste0("\\(\\bar{x} PNRI =\\) ", round(mean(df6$PNRI), 3)),
          br(),
          paste0("\\(\\bar{y} Keluhan =\\) ", round(mean(df6$Keluhan), 3)),
          br(),
          paste0("\\(n =\\) ", length(df6$Keluhan)),
          br(),
          paste0("\\(\\hat{\\beta} = (X^TX)^-1 X^TY \\)"),
          br(),
          paste0("\\(\\hat{\\beta}_0 = \\) ", round(model$coef[[1]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_1 = \\)", round(model$coef[[2]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_2 = \\) ", round(model$coef[[3]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_3 = \\) ", round(model$coef[[4]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_4 = \\)", round(model$coef[[5]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_5 = \\)", round(model$coef[[6]], 3)),
          br(),
          paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x1 + \\hat{\\beta}_2 x2 + \\hat{\\beta}_3 x3 + \\hat{\\beta}_4 x4 + \\hat{\\beta}_5 x5  =\\) ", round(model$coef[[1]], 3), " + ", round(model$coef[[2]], 3), "\\( x1 \\)", " + ", round(model$coef[[3]], 3), "\\( x2 \\)", " + ", round(model$coef[[4]], 3), "\\( x3 \\)", " + ", round(model$coef[[5]], 3), "\\( x4 \\)", " + ", round(model$coef[[6]], 3), "\\( x5 \\)")
        )
      }else{
        model = lm(df6$Air_minum~df6$Leding+df6$Pompa+df6$Sumur_lin+df6$Mataair_lin)
        withMathJax(
          paste0("\\(\\bar{x} Leding =\\) ", round(mean(df6$Leding), 3)),
          br(),
          paste0("\\(\\bar{x} Pompa =\\) ", round(mean(df6$Pompa), 3)),
          br(),
          paste0("\\(\\bar{x} Sumur =\\) ", round(mean(df6$Sumur_lin), 3)),
          br(),
          paste0("\\(\\bar{x} Mata Air =\\) ", round(mean(df6$Mataair_lin), 3)),
          br(),
          paste0("\\(\\bar{y} Air Minum =\\) ", round(mean(df6$Air_minum), 3)),
          br(),
          paste0("\\(n =\\) ", length(df6$Air_minum)),
          br(),
          paste0("\\(\\hat{\\beta} = (X^TX)^-1 X^TY \\)"),
          br(),
          paste0("\\(\\hat{\\beta}_0 = \\) ", round(model$coef[[1]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_1 = \\)", round(model$coef[[2]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_2 = \\) ", round(model$coef[[3]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_3 = \\) ", round(model$coef[[4]], 3)),
          br(),
          paste0("\\(\\hat{\\beta}_4 = \\)", round(model$coef[[5]], 3)),
          br(),
          paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x1 + \\hat{\\beta}_2 x2 + \\hat{\\beta}_3 x3 + \\hat{\\beta}_4 x4 =\\) ", round(model$coef[[1]], 3), " + ", round(model$coef[[2]], 3), "\\( x1 \\)", " + ", round(model$coef[[3]], 3), "\\( x2 \\)", " + ", round(model$coef[[4]], 3), "\\( x3 \\)", " + ", round(model$coef[[5]], 3), "\\( x4 \\)")
          
        )
      }
    }
  })
  
  
  
  output$plotreg = renderPlot({
    if(input$check=="Sederhana"){
      if(input$sdh=="Akses Sanitasi Layak terhadap Imunisasi Balita"){
        model = lm(df6$Imunisasi_Balita~df6$Sanitasi)
        par(mfrow = c(2, 2))
        plot(model, which = c(1:3, 5))
      }else if(input$sdh=="Jumlah Tenaga Kesehatan terhadap Jumlah Desa yang Memiliki Sarana Rumah Sakit"){
        model = lm(df6$Desa_Rs~df6$Tenaga)
        par(mfrow = c(2, 2))
        plot(model, which = c(1:3, 5))
      }else{
        model = lm(df6$Keluhan~df6$Sanitasi)
        par(mfrow = c(2, 2))
        plot(model, which = c(1:3, 5))
      }
    }else{
      if(input$sdh1=="Penyakit Menular terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$aids+df6$kusta+df6$malaria+df6$dbd)
        par(mfrow = c(2, 2))
        plot(model, which = c(1:3, 5))
      }else if(input$sdh1=="Kelompok Umur yang Merokok terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$limabelas+df6$dualima+df6$tigalima+df6$empatlima+df6$limalima+df6$enalima)
        par(mfrow = c(2, 2))
        plot(model, which = c(1:3, 5))
      }else if(input$sdh1=="Sarana dan Tenaga Medis terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$RSU+df6$RSK+df6$PRI+df6$PNRI+df6$Tenaga)
        par(mfrow = c(2, 2))
        plot(model, which = c(1:3, 5))
      }else{
        model = lm(df6$Air_minum~df6$Leding+df6$Pompa+df6$Sumur_lin+df6$Mataair_lin)
        par(mfrow = c(2, 2))
        plot(model, which = c(1:3, 5))
      }
    }
  })
  
  
  output$summary = renderPrint({
    if(input$check=="Sederhana"){
      if(input$sdh=="Akses Sanitasi Layak terhadap Imunisasi Balita"){
        model = lm(df6$Imunisasi_Balita~df6$Sanitasi)
        summary(model)
      }else if(input$sdh=="Jumlah Tenaga Kesehatan terhadap Jumlah Desa yang Memiliki Sarana Rumah Sakit"){
        model = lm(df6$Desa_Rs~df6$Tenaga)
        summary(model)
      }else{
        model = lm(df6$Keluhan~df6$Sanitasi)
        summary(model)
      }
    }else{
      if(input$sdh1=="Penyakit Menular terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$aids+df6$kusta+df6$malaria+df6$dbd)
        summary(model)
      }else if(input$sdh1=="Kelompok Umur yang Merokok terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$limabelas+df6$dualima+df6$tigalima+df6$empatlima+df6$limalima+df6$enalima)
        summary(model)
      }else if(input$sdh1=="Sarana dan Tenaga Medis terhadap Keluhan Kesehatan"){
        model = lm(df6$Keluhan~df6$RSU+df6$RSK+df6$PRI+df6$PNRI+df6$Tenaga)
        summary(model)
      }else{
        model = lm(df6$Air_minum~df6$Leding+df6$Pompa+df6$Sumur_lin+df6$Mataair_lin)
        summary(model)
      }
    }
  })    
  
  
  
  output$RSU <- renderValueBox({
    value <- df1$RSU %>% subset(df1$Provinsi==input$PROVINCEDF1 & df1$Tahun==input$TahunDF1)
    infoBox(tags$p("Rumah Sakit Umum",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("hospital"))
  })  
  
  output$RSK <- renderValueBox({
    value <- df1$RSK %>% subset(df1$Provinsi==input$PROVINCEDF1 & df1$Tahun==input$TahunDF1)
    infoBox(tags$p("Rumah Sakit Khusus",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("hospital"))
  })
  
  output$PRI <- renderValueBox({
    value <- df1$PRI %>% subset(df1$Provinsi==input$PROVINCEDF1 & df1$Tahun==input$TahunDF1)
    infoBox(tags$p("Puskesmas Rawat Inap",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("hospital"))
  })
  
  output$PRNI <- renderValueBox({
    value <- df1$PNRI %>% subset(df1$Provinsi==input$PROVINCEDF1 & df1$Tahun==input$TahunDF1)
    infoBox(tags$p("Puskesmas Non Rawat Inap",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("hospital"))
  })
  
  output$MAPDF1 <- renderPlot({
    shp2 <- filter(shp1, Tahun == input$TahunDF1)
    ggplot(shp2, aes(fill = get(input$SARANADF1))) + 
      geom_sf() + scale_fill_gradient(name="Jumlah Sarana", low = "#9fc5e8", high ="#0b5394", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank())
    
  })
  
  output$BAR1DF1 <- renderPlot({
    
    pp1 <- filter(df1, Tahun==input$TahunDF1)
    pp2 <- arrange(pp1, desc(get(input$SARANADF1)))[c(1:5),]
    
    ggplot(pp2) +
      aes(x =reorder(Provinsi,get(input$SARANADF1)), y = get(input$SARANADF1)) +
      geom_col(fill = "#112446") +
      theme_minimal() + labs(y = input$SARANADF1, x = "Provinsi") +coord_flip() 
    
  })
  
  
  output$BAR2DF1 <- renderPlot({
    
    pp1 <- filter(df1, Tahun==input$TahunDF1)
    pp2 <- arrange(pp1, get(input$SARANADF1))[c(1:5),]
    
    ggplot(pp2) +
      aes(x =reorder(Provinsi,-get(input$SARANADF1)), y = get(input$SARANADF1)) +
      geom_col(fill = "#112446") +
      theme_minimal() + labs(y = input$SARANADF1, x = "Provinsi") + coord_flip()
    
  })
  
  
  
  output$HISTDF1 <- renderPlot({ ggplot(df1) +
      aes(x = get(input$SARANADF1)) +
      geom_histogram(bins = 30L, fill = "#112446") +
      theme_minimal() + labs(x = input$SARANADF1, y="count")
  })
  
  
  output$structure = renderPrint(
    str(df6)
  )
  output$sumari = renderPrint(
    summary(df6)
  )
  output$dfb = renderDataTable(
    df6,extensions = 'Buttons',options=list(dom='Bfrtip',buttons=list('copy','pdf','excel','csv','print'))
  )
  
  
  output$TM <- renderValueBox({
    value <- df2$Tenaga.Medis %>% subset(df2$Provinsi==input$PROVINCEDF2 & df2$Tahun==input$TahunDF2)
    infoBox(tags$p("Tenaga Medis",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("user-nurse"))
  })  
  
  output$TK <- renderValueBox({
    value <- df2$Tenaga.Keperawatan %>% subset(df2$Provinsi==input$PROVINCEDF2 & df2$Tahun==input$TahunDF2)
    infoBox(tags$p("Tenaga Keperawatan",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("user-nurse"))
  })
  
  output$PK <- renderValueBox({
    value <- df2$psikologi.klinis %>% subset(df2$Provinsi==input$PROVINCEDF2 & df2$Tahun==input$TahunDF2)
    infoBox(tags$p("Tenga Psikologi Klinis",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("user-nurse"))
  })
  
  output$TKF <- renderValueBox({
    value <- df2$Tenaga.Kefarmasian %>% subset(df2$Provinsi==input$PROVINCEDF2 & df2$Tahun==input$TahunDF2)
    infoBox(tags$p("Puskesmas Non Rawat Inap",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("user-nurse"))
  })
  
  output$MAPDF2 <- renderPlot({
    qq1 <- filter(shp2, Tahun == input$TahunDF2)
    ggplot(qq1, aes(fill = get(input$SARANADF2))) + 
      geom_sf() + scale_fill_gradient(name="Jumlah Tenaga Kerja", low = "#9fc5e8", high ="#0b5394", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            rect = element_blank())
    
  })
  
  output$BAR1DF2 <- renderPlot({
    
    pp1 <- filter(df2, Tahun==input$TahunDF2)
    pp2 <- arrange(pp1, desc(get(input$SARANADF2)))[c(1:5),]
    
    ggplot(pp2) +
      aes(x = reorder(Provinsi, get(input$SARANADF2)), y = get(input$SARANADF2)) +
      geom_col(fill = "#112446") +
      theme_minimal() + labs(y = input$SARANADF2, x = "Provinsi") + coord_flip()
    
  })
  
  output$BAR2DF2 <- renderPlot({
    
    pp1 <- filter(df2, Tahun==input$TahunDF2)
    pp2 <- arrange(pp1, get(input$SARANADF2))[c(1:5),]
    
    ggplot(pp2) +
      aes(x =reorder(Provinsi,-get(input$SARANADF2)), y = get(input$SARANADF2)) +
      geom_col(fill = "#112446") +
      theme_minimal() + labs(y = input$SARANADF2, x = "Provinsi") + coord_flip()
    
  })
  
  
  
  output$HISTDF2 <- renderPlot({ ggplot(df2) +
      aes(x = get(input$SARANADF2)) +
      geom_histogram(bins = 30L, fill = "#112446") +
      theme_minimal() + labs(x = input$SARANADF2, y="count")
  })
  
  
  
  
  output$Desa <- renderValueBox({
    value <- df1.3$Jumlah %>% subset(df1.3$Provinsi==input$PROVINCEDF1.3 & df1.3$Tahun==input$TAHUNDF1.3 &df1.3$Sarana=='Desa/Kelurahan')
    infoBox(tags$p("Total Desa",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("home"))
  })
  
  output$Sarana <- renderValueBox({
    value <- df1.3$Jumlah %>% subset(df1.3$Provinsi==input$PROVINCEDF1.3 & df1.3$Tahun==input$TAHUNDF1.3 &df1.3$Sarana==input$SARANADF1.3)
    infoBox(tags$p("Desa dengan Sarana",style="font-weight:bold;"),
            value = paste(value),
            color = "blue",
            fill=T,
            icon = icon("hospital"))
  })
  
  
  
  
  
  output$BARDF1.3 <- renderPlot({
    anjay <- filter(df1.3, Tahun==2020)
    anjay
    databar <- filter(df1.3, Sarana!='Desa/Kelurahan' & Tahun==input$TAHUNDF1.3 & Provinsi==input$PROVINCEDF1.3)  
    
    ggplot(databar) +
      aes(x = Sarana, y = Jumlah) +
      geom_col(fill = "#112446") +
      theme_minimal() + labs()
  })
  
  
  output$PIEDF1.3 <- renderPlot({
    pop <- filter(df1.3, Provinsi==input$PROVINCEDF1.3 & Tahun==input$TAHUNDF1.3 & Sarana==input$SARANADF1.3) %>% select(Jumlah)
    pop
    pop1 <- filter(df1.3, Provinsi==input$PROVINCEDF1.3 & Tahun==input$TAHUNDF1.3 & Sarana=='Desa/Kelurahan') %>% select(Jumlah)
    
    pop3 <- round(pop/pop1,3)
    pop3
    
    
    pop2 <- round((1-pop3),3)
    
    nama = c("Desa dengan Sarana","Total Desa")
    datam1 <- rbind(pop3,pop2)
    datam = cbind(datam1,nama)
    
    datam
    colnames(datam) <- c("Jumlah", "Nama")
    datam = as.data.frame(datam)
    datam
    
    
    
    ggplot(datam, aes(x = "", y = Jumlah, fill =Nama)) +
      geom_col(color = "black") +
      geom_text(aes(label = Jumlah),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") + theme(rect = element_blank()) + scale_fill_manual(values=c("#ff3f31","#00245d"))
  })
  
  
  
  
  
  
  
  
  
  
  
  #--------------------------------------EPIDEMMIOLOGI------------------------
  
  output$AIDS <- renderValueBox({
    value <- df3a$Aids %>% subset(df3a$Provinsi==input$PROVINCEdf3a & df3a$Tahun == input$TAHUNdf3a)
    infoBox(tags$p("Jumlah Kasus AIDS", style = "front-weight:blod;"),
            value = paste(value),
            color = "blue",
            fill = T,
            icon = icon("virus"))
  })
  
  output$Malaria <- renderValueBox({
    value <- df3a$malaria %>% subset(df3a$Provinsi==input$PROVINCEdf3a & df3a$Tahun == input$TAHUNdf3a)
    infoBox(tags$p("Malaria/1000 orang", style = "front-weight:blod;"),
            value = paste(value),
            color = "blue",
            fill = T,
            icon = icon("virus"))
  })
  
  output$DBD <- renderValueBox({
    value <- df3a$dbd %>% subset(df3a$Provinsi==input$PROVINCEdf3a & df3a$Tahun == input$TAHUNdf3a)
    infoBox(tags$p("DBD/10.000 orang", style = "front-weight:blod;"),
            value = paste(value),
            color = "blue",
            fill = T,
            icon = icon("virus"))
  })
  
  output$KUSTA <- renderValueBox({
    value <- df3a$kusta %>% subset(df3a$Provinsi==input$PROVINCEdf3a & df3a$Tahun == input$TAHUNdf3a)
    infoBox(tags$p("Kusta/100.000 orang", style = "front-weight:blod;"),
            value = paste(value),
            color = "blue",
            fill = T,
            icon = icon("virus"))
  })
  
  
  output$BARdf3a <- renderPlot({
    pan <- filter(df3a, Tahun==input$TAHUNdf3a)
    
    obs.number = input$slider
    
    urut3 <- arrange(pan, get(input$JENISdf3a))
    pan1 <- urut3[1:obs.number,]
    
    ggplot(pan1) +
      aes(x =reorder(Provinsi,-get(input$JENISdf3a)), y = get(input$JENISdf3a)) +
      geom_col(fill = "#112446") +
      theme_minimal() + labs(x="Provinsi", y=input$JENISdf3a) + coord_flip()
    
  })
  
  output$HISTdf4a <- renderPlot({
    num.obs1 =input$slider1
    
    df4ahist <- filter(df4a, Tahun==input$TAHUNdf4a)
    ggplot(df4ahist) +
      aes(x = imunisasi) +
      geom_histogram(bins = num.obs1, fill = "#0B1425") +
      theme_minimal() + labs(x="Balita Penerima Imunisasi Dasar",y="count")
    
    
  })
  
  output$BARdf4a <- renderPlot({
    df4abar <- filter(df4a, Tahun==input$TAHUNdf4a)
    obs.num1 = input$slider2
    
    if(input$urutan == 'Tertinggi'){
      
      datf <- arrange(df4abar, -imunisasi)
      
      df4abarfix <- datf[1:obs.num1,]
      ggplot(df4abarfix) +
        aes(x =reorder(Provinsi,imunisasi), y =imunisasi) +
        geom_col(fill = "#112446") +
        theme_minimal() + labs(x="Balita Penerima Imunisasi Dasar", y="Count") + coord_flip()
      
      
    } else{
      
      datf <- arrange(df4abar, imunisasi)
      
      df4abarfix <- datf[1:obs.num1,]
      ggplot(df4abarfix) +
        aes(x =reorder(Provinsi,-imunisasi), y =imunisasi) +
        geom_col(fill = "#112446") +
        theme_minimal() + labs(x="Balita Penerima Imunisasi Dasar", y="count") + coord_flip()
      
    }
    
  })
  
  
  
  output$linedf8 <- renderPlot({
    datag <- filter(df8, Provinsi==input$PROVINCEDF7)
    ggplot(datag) +
      aes(x = Tahun, y = keluhan,) +
      geom_line(size = 0.5, colour = "#112446") +
      scale_fill_hue(direction = 1) +
      theme_minimal()+ylim(10,40)
    
  })
  
  output$bardf7 <- renderPlot({
    datak <- filter(df7, Provinsi==input$PROVINCEDF7)
    ggplot(datak) +
      aes(x = umur, weight = rokok) +
      geom_bar(fill = "#112446") +
      theme_minimal()
  })
  
 
  
}

shinyApp(ui,server)

