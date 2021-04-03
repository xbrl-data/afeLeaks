#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

styles <- "
.topbar {
    padding: 20px 55px;
    background-color: #203647;
    font-size: 20px;
    color: #fff;
    overflow: hidden;
}
.top_title {
  margin-left: 420px;
  display: flex;
  font-size: 30px;
  position: absolute;
  top: 41%;
  
}
.topbar__img {
  height: auto;
  width: 400px;
}
.top_line {
  border-left: 1px solid #ffffff;
  margin-left: 10px;
  font-weight: 700;
}
.topbar-modal, .tex_sub {
  font-size: 14px;
  color: #EEFBFB;
}
@media only screen and (min-width: 768px) {
  .topbar, .tex_sub {
    font-size: 20px;
  }
}
@media only screen and (min-width: 1024px) {
  .topbar, .tex_sub {
    font-size: 32px;
  }
}
}
"

library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(leafgl)
library(sf)
library(highcharter)
library(bsplus)
library(tablerDash)
library(tidyverse)
library(lubridate)
library(dbplyr)
library(DBI)
library(pool)
library(RMySQL)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = readRDS('./data/login.rds')$dbName,
  host = readRDS('./data/login.rds')$HOST,
  username =readRDS('./data/login.rds')$user,
  password = readRDS('./data/login.rds')$pass
)

onStop(function() {
  poolClose(pool)
})
#userlist <- readRDS('./data/userlist.rds')


cols <- c('#0D1540', '#06357a', '#00a4e3', '#adafb2', '#a31c37', '#d26400', '#eaa814', '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00')
options(scipen = 999)
options(stringsAsFactors = FALSE)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
impute.mode <- function(x) replace(x, is.na(x), getmode(x))



wellDf <- pool %>% dbReadTable('afeLeaks') %>% rename(Complete = comp, Operator = operator, Sand = sand,State = state,
                                                  Parish = county, Lease = lease, Well = wellNum, Capex = Cost) %>%
    mutate(bhLat = replace(bhLat, is.na(bhLat), surfLat[is.na(bhLat)]),
           bhLong = replace(bhLong, is.na(bhLong), surfLong[is.na(bhLong)]), fpYear = year(Complete), CostPerFt = Capex/perf) %>%
 left_join(readRDS('./data/wellDf.rds'))


# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
  title = 'AFE Leaks',
  tags$style(HTML(styles)),
    theme = bslib::bs_theme(
        version = 4, "border-width" = "3px", "border-radius" = "6px", bootswatch = 'lumen',
        bg = '#EEFBFB', fg = '#203647',
        primary = '#4DA8DA',
        secondary  = '#007CC7',
        warning = '#B73225',
        base_font = font_google("Inter"),
        heading_font  = font_google("Roboto"),

        code_font = font_google("Inter"),
        "font-size-base" = "0.75rem", "enable-rounded" = T),
    # ui <- panelsPage(
        #styles = styles,
        # header =  topbar(title = 'Herramienta | CO2',
        #        image = 'https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fupload.wikimedia.org%2Fwikipedia%2Fcommons%2Fthumb%2F0%2F0b%2FQt_logo_2016.svg%2F1200px-Qt_logo_2016.svg.png&f=1&nofb=1',
        #        background_color = 'steelblue'),
        headerPanel(
          
          div(style="", class="topbar",
              fluidRow(   
                
                tags$a(img(class="topbar__img", src = "White logo - no background.svg"),
                       href = 'https://www.shaleInsights.blog', target = "blank"),
                     HTML("<div class = 'top_title'>| &nbsp; AFE Leaks 
                                
                          </div>")

          )
        )),
  
  fluidRow(
    column(width = 12, offset = 0,
           shiny::actionButton(inputId='ab1', label="AFE Leaks Dataset", 
                               icon = icon("dollar-sign"), class = "btn-success",
                               onclick ="window.open('https://shaleinsights.blog/product/afe-leaks-data/', '_blank')"),
    bs_modal(
      id = "modal_info",
      title = "Info",
      size = "large",
      body = 
        fluidRow(
          tablerProfileCard(
            title = 'Shale Insights',
            width = 12,
            subtitle = 'Data Driven Insights in Shale Oil & Gas.  Contact us at afeleaks@gmail.com.',
            background = 'back.jpg',
            src = 'prof.jpg',
            tablerSocialLinks(
              tablerSocialLink(
                name = 'Shale Insights',
                href = 'https://twitter.com/SInights',
                icon = 'twitter'
              ),
              tablerSocialLink(
                name = 'Faux Right',
                href = 'https://twitter.com/FauxRight',
                icon = 'twitter'
              ),
              tablerSocialLink(
                name = 'Facebook',
                href = 'https://www.facebook.com/shaleInsights',
                icon = 'facebook'
              ),
              tablerSocialLink(
                name = 'LinkedIn',
                href = 'https://www.linkedin.com/in/shale-insights-3b259a1b6/',
                icon = 'linkedin'
              ),
              tablerSocialLink(
                name = 'ShaleInsights.blog',
                href = 'https://shaleInsights.blog',
                icon = 'home'
              ),
              tablerSocialLink(
                name = 'AFE Leaks Data',
                href = 'https://shaleinsights.blog/product/afe-leaks-data/',
                icon = 'dollar-sign'
              )
            )
          )
          
        )
    ),
    bs_button("Contact", button_type = "warning") %>%
      bs_attach_modal(id_modal = "modal_info"),
    bs_modal(
      id = "modal_info2",
      title = "Info",
      size = "large",
      body = 
        (
          fluidRow(
            
            column(width = 12,
                   h4('Data Sources:')),
            column(width = 12,
                   fluidRow(
              tablerBlogCard(
                width = 3,
                title = 'Texas Comptroller',
                href = 'https://mycpa.cpa.state.tx.us/cong/loginForward.do?phase=check',
                author = 'State of Texas',
                "Texas Capex Data"
              ),
              tablerBlogCard(
                width = 3,
                title = 'Texas RRC',
                href = 'https://www.rrc.texas.gov/about-us/resource-center/research/data-sets-available-for-download/',
                author = 'State of Texas',
                "Texas Well Information"
              ),
              tablerBlogCard(
                width = 3,
                title = 'Louisiana DNR',
                href = 'https://ucmwww.dnr.state.la.us/ucmsearch/',
                author = 'State of Louisiana',
                "Louisiana Capex Data"
              ),
              tablerBlogCard(
                width = 3,
                title = 'Louisiana SONRIS',
                href = 'http://www.sonris.com/',
                author = 'State of Louisiana',
                "Louisiana Well Information"
              )
                   )
            ),
            
            column(width = 12,
              h4('Packages Used:')
            ),
         

                 tablerBlogCard(
                   width = 4,
                   title = 'sf',
                   href = 'https://r-spatial.github.io/sf/',
                   src = 'https://user-images.githubusercontent.com/520851/34887433-ce1d130e-f7c6-11e7-83fc-d60ad4fae6bd.gif'
                 ),
                 
                 tablerBlogCard(
                   width = 4,
                   title = 'stringr',
                   href = 'https://stringr.tidyverse.org/',
                   src = 'https://github.com/tidyverse/stringr/raw/master/pkgdown/favicon/apple-touch-icon-152x152.png'
                 ),
                 
         
        tablerBlogCard(
          width = 4,
          title = 'Shiny',
          href = 'https://shiny.rstudio.com/',
          src = 'https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png'
        ),
        tablerBlogCard(
          width = 4,
          title = 'lubridate',
          href = 'https://lubridate.tidyverse.org/',
          src = 'https://github.com/tidyverse/lubridate/raw/master/man/figures/logo.png'
        ),
        tablerBlogCard(
          width = 4,
          title = 'dplyr',
          href = 'https://dplyr.tidyverse.org/',
          src = 'https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/dplyr.png'
        
      ),

        tablerBlogCard(
          width = 4,
          title = 'Highcharter',
          href = 'http://jkunst.com/highcharter/highcharts.html',
          src = 'https://github.com/jbkunst/highcharter/raw/master/docs/logo.png'
        ),
        tablerBlogCard(
          width = 4,
          title = 'tablerDash',
          href = 'https://rinterface.github.io/tablerDash/',
          src = 'https://camo.githubusercontent.com/0f30d966d092be8ac21f4e01a73c52a83e27aa47d3cd54bd9d7981acc55c5145/68747470733a2f2f72696e746572666163652e636f6d2f696e73742f696d616765732f7461626c6572446173682e737667'
        ),
        
        tablerBlogCard(
          width = 4,
          title = 'tidyr',
          href = 'https://www.tidyverse.org/',
          src = 'https://github.com/tidyverse/tidyr/raw/master/man/figures/logo.png'
        
      ),

        
  

        column(width = 4,
         
          tablerBlogCard(
            width = 12,
            title = 'Leaflet',
            href = 'https://rstudio.github.io/leaflet/'
          ),
          tablerBlogCard(
            width = 12,
            title = 'LeafGL',
            href = 'https://github.com/r-spatial/leafgl'
          ),
          tablerBlogCard(
            width = 12,
            title = 'Shiny Widgets',
            href = 'https://github.com/dreamRs/shinyWidgets'
          ),
          tablerBlogCard(
            width = 12,
            title = 'bsplus',
            href = 'http://ijlyttle.github.io/bsplus/'
          ),
          tablerBlogCard(
            width = 12,
            title = 'bslib',
            href = 'https://rstudio.github.io/bslib/'
          )
      
        )
        
    ))),
    bs_button("Info", button_type = "default") %>%
      bs_attach_modal(id_modal = "modal_info2")
  )),
        br(),
     fluidRow(
       
       column(
         width = 2,
         panel(
          selectizeGroupUI(
            id = "my-filters",
            params = list(
              State = list(inputId = "State", title = "State:"),
              Parish = list(inputId = "Parish", title = "County/Parish:"),
              Sand = list(inputId = "Sand", title = "Reservoir:"),
              
              Operator = list(inputId = "Operator", title = "Operator:"),
              Lease = list(inputId = 'Lease', title = "Lease:"),
              Well = list(inputId = 'Well', title = "Well:")
            ),
            label = "",
            inline = F
          ),heading = 'Filters', 
          footer = uiOutput('wellCount'),
          status = 'primary')
         
       ),
       column(width = 10,
              
              leafletOutput('map', height = '550px'))
    ),
  br(),
  fluidRow(
    column(
      width = 6,
      highchartOutput('opPie')
    ),
    column(
      width = 6,
      highchartOutput('countyPie')
      
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      highchartOutput('costTrend', height = '550px')
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      highchartOutput('twoYearOp', height = '450px')
    ),
    column(
      width = 12,
      highchartOutput('twoYearRes')
      
    )
  ),
  fluidRow(
    
    column(
      width = 12,
      highchartOutput('fnd'),
      h5('Does not include Louisiana EUR Forecasts yet.'),
      fluidRow(
        shinyWidgets::prettyRadioButtons('equiv', '', choices = c('BOE', 'MCFE'), status = 'primary', shape = 'square'),
        numericInput('conv', 'Gas to Oil Coversion for Equivalent', value = 6, min = 1)
      )
    )
  )
        

               
            
       
             
                  
    )
    
    server <- function(input, output, session) {
        
    
      
        values <- reactiveValues()
        
        res_mod <- callModule(
            module = selectizeGroupServer,
            id = "my-filters",
            data = wellDf,
            vars = c("State", "Parish", "Sand",  "Operator", "Lease", "Well")
        )
        #
        wellList <- reactive({
            wellDf %>% filter(State %in% res_mod()$State) %>%
                filter(Sand %in% res_mod()$Sand) %>%
                filter(Parish %in% res_mod()$Parish) %>%
                filter(Operator %in% res_mod()$Operator)%>%
                filter(Lease %in% res_mod()$Lease)%>%
                filter(Well %in% res_mod()$Well)
        })

        output$wellCount <- renderUI({
            #req(res_mod())
            #df1 <- wellList() %>% filter(operator %in% res_mod()$operator)
            HTML(paste0("<b>Well Count: </b>", nrow(wellList())))

        })
        
        pts <- reactive({
          if(is.null(wellList())){
            NULL
          } else {
            
            st_as_sf(wellList()) %>% st_set_crs(4326) %>% mutate(type = as.character(st_geometry_type(geom))) %>%
              filter(type == 'LINESTRING') %>% subset(select = -c(type))
          }
          
        })
        
        # pts1 <- reactive({
        #   
        #   if(nrow(wellList() %>% filter(State == 'Louisiana')) == 0||is.null(wellList())){
        #     NULL
        #   } else {
        #     
        #     st_as_sf(wellList() %>% subset(select = -c(geom)) %>% filter(State == 'Louisiana'), coords = c("long", "lat"), crs = 4326)
        #     
        #   }
        # })
        # 
        output$map <- renderLeaflet({
          
          counties <- readRDS('./data/countyShp.rds') #%>% mutate(geom = st_centroid(geometry))
          
        
            leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
              addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
              addLabelOnlyMarkers(data = counties$geom, label = as.character(counties$NAME),
                                  labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
              #addGlPoints(data = pts1(),  popup = pts1()$popup, layerId = 'spP')%>%
              addGlPolylines(data = pts()$geom,color ='#203647',  popup = pts()$popup, weight = 1) 
        
          
          
        })
        
        
        output$opPie <- renderHighchart(
          
          
          highchart() %>%
            hc_add_series( wellList() %>% group_by(Operator) %>% summarise(count = n()) %>% ungroup() %>%
                             arrange(desc(count)), type = 'pie', hcaes(x = Operator, y = count),
                           name = 'Total Wells in Sample',
                           showInLegend = FALSE) %>%
            hc_colors(cols) %>%
            hc_title(text = 'Total Wells by Operator')   %>%
            hc_add_theme(hc_theme_bloom(chart = list(backgroundColor = '#EEFBFB')))
          
        )
        
        output$countyPie <- renderHighchart(
          
          
          highchart() %>%
            hc_add_series( wellList() %>% group_by(Parish) %>% summarise(count = n()) %>% ungroup() %>%
                             arrange(desc(count)), type = 'pie', hcaes(x = Parish, y = count),
                           name = 'Total Wells in Sample',
                           showInLegend = FALSE) %>%
            hc_colors(cols) %>%
            hc_title(text = 'Total Wells by County')     %>%
            hc_add_theme(hc_theme_bloom(chart = list(backgroundColor = '#EEFBFB')))
          
        )
        
        output$costTrend <- renderHighchart({
          
          
          df1 <- wellList() %>% mutate(Quarter = quarter(Complete), 
                                       Year = year(Complete)) %>%
            group_by(Year, Quarter) %>% summarise(Capex2 = as.integer(mean(Capex/perf)),
                                                  Capex = as.integer(mean(Capex)/1000),
                                                  count = n()) %>% ungroup() %>%
            mutate(Date = paste0(Year, '-01-', Quarter*3), Date = as.Date(Date, format = '%Y-%d-%m')) %>%
            select(Date, Capex, Capex2, count) %>% filter(!is.na(Date))
          
          
          highchart() %>%
            hc_add_series(df1, type = 'column', hcaes(x = Date, y = Capex),
                          name = 'Reported D&C, US$Thousands',
                          showInLegend = T) %>%
            hc_add_series(df1, type = 'line', hcaes(x = Date, y = count), color = 'transparent',
                          name = 'Well Count',
                          showInLegend = F) %>%
            #hc_colors(cols[c(3,5)]) %>%
            hc_title(text = 'Average Well Cost Over Time') %>%
            hc_subtitle(text = '<i>By Quarter Completed</i>') %>%
            hc_legend(floating = F,
                      verticalAlign = 'top',
                      align = 'center',
                      layout = 'horizontal',
                      itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
            hc_yAxis_multiples(list(title = list(text = '<b>Capex, US$Thousands</b>'),
                                    labels = list(format = '${value}'), opposite=FALSE),
                               list(title = list(text = '<b>Capex, US$/Ft</b>'),
                                    labels = list(format = '${value}'), opposite=TRUE))%>%
            hc_add_series(df1, type = 'spline', hcaes(x = Date, y = Capex2), yAxis = 1, name = 'Capex Per Ft',
                          showInLegend = T, marker = list(enabled = F)) %>%
            hc_tooltip(shared = T) %>%
            hc_xAxis(type = 'datetime') %>%
            hc_add_theme(hc_theme_bloom(chart = list(backgroundColor = '#EEFBFB'))) %>%
            hc_exporting(enabled = T)
          
        })
        
        
        output$twoYearOp <- renderHighchart({
          
          
          df1 <- wellList() %>% mutate(Quarter = quarter(Complete), 
                                       Year = year(Complete)) %>%
            filter(Year >= max(Year)-1) %>%
            group_by(Operator) %>% summarise(Capex2 = as.integer(mean(Capex/perf)),
                                             Capex = as.integer(mean(Capex)/1000),
                                             count = n()) %>% ungroup() %>%
            slice_max(count, n = 15) %>%
            arrange(desc(Capex2)) 
          
          
          highchart() %>%
            hc_add_series(df1, type = 'column', hcaes(x = Operator, y = Capex),
                          name = 'Reported D&C, US$Thousands',
                          showInLegend = T) %>%
            hc_add_series(df1, type = 'line', hcaes(x = Operator, y = count), color = 'transparent',
                          name = 'Well Count',
                          showInLegend = F) %>%
            #hc_colors(cols[c(3,5)]) %>%
            hc_title(text = 'Average Well Cost Last 2 Active Years') %>%
            hc_subtitle(text = '<i>By Top 15 Operators</i>') %>%
            hc_legend(floating = F,
                      verticalAlign = 'top',
                      align = 'center',
                      layout = 'horizontal',
                      itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
            hc_yAxis_multiples(list(title = list(text = '<b>Capex, US$Thousands</b>'),
                                    labels = list(format = '${value}'), opposite=FALSE),
                               list(title = list(text = '<b>Capex, US$/Ft</b>'),
                                    labels = list(format = '${value}'), opposite=TRUE))%>%
            hc_add_series(df1, type = 'spline', hcaes(x = Operator, y = Capex2), yAxis = 1, name = 'Capex Per Ft',
                          showInLegend = T, marker = list(enabled = F)) %>%
            hc_tooltip(shared = T) %>%
            hc_xAxis(categories = df1$Operator) %>%
            hc_add_theme(hc_theme_bloom(chart = list(backgroundColor = '#EEFBFB'))) %>%
            hc_exporting(enabled = T)
          
        })
        
        output$twoYearRes <- renderHighchart({
          
          
          df1 <- wellList() %>% mutate(Quarter = quarter(Complete), 
                                       Year = year(Complete)) %>%
            filter(Year >= max(Year)-1) %>%
            group_by(Sand) %>% summarise(Capex2 = as.integer(mean(Capex/perf)),
                                         Capex = as.integer(mean(Capex)/1000),
                                         count = n()) %>% ungroup() %>%
            #slice_max(count, n = 15) %>%
            arrange(desc(Capex2)) 
          
          
          highchart() %>%
            hc_add_series(df1, type = 'column', hcaes(x = Sand, y = Capex),
                          name = 'Reported D&C, US$Thousands',
                          showInLegend = T) %>%
            hc_add_series(df1, type = 'line', hcaes(x = Sand, y = count), color = 'transparent',
                          name = 'Well Count',
                          showInLegend = F) %>%
            #hc_colors(cols[c(3,5)]) %>%
            hc_title(text = 'Average Well Cost Last 2 Active Years', align = 'left') %>%
            hc_subtitle(text = '<i>By Reservoir</i>', align = 'left') %>%
            hc_legend(floating = F,
                      verticalAlign = 'top',
                      align = 'center',
                      layout = 'horizontal',
                      itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
            hc_yAxis_multiples(list(title = list(text = '<b>Capex, US$Thousands</b>'),
                                    labels = list(format = '${value}'), opposite=FALSE),
                               list(title = list(text = '<b>Capex, US$/Ft</b>', style = list(fontSize = '18px')),
                                    labels = list(format = '${value}'), opposite=TRUE))%>%
            hc_add_series(df1, type = 'spline', hcaes(x = Sand, y = Capex2), yAxis = 1, name = 'Capex Per Ft',
                          showInLegend = T, marker = list(enabled = F)) %>%
            hc_tooltip(shared = T) %>%
            hc_xAxis(categories = df1$Sand) %>%
            hc_add_theme(hc_theme_bloom(chart = list(backgroundColor = '#EEFBFB'))) %>%
            hc_exporting(enabled = T)
          
        })
        
        
        output$fnd <- renderHighchart({
          
          if(input$conv <= 0){
            NULL
          } else {
            
            df1 <- wellList() %>% mutate(Quarter = quarter(Complete), 
                                         Year = year(Complete)) %>%
              filter(!is.na(oilEUR)) %>% filter(!is.na(gasEUR)) %>%
              filter(Complete <= as.Date('2019-12-31')) %>%
              mutate(oilEUR = oilEUR*input$conv, eur = oilEUR+gasEUR, fnd = Capex/eur) %>% filter(fnd < 10) %>%
              group_by(Year, Quarter) %>% summarise(fnd = mean(fnd),
                                                    count = n()) %>% ungroup() %>%
              mutate(Date = paste0(Year, '-01-', Quarter*3), Date = as.Date(Date, format = '%Y-%d-%m')) %>%
              select(Date, fnd, count) %>% filter(!is.na(Date))
            
            if(nrow(df1) == 0){
              NULL
            } else {
              
              if(input$equiv == 'BOE'){
                df1$fnd <- df1$fnd*input$conv
              }
              
              
              
              highchart() %>%
                hc_add_series(df1, type = 'column', hcaes(x = Date, y = round(fnd, 1)),
                              name = glue::glue('FND, $/{input$equiv}'),
                              showInLegend = T) %>%
                hc_add_series(df1, type = 'line', hcaes(x = Date, y = count),# color = 'transparent',
                              name = 'Well Count', yAxis = 1, marker = list(enabled = F),
                              showInLegend = T) %>%
                #hc_colors(cols[c(3,5)]) %>%
                hc_title(text = 'Average Finding & Development Cost Over Time', align = 'left') %>%
                hc_subtitle(text = '<i>Capex/EUR By Quarter Completed</i>', align = 'left') %>%
                hc_legend(floating = F,
                          verticalAlign = 'top',
                          align = 'center',
                          layout = 'horizontal',
                          itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
                hc_yAxis_multiples(list(title = list(text = glue::glue('<b>F&D, US$/{input$equiv}</b>')),
                                        labels = list(format = '${value}'), opposite=FALSE),
                                   list(title = list(text = '<b>Well Count</b>'),
                                        labels = list(format = '{value}'), opposite=TRUE))%>%
                
                hc_tooltip(shared = T) %>%
                hc_xAxis(type = 'datetime') %>%
                hc_add_theme(hc_theme_bloom(chart = list(backgroundColor = '#EEFBFB'))) %>%
                hc_exporting(enabled = T)
            }
          }
        })

      
    }
# Run the application 
shinyApp(ui = ui, server = server)
