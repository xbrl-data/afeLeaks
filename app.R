library(shiny)
library(shinyjs)
library(tablerDash)
library(shinyWidgets)
library(shinyEffects)
library(shinyBS)
library(shinybusy)
library(feedeR)
library(rvest)
library(httr)
#library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(highcharter)
library(quantmod)
library(glue)
library(lubridate)
library(edgarWebR)
library(DT)
library(sf)
library(kableExtra)
library(zoo)
library(finreportr)
library(leaflet)
library(leafgl)
#new
library(SDAR)
library(plotly)
library(rhandsontable)
library(bsplus)
library(readr)


cols <- c('#0D1540', '#06357a', '#00a4e3', '#adafb2', '#a31c37', '#d26400', '#eaa814', '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00')
options(scipen = 999)
options(stringsAsFactors = FALSE)
options(shiny.maxRequestSize=30*1024^2)

options(scipen = 999)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
impute.mode <- function(x) replace(x, is.na(x), getmode(x))



#userlist <- readRDS('./data/userlist.rds')


wellDf <- readRDS('./data/wellDf.rds') %>% rename(Complete = comp, Operator = operator, Sand = sand,State = state,
                                                  Parish = county, Lease = lease, Well = wellNum, Capex = Cost) %>%
  mutate(bhLat = replace(bhLat, is.na(bhLat), surfLat[is.na(bhLat)]),
         bhLong = replace(bhLong, is.na(bhLong), surfLong[is.na(bhLong)]))

# filings <- function(comp.ticker) {
#   
#   FilingsonEdgar <- edgarWebR::company_filings(x = comp.ticker, type = "10-")
#   FilingsonEdgar <- FilingsonEdgar %>% mutate(QUARTER = quarter(filing_date)-1, YEAR = year(filing_date))
#   
#   FilingsonEdgar$QUARTER[FilingsonEdgar$QUARTER == 0] <- 4
#   FilingsonEdgar$YEAR[FilingsonEdgar$QUARTER == 4] <- FilingsonEdgar$YEAR[FilingsonEdgar$QUARTER == 4] -1
#   FilingsonEdgar$PERIOD <- paste0('Q', FilingsonEdgar$QUARTER, FilingsonEdgar$YEAR) 
#   FilingsonEdgar <- FilingsonEdgar %>% filter(YEAR >= 2010)
#   #FilingsonEdgar <- FilingsonEdgar%>% filter(!grepl('A', type))
#   FilingsonEdgar <- FilingsonEdgar %>% 
#     mutate(PERIOD = replace(PERIOD, grepl('A', type), paste0(PERIOD[grepl('A',type)], 'A'))) %>%
#     filter(!grepl('A', type)) %>% mutate(Ticker = comp.ticker) %>% mutate(PERIOD2= paste0(Ticker,PERIOD))
#   
#   return(FilingsonEdgar)
# }
# 
# elapsed_months <- function(end_date, start_date) {
#   ed <- as.POSIXlt(end_date) %m+% days(1)
#   sd <- as.POSIXlt(start_date)
#   12 * (ed$year - sd$year) + (ed$mon - sd$mon)
# }
# 
# #comp.ticker <- 'HAL'
# 
# xbrlData <- function(filings){
#   
#   FilingsonEdgar <- filings
#   
#   df1 <- lapply(split(FilingsonEdgar, FilingsonEdgar[,'PERIOD2']), function (df1) tryCatch({
#     
#     ### Future Loop 1
#     DocumentsonEdgar <-  edgarWebR::filing_documents(x = df1$href)
#     link <- DocumentsonEdgar[DocumentsonEdgar[5] == 'XML'|DocumentsonEdgar[5] == 'EX-101.INS', 4]
#     
#     xbrl.vars <- XBRL::xbrlDoAll(link, cache.dir = './data/', verbose=TRUE)
#     
#     
#     
#     xbrl.vars
#     
#   },
#   error = function(e) {
#     e
#     NULL
#   }))
#   
#   df1 <- df1[lengths(df1) != 0]
#   
#   comp.ticker <- filings$Ticker[1]
#   
#   facts <- data.frame()
#   labels <- data.frame()
#   preso <- data.frame()
#   roles <- data.frame()
#   context <-data.frame()
#   calcs <- data.frame()
#   elements <- data.frame()
#   units <- data.frame()
#   defs <- data.frame()
#   i<-1
#   
#   while(i <= length(df1)){
#     df2 <- df1[[i]]$fact %>% mutate(period = names(df1)[i], ticker = comp.ticker)%>% 
#       mutate(elementId = stringr::word(elementId, 2, sep = '_'))%>% subset(select = -c(scale, sign, factId, ns)) %>%
#       mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% filter(!is.na(fact))
#     facts <- dplyr::bind_rows(facts, df2)
#     
#     df2 <- df1[[i]]$label %>% mutate(period = names(df1)[i], ticker = comp.ticker)%>% 
#       mutate(elementId = stringr::word(elementId, 2, sep = '_')) %>% subset(select = -c(lang, href)) %>%
#       mutate(labelString = trimws(gsub("\\[[^\\]]*\\]", "", labelString, perl=TRUE)))
#     labels <- dplyr::bind_rows(labels, df2)
#     
#     df2 <- df1[[i]]$presentation %>% mutate(period = names(df1)[i], ticker = comp.ticker) %>% 
#       mutate(toElementId = stringr::word(toElementId, 2, sep = '_'),
#              fromElementId = stringr::word(fromElementId, 2, sep = '_'))%>%
#       subset(select = -c(fromHref,toHref, contextElement, usable, closed, arcrole))
#     preso <- dplyr::bind_rows(preso, df2)
#     
#     df2 <- df1[[i]]$role %>% mutate(period = names(df1)[i], ticker = comp.ticker) %>% 
#       select(roleId, type, description,  period, ticker)
#     roles <- dplyr::bind_rows(roles, df2)
#     
#     df2 <- df1[[i]]$context %>% mutate(period = names(df1)[i], ticker = comp.ticker)%>% 
#       mutate(value1 = stringr::word(value1, 2, sep = ':'),
#              value2 = stringr::word(value2, 2, sep = ':'),
#              value3 = stringr::word(value3, 2, sep = ':'),
#              value4 = stringr::word(value4, 2, sep = ':')) %>%
#       subset(select = -c(dimension1, dimension2, dimension3, dimension4, scheme, identifier))
#     context <- dplyr::bind_rows(context, df2)
#     
#     df2 <- df1[[i]]$calculation %>% mutate(period = names(df1)[i], ticker = comp.ticker)%>% 
#       mutate(toElementId = stringr::word(toElementId, 2, sep = '_'),
#              fromElementId = stringr::word(fromElementId, 2, sep = '_')) %>%
#       subset(select = -c(fromHref,toHref, contextElement, usable, closed, order, preferredLabel))
#     calcs <- dplyr::bind_rows(calcs, df2)
#     
#     df2 <- df1[[i]]$element %>% mutate(period = names(df1)[i], ticker = comp.ticker)%>% 
#       mutate(elementId = stringr::word(elementId, 2, sep = '_'))%>%
#       subset(select = -c(type, substitutionGroup, abstract, nillable, ns))
#     elements <- dplyr::bind_rows(elements, df2)
#     
#     df2 <- df1[[i]]$unit %>% mutate(period = names(df1)[i])
#     units <- dplyr::bind_rows(units, df2)
#     
#     df2 <- df1[[i]]$definition %>% mutate(period = names(df1)[i], ticker = comp.ticker) %>%
#       mutate(toElementId = stringr::word(toElementId, 2, sep = '_'),
#              fromElementId = stringr::word(fromElementId, 2, sep = '_')) %>%
#       subset(select = -c(fromHref,toHref, contextElement, usable, closed, order, preferredLabel))
#     defs <- dplyr::bind_rows(defs, df2)
#     
#     i <- i+ 1
#   }
#   
#   labels <- labels %>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
#                               yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
#     filter(!duplicated(paste0(elementId, labelRole))) %>% subset(select = -c(yr, qtr, period))
#   
#   #labels <- df1$label
#   
#   
#   df1 <- list(facts, labels, preso, roles, context, calcs, elements, units, defs)
#   names(df1) <- c('fact', 'label', 'presentation', 'role', 'context', 'calculation', 'element', 'unit', 'definition')
#   
#   rm(facts, labels, preso, roles, context, calcs, elements, units, defs)
#   
#   df1$role <- df1$role%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
#                                  yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr))%>%
#     filter(!duplicated(roleId))%>% filter(type == 'Statement'|type == 'Disclosure' & grepl('DETAILS', toupper(roleId))|
#                                             type == 'Disclosure' & grepl('DETAILS', toupper(description))) %>%
#     filter(!grepl('PARENTH', toupper(roleId))) %>% subset(select = -c(period, ticker, yr, qtr)) %>%
#     mutate(type = replace(type, grepl('SUPPLEMENTAL', toupper(roleId)), 'Disclosure'))
#   
#   
#   
#   return(df1)
# }


# shiny app code
shiny::shinyApp(
  ui = (
    tablerDashPage(
    enable_preloader = TRUE,
    loading_duration = 2,
    navbar = tablerDashNav(
      id = "mymenu",
      src = "back.jpg",
      
      useShinyjs(),

      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          tabName = "info",
          icon = "home",
          "Info"
        ),
        tablerNavMenuItem(
          tabName = "afeLeaks",
          icon = "box",
          "AFE Leaks"
        ),
        # tablerNavMenuItem(
        #   tabName = "filings",
        #   icon = "dollar-sign",
        #   "Financials"
        # ),
        tablerNavMenuItem(
          tabName = "log",
          icon = "disc",
          "Log Analysis"
        )
      )#,
      

      
    ),
    footer = tablerDashFooter(
      copyrights = "@Shale Insights, 2021"
    ),
    title = "AFE Leaks from Shale Insights",
    body = tablerDashBody(
      
    
      useShinyjs(),
      
      # custom jquery to hide some inputs based on the selected tag
      # actually tablerDash would need a custom input/output binding
      # to solve this issue once for all
      tags$head(

        
        # test whether mobile or not
        tags$script(
          "$(document).on('shiny:connected', function(event) {
            var isMobile = /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
            Shiny.onInputChange('isMobile', isMobile);
          });
          "
        #)
        ),
        tags$style(HTML('
                                /* body */
                                .page-main {
                                background-color: #d5e4eb;
                                }

                                /* cards */

                                .card {
                                background-color: white;
                                max-height: 2000px;
  
                                border-radius: 0.5rem;
                                box-shadow: rgba(0, 15, 23, 0.08) 0px 0.125rem 0.5rem 0px, rgba(0, 15, 23, 0.08) 0px 0.25rem 0.75rem 0px;

                                }

                                '))
      ),
      
      # custom shinyWidgets skins
      chooseSliderSkin("Round"),
      
      # use shinyEffects
      setShadow(class = "galleryCard"),
      setZoom(class = "galleryCard"),
      
        tablerTabItems(
          tablerTabItem(
            tabName = "info",
            # hidden(div(
            #   id = 'infoHide',
              add_busy_bar(color = cols[2]),
            tablerInfoCard(
              value = "AFE Leaks Disclaimer",
              status= "primary",
              icon = "home",
              description = 'This website does not provide financial advice.
          The information contained on this Website and
          the resources available for download through
          this website is not intended as, and
          shall not be understood or construed as,
          financial advice.  I am not an attorney,
          accountant, or financial advisor, nor am
          I holding myself out to be, and the information
          contained on this Website is not a substitute
          for financial advice from a professional who
          is aware of the facts and circumstances of
          your individual station.
        
          We have done our best to ensure that the
          information provided on this Website and the
          resources available for download are
          accurate and provide valuable information.
          Using the data on this site is at your own 
          risk and you acknowledge that it may not be 100% accurate.
          Regardless of anything to the contrary,
          nothing available on or through this
          Website should be understood as a recommendation
          that you should not consult with a financial
          professional to address your particular
          information.  We expressly recommend that
          you seek advice from a professional.',
              width = 12
            ),
            fluidRow(
              column(width = 6,
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
              ),
              fluidRow(
                column(width = 4,
                  tablerBlogCard(
                    width = 12,
                    title = 'sf',
                    href = 'https://r-spatial.github.io/sf/',
                    src = 'https://user-images.githubusercontent.com/520851/34887433-ce1d130e-f7c6-11e7-83fc-d60ad4fae6bd.gif'
                  ),
                  
                  tablerBlogCard(
                    width = 12,
                    title = 'stringr',
                    href = 'https://stringr.tidyverse.org/',
                    src = 'https://github.com/tidyverse/stringr/raw/master/pkgdown/favicon/apple-touch-icon-152x152.png'
                  ),
                  
                  tablerBlogCard(
                    width = 12,
                    title = 'glue',
                    href = 'https://glue.tidyverse.org/',
                    src = 'https://github.com/tidyverse/glue/raw/master/pkgdown/favicon/apple-touch-icon-180x180.png'
                  )
                ),
                column(width = 4,
                       tablerBlogCard(
                         width = 12,
                         title = 'shinyEffects',
                         href = 'https://rinterface.github.io/shinyEffects/',
                         src = 'https://github.com/RinteRface/shinyEffects/raw/master/man/figures/shinyEffects_pink.png'
                       ),
                       
                       
                       
                       tablerBlogCard(
                         width = 12,
                         title = 'shinyjs',
                         href = 'https://deanattali.com/shinyjs/',
                         src = 'https://github.com/daattali/shinyjs/raw/master/inst/img/shinyjs-logo-whitebg-small.png'
                       ),
                       tablerBlogCard(
                         width = 12,
                         title = 'finreportr',
                         href = 'https://github.com/sewardlee337/finreportr'
                       ),
                       tablerBlogCard(
                         width = 12,
                         title = 'DT',
                         href = 'https://rstudio.github.io/DT/'
                       ),
                       tablerBlogCard(
                         width = 12,
                         title = 'zoo',
                         href = 'https://r-forge.r-project.org/projects/zoo/'
                       )
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
                    title = 'shinyBS',
                    href = 'https://ebailey78.github.io/shinyBS/'
                  ),
                  tablerBlogCard(
                    width = 12,
                    title = 'shinybusy',
                    href = 'https://dreamrs.github.io/shinybusy/'
                  ),
                  tablerBlogCard(
                    width = 12,
                    title = 'httr',
                    href = 'https://httr.r-lib.org/'
                  )
                )
              )
            ),
            column(
              width = 6,
              fluidRow(
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
              )
              ),
              fluidRow(
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
              )
              ),
              fluidRow(
                
                tablerBlogCard(
                  width = 4,
                  title = 'feedeR',
                  href = 'https://github.com/datawookie/feedeR',
                  src = 'https://github.com/datawookie/feedeR/raw/master/man/figures/feedeR-hex.png'
                ),
                
                tablerBlogCard(
                  width = 4,
                  title = 'rvest',
                  href = 'https://rvest.tidyverse.org/',
                  src = 'https://github.com/tidyverse/rvest/raw/master/man/figures/logo.png'
                ),
                
                tablerBlogCard(
                  width = 4,
                  title = 'kableExtra',
                  href = 'https://haozhu233.github.io/kableExtra/',
                  src = 'https://github.com/haozhu233/kableExtra/raw/master/docs/kableExtra.svg'
                )
                
              ),
              fluidRow(
                
                tablerBlogCard(
                  width = 4,
                  title = 'quantmod',
                  href = 'http://www.quantmod.com/'
                ),
                tablerBlogCard(
                  width = 4,
                  title = 'edgarWebR',
                  href = 'https://mwaldstein.github.io/edgarWebR/'
                )
              )
            )

            ) 
              #))
            ),
          tablerTabItem(
            tabName = "afeLeaks",
            tablerInfoCard(
              value = "Data Sourcing",
              status= "primary",
              icon = "box",
              description = 'We collect data from  various state websites, and neither we nor
        those states endorse the information or make any statements
        verifying the accuracy of such data.  We interpret this data and
        present it here.  This data can be made available in raw format.  Please reach out to us at
        afeleaks@gmail.com for more information.  All data was collected between 7/1/2020 and 12/29/2020.  The data is
        available from the following sources, which is all available publicly:',
              width = 12
            ),
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
            ),
            fluidRow(
              column(
                width = 4,
                selectizeGroupUI(
                  id = "my-filters",
                  params = list(
                    State = list(inputId = "State", title = "State:"),
                    Parish = list(inputId = "Parish", title = "County/Parish:"),
                    Sand = list(inputId = "Sand", title = "Sand:"),
                    
                    Operator = list(inputId = "Operator", title = "Operator:"),
                    Lease = list(inputId = 'Lease', title = "Lease:"),
                    Well = list(inputId = 'Well', title = "Well:")
                  ),
                  label = "Filters",
                  inline = FALSE
                ),
                
                uiOutput('wellCount')
              ),
              column(width = 8,
                     tablerCard(
                       #title = 'Well Locations',
                       width = 12,
                       collapsible = T,
                       closable = F,
                       p('Well Information Shown when Clicked'),
                     leafletOutput('map', height = '500px'))
              )
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
                highchartOutput('costTrend')
              )
            ),
            br(),
            fluidRow(
              column(
                width = 6,
                highchartOutput('twoYearOp')
              ),
              column(
                width = 6,
                highchartOutput('twoYearRes')
                
              )
            ),
            fluidRow(
              shinyWidgets::prettyRadioButtons('equiv', '', choices = c('BOE', 'MCFE'), status = 'primary', shape = 'square'),
              numericInput('conv', 'Gas to Oil Coversion for Equivalent', value = 6, min = 1),
              column(
                width = 12,
                h5('Does not include Louisiana EUR Forecasts yet.'),
                highchartOutput('fnd')
              )
            )
            # )
            # hidden(div(
            #   id = 'infoHide',

            
            
            #))
          ),
          # tablerTabItem(
          #   tabName = 'filings',
          #   
          #   
          #   tablerInfoCard(
          #     value = "Financials with XBRL",
          #     status= "primary",
          #     icon = "dollar-sign",
          #     description = 'Another project of ours is scraping XBRL data.  Still working through it, but in the name of saving storage
          #     space, this tab allows you to select a company and time period, and then will pull the data from the SEC website.  The common
          #     tables are shown, and you can select custom tables from the filings as well as show some common calculations from the filings.
          #     Be patient, the longer the time period, the longer it takes to pull from the SEC site.  A black flashing box indicates
          #     something is loading, and nothing else will work during this time.  Clicking around will just slow it down.',
          #     width = 12
          #   ),
          #   selectInput('operator', 'Select Company', choices = sort(userlist$tickers), selected = sort(userlist$tickers)[513]),
          #   fluidRow(
          #     column(
          #       width = 8,
          #       
          #       tablerCard(
          #         #title = 'Share Price Performance',
          #         closable = F,
          #         width = 12,
          #         highchartOutput('sharePrice', height = '400px')
          #       ),
          #       tablerCard(
          #         title = "News Feed",
          #         closable = F,
          #         width = 12,
          #         DT::dataTableOutput('news')
          #       )
          #     ),
          #     column(
          #       width = 4,
          #       
          #       
          #       tablerCard(
          #         title = 'Filings',
          #         closable = F,
          #         width = 12,
          #         DT::dataTableOutput('frame')
          #       )
          #     )
          #   ),
          #  
          #   
          #   
          #     fluidRow(
          #       sliderTextInput('periodSelect', 'Select Data Range', choices = as.character(seq(2020, 2010, -1)), selected = '2019')
          #       ),
          #     fluidRow(
          #       bsButton('gather', 'Get Financial Data', style = 'primary')
          #     ),
          #   fluidRow(
          #     sliderInput('level', 'Number of SubComponents', min = 1, max = 4, value = 1, step = 1)
          #   ),
          #     br(),
          #   
          #     fluidRow(
          #       column(
          #         width = 12,
          #         
          #         
          #         tablerCard(
          #           title = 'Balance Sheet',
          #           closable = F,
          #           width = 12,
          #           overflow = F,
          #           #div(style = 'overflow-x: scroll; overflow-y: scroll',
          #           #column(width = 12,
          #               #DT::dataTableOutput("bs",width = "100%", height = '25%')#,style = "overflow-y: scroll"
          #           htmlOutput('bs')
          #           #)
          #           #DT::dataTableOutput('bs', height = '500px')
          #         )
          #       ),
          #       column(
          #         width = 12,
          #         
          #         
          #         tablerCard(
          #           title = 'Income Statement',
          #           closable = F,
          #           
          #           width = 12,
          #           overflow = F,
          #           #div(style = 'overflow-x: scroll; overflow-y: scroll',
          #           #column(width = 12,
          #           htmlOutput('is')
          #         )
          #       ),
          #       column(
          #         width = 12,
          #         
          # 
          #         tablerCard(
          #           title = 'Cash Flow Statement',
          #           closable = F,
          #           width = 12,
          #           overflow = F,
          #           #div(style = 'overflow-x: scroll; overflow-y: scroll',
          #           #column(width = 12,
          #           htmlOutput('cf')
          #         )
          #       )
          #     ),
          #     
          #     fluidRow(
          #       
          #       column(width = 12,
          #         tablerCard(
          #         
          #         title = 'Other Table',
          #         closable = F,
          #         width = 12,
          #         overflow = F,
          #         
          #         fluidRow(
          #         selectInput('otherTable', 'Other Table', choices = NULL, selected = NULL)
          #         ),
          # 
          #         
          #         #div(style = 'overflow-x: scroll; overflow-y: scroll',
          #         #column(width = 12,
          #         htmlOutput('ot')
          #       )
          #     
          #   )),
          # 
          #       fluidRow(
          #         column(
          #           width = 12,
          #           
          #           tablerCard(
          #             #title = 'Share Price Performance',
          #             closable = F,
          #             width = 12,
          #             highchartOutput('cR', height = '530px')
          #           )
          #         ),
          #         column(
          #           width = 12,
          #           
          #           tablerCard(
          #             #title = 'Share Price Performance',
          #             closable = F,
          #             width = 12,
          #             highchartOutput('cfs', height = '530px')
          #           )
          #         ),
          #         column(
          #           width = 12,
          #           
          #           tablerCard(
          #             #title = 'Share Price Performance',
          #             closable = F,
          #             width = 12,
          #             highchartOutput('nD', height = '530px')
          #           )
          #         ),
          #         column(
          #           width = 12,
          #           
          #           tablerCard(
          #             #title = 'Share Price Performance',
          #             closable = F,
          #             width = 12,
          #             highchartOutput('ev', height = '530px')
          #           )
          #         ),
          #         column(
          #           width = 12,
          #           
          #           tablerCard(
          #             #title = 'Share Price Performance',
          #             closable = F,
          #             width = 12,
          #             highchartOutput('zS', height = '530px')
          #           )
          #         )
          #       )
          #       
          #     ),
          tablerTabItem(
            tabName = "log",
            # hidden(div(
            #   id = 'infoHide',
           # add_busy_bar(color = cols[2]),
            tablerInfoCard(
              value = "Log Analysis",
              status= "primary",
              icon = "disc",
              description = 'This tab allows a user to beta test log analysis via las log format.',
              width = 12
            ),
           fluidRow(
             column(
               width = 3,
               tablerCard(
                 status = 'primary',
                 width = 12,
                 closable = F,
                 fileInput("file1", "Choose LAS File",
                           multiple = FALSE,
                           accept = c(".las")),
                 
                 textOutput('formCheck1'),
                 selectInput(
                   "depth", 
                   "Depth Track",
                   c("")
                 ),
                 bs_button("View Table") %>%
                   bs_attach_modal(id_modal = "modal_markdown"),
                 bs_modal(
                   id = "modal_markdown", 
                   title = "Data Table",
                   body = fluidRow(DT::dataTableOutput('logTable'),
                                   downloadButton("downloadData", "Download")),
                   footer = tags$span(
                     bs_modal_closebutton("Close")
                   ),
                   size = "large"
                 )
               )
             ),
             column(width = 3,
                    h4('Custom Column:'),
                    p('Allows user to enter a custom formula based on the columns within the file.
                   The common convention is similar to excel.  For example, if I for some reason wanted to
                   double to DPOR column, the user would enter DPOR*2. Common operators (using DPOR as the variable) are:'),
                    p('Simple Math: + - * / ^'),
                    p('Square Root: sqrt(DPOR)'),
                    p('Natural Log: log(DPOR)'),
                    p('Exponential: exp(DPOR)'),
                    p('Log Base 10: log10(DPOR)')
             ),
             column(width = 6,
                    textInput('userColumn', 'Variable Name (Cannot exist in current table)', placeholder = 'DPOR2'),
                    textInput('userFormula', 'Formula (Realize this calculation is meaningless, just an example)', placeholder = 'DPOR*2 + (GR - NPHI)'),
                    actionButton('addFormula', '', icon = icon('plus'), class = "btn-primary"),
                    textOutput('formCheck'))
           ),
           fluidRow(
             
             column(width = 3,
                    selectizeInput(
                      "track1", 
                      "Track 1",
                      c(""),
                      multiple = T
                    ),
                    
                    
                    radioButtons('track1Type', '', choices = c('log', 'linear'), selected = 'linear')
             ),
             column(width = 3,
                    
                    selectizeInput(
                      "track2", 
                      "Track 2",
                      c(""),
                      multiple = T
                    ),
                    radioButtons('track2Type', '', choices = c('log', 'linear'), selected = 'linear')
             ),
             column(width = 3,
                    
                    selectizeInput(
                      "track3", 
                      "Track 3",
                      c(""),
                      multiple = T
                    ),
                    radioButtons('track3Type', '', choices = c('log', 'linear'), selected = 'linear')),
             column(width = 3,
                    
                    selectizeInput(
                      "track4", 
                      "Track 4",
                      c(""),
                      multiple = T
                    ),
                    radioButtons('track4Type', '', choices = c('log', 'linear'), selected = 'linear'))
           ),
           tablerCard(
             title = "Log Presentation",
             status = "primary",
             closable = F,
             width = 12,
             
             column(width = 12,
                    sliderInput("depthSlide", label = h3("Depth Interval"), min = 0, 
                                max = 100, value = c(40, 60), width = '100%')
                    
                    
             ),
             fluidRow(
               #column(width = 3),
               column(width = 3,
                      rhandsontable::rHandsontableOutput('track1Scale')),
               column(width = 3,
                      rhandsontable::rHandsontableOutput('track2Scale')),
               column(width = 3,
                      rhandsontable::rHandsontableOutput('track3Scale')),
               column(width = 3,
                      rhandsontable::rHandsontableOutput('track4Scale'))
             ),
             fluidRow(
               #column(width = 3),
               column(width = 3,
                      
                      plotlyOutput('track1Plot', height = '700px')),
               column(width = 3,
                      
                      plotlyOutput('track2Plot', height = '700px')),
               column(width = 3,
                      
                      plotlyOutput('track3Plot', height = '700px')),
               column(width = 3,
                      
                      plotlyOutput('track4Plot', height = '700px'))
             )
             
           ),
           fluidRow(
             column(width = 4,
                    tablerCard(
                      closable = F,
                      status = 'primary',
                      width = 12,
                      
                      DT::dataTableOutput('topsList')
                    )),
             column(width = 4,
                    tablerCard(
                      title = 'Cutoffs',
                      status = 'primary',
                      closable = F,
                      width = 12,
                      rHandsontableOutput('cutoffs')
                    )),
             column(width = 4,
                    tablerCard(
                      title = 'Pay',
                      status = 'primary',
                      closable = F,
                      width = 12,
                      DT::dataTableOutput('pay')
                    )
             )
           )
           
           
           
          )
          )
          
      )
    )
  ),
  server = function(input, output, session) {
    
    values <- reactiveValues()
    
    
    
    # determine whether we are on mobile or not
    # relies on a simple Shiny.onInputChange
    isMobile <- reactive(input$isMobile)
    # shinyjs::hide('fieldHide')
    # shinyjs::hide('areaHide')
    # shinyjs::hide('companyHide')
    shinyjs::show('dropHide')
    shinyjs::show('infoHide')
    # shinyjs::hide('allHide')
    
    #shinyjs::enable('login')
    # shinyjs::hide('login')
    # shinyjs::hide('wmacUser')
    # shinyjs::hide('wmacPass')
    res_mod <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = wellDf,
      vars = c("State", "Parish", "Sand",  "Operator", "Lease", "Well")
    )
    
    wellList <- reactive({
      wellDf %>% filter(State %in% res_mod()$State) %>%
        filter(Sand %in% res_mod()$Sand) %>%
        filter(Parish %in% res_mod()$Parish) %>%
        filter(Operator %in% res_mod()$Operator)%>%
        filter(Lease %in% res_mod()$Lease)%>%
        filter(Well %in% res_mod()$Well)
    })
    
    pts <- reactive({
      if(nrow(wellList() %>% filter(State == 'Texas')) == 0||is.null(wellList())){
        NULL
      } else {
      
        st_as_sf(wellList()) %>% st_set_crs(4326) %>% filter(State == 'Texas')
      }

    })
    
    pts1 <- reactive({
      
      if(nrow(wellList() %>% filter(State == 'Louisiana')) == 0||is.null(wellList())){
        NULL
      } else {
      
        st_as_sf(wellList() %>% subset(select = -c(geom)) %>% filter(State == 'Louisiana'), coords = c("long", "lat"), crs = 4326)
   
      }
    })
    
    output$wellCount <- renderUI({
      #req(res_mod())
      #df1 <- wellList() %>% filter(operator %in% res_mod()$operator)
      HTML(paste0("<b>Well Count: </b>", nrow(wellList())))
      
    })
    
    
    
    output$map <- renderLeaflet({
      
      counties <- readRDS('./data/countyShp.rds') #%>% mutate(geom = st_centroid(geometry))
      
      if(is.null(pts())||nrow(pts())==0){
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addLabelOnlyMarkers(data = counties$geom, label = as.character(counties$NAME),
                              labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
          addGlPoints(data = pts1(),  popup = pts1()$popup, layerId = 'spP')
      } else if(is.null(pts1())||nrow(pts1())==0){
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addLabelOnlyMarkers(data = counties$geom, label = as.character(counties$NAME),
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
        #addGlPoints(data = pts1(),  popup = pts1()$popup, layerId = 'spP')%>%
        addGlPolylines(data = pts()$geom, popup = pts()$popup, layerId = 'spC') 
      } else {
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addLabelOnlyMarkers(data = counties$geom, label = as.character(counties$NAME),
                              labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
          addGlPoints(data = pts1(),  popup = pts1()$popup, layerId = 'spP')%>%
          addGlPolylines(data = pts()$geom, popup = pts()$popup, layerId = 'spC')
      }
    
      
    })
    
    
    output$opPie <- renderHighchart(
      
      
      highchart() %>%
        hc_add_series( wellList() %>% group_by(Operator) %>% summarise(count = n()) %>% ungroup() %>%
                         arrange(desc(count)), type = 'pie', hcaes(x = Operator, y = count),
                      name = 'Total Wells in Sample',
                      showInLegend = FALSE) %>%
        hc_colors(cols) %>%
        hc_title(text = 'Total Wells by Operator', align = 'left')  
      
    )
    
    output$countyPie <- renderHighchart(
      
      
      highchart() %>%
        hc_add_series( wellList() %>% group_by(Parish) %>% summarise(count = n()) %>% ungroup() %>%
                         arrange(desc(count)), type = 'pie', hcaes(x = Parish, y = count),
                       name = 'Total Wells in Sample',
                       showInLegend = FALSE) %>%
        hc_colors(cols) %>%
        hc_title(text = 'Total Wells by County', align = 'left')  
      
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
        hc_colors(cols[c(3,5)]) %>%
        hc_title(text = 'Average Well Cost Over Time', align = 'left') %>%
        hc_subtitle(text = '<i>By Quarter Completed</i>', align = 'left') %>%
        hc_legend(floating = F,
                  verticalAlign = 'top',
                  align = 'center',
                  layout = 'horizontal',
                  itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
        hc_yAxis_multiples(list(title = list(text = '<b>Capex, US$Thousands</b>', style = list(fontSize = '10px')),
                                labels = list(format = '${value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=FALSE),
                           list(title = list(text = '<b>Capex, US$/Ft</b>', style = list(fontSize = '10px')),
                                labels = list(format = '${value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=TRUE))%>%
        hc_add_series(df1, type = 'spline', hcaes(x = Date, y = Capex2), yAxis = 1, name = 'Capex Per Ft',
                      showInLegend = T, marker = list(enabled = F)) %>%
        hc_tooltip(shared = T) %>%
        hc_xAxis(type = 'datetime', title = list(text = '<b></b>', style = list(fontSize = '12px')),
                 labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))
      
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
        hc_colors(cols[c(3,5)]) %>%
        hc_title(text = 'Average Well Cost Last 2 Active Years', align = 'left') %>%
        hc_subtitle(text = '<i>By Top 15 Operators</i>', align = 'left') %>%
        hc_legend(floating = F,
                  verticalAlign = 'top',
                  align = 'center',
                  layout = 'horizontal',
                  itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
        hc_yAxis_multiples(list(title = list(text = '<b>Capex, US$Thousands</b>', style = list(fontSize = '10px')),
                                labels = list(format = '${value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=FALSE),
                           list(title = list(text = '<b>Capex, US$/Ft</b>', style = list(fontSize = '10px')),
                                labels = list(format = '${value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=TRUE))%>%
        hc_add_series(df1, type = 'spline', hcaes(x = Operator, y = Capex2), yAxis = 1, name = 'Capex Per Ft',
                      showInLegend = T, marker = list(enabled = F)) %>%
        hc_tooltip(shared = T) %>%
        hc_xAxis(categories = df1$Operator, title = list(text = '<b></b>', style = list(fontSize = '12px')),
                 labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))
      
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
        hc_colors(cols[c(3,5)]) %>%
        hc_title(text = 'Average Well Cost Last 2 Active Years', align = 'left') %>%
        hc_subtitle(text = '<i>By Reservoir</i>', align = 'left') %>%
        hc_legend(floating = F,
                  verticalAlign = 'top',
                  align = 'center',
                  layout = 'horizontal',
                  itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
        hc_yAxis_multiples(list(title = list(text = '<b>Capex, US$Thousands</b>', style = list(fontSize = '10px')),
                                labels = list(format = '${value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=FALSE),
                           list(title = list(text = '<b>Capex, US$/Ft</b>', style = list(fontSize = '10px')),
                                labels = list(format = '${value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=TRUE))%>%
        hc_add_series(df1, type = 'spline', hcaes(x = Sand, y = Capex2), yAxis = 1, name = 'Capex Per Ft',
                      showInLegend = T, marker = list(enabled = F)) %>%
        hc_tooltip(shared = T) %>%
        hc_xAxis(categories = df1$Sand, title = list(text = '<b></b>', style = list(fontSize = '12px')),
                 labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))
      
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
            hc_colors(cols[c(3,5)]) %>%
            hc_title(text = 'Average Finding & Development Cost Over Time', align = 'left') %>%
            hc_subtitle(text = '<i>Capex/EUR By Quarter Completed</i>', align = 'left') %>%
            hc_legend(floating = F,
                      verticalAlign = 'top',
                      align = 'center',
                      layout = 'horizontal',
                      itemStyle = list(fontFamily = 'Arial', color = cols[1])) %>%
            hc_yAxis_multiples(list(title = list(text = glue::glue('<b>F&D, US$/{input$equiv}</b>'), style = list(fontSize = '10px')),
                                    labels = list(format = '${value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=FALSE),
                               list(title = list(text = '<b>Well Count</b>', style = list(fontSize = '10px')),
                                    labels = list(format = '{value}', style = list(fontSize = '12px', fontWeight = 'bold')), opposite=TRUE))%>%
    
            hc_tooltip(shared = T) %>%
            hc_xAxis(type = 'datetime', title = list(text = '<b></b>', style = list(fontSize = '12px')),
                     labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))
        }
      }
    })
    
    # 
    # 
    # ##Finance
    # 
    # 
    # 
    # output$sharePrice <- renderHighchart({
    #   
    #   symbol <- input$operator
    #   
    #   if(is.na(symbol)|symbol %in% c('AST', 'EPE', 'SN', 'SRCI')){
    #     NULL
    #   } else {
    #     
    #     hchart(getSymbols(symbol, from = Sys.Date() - lubridate::years(5), auto.assign = FALSE)) %>% #hc_colors(cols) %>%
    #       hc_add_theme(hc_theme_economist()) %>%
    #       hc_title(text = glue::glue("{symbol} Share Price Performance"), align = 'left') %>%
    #       hc_subtitle(text = 'Trailing 5 Years') %>%
    #       hc_credits(enabled = TRUE, text = 'Source: Yahoo Finance, Powered by Highcharts')
    #   }
    #   
    # })
    # 
    # 
    # 
    # 
    # 
    # 
    # observe({ 
    #   if(is.null(input$operator)){
    #     NULL
    #   } else {
    #     values$check <- NULL
    #     opList <- input$operator
    #     
    #     if(is.na(opList)){
    #       opList <- 'MRO'
    #     }
    #     
    #     if(opList %in% c('AST', 'BP', 'EPE', 'REP', 'SN', 'SRCI')){
    #       NULL
    #     } else {
    #     #check <- readRDS('check3.rds') %>% filter(!duplicated(label)) %>% filter(comp.ticker == opList) %>% filter(year > 2013)
    #     comp.ticker <- opList
    #     
    #     check1 <- company_filings(comp.ticker, type = '10-', count = 100, page = 1)
    #     if(nrow(check1) == 0){
    #       NULL
    #     } else {
    #       #check2 <- company_filings(comp.ticker, type = '10-Q', count = 100, page = 1)
    #       #check2 <- check2 %>% filter(year(filing_date) >= 2017)
    #       check <- check1 %>% arrange(desc(filing_date)) %>% filter(type != '10-K/A') %>% filter(type != '10-Q/A') %>% filter(year(filing_date) >= year(today())-10)
    #       rm(check1)
    #       check$comp.ticker <- comp.ticker
    #       compInfo <- CompanyInfo(comp.ticker)
    #       check <- merge(check, compInfo)
    #       rm(compInfo)
    #       
    #       check$accession_number <- check$href
    #       check$accession_number <- gsub('/', ' ', check$accession_number, fixed = T)
    #       check$accession_number <- word(check$accession_number, 8)
    #       check <- check %>% rowwise() %>% 
    #         mutate(
    #           tbls = (data.frame(read_html(href) %>% html_nodes('table') %>% .[1] %>% html_table(fill=TRUE)) %>% 
    #                     filter(grepl('10-Q', Type)|grepl('10-K', Type)))$Document[1])
    #       check$tbls <- gsub('iXBRL', '', check$tbls)
    #       check$tbls <- str_trim(check$tbls)
    #       check$url1 <- paste('https://www.sec.gov/Archives/edgar/data', as.numeric(check$CIK), gsub('-', '', check$accession_number), check$tbls, sep='/')
    #       check$date <- paste('Q', quarter(check$filing_date)-1, year(check$filing_date),sep='')
    #       check <- check %>% mutate(date = replace(date, quarter(filing_date)-1 == 0, paste('Q4', year(filing_date[(quarter(filing_date)-1 == 0)])-1, sep='')))
    #       
    #       #x <- paste0(levels(as.factor(check$date)))
    #       
    #       
    #       values$check <- check
    #     }
    #     }
    #   }
    # })
    # 
    # 
    # 
    # observe({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #     df1 <- values$xbrl
    #     
    #     role1 <- df1$role %>% filter(type != 'Statement') %>% filter(type == 'Disclosure') %>%
    #       filter(grepl('DETAILS', toupper(roleId))|grepl('DETAILS', toupper(description)))
    #     
    #     updateSelectInput(session, 'otherTable', choices = unique(role1$description), selected = role1$description[1])
    #     
    #     
    #     
    #     
    #     
    #   }
    # })
    # 
    # output$news <- DT::renderDataTable({
    #   if(is.null(input$operator)){
    #     NULL
    #   } else {
    #     
    #     comp.ticker <- input$operator
    #     compInfo <- edgarWebR::company_information(comp.ticker)
    #     compInfo$name <- gsub(' ', '%20', compInfo$name)
    #     
    #     feed1 <- feed.extract(paste0("https://news.google.com/rss/search?q=", compInfo$name))
    #     feed1 <- feed1$items %>% select(Date = date, Link = description)
    #     
    #     if(nrow(feed1) == 0){
    #       NULL
    #     } else {
    #     
    #     feed1$Link <- gsub('<a ', "<a target = '_blank' ", feed1$Link, fixed = T)
    #     feed1$Date <- as.Date(feed1$Date)
    #     
    #     DT::datatable(feed1, rownames = FALSE, escape = FALSE,extensions = c('ColReorder', 'FixedHeader','KeyTable'),
    #                   options = list(dom = 'tp', fixedHeader=TRUE, keys=TRUE,
    #                                  deferRender = TRUE, 
    #                                  colReorder = TRUE))
    #     
    #     }
    #     
    #   }
    # })
    # 
    # 
    # 
    # observeEvent(input$gather, {
    #   
    #   comp.ticker <- input$operator
    #   
    #   values$xbrl <- xbrlData(filings(comp.ticker = input$operator) %>% filter(YEAR >= as.numeric(input$periodSelect)))
    # 
    #   
    #   
    # })
    # 
    # output$frame <- DT::renderDataTable({
    #   if(is.null(values$check)){
    #     NULL
    #   } else {
    #     my_test <- values$check %>% arrange(desc(filing_date))# %>% filter(date %in% input$Filing)
    #     #nodes <- read_html(my_test$url1) %>% html_nodes('table') #%>% #.[7] %>% html_table(fill=TRUE)
    #     
    #     
    #     #print(my_test)
    #     
    #     my_test <- as.data.frame(my_test[,c('date', 'url1', 'filing_date')])
    #     my_test$url1 <- paste0("<a target='_blank' href='",my_test$url1,"'>",my_test$date,"</a>")
    #     my_test$filing_date <- date(my_test$filing_date)
    #     names(my_test) <- c('Period', 'Filing','Filing Date')
    #     my_test <- subset(my_test, select=-c(Period))
    #     #print(head(my_test))
    #     DT::datatable(my_test, rownames = FALSE, escape = FALSE,extensions = c('ColReorder', 'FixedHeader','KeyTable'),
    #                   options = list(dom = 'tp', fixedHeader=TRUE, keys=TRUE,
    #                                  pageLength = 30,
    #                                  deferRender = TRUE, 
    #                                  colReorder = TRUE))
    #   }
    # })
    # 
    # bs <- reactive({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #     df1 <- values$xbrl
    #     role1 <- df1$role %>% filter(type == "Statement") %>% filter(grepl('BALANCE', toupper(roleId))|grepl('FINANCIALPOS', toupper(roleId)))
    #     
    #     
    #     dfx <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order = seq(1, n(), 1)) %>% subset(select = -c(order1, order2, id1, id2))
    #     
    #     dfx1 <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order1 = seq(1, n(), 1)) %>% subset(select = -c(order, order2, id1, id2))
    #     
    #     dfx <- dfx %>% left_join(dfx1)
    #     dfx <- dfx %>% rowwise() %>% mutate(order = min(order, order1)) %>% ungroup() %>% subset(select = -c(order1))
    #     
    #     # dfx$order[grepl('PropertyPlantAndEquipmentOtherAccumulatedDepreciation', dfx$toElementId)] <-
    #     #   dfx$order[grepl('PropertyPlantAndEquipmentOtherNet', dfx$toElementId)] - 0.5
    #     # 
    #     # dfx$order[grepl('GainsLossesOnExtinguishmentOfDebt', dfx$toElementId)] <-
    #     #   dfx$order[grepl('InterestExpense', dfx$toElementId)] - 0.5
    #     
    #     bs <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                           yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #       group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #       subset(select = -c(order)) %>% left_join(dfx) %>% 
    #       subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #       left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #       subset(select = -c(yr, qtr,period, ticker)) %>%
    #       mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #       left_join(df1$context) %>% 
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #       #group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #       group_by(startDate, endDate, elementId, fromElementId, value1, value2, value3, value4) %>% filter(date1 == max(date1)) %>%
    #       ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #       filter(year(endDate) >= min(yr)) %>%
    #       mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #       subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #       group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       arrange(order, desc(date1)) %>%
    #       filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #       left_join(df1$label) %>% filter(labelRole == preferredLabel) %>% subset(select = -c(labelRole, preferredLabel, ticker)) %>%
    #       left_join(df1$label %>% select(value1 = elementId, label1 = labelString) %>% 
    #                   group_by(value1) %>% filter(nchar(label1) == min(nchar(label1))) %>% ungroup() %>%
    #                   filter(!duplicated(value1)))%>%
    #       left_join(df1$label %>% select(value2 = elementId, label2 = labelString) %>%# filter(!duplicated(value2))%>% 
    #                   group_by(value2) %>% filter(nchar(label2) == min(nchar(label2))) %>% ungroup() %>%
    #                   filter(!duplicated(value2)))%>%
    #       left_join(df1$label %>% select(value3 = elementId, label3 = labelString) %>% #filter(!duplicated(value3))%>% 
    #                   group_by(value3) %>% filter(nchar(label3) == min(nchar(label3))) %>% ungroup() %>%
    #                   filter(!duplicated(value3)))%>%
    #       left_join(df1$label %>% select(value4 = elementId, label4 = labelString) %>% #filter(!duplicated(value4))%>% 
    #                   group_by(value4) %>% filter(nchar(label4) == min(nchar(label4))) %>% ungroup() %>%
    #                   filter(!duplicated(value4))) %>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('COMMON STOCK', toupper(labelString)), 'Common Stock'))%>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('PREFERRED STOCK', toupper(labelString)), 'Preferred Stock')) %>%
    #       mutate(labelString = gsub('â€™', '', labelString, fixed = T)) %>% mutate(label1 = replace(label1, !is.na(label1), paste0('-',label1[!is.na(label1)])))%>%
    #       mutate(label2 = replace(label2, !is.na(label2), paste0('-',label2[!is.na(label2)])))%>%
    #       mutate(label3 = replace(label3, !is.na(label3), paste0('-',label3[!is.na(label3)])))%>%
    #       mutate(label4 = replace(label4, !is.na(label4), paste0('-',label4[!is.na(label4)])))  %>%
    #       mutate(label1 = replace(label1, is.na(label1), ''),
    #              label2 = replace(label2, is.na(label2), ''),
    #              label3 = replace(label3, is.na(label3), ''),
    #              label4 = replace(label4, is.na(label4), '')) %>%
    #       mutate(level = 5, level = replace(level, label4 == '', 4), level = replace(level, label3 == '', 3),
    #              level = replace(level, label2 == '', 2), level = replace(level, label1 == '', 1)) %>%
    #       mutate(labelString = paste0(labelString, label1, label2, label3, label4)) %>%
    #       subset(select = -c(value1, value2, value3, value4, label1, label2, label3, label4)) %>%
    #       #group_by(fromElementId, labelString) %>% mutate(elementId = getmode(elementId), order = getmode(order)) %>% ungroup() %>% distinct() %>%
    #       left_join(df1$calculation %>% select(elementId = toElementId, arcrole) %>% distinct() %>% filter(!duplicated(elementId))) %>%
    #       mutate(level = replace(level, is.na(arcrole), 0)) %>% subset(select = -c(arcrole)) %>%# mutate(order = replace(order, level == 0, 99)) %>%
    #       mutate(order = as.numeric(order) + level/10) %>%
    #       mutate(fromElementId = gsub('([[:upper:]])', ' \\1', fromElementId),
    #              elementId = gsub('([[:upper:]])', ' \\1', elementId)) %>%
    #       rowwise() %>% mutate(labelString = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), labelString),
    #                            fromElementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), fromElementId),
    #                            elementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), elementId)) %>%
    #       ungroup() %>% left_join(df1$unit %>% select(unitId, measure) %>% distinct() %>% filter(!duplicated(unitId))) %>% subset(select = -c(unitId)) %>%
    #       rename(units = measure)%>% mutate(units = word(units, 2, sep = fixed(":"))) %>% subset(select = -c(endDate)) %>%
    #       mutate(fromElementId = replace(fromElementId, level == 0, paste0('<b>', fromElementId[level == 0], '</b>')),
    #              elementId = replace(elementId, level == 0, paste0('<b>', elementId[level == 0], '</b>')),
    #              labelString = replace(labelString, level == 0, paste0('<b>', labelString[level == 0], '</b>'))) %>% 
    #       distinct() %>%
    #       group_by(fromElementId, elementId,labelString, date1) %>% filter(fact == max(fact)) %>% ungroup() %>% 
    #       group_by(date1, elementId, labelString, level) %>% 
    #       mutate(fromElementId = getmode(fromElementId), order = getmode(order), fact = max(fact), decimals = max(decimals),
    #              units = getmode(units)) %>% ungroup() %>%
    #       filter(!duplicated(paste0(date1, fact, labelString, fromElementId))) %>% 
    #       spread(date1, fact) %>% arrange(order) %>% rename(Parent = fromElementId, GAAP= elementId, Label = labelString) %>%
    #       subset(select = -c(decimals))%>% group_by(Parent, GAAP, Label, level, units, order) %>% summarise_all(sum, na.rm = T) %>%
    #       ungroup() %>% arrange(order) %>% subset(select = -c(order))
    #     
    #     
    #     
    #     bs <- bs[,colSums(is.na(bs))<(nrow(bs)/1.1)]
    #     bs[,6:length(bs)][is.na(bs[,6:length(bs)])] <- 0
    #     
    #     names1 <- data.frame(names1 = names(bs[,6:length(bs)]))
    #     if(grepl('Q', names1$names1[1])){
    #       NULL
    #     } else {
    #       names1$date <- as.Date(substr(names1$names1, nchar(names1$names1)-10,nchar(names1$names1)))
    #       names1$month <- as.numeric(substr(names1$names1, 1, 2))
    #       names1 <- names1 %>% arrange(date, month)
    #       #names2 <- names(bs)
    #       names1 <- append(names(bs)[1:5], names1$names1)
    #       
    #       bs <- bs[,names1]
    #     }
    #     
    #     
    #     
    #     bs1 <- which(rowSums(bs[,6:length(bs)])==0)
    #     if(length(bs1) > 0){
    #       bs <- bs[-bs1,]
    #     }
    #     
    #     bs
    #     
    #     
    #   }
    # })
    # 
    # output$bs <- renderText({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #    
    #     
    #     bs() %>% filter(level <= input$level) %>%
    #       kbl(format = 'html', escape = F) %>%
    #       kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
    #       column_spec(1:4, bold = T, border_right = F) %>%
    #       column_spec(5, bold = T, border_right = T) %>%
    #       #scroll_box(width = "100%", height = "25%")%>%
    #       footnote('Balance Sheet', general_title = '') %>%
    #       scroll_box(width = '100%', height = '500px', fixed_thead = T)
    #     
    #     
    #     # DT::datatable(bs %>% filter(level <= input$level) , escape = F, rownames=FALSE,extensions = c('Buttons', 'Scroller', 'FixedColumns', 'FixedHeader'),
    #     #               options = list(dom='Bfrtip', paging = FALSE,
    #     #                              buttons = c('copy'), scrollY = '500px',
    #     #                              scrollX = T,fixedColumns = list(leftColumns = 3), fixedHeader = TRUE,
    #     #                              info = FALSE, ordering = FALSE, searching = FALSE),
    #     #               caption = htmltools::tags$caption(
    #     #                 style = 'caption-side: bottom; text-align: center;',
    #     #                 'Table: ', htmltools::em('Balance Sheet')),
    #     #               class = 'cell-border stripe')
    #     
    #   }
    # })
    # 
    # is <- reactive({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #     df1 <- values$xbrl
    #     
    #     role1 <- df1$role %>% filter(type == 'Statement') %>% filter(!grepl('PARENT', toupper(roleId))) %>%
    #       filter(!grepl('BALANCE', toupper(roleId))) %>%
    #       filter(!grepl('COMPREHENSIVEINC', toupper(roleId))) %>%
    #       filter(!grepl('COMPREHENSIVELOS', toupper(roleId))) %>%
    #       filter(!grepl('CASHFLOWS', toupper(roleId))) %>%
    #       filter(!grepl('FINANCIALPOS', toupper(roleId)))%>%
    #       filter(!grepl('EQUIT', toupper(roleId)))%>%
    #       filter(!grepl('SUPPLEMENT', toupper(roleId)))
    #     
    #     
    #     if(nrow(role1) == 0){
    #       role1 <- df1$role %>% filter(type == 'Statement') %>% filter(!grepl('PARENT', toupper(roleId))) %>%
    #         filter(!grepl('BALANCE', toupper(roleId))) %>%
    #         #filter(!grepl('COMPREHENSIVEINC', toupper(roleId))) %>%
    #         #filter(!grepl('COMPREHENSIVELOS', toupper(roleId))) %>%
    #         filter(!grepl('CASHFLOWS', toupper(roleId))) %>%
    #         filter(!grepl('FINANCIALPOS', toupper(roleId)))%>%
    #         filter(!grepl('EQUIT', toupper(roleId)))%>%
    #         filter(!grepl('SUPPLEMENT', toupper(roleId)))
    #     }
    #     
    #     
    #     dfx <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order = seq(1, n(), 1)) %>% subset(select = -c(order1, order2, id1, id2))
    #     
    #     dfx1 <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order1 = seq(1, n(), 1)) %>% subset(select = -c(order, order2, id1, id2))
    #     
    #     dfx <- dfx %>% left_join(dfx1)
    #     dfx <- dfx %>% rowwise() %>% mutate(order = min(order, order1)) %>% ungroup() %>% subset(select = -c(order1))
    #     
    #     # dfx$order[grepl('PropertyPlantAndEquipmentOtherAccumulatedDepreciation', dfx$toElementId)] <-
    #     #   dfx$order[grepl('PropertyPlantAndEquipmentOtherNet', dfx$toElementId)] - 0.5
    #     # 
    #     # dfx$order[grepl('GainsLossesOnExtinguishmentOfDebt', dfx$toElementId)] <-
    #     #   dfx$order[grepl('InterestExpense', dfx$toElementId)] - 0.5
    #     
    #     bs <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                           yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #       group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #       subset(select = -c(order)) %>% left_join(dfx) %>% 
    #       subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #       left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #       subset(select = -c(yr, qtr,period, ticker)) %>%
    #       mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #       left_join(df1$context) %>% 
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #       #group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #       group_by(startDate, endDate, elementId, fromElementId, value1, value2, value3, value4) %>% filter(date1 == max(date1)) %>%
    #       ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #       filter(year(endDate) >= min(yr)) %>% filter(monthsOn %in% c(3,6,9,12)) %>%
    #       mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #       subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #       group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       arrange(order, desc(date1)) %>%
    #       filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #       left_join(df1$label) %>% filter(labelRole == preferredLabel) %>% subset(select = -c(labelRole, preferredLabel, ticker)) %>%
    #       left_join(df1$label %>% select(value1 = elementId, label1 = labelString) %>% 
    #                   group_by(value1) %>% filter(nchar(label1) == min(nchar(label1))) %>% ungroup() %>%
    #                   filter(!duplicated(value1)))%>%
    #       left_join(df1$label %>% select(value2 = elementId, label2 = labelString) %>%# filter(!duplicated(value2))%>% 
    #                   group_by(value2) %>% filter(nchar(label2) == min(nchar(label2))) %>% ungroup() %>%
    #                   filter(!duplicated(value2)))%>%
    #       left_join(df1$label %>% select(value3 = elementId, label3 = labelString) %>% #filter(!duplicated(value3))%>% 
    #                   group_by(value3) %>% filter(nchar(label3) == min(nchar(label3))) %>% ungroup() %>%
    #                   filter(!duplicated(value3)))%>%
    #       left_join(df1$label %>% select(value4 = elementId, label4 = labelString) %>% #filter(!duplicated(value4))%>% 
    #                   group_by(value4) %>% filter(nchar(label4) == min(nchar(label4))) %>% ungroup() %>%
    #                   filter(!duplicated(value4))) %>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('COMMON STOCK', toupper(labelString)), 'Common Stock'))%>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('PREFERRED STOCK', toupper(labelString)), 'Preferred Stock')) %>%
    #       mutate(labelString = gsub('â€™', '', labelString, fixed = T)) %>% mutate(label1 = replace(label1, !is.na(label1), paste0('-',label1[!is.na(label1)])))%>%
    #       mutate(label2 = replace(label2, !is.na(label2), paste0('-',label2[!is.na(label2)])))%>%
    #       mutate(label3 = replace(label3, !is.na(label3), paste0('-',label3[!is.na(label3)])))%>%
    #       mutate(label4 = replace(label4, !is.na(label4), paste0('-',label4[!is.na(label4)])))  %>%
    #       mutate(label1 = replace(label1, is.na(label1), ''),
    #              label2 = replace(label2, is.na(label2), ''),
    #              label3 = replace(label3, is.na(label3), ''),
    #              label4 = replace(label4, is.na(label4), '')) %>%
    #       mutate(level = 5, level = replace(level, label4 == '', 4), level = replace(level, label3 == '', 3),
    #              level = replace(level, label2 == '', 2), level = replace(level, label1 == '', 1)) %>%
    #       mutate(labelString = paste0(labelString, label1, label2, label3, label4)) %>%
    #       subset(select = -c(value1, value2, value3, value4, label1, label2, label3, label4)) %>%
    #       #group_by(fromElementId, labelString) %>% mutate(elementId = getmode(elementId), order = getmode(order)) %>% ungroup() %>% distinct() %>%
    #       left_join(df1$calculation %>% select(elementId = toElementId, arcrole) %>% distinct() %>% filter(!duplicated(elementId))) %>%
    #       mutate(level = replace(level, is.na(arcrole), 0)) %>% subset(select = -c(arcrole)) %>%# mutate(order = replace(order, level == 0, 99)) %>%
    #       mutate(order = as.numeric(order) + level/10) %>%
    #       mutate(fromElementId = gsub('([[:upper:]])', ' \\1', fromElementId),
    #              elementId = gsub('([[:upper:]])', ' \\1', elementId)) %>%
    #       rowwise() %>% mutate(labelString = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), labelString),
    #                            fromElementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), fromElementId),
    #                            elementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), elementId)) %>%
    #       ungroup() %>% left_join(df1$unit %>% select(unitId, measure) %>% distinct() %>% filter(!duplicated(unitId))) %>% subset(select = -c(unitId)) %>%
    #       rename(units = measure)%>% mutate(units = word(units, 2, sep = fixed(":"))) %>% subset(select = -c(endDate)) %>%
    #       mutate(fromElementId = replace(fromElementId, level == 0, paste0('<b>', fromElementId[level == 0], '</b>')),
    #              elementId = replace(elementId, level == 0, paste0('<b>', elementId[level == 0], '</b>')),
    #              labelString = replace(labelString, level == 0, paste0('<b>', labelString[level == 0], '</b>'))) %>% distinct() %>%
    #       group_by(fromElementId, elementId,labelString, date1) %>% filter(fact == max(fact)) %>% ungroup() %>% 
    #       group_by(date1, elementId, labelString, level) %>% 
    #       mutate(fromElementId = getmode(fromElementId), order = getmode(order), fact = max(fact), decimals = max(decimals),
    #              units = getmode(units)) %>% ungroup() %>%
    #       filter(!duplicated(paste0(date1, fact, labelString, fromElementId))) %>% 
    #       spread(date1, fact) %>% arrange(order) %>% rename(Parent = fromElementId, GAAP= elementId, Label = labelString) %>%
    #       subset(select = -c(decimals))%>% group_by(Parent, GAAP, Label, level, units, order) %>% summarise_all(sum, na.rm = T) %>%
    #       ungroup() %>% arrange(order) %>% subset(select = -c(order))
    #     
    #     
    #     
    #     bs <- bs[,colSums(is.na(bs))<(nrow(bs)/1.1)]
    #     bs[,6:length(bs)][is.na(bs[,6:length(bs)])] <- 0
    #     
    #     names1 <- data.frame(names1 = names(bs[,6:length(bs)]))
    #     if(grepl('Q', names1$names1[1])){
    #       NULL
    #     } else {
    #       names1$date <- as.Date(substr(names1$names1, nchar(names1$names1)-10,nchar(names1$names1)))
    #       names1$month <- as.numeric(substr(names1$names1, 1, 2))
    #       names1 <- names1 %>% arrange(date, month)
    #       #names2 <- names(bs)
    #       names1 <- append(names(bs)[1:5], names1$names1)
    #       
    #       bs <- bs[,names1]
    #     }
    #     
    #     
    #     
    #     bs1 <- which(rowSums(bs[,6:length(bs)])==0)
    #     if(length(bs1) > 0){
    #       bs <- bs[-bs1,]
    #     }
    #     
    #     bs
    #   }
    # })
    # 
    # 
    # output$is <- renderText({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #    
    #     
    #     is() %>% filter(level <= input$level) %>%
    #       kbl(format = 'html', escape = F) %>%
    #       kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
    #       column_spec(1:4, bold = T, border_right = F) %>%
    #       column_spec(5, bold = T, border_right = T) %>%
    #       #scroll_box(width = "100%", height = "25%")%>%
    #       footnote('Income Statement', general_title = '') %>%
    #       scroll_box(width = '100%', height = '500px', fixed_thead = T)
    #     # DT::datatable(bs %>% filter(level <= input$level) , escape = F, rownames=FALSE,extensions = c('Buttons', 'Scroller', 'FixedColumns', 'FixedHeader'),
    #     #               options = list(dom='Bfrtip', paging = FALSE,
    #     #                              buttons = c('copy'), scrollY = '500px',
    #     #                              scrollX = T,fixedColumns = list(leftColumns = 3), fixedHeader = TRUE,
    #     #                              info = FALSE, ordering = FALSE, searching = FALSE),
    #     #               caption = htmltools::tags$caption(
    #     #                 style = 'caption-side: bottom; text-align: center;',
    #     #                 'Table: ', htmltools::em('Income Statement')),
    #     #               class = 'cell-border stripe')
    #     
    #     
    #   }
    # })
    # # 
    # # 
    # 
    # 
    # 
    # 
    # cf <- reactive({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #     df1 <- values$xbrl
    #     
    #     role1 <- df1$role %>% filter(type == 'Statement') %>% filter(!grepl('PARENT', toupper(roleId))) %>%
    #       filter(grepl('CASHFLOW', toupper(roleId)))
    #     
    #     dfx <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order = seq(1, n(), 1)) %>% subset(select = -c(order1, order2, id1, id2))
    #     
    #     dfx1 <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order1 = seq(1, n(), 1)) %>% subset(select = -c(order, order2, id1, id2))
    #     
    #     dfx <- dfx %>% left_join(dfx1)
    #     dfx <- dfx %>% rowwise() %>% mutate(order = min(order, order1)) %>% ungroup() %>% subset(select = -c(order1))
    #     
    #     # dfx$order[grepl('PropertyPlantAndEquipmentOtherAccumulatedDepreciation', dfx$toElementId)] <-
    #     #   dfx$order[grepl('PropertyPlantAndEquipmentOtherNet', dfx$toElementId)] - 0.5
    #     # 
    #     # dfx$order[grepl('GainsLossesOnExtinguishmentOfDebt', dfx$toElementId)] <-
    #     #   dfx$order[grepl('InterestExpense', dfx$toElementId)] - 0.5
    #     
    #     bs <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                           yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #       group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #       subset(select = -c(order)) %>% left_join(dfx) %>% 
    #       subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #       left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #       subset(select = -c(yr, qtr,period, ticker)) %>%
    #       mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #       left_join(df1$context) %>% 
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #       #group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #       group_by(startDate, endDate, elementId, fromElementId, value1, value2, value3, value4) %>% filter(date1 == max(date1)) %>%
    #       ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #       filter(year(endDate) >= min(yr)) %>% filter(monthsOn %in% c(3,6,9,12)) %>%
    #       mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #       subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #       group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       arrange(order, desc(date1)) %>%
    #       filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #       left_join(df1$label) %>% filter(labelRole == preferredLabel) %>% subset(select = -c(labelRole, preferredLabel, ticker)) %>%
    #       left_join(df1$label %>% select(value1 = elementId, label1 = labelString) %>% 
    #                   group_by(value1) %>% filter(nchar(label1) == min(nchar(label1))) %>% ungroup() %>%
    #                   filter(!duplicated(value1)))%>%
    #       left_join(df1$label %>% select(value2 = elementId, label2 = labelString) %>%# filter(!duplicated(value2))%>% 
    #                   group_by(value2) %>% filter(nchar(label2) == min(nchar(label2))) %>% ungroup() %>%
    #                   filter(!duplicated(value2)))%>%
    #       left_join(df1$label %>% select(value3 = elementId, label3 = labelString) %>% #filter(!duplicated(value3))%>% 
    #                   group_by(value3) %>% filter(nchar(label3) == min(nchar(label3))) %>% ungroup() %>%
    #                   filter(!duplicated(value3)))%>%
    #       left_join(df1$label %>% select(value4 = elementId, label4 = labelString) %>% #filter(!duplicated(value4))%>% 
    #                   group_by(value4) %>% filter(nchar(label4) == min(nchar(label4))) %>% ungroup() %>%
    #                   filter(!duplicated(value4))) %>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('COMMON STOCK', toupper(labelString)), 'Common Stock'))%>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('PREFERRED STOCK', toupper(labelString)), 'Preferred Stock')) %>%
    #       mutate(labelString = gsub('â€™', '', labelString, fixed = T)) %>% mutate(label1 = replace(label1, !is.na(label1), paste0('-',label1[!is.na(label1)])))%>%
    #       mutate(label2 = replace(label2, !is.na(label2), paste0('-',label2[!is.na(label2)])))%>%
    #       mutate(label3 = replace(label3, !is.na(label3), paste0('-',label3[!is.na(label3)])))%>%
    #       mutate(label4 = replace(label4, !is.na(label4), paste0('-',label4[!is.na(label4)])))  %>%
    #       mutate(label1 = replace(label1, is.na(label1), ''),
    #              label2 = replace(label2, is.na(label2), ''),
    #              label3 = replace(label3, is.na(label3), ''),
    #              label4 = replace(label4, is.na(label4), '')) %>%
    #       mutate(level = 5, level = replace(level, label4 == '', 4), level = replace(level, label3 == '', 3),
    #              level = replace(level, label2 == '', 2), level = replace(level, label1 == '', 1)) %>%
    #       mutate(labelString = paste0(labelString, label1, label2, label3, label4)) %>%
    #       subset(select = -c(value1, value2, value3, value4, label1, label2, label3, label4)) %>%
    #       #group_by(fromElementId, labelString) %>% mutate(elementId = getmode(elementId), order = getmode(order)) %>% ungroup() %>% distinct() %>%
    #       left_join(df1$calculation %>% select(elementId = toElementId, arcrole) %>% distinct() %>% filter(!duplicated(elementId))) %>%
    #       mutate(level = replace(level, is.na(arcrole), 0)) %>% subset(select = -c(arcrole)) %>%# mutate(order = replace(order, level == 0, 99)) %>%
    #       mutate(order = as.numeric(order) + level/10) %>%
    #       mutate(fromElementId = gsub('([[:upper:]])', ' \\1', fromElementId),
    #              elementId = gsub('([[:upper:]])', ' \\1', elementId)) %>%
    #       rowwise() %>% mutate(labelString = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), labelString),
    #                            fromElementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), fromElementId),
    #                            elementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), elementId)) %>%
    #       ungroup() %>% left_join(df1$unit %>% select(unitId, measure) %>% distinct() %>% filter(!duplicated(unitId))) %>% subset(select = -c(unitId)) %>%
    #       rename(units = measure)%>% mutate(units = word(units, 2, sep = fixed(":"))) %>% subset(select = -c(endDate)) %>%
    #       mutate(fromElementId = replace(fromElementId, level == 0, paste0('<b>', fromElementId[level == 0], '</b>')),
    #              elementId = replace(elementId, level == 0, paste0('<b>', elementId[level == 0], '</b>')),
    #              labelString = replace(labelString, level == 0, paste0('<b>', labelString[level == 0], '</b>'))) %>% distinct() %>%
    #       group_by(fromElementId, elementId,labelString, date1) %>% filter(fact == max(fact)) %>% ungroup() %>% 
    #       group_by(date1, elementId, labelString, level) %>% 
    #       mutate(fromElementId = getmode(fromElementId), order = getmode(order), fact = max(fact), decimals = max(decimals),
    #              units = getmode(units)) %>% ungroup() %>%
    #       filter(!duplicated(paste0(date1, fact, labelString, fromElementId))) %>% 
    #       spread(date1, fact) %>% arrange(order) %>% rename(Parent = fromElementId, GAAP= elementId, Label = labelString) %>%
    #       subset(select = -c(decimals))%>% group_by(Parent, GAAP, Label, level, units, order) %>% summarise_all(sum, na.rm = T) %>%
    #       ungroup() %>% arrange(order) %>% subset(select = -c(order))
    #     
    #     
    #     
    #     bs <- bs[,colSums(is.na(bs))<(nrow(bs)/1.1)]
    #     bs[,6:length(bs)][is.na(bs[,6:length(bs)])] <- 0
    #     
    #     names1 <- data.frame(names1 = names(bs[,6:length(bs)]))
    #     if(grepl('Q', names1$names1[1])){
    #       NULL
    #     } else {
    #       names1$date <- as.Date(substr(names1$names1, nchar(names1$names1)-10,nchar(names1$names1)))
    #       names1$month <- as.numeric(substr(names1$names1, 1, 2))
    #       names1 <- names1 %>% arrange(date, month)
    #       #names2 <- names(bs)
    #       names1 <- append(names(bs)[1:5], names1$names1)
    #       
    #       bs <- bs[,names1]
    #     }
    #     
    #     
    #     
    #     bs1 <- which(rowSums(bs[,6:length(bs)])==0)
    #     if(length(bs1) > 0){
    #       bs <- bs[-bs1,]
    #     }
    #     
    #     bs
    #     
    #   }
    # })
    # 
    # 
    # 
    # output$cf <-renderText({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    # 
    #     
    #     
    #     cf() %>% filter(level <= input$level) %>%
    #       kbl(format = 'html', escape = F) %>%
    #       kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
    #       column_spec(1:4, bold = T, border_right = F) %>%
    #       column_spec(5, bold = T, border_right = T) %>%
    #       #scroll_box(width = "100%", height = "25%")%>%
    #       footnote('Cash Flow Statement', general_title = '') %>%
    #       scroll_box(width = '100%', height = '500px', fixed_thead = T)
    # 
    #     # DT::datatable(bs  %>% filter(level <= input$level), escape = F, rownames=FALSE,extensions = c('Buttons', 'Scroller', 'FixedColumns', 'FixedHeader'),
    #     #               options = list(dom='Bfrtip', paging = FALSE,
    #     #                              buttons = c('copy'), scrollY = '500px',
    #     #                              scrollX = T,fixedColumns = list(leftColumns = 3), fixedHeader = TRUE,
    #     #                              info = FALSE, ordering = FALSE, searching = FALSE),
    #     #               caption = htmltools::tags$caption(
    #     #                 style = 'caption-side: bottom; text-align: center;',
    #     #                 'Table: ', htmltools::em('Cash Flow Statement')),
    #     #               class = 'cell-border stripe')
    #     
    # 
    #   }
    # })
    # # 
    # # 
    # 
    # ot <- reactive({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #     df1 <- values$xbrl
    #     
    #     role1 <- df1$role %>% filter(description == input$otherTable)
    #     dfx <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order = seq(1, n(), 1)) %>% subset(select = -c(order1, order2, id1, id2))
    #     
    #     dfx1 <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order1 = seq(1, n(), 1)) %>% subset(select = -c(order, order2, id1, id2))
    #     
    #     dfx <- dfx %>% left_join(dfx1)
    #     dfx <- dfx %>% rowwise() %>% mutate(order = min(order, order1)) %>% ungroup() %>% subset(select = -c(order1))
    #     
    #     # dfx$order[grepl('PropertyPlantAndEquipmentOtherAccumulatedDepreciation', dfx$toElementId)] <-
    #     #   dfx$order[grepl('PropertyPlantAndEquipmentOtherNet', dfx$toElementId)] - 0.5
    #     # 
    #     # dfx$order[grepl('GainsLossesOnExtinguishmentOfDebt', dfx$toElementId)] <-
    #     #   dfx$order[grepl('InterestExpense', dfx$toElementId)] - 0.5
    #     
    #     bs <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                           yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #       group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #       subset(select = -c(order)) %>% left_join(dfx) %>% 
    #       subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #       left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #       subset(select = -c(yr, qtr,period, ticker)) %>%
    #       mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #       left_join(df1$context) %>% 
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #       #group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #       group_by(startDate, endDate, elementId, fromElementId, value1, value2, value3, value4) %>% filter(date1 == max(date1)) %>%
    #       ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #       filter(year(endDate) >= min(yr)) %>%# filter(monthsOn %in% c(3,6,9,12)) %>%
    #       mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #       subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #       group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       arrange(order, desc(date1)) %>%
    #       filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #       left_join(df1$label) %>% filter(labelRole == preferredLabel) %>% subset(select = -c(labelRole, preferredLabel, ticker)) %>%
    #       left_join(df1$label %>% select(value1 = elementId, label1 = labelString) %>% 
    #                   group_by(value1) %>% filter(nchar(label1) == min(nchar(label1))) %>% ungroup() %>%
    #                   filter(!duplicated(value1)))%>%
    #       left_join(df1$label %>% select(value2 = elementId, label2 = labelString) %>%# filter(!duplicated(value2))%>% 
    #                   group_by(value2) %>% filter(nchar(label2) == min(nchar(label2))) %>% ungroup() %>%
    #                   filter(!duplicated(value2)))%>%
    #       left_join(df1$label %>% select(value3 = elementId, label3 = labelString) %>% #filter(!duplicated(value3))%>% 
    #                   group_by(value3) %>% filter(nchar(label3) == min(nchar(label3))) %>% ungroup() %>%
    #                   filter(!duplicated(value3)))%>%
    #       left_join(df1$label %>% select(value4 = elementId, label4 = labelString) %>% #filter(!duplicated(value4))%>% 
    #                   group_by(value4) %>% filter(nchar(label4) == min(nchar(label4))) %>% ungroup() %>%
    #                   filter(!duplicated(value4))) %>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('COMMON STOCK', toupper(labelString)), 'Common Stock'))%>%
    #       mutate(labelString = replace(labelString, grepl('PAR VALUE', toupper(labelString)) & grepl('PREFERRED STOCK', toupper(labelString)), 'Preferred Stock')) %>%
    #       mutate(labelString = gsub('â€™', '', labelString, fixed = T)) %>% mutate(label1 = replace(label1, !is.na(label1), paste0('-',label1[!is.na(label1)])))%>%
    #       mutate(label2 = replace(label2, !is.na(label2), paste0('-',label2[!is.na(label2)])))%>%
    #       mutate(label3 = replace(label3, !is.na(label3), paste0('-',label3[!is.na(label3)])))%>%
    #       mutate(label4 = replace(label4, !is.na(label4), paste0('-',label4[!is.na(label4)])))  %>%
    #       mutate(label1 = replace(label1, is.na(label1), ''),
    #              label2 = replace(label2, is.na(label2), ''),
    #              label3 = replace(label3, is.na(label3), ''),
    #              label4 = replace(label4, is.na(label4), '')) %>%
    #       mutate(level = 5, level = replace(level, label4 == '', 4), level = replace(level, label3 == '', 3),
    #              level = replace(level, label2 == '', 2), level = replace(level, label1 == '', 1)) %>%
    #       mutate(labelString = paste0(labelString, label1, label2, label3, label4)) %>%
    #       subset(select = -c(value1, value2, value3, value4, label1, label2, label3, label4)) %>%
    #       #group_by(fromElementId, labelString) %>% mutate(elementId = getmode(elementId), order = getmode(order)) %>% ungroup() %>% distinct() %>%
    #       left_join(df1$calculation %>% select(elementId = toElementId, arcrole) %>% distinct() %>% filter(!duplicated(elementId))) %>%
    #       mutate(level = replace(level, is.na(arcrole), 0)) %>% subset(select = -c(arcrole)) %>%# mutate(order = replace(order, level == 0, 99)) %>%
    #       mutate(order = as.numeric(order) + level/10) %>%
    #       mutate(fromElementId = gsub('([[:upper:]])', ' \\1', fromElementId),
    #              elementId = gsub('([[:upper:]])', ' \\1', elementId)) %>%
    #       rowwise() %>% mutate(labelString = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), labelString),
    #                            fromElementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), fromElementId),
    #                            elementId = paste0(paste(replicate((as.integer(level))*5,'&nbsp;'),collapse=""), elementId)) %>%
    #       ungroup() %>% left_join(df1$unit %>% select(unitId, measure) %>% distinct() %>% filter(!duplicated(unitId))) %>% subset(select = -c(unitId)) %>%
    #       rename(units = measure)%>% mutate(units = word(units, 2, sep = fixed(":"))) %>% subset(select = -c(endDate)) %>%
    #       mutate(fromElementId = replace(fromElementId, level == 0, paste0('<b>', fromElementId[level == 0], '</b>')),
    #              elementId = replace(elementId, level == 0, paste0('<b>', elementId[level == 0], '</b>')),
    #              labelString = replace(labelString, level == 0, paste0('<b>', labelString[level == 0], '</b>'))) %>% distinct() %>%
    #       group_by(fromElementId, elementId,labelString, date1) %>% filter(fact == max(fact)) %>% ungroup() %>% 
    #       group_by(date1, elementId, labelString, level) %>% 
    #       mutate(fromElementId = getmode(fromElementId), order = getmode(order), fact = max(fact), decimals = max(decimals),
    #              units = getmode(units)) %>% ungroup() %>%
    #       filter(!duplicated(paste0(date1, fact, labelString, fromElementId))) %>% 
    #       spread(date1, fact) %>% arrange(order) %>% rename(Parent = fromElementId, GAAP= elementId, Label = labelString) %>%
    #       subset(select = -c(decimals))%>% group_by(Parent, GAAP, Label, level, units, order) %>% summarise_all(sum, na.rm = T) %>%
    #       ungroup() %>% arrange(order) %>% subset(select = -c(order))
    #     
    #     
    #     
    #     
    #     
    #     bs <- bs[,colSums(is.na(bs))<(nrow(bs)/1.1)]
    #     bs[,6:length(bs)][is.na(bs[,6:length(bs)])] <- 0
    #     
    #     names1 <- data.frame(names1 = names(bs[,6:length(bs)]))
    #     if(grepl('Q', names1$names1[1])){
    #       NULL
    #     } else {
    #       names1$date <- as.Date(substr(names1$names1, nchar(names1$names1)-10,nchar(names1$names1)))
    #       names1$month <- as.numeric(substr(names1$names1, 1, 2))
    #       names1 <- names1 %>% arrange(date, month)
    #       #names2 <- names(bs)
    #       names1 <- append(names(bs)[1:5], names1$names1)
    #       
    #       bs <- bs[,names1]
    #     }
    #     
    #     
    #     
    #     bs1 <- which(rowSums(bs[,6:length(bs)])==0)
    #     if(length(bs1) > 0){
    #       bs <- bs[-bs1,]
    #     }
    #     
    #     bs
    #   }
    # })
    # 
    # output$ot <- renderText({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    # 
    #    
    #     
    #     ot() %>% filter(level <= input$level) %>%
    #       kbl(format = 'html', escape = F) %>%
    #       kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12) %>%
    #       column_spec(1:4, bold = T, border_right = F) %>%
    #       column_spec(5, bold = T, border_right = T) %>%
    #       #scroll_box(width = "100%", height = "25%")%>%
    #       footnote('Custom Table', general_title = '') %>%
    #       scroll_box(width = '100%', height = '500px', fixed_thead = T)
    #     # DT::datatable(bs  %>% filter(level <= input$level), escape = F, rownames=FALSE,extensions = c('Buttons', 'Scroller', 'FixedColumns', 'FixedHeader'),
    #     #               options = list(dom='Bfrtip', paging = FALSE,
    #     #                              buttons = c('copy'), scrollY = '500px',
    #     #                              scrollX = T,fixedColumns = list(leftColumns = 3), fixedHeader = TRUE,
    #     #                              info = FALSE, ordering = FALSE, searching = FALSE),
    #     #               caption = htmltools::tags$caption(
    #     #                 style = 'caption-side: bottom; text-align: center;',
    #     #                 'Table: ', htmltools::em('Custom Table')),
    #     #               class = 'cell-border stripe')
    #     
    # 
    # 
    #   }
    # })
    # 
    # observe({
    #   if(is.null(values$xbrl)){
    #     shinyjs::hide('graphHide')
    #   } else {
    #     shinyjs::show('graphHide')
    #   }
    # })
    # 
    # 
    # output$cR <- renderHighchart({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     df1 <- values$xbrl
    #     
    #     cR <- bs() %>% mutate(GAAP = trimws(gsub('&nbsp;', '', GAAP, fixed = T)), GAAP = gsub(' ', '', GAAP)) %>% filter(level <= 1) %>%
    #       filter(GAAP == 'AssetsCurrent'|GAAP == 'LiabilitiesCurrent') %>% subset(select = -c(Parent, Label, level, units)) %>%
    #       gather(period, fact, -c(GAAP)) %>% rename(elementId = GAAP) %>% spread(elementId, fact) %>% arrange(period) %>%
    #       na.omit %>%
    #       mutate(cR = round(AssetsCurrent/LiabilitiesCurrent, 2))
    #     
    #     # cR <- df1$fact %>% filter(elementId %in% c('AssetsCurrent', 'LiabilitiesCurrent')) %>% filter(!grepl('Other', elementId)) %>%
    #     #   left_join(df1$context) %>% filter(is.na(value1)) %>% subset(select = -c(value1, value2, value3, value4)) %>%
    #     #   mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #     #          yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #     #   subset(select = -c(period, ticker)) %>% filter(!duplicated(paste0(elementId, endDate))) %>%
    #     #   select(elementId, endDate, fact) %>% spread(elementId, fact) %>%
    #     #   filter(!is.na(AssetsCurrent)) %>% filter(!is.na(LiabilitiesCurrent)) %>%
    #     #   mutate(cR = round(AssetsCurrent/LiabilitiesCurrent, 2))
    #     
    #     if(nrow(cR) == 0){
    #       NULL
    #     } else {
    #       highchart() %>%
    #         hc_colors(cols) %>%
    #         hc_add_series(cR, type = 'column',
    #                       hcaes(x = period, y = round(AssetsCurrent/1000, 1)), 
    #                       color = cols[3], name = 'Current Assets', tooltip = list(pointFormat = "Current Assets: ${point.y:,.0f} 000")) %>%
    #         hc_add_series(cR, type = 'column', 
    #                       hcaes(x = period, 
    #                             y = round(-1*LiabilitiesCurrent/1000, 1)), 
    #                       color = cols[5], name = 'Current Liabilities', tooltip = list(pointFormat = "Current Liabilities: ${point.y:,.0f} 000")) %>%
    #         hc_add_series(cR, type = 'line', 
    #                       hcaes(x = period, y = cR),
    #                       color = cols[2], name = 'Current Ratio', yAxis = 1, marker = list(enabled = F)) %>%
    #         hc_xAxis(categories = cR$period, labels = list(style = list(fontSize = '10pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                color =  cols[1]))) %>%
    #         hc_yAxis_multiples(list(gridLineColor = 'transparent', labels = list(format = "${value:,.0f}",
    #                                                                              style = list(fontSize = '10pt', fontWeight = 'bold', 
    #                                                                                           fontFamily = 'Arial', color =  cols[1])),
    #                                 title = list(text = 'Assets/Liabilities in Thousands',
    #                                              style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                           color =  cols[1]))),
    #                            list(opposite = TRUE, min = 0, labels = list(step = 0.5, style = list(fontSize = '10pt', fontWeight = 'bold', 
    #                                                                                                  fontFamily = 'Arial', color =  cols[1])),
    #                                 gridLineColor = 'transparent',
    #                                 title = list(text = 'Current Ratio',
    #                                              style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                           color =  cols[1])))) %>%
    #         hc_plotOptions(column = list(stacking = 'normal')) %>%
    #         hc_title(text =glue::glue('{df1$fact$ticker[1]} Current Ratio'),align = 'left',
    #                  style = list(fontSize = '20pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                                  color =  cols[2]) )%>%
    #         hc_subtitle(text =glue::glue('Source: SEC'),align = 'left',
    #                     style = list(fontSize = '14pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                                  color =  cols[3]) )%>%
    #         hc_legend(floating = T,
    #                   verticalAlign = 'top',
    #                   align = 'center',
    #                   layout = 'horizontal',
    #                   itemStyle = list(fontFamily = 'Arial', color = cols[2]))
    #     }
    #     
    #   }
    # 
    # })
    # 
    # observeEvent(input$operator, {
    #   values$xbrl <- NULL
    # })
    # 
    # output$cfs <- renderHighchart({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     df1 <- values$xbrl
    #     #saveRDS(df1, 'rigXBRL.rds')
    #     df2 <- df1$fact %>% filter(grepl('Activities', elementId)) %>% 
    #       filter(grepl('Cash', elementId))%>%
    #       filter(!grepl('Operations', elementId)) %>%
    #       filter(!grepl('Other', elementId)) %>%
    #       filter(!grepl('Increase', elementId)) %>%
    #       filter(!grepl('Decrease', elementId)) %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       left_join(df1$context) %>% filter(is.na(value1)) %>%
    #       filter(!duplicated(paste0(startDate, endDate, elementId))) %>%
    #       mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #       mutate(monthsOn = round(monthsOn/3, 0)*3) %>%
    #       mutate(yr = year(startDate)) %>%
    #       group_by(endDate, elementId) %>% filter(monthsOn == max(monthsOn)) %>% ungroup() %>%
    #       #filter(month(endDate) == monthsOn) %>%
    #       subset(select = -c( contextId, unitId, period, ticker,  qtr,
    #                          value1, value2, value3, value4)) %>%
    #       mutate(monthsOn = paste0('M', monthsOn)) %>% subset(select = -c(decimals, endDate)) %>%
    #       spread(monthsOn,fact) %>% filter(!(is.na(M3)&is.na(M6)&is.na(M9))) %>%
    #       mutate(M12 = M12 - M9, M9 = M9-M6, M6 = M6 - M3)
    #     
    #     if(length(df2) < 6){
    #       
    #       df2 <- df1$fact %>% filter(grepl('Activities', elementId)) %>% 
    #         filter(grepl('Cash', elementId))%>%
    #         filter(!grepl('Operations', elementId)) %>%
    #         filter(!grepl('Other', elementId)) %>%
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #         left_join(df1$context) %>% filter(is.na(value1)) %>%
    #         filter(!duplicated(paste0(startDate, endDate, elementId))) %>%
    #         mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #         mutate(monthsOn = round(monthsOn/3, 0)*3) %>%
    #         filter(3 == monthsOn) %>%
    #         subset(select = -c(contextId, unitId, period, ticker, yr, qtr,
    #                            value1, value2, value3, value4)) %>%
    #         mutate(yr = year(startDate), monthsOn = paste0('M', monthsOn)) %>% subset(select = -c(decimals, endDate)) 
    #       
    #     } else {
    #       df2 <- df2 %>% gather(monthsOn, fact, -c(elementId, yr, startDate))
    #     }
    #     
    #     df2 <- df2 %>% filter(!is.na(fact)) %>% mutate(monthsOn = as.numeric(gsub('M', '', monthsOn))) %>%
    #       mutate(yr = as.Date(startDate) %m+% months(monthsOn)) #%>% mutate(yr = as.Date(paste0(year(yr),'-',month(yr),'-',days_in_month(yr))))
    #     
    #     df2$elementId <- gsub('NetCashProvidedByUsedIn', '', df2$elementId)
    #     df2 <- df2 %>% mutate(elementId = gsub('([[:upper:]])', ' \\1', elementId))
    #     
    #     df3 <- df2 %>% group_by(yr) %>% summarise(fact = sum(fact)) %>% ungroup()
    #     
    #     if(nrow(df2) == 0){
    #       NULL
    #     } else {
    #       
    #       highchart() %>%
    #         hc_colors(cols[2:length(cols)]) %>%
    #         hc_add_series(df2, type = 'column',
    #                       hcaes(x = as.Date(yr), y = round(fact/1000, 1), group = elementId), 
    #                       tooltip = list(pointFormat = "{point.elementId}: ${point.y:,.0f} 000")) %>%
    # 
    #         hc_add_series(df3, type = 'line', 
    #                       hcaes(x = as.Date(yr), y = round(fact/1000, 1)),
    #                      name = 'Total Cash',  marker = list(enabled = F),
    #                      tooltip = list(pointFormat = "Cash: ${point.y:,.0f} 000")) %>%
    #         hc_xAxis(type = 'datetime', labels = list(style = list(fontSize = '10pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                color =  cols[1]))) %>%
    #         hc_yAxis(gridLineColor = 'transparent', labels = list(format = "${value:,.0f}",
    #                                                                              style = list(fontSize = '10pt', fontWeight = 'bold', 
    #                                                                                           fontFamily = 'Arial', color =  cols[1])),
    #                                 title = list(text = 'Cash Flow in Thousands',
    #                                              style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                           color =  cols[1]))) %>%
    #         hc_plotOptions(column = list(stacking = 'normal')) %>%
    #         hc_title(text =glue::glue('{df1$fact$ticker[1]} Quarterly Cash Flows'),align = 'left',
    #                  style = list(fontSize = '20pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                               color =  cols[2]) )%>%
    #         hc_subtitle(text =glue::glue('Source: SEC'),align = 'left',
    #                     style = list(fontSize = '14pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                  color =  cols[3]) )%>%
    #         hc_legend(floating = F,
    #                   verticalAlign = 'top',
    #                   align = 'center',
    #                   layout = 'horizontal',
    #                   itemStyle = list(fontFamily = 'Arial', color = cols[2]))
    #       
    #     }
    #   
    #     
    #   }
    #     
    #   
    #   
    # })
    # 
    # 
    # output$nD <- renderHighchart({
    #   
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #   
    #     df1 <- values$xbrl 
    #     
    #     # saveRDS(df1, 'rigXBRL.rds')
    #     
    #     role1 <- df1$role %>% filter(type == "Statement") %>% filter(grepl('BALANCE', toupper(roleId))|grepl('FINANCIALPOS', toupper(roleId)))
    #     
    #     
    #     dfx <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order = seq(1, n(), 1)) %>% subset(select = -c(order1, order2, id1, id2))
    #     
    #     dfx1 <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order1 = seq(1, n(), 1)) %>% subset(select = -c(order, order2, id1, id2))
    #     
    #     dfx <- dfx %>% left_join(dfx1)
    #     dfx <- dfx %>% rowwise() %>% mutate(order = min(order, order1)) %>% ungroup() %>% subset(select = -c(order1))
    #     
    #     # dfx$order[grepl('PropertyPlantAndEquipmentOtherAccumulatedDepreciation', dfx$toElementId)] <-
    #     #   dfx$order[grepl('PropertyPlantAndEquipmentOtherNet', dfx$toElementId)] - 0.5
    #     # 
    #     # dfx$order[grepl('GainsLossesOnExtinguishmentOfDebt', dfx$toElementId)] <-
    #     #   dfx$order[grepl('InterestExpense', dfx$toElementId)] - 0.5
    #     
    #     nD <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                           yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #       group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #       subset(select = -c(order)) %>% left_join(dfx) %>% 
    #       subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #       left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #       subset(select = -c(yr, qtr,period, ticker)) %>%
    #       mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #       left_join(df1$context) %>% 
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #       #group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #       group_by(startDate, endDate, elementId, fromElementId, value1, value2, value3, value4) %>% filter(date1 == max(date1)) %>%
    #       ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #       filter(year(endDate) >= min(yr)) %>%
    #       mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #       subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #       group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       arrange(order, desc(date1)) %>%
    #       filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #       filter(is.na(value1))  %>%
    #       filter(elementId == 'Cash'|grepl('CashAndCash', elementId)|grepl('Debt', elementId)|grepl('Notes', elementId)|grepl('LineOfC', elementId)) %>%
    #       select(elementId, fact, endDate)  %>%
    #       mutate(elementId = replace(elementId, grepl('Cash', elementId), 'cash'),
    #              elementId = replace(elementId, grepl('DebtC', elementId), 'current'),
    #              elementId = replace(elementId, grepl('DebtNon', elementId), 'nonCurrent1'),
    #              elementId = replace(elementId, grepl('LongTermDebtAndCapital', elementId), 'nonCurrent2'),
    #              elementId = replace(elementId, grepl('LongTermN', elementId), 'nonCurrent3'),
    #              elementId = replace(elementId, grepl('LineOfC', elementId), 'nonCurrent4'),
    #              elementId = replace(elementId, grepl('LongTerm', elementId), 'nonCurrent5')) %>%
    #       filter(elementId %in% c('cash', 'nonCurrent1', 'nonCurrent2', 'nonCurrent3', 'nonCurrent4',  'nonCurrent5','current')) %>%
    #       filter(!duplicated(paste0(elementId, endDate))) %>%
    #       select(elementId, endDate, fact) %>% mutate(elementId = replace(elementId, grepl('urrent', elementId), 'debt')) %>%
    #       group_by(elementId, endDate) %>% summarise(fact = sum(fact, na.rm = T)) %>% ungroup()
    #     
    #     
    #     
    #     
    #     if(length(unique(nD$elementId)) == 1){
    #       
    #       
    #       
    #       
    #       nD <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                             yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #         filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #         group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #         subset(select = -c(order)) %>% left_join(dfx) %>% 
    #         subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #         left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #         subset(select = -c(yr, qtr,period, ticker)) %>%
    #         mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #         left_join(df1$context) %>% 
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #         group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #         group_by(startDate, endDate, elementId, fromElementId) %>% filter(date1 == max(date1)) %>%
    #         ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #         filter(year(endDate) >= min(yr)) %>%
    #         mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #         subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #         group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #         arrange(order, desc(date1)) %>%
    #         filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #         filter(is.na(value1))  %>%
    #         filter(elementId == 'Cash'|grepl('CashAndCash', elementId)|grepl('Debt', elementId)|grepl('Notes', elementId)|grepl('LineOfC', elementId)) %>%
    #         select(elementId, fact, endDate) %>%
    #         mutate(elementId = replace(elementId, grepl('Cash', elementId), 'cash'),
    #                elementId = replace(elementId, grepl('Debt', elementId), 'debt')) %>%
    #         filter(!duplicated(paste0(elementId, endDate))) %>%
    #         select(elementId, endDate, fact) %>% #mutate(elementId = replace(elementId, grepl('urrent', elementId), 'debt')) %>%
    #         group_by(elementId, endDate) %>% summarise(fact = sum(fact, na.rm = T)) %>% ungroup() %>% filter(elementId %in% c('debt', 'cash'))
    #     }
    #     
    #     nD <- nD %>% spread(elementId, fact) %>%
    #       filter(!is.na(cash)) %>% filter(!is.na(debt)) %>% mutate(netDebt = debt - cash)
    #     
    #     if(nrow(nD) == 0){
    #       NULL
    #     } else {
    #       
    #       highchart() %>%
    #         hc_colors(cols) %>%
    #         hc_add_series(nD, type = 'column',
    #                       hcaes(x = as.Date(endDate), y = round(debt/1000, 1)), 
    #                       color = cols[3], name = 'Total Debt', tooltip = list(pointFormat = "Total Debt: ${point.y:,.0f} 000")) %>%
    #         hc_add_series(nD, type = 'column', 
    #                       hcaes(x = as.Date(endDate), 
    #                             y = round(-1*cash/1000, 1)), 
    #                       color = cols[5], name = 'Cash', tooltip = list(pointFormat = "Cash: ${point.y:,.0f} 000")) %>%
    #         hc_add_series(nD, type = 'line', 
    #                       hcaes(x = as.Date(endDate), y = round(netDebt/1000, 1)),
    #                       color = cols[2], name = 'Net Debt',marker = list(enabled = F),
    #                       tooltip = list(pointFormat = "Net Debt: ${point.y:,.0f} 000")
    #         ) %>%
    #         hc_xAxis(type = 'datetime', labels = list(style = list(fontSize = '10pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                color =  cols[1]))) %>%
    #         hc_yAxis(gridLineColor = 'transparent', labels = list(format = "${value:,.0f}",
    #                                                               style = list(fontSize = '10pt', fontWeight = 'bold', 
    #                                                                            fontFamily = 'Arial', color =  cols[1])),
    #                  title = list(text = 'Net Debt Components in Thousands',
    #                               style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                            color =  cols[1]))) %>%
    #         hc_plotOptions(column = list(stacking = 'normal')) %>%
    #         hc_title(text =glue::glue('{df1$fact$ticker[1]} Net Debt'),align = 'left',
    #                  style = list(fontSize = '20pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                               color =  cols[2]) )%>%
    #         hc_subtitle(text =glue::glue('Source: SEC'),align = 'left',
    #                     style = list(fontSize = '14pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                  color =  cols[3]) )%>%
    #         hc_legend(floating = T,
    #                   verticalAlign = 'top',
    #                   align = 'center',
    #                   layout = 'horizontal',
    #                   itemStyle = list(fontFamily = 'Arial', color = cols[2]))
    #       
    #     }
    #       
    #       
    #   }
    #   
    # })
    # 
    # 
    # 
    # output$ev <- renderHighchart({
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     df1 <- values$xbrl
    #     comp.ticker <- input$operator
    #     
    #     sP <- getSymbols(comp.ticker, from = min(as.Date(df1$context$endDate)) %m+% months(-1),
    #                      to = max(df1$context$endDate),warnings = FALSE,
    #                      auto.assign = F)
    #     
    #     names(sP) <- word(names(sP), 2, sep = fixed('.'))
    #     
    #     splits <- getSplits(comp.ticker)
    #     
    #     
    #     if(is.na(splits)){
    #       sP <- as.data.frame(sP) %>% mutate(endDate = date(sP)) 
    #     } else {
    #       names(splits) <- word(names(splits), 2, sep = fixed('.'))
    #       splits <- as.data.frame(splits) %>% mutate(endDate = date(splits))
    #       splits <- splits[nrow(splits):1,]
    #       splits$spl1 <- splits$spl
    #       i <- 2
    #       while(i <= nrow(splits)){
    #         
    #         splits$spl1[i] <- splits$spl1[i]*splits$spl1[i-1]
    #         
    #         i <- i + 1
    #       }
    #       
    #       
    #       sP <- as.data.frame(sP) %>% mutate(endDate = date(sP)) %>% left_join(splits %>% select(endDate, spl1))
    #       sP <- sP %>% arrange(desc(endDate))
    #       sP$spl1[1] <- 1
    #       sP$spl1<- na.locf(sP$spl1)
    #       sP$Adjusted <- sP$Adjusted/sP$spl1
    #     } 
    #     
    #     sP <- sP %>% group_by(year(endDate), month(endDate)) %>% filter(day(endDate) == max(day(endDate))) %>% ungroup()
    #     sP$endDate <- paste0(year(sP$endDate), '-', month(sP$endDate), '-', days_in_month(sP$endDate))
    #     sP$endDate <- as.Date(sP$endDate)
    #     
    #    
    #     
    #     
    #     shares <- df1$fact %>% filter(grepl('OfDilutedShares', elementId)) %>% 
    #       left_join(df1$context) %>%
    #       filter(is.na(value1)) %>% mutate(monthsOn = elapsed_months(endDate, startDate))  %>%
    #       group_by(elementId,startDate, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange((yr), (qtr)) %>%
    #       filter(!duplicated(paste0(elementId,startDate, endDate))) %>%
    #       group_by(endDate) %>% filter(monthsOn == min(monthsOn)) %>% ungroup() %>%
    #       mutate(endDate = as.Date(endDate)) %>%
    #       select(endDate, shares = fact) %>% group_by(endDate) %>% filter(shares == max(shares)) %>% ungroup()
    #     
    #     if(nrow(shares) == 0){
    #       shares <- df1$fact %>% filter(grepl('WeightedAverageNumber', elementId)) %>% 
    #         left_join(df1$context) %>%
    #         filter(is.na(value1)) %>% mutate(monthsOn = elapsed_months(endDate, startDate))  %>%
    #         group_by(elementId,startDate, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5))) %>% arrange((yr), (qtr)) %>%
    #         filter(!duplicated(paste0(elementId,startDate, endDate))) %>%
    #         group_by(endDate) %>% filter(monthsOn == min(monthsOn)) %>% ungroup() %>%
    #         mutate(endDate = as.Date(endDate)) %>%
    #         select(endDate, shares = fact) %>% group_by(endDate) %>% filter(shares == max(shares)) %>% ungroup()
    #     }
    #     
    #     shares <- shares %>% mutate(endDate = as.Date(paste0(year(endDate), '-', month(endDate), '-', days_in_month(endDate)))) %>%
    #       left_join(sP %>% select(endDate, sharePrice = Adjusted))
    #     
    #     
    #     role1 <- df1$role %>% filter(type == "Statement") %>% filter(grepl('BALANCE', toupper(roleId))|grepl('FINANCIALPOS', toupper(roleId)))
    #     
    #     
    #     dfx <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order = seq(1, n(), 1)) %>% subset(select = -c(order1, order2, id1, id2))
    #     
    #     dfx1 <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       filter(!grepl('Abstract', toElementId)) %>% group_by(period) %>%
    #       mutate(id1 = lag(toElementId), id2 = lead(toElementId)) %>% ungroup() %>%
    #       mutate(id1 = replace(id1, is.na(id1), toElementId[is.na(id1)]),
    #              id2 = replace(id2, is.na(id2), toElementId[is.na(id2)])) %>%
    #       group_by(toElementId) %>% mutate(id1 = getmode(id1), id2 = getmode(id2)) %>% ungroup() %>%
    #       group_by(toElementId) %>% mutate(order = getmode(order)) %>% ungroup() %>%
    #       select(toElementId, order, id1, id2) %>% distinct() %>% mutate(order1 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order1 = min(order1)) %>% ungroup() %>% mutate(order2 = seq(1, n(), 1)) %>%
    #       group_by(id1) %>% mutate(order2 = min(order2)) %>% ungroup() %>% arrange(order1, order2, order) %>%
    #       mutate(order1 = seq(1, n(), 1)) %>% subset(select = -c(order, order2, id1, id2))
    #     
    #     dfx <- dfx %>% left_join(dfx1)
    #     dfx <- dfx %>% rowwise() %>% mutate(order = min(order, order1)) %>% ungroup() %>% subset(select = -c(order1))
    #     
    #     # dfx$order[grepl('PropertyPlantAndEquipmentOtherAccumulatedDepreciation', dfx$toElementId)] <-
    #     #   dfx$order[grepl('PropertyPlantAndEquipmentOtherNet', dfx$toElementId)] - 0.5
    #     # 
    #     # dfx$order[grepl('GainsLossesOnExtinguishmentOfDebt', dfx$toElementId)] <-
    #     #   dfx$order[grepl('InterestExpense', dfx$toElementId)] - 0.5
    #     
    #     nD <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                           yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #       group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #       subset(select = -c(order)) %>% left_join(dfx) %>% 
    #       subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #       left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #       subset(select = -c(yr, qtr,period, ticker)) %>%
    #       mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #       left_join(df1$context) %>% 
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #       # group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #       group_by(startDate, endDate, elementId, fromElementId, value1, value2, value3, value4) %>% filter(date1 == max(date1)) %>%
    #       ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #       filter(year(endDate) >= min(yr)) %>%
    #       mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #       subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #       group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       arrange(order, desc(date1)) %>%
    #       filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #       filter(is.na(value1))  %>%
    #       filter(elementId == 'Cash'|grepl('CashAndCash', elementId)|grepl('Debt', elementId)|grepl('Notes', elementId)|grepl('LineOfC', elementId)) %>%
    #       select(elementId, fact, endDate)  %>%
    #       mutate(elementId = replace(elementId, grepl('Cash', elementId), 'cash'),
    #              elementId = replace(elementId, grepl('DebtC', elementId), 'current'),
    #              elementId = replace(elementId, grepl('DebtNon', elementId), 'nonCurrent1'),
    #              elementId = replace(elementId, grepl('LongTermDebtAndCapital', elementId), 'nonCurrent2'),
    #              elementId = replace(elementId, grepl('LongTermN', elementId), 'nonCurrent3'),
    #              elementId = replace(elementId, grepl('LineOfC', elementId), 'nonCurrent4'),
    #              elementId = replace(elementId, grepl('LongTerm', elementId), 'nonCurrent5')) %>%
    #       filter(elementId %in% c('cash', 'nonCurrent1', 'nonCurrent2', 'nonCurrent3', 'nonCurrent4',  'nonCurrent5','current')) %>%
    #       filter(!duplicated(paste0(elementId, endDate))) %>%
    #       select(elementId, endDate, fact) %>% mutate(elementId = replace(elementId, grepl('urrent', elementId), 'debt')) %>%
    #       group_by(elementId, endDate) %>% summarise(fact = sum(fact, na.rm = T)) %>% ungroup()
    #     
    #     
    #     
    #     
    #     if(length(unique(nD$elementId)) == 1){
    #       
    #       
    #       
    #       
    #       nD <- df1$presentation %>% filter(roleId %in% role1$roleId)%>% mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                                                                             yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #         filter(!duplicated(toElementId)) %>% #subset(select = -c(period)) %>%
    #         group_by(toElementId) %>% mutate(fromElementId = getmode(fromElementId)) %>% ungroup() %>%
    #         subset(select = -c(order)) %>% left_join(dfx) %>% 
    #         subset(select = -c(yr, qtr, ticker, roleId, period)) %>% rename(elementId = toElementId) %>%
    #         left_join(df1$fact) %>% filter(!is.na(as.numeric(fact)))%>%
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5))) %>% arrange(order, desc(yr), desc(qtr)) %>%
    #         subset(select = -c(yr, qtr,period, ticker)) %>%
    #         mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>% 
    #         left_join(df1$context) %>% 
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5)), date1 = as.Date(paste0(yr, '-',qtr*3,'-01'))) %>%
    #         group_by(fromElementId, startDate, endDate) %>% filter(date1 == max(date1)) %>% ungroup() %>%
    #         group_by(startDate, endDate, elementId, fromElementId) %>% filter(date1 == max(date1)) %>%
    #         ungroup() %>% distinct() %>% mutate(monthsOn = elapsed_months(endDate, startDate)) %>%
    #         filter(year(endDate) >= min(yr)) %>%
    #         mutate(date1 = if_else(is.na(startDate), paste0(year(endDate),'Q',quarter(endDate)), paste0(monthsOn, ' months ended ', endDate))) %>%
    #         subset(select = -c(period, yr, qtr, ticker, monthsOn, startDate, contextId)) %>%
    #         group_by(fromElementId, elementId, value1, value2, value3, value4, date1) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #         arrange(order, desc(date1)) %>%
    #         filter(!duplicated(paste0(fromElementId, elementId, date1, value1, value2, value3, value4))) %>%
    #         filter(is.na(value1))  %>%
    #         filter(elementId == 'Cash'|grepl('CashAndCash', elementId)|grepl('Debt', elementId)|grepl('Notes', elementId)|grepl('LineOfC', elementId)) %>%
    #         select(elementId, fact, endDate) %>%
    #         mutate(elementId = replace(elementId, grepl('Cash', elementId), 'cash'),
    #                elementId = replace(elementId, grepl('Debt', elementId), 'debt')) %>%
    #         filter(!duplicated(paste0(elementId, endDate))) %>%
    #         select(elementId, endDate, fact) %>% #mutate(elementId = replace(elementId, grepl('urrent', elementId), 'debt')) %>%
    #         group_by(elementId, endDate) %>% summarise(fact = sum(fact, na.rm = T)) %>% ungroup() %>% filter(elementId %in% c('debt', 'cash'))
    #     }
    #     
    #     nD <- nD %>% spread(elementId, fact) %>%
    #       filter(!is.na(cash)) %>% filter(!is.na(debt)) %>% mutate(netDebt = debt - cash)
    #     
    #     
    #     nD <- nD %>% mutate(endDate = as.Date(endDate)) %>%
    #       mutate(endDate = as.Date(paste0(year(endDate), '-', month(endDate), '-', days_in_month(endDate)))) %>%
    #       left_join(shares)
    #     nD$mc <- nD$shares*nD$sharePrice
    #     nD$ev <- nD$mc+nD$netDebt
    #     
    #     nD <- nD %>% group_by(endDate) %>% filter(shares == max(shares))
    #     
    #     
    #     if(nrow(nD) == 0){
    #       NULL
    #     } else {
    #       
    #       highchart() %>%
    #         hc_colors(cols) %>%
    #         hc_add_series(nD, type = 'column',
    #                       hcaes(x = as.Date(endDate), y = round(debt/1000, 1)), 
    #                       color = cols[3], name = 'Total Debt', tooltip = list(pointFormat = "Total Debt: ${point.y:,.0f} 000")) %>%
    #         hc_add_series(nD, type = 'column',
    #                       hcaes(x = as.Date(endDate), y = round(mc/1000, 1)), 
    #                       color = cols[5], name = 'Market Cap', tooltip = list(pointFormat = "Market Cap: ${point.y:,.0f} 000")) %>%
    #         hc_add_series(nD, type = 'column', 
    #                       hcaes(x = as.Date(endDate), 
    #                             y = round(-1*cash/1000, 1)), 
    #                       color = cols[6], name = 'Cash', tooltip = list(pointFormat = "Cash: ${point.y:,.0f} 000")) %>%
    #         hc_add_series(nD, type = 'line', 
    #                       hcaes(x = as.Date(endDate), y = round(ev/1000, 1)),
    #                       color = cols[2], name = 'Enterprise Value',marker = list(enabled = F),
    #                       tooltip = list(pointFormat = "EV: ${point.y:,.0f} 000")) %>%
    #         hc_xAxis(type = 'datetime', labels = list(style = list(fontSize = '10pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                color =  cols[1]))) %>%
    #         hc_yAxis(gridLineColor = 'transparent', labels = list(format = "${value:,.0f}",
    #                                                               style = list(fontSize = '10pt', fontWeight = 'bold', 
    #                                                                            fontFamily = 'Arial', color =  cols[1])),
    #                  title = list(text = 'EV Components in Thousands',
    #                               style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                            color =  cols[1]))) %>%
    #         hc_plotOptions(column = list(stacking = 'normal')) %>%
    #         hc_title(text =glue::glue('{df1$fact$ticker[1]} Enterprise Value'),align = 'left',
    #                  style = list(fontSize = '20pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                               color =  cols[2]) )%>%
    #         hc_subtitle(text =glue::glue('Source: SEC'),align = 'left',
    #                     style = list(fontSize = '14pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                  color =  cols[3]) )%>%
    #         hc_legend(floating = T,
    #                   verticalAlign = 'top',
    #                   align = 'center',
    #                   layout = 'horizontal',
    #                   itemStyle = list(fontFamily = 'Arial', color = cols[2]))
    #       
    #     }
    #   
    #   }
    #   
    # })
    # 
    # output$zS <- renderHighchart({
    #   
    #   if(is.null(values$xbrl)){
    #     NULL
    #   } else {
    #     
    #     df1 <- values$xbrl
    #     comp.ticker <- input$operator
    #   
    #     wcTags <- c('AssetsCurrent', 'LiabilitiesCurrent', 'Liabilities',  'Assets',
    #                 'RetainedEarningsAccumulatedDeficit', 'LiabilitiesAndStockholdersEquity')
    #     wcTags1 <-  'StockholdersEquity'
    #     #wcTags2 <- c('BeforeIncomeTax', 'WeightedAverageDiluted')
    #     
    #     bs1 <- df1$calculation %>%
    #       select(elementId = fromElementId, calcRoleId = arcrole, period, roleId) %>%
    #       unique() %>% filter(elementId %in% wcTags|grepl(wcTags1, elementId)) %>%
    #       filter(!duplicated(paste0(elementId, period))) %>%
    #       left_join(df1$fact) %>% left_join(df1$context) %>%
    #       filter(is.na(value1)) %>%
    #       group_by(elementId, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(paste0(elementId, endDate))) %>%
    #       mutate(elementId = replace(elementId, grepl('StockholdersEquity', elementId) & !grepl('Liab', elementId), 'StockholdersEquity')) %>%
    #       group_by(elementId, endDate) %>% filter(fact == max(fact)) %>% ungroup() %>%
    #       select(elementId, fact, endDate) %>% distinct() %>% arrange(desc(endDate)) %>% spread(elementId, fact) %>%
    #       mutate(libs = LiabilitiesAndStockholdersEquity - StockholdersEquity) %>% rowwise() %>%
    #       mutate(wc = AssetsCurrent - LiabilitiesCurrent, libs = if('Liabilities' %in% names(.)) Liabilities else libs) %>% ungroup() %>%
    #       left_join(df1$fact %>% filter(elementId == 'RetainedEarningsAccumulatedDeficit') %>% 
    #                   left_join(df1$context) %>%
    #                   filter(is.na(value1)) %>%
    #                   group_by(elementId, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #                   mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                          yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #                   filter(!duplicated(paste0(elementId, endDate))) %>% select(endDate, RetainedEarnings = fact)) %>%
    #       mutate(A = wc/Assets,B = RetainedEarnings/Assets) %>% distinct()
    #     
    #     bs1 <- bs1 %>% filter(!is.na(B))
    #     
    #     bs1$libs[is.na(bs1$libs)] <- bs1$LiabilitiesAndStockholdersEquity[is.na(bs1$libs)]-bs1$StockholdersEquity[is.na(bs1$libs)]
    #     #bs1 <- bs1 %>% na.omit
    #     role1 <- df1$role %>% filter(type == 'Statement') %>% filter(!grepl('PARENT', toupper(roleId))) %>%
    #       filter(!grepl('BALANCE', toupper(roleId))) %>%
    #       filter(!grepl('COMPREHENSIVEINC', toupper(roleId))) %>%
    #       filter(!grepl('COMPREHENSIVELOS', toupper(roleId))) %>%
    #       filter(!grepl('CASHFLOWS', toupper(roleId))) %>%
    #       filter(!grepl('FINANCIALPOS', toupper(roleId)))%>%
    #       filter(!grepl('EQUIT', toupper(roleId)))%>%
    #       filter(!grepl('SUPPLEMENT', toupper(roleId)))
    #     
    #     
    #     if(nrow(role1) == 0|comp.ticker == 'CLR'|comp.ticker == 'NBL'|comp.ticker == 'CXO'|comp.ticker == 'AR'){
    #       role2 <- df1$role %>% filter(type == 'Statement') %>% filter(!grepl('PARENT', toupper(roleId))) %>%
    #         filter(!grepl('BALANCE', toupper(roleId))) %>%
    #         #filter(!grepl('COMPREHENSIVEINC', toupper(roleId))) %>%
    #         #filter(!grepl('COMPREHENSIVELOS', toupper(roleId))) %>%
    #         filter(!grepl('CASHFLOWS', toupper(roleId))) %>%
    #         filter(!grepl('FINANCIALPOS', toupper(roleId)))%>%
    #         filter(!grepl('EQUIT', toupper(roleId)))%>%
    #         filter(!grepl('SUPPLEMENT', toupper(roleId))) 
    #       role1 <- role1 %>% rbind(role2) %>% distinct()
    #     }
    #     
    #     ebit <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #       rename(elementId = toElementId) %>% left_join(df1$fact) %>% filter(grepl('BeforeIncome', elementId)) %>% mutate(el1 = 'ebit') %>%
    #       filter(!duplicated(paste0(el1, period))) %>% select(elementId, period) %>%
    #       left_join(df1$fact) %>% left_join(df1$context) %>%
    #       filter(is.na(value1)) %>% mutate(monthsOn = elapsed_months(endDate, startDate))  %>%
    #       mutate(monthsOn = round(monthsOn/3, 0)*3) %>% mutate(elementId = 'el1') %>%
    #       group_by(elementId,startDate, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(paste0(elementId,startDate, endDate))) 
    #     
    #     if(comp.ticker == "CXO"){
    #       
    #       
    #       ebit <- df1$presentation %>% filter(roleId %in% role1$roleId) %>%
    #         rename(elementId = toElementId) %>% left_join(df1$fact) %>% 
    #         filter(grepl('BeforeIncome', elementId)|elementId == 'IncomeLossAttributableToParent') %>% mutate(el1 = 'ebit') %>%
    #         filter(!duplicated(paste0(el1, period))) %>% select(elementId, period) %>%
    #         left_join(df1$fact) %>% left_join(df1$context) %>%
    #         filter(is.na(value1)) %>% mutate(monthsOn = elapsed_months(endDate, startDate))  %>%
    #         mutate(monthsOn = round(monthsOn/3, 0)*3) %>%
    #         group_by(elementId,startDate, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #         filter(!duplicated(paste0(elementId,startDate, endDate))) 
    #     }
    #     
    #     ebit <- ebit %>% filter(monthsOn %in% c(3, 12)) #%>% filter(!grepl('10', endDate))
    #     ebit1 <- ebit %>% filter(monthsOn == 3) %>% select(endDate, ebit1 = fact)
    #     
    #     bs1 <- bs1 %>% left_join(ebit1)
    #     
    #     ebit2 <- ebit %>% filter(monthsOn == 12) %>% select(endDate, ebit2 = fact)
    #     
    #     bs1 <- bs1 %>% left_join(ebit2)
    #     bs1$ebit2 <- bs1$ebit2-lag(bs1$ebit1,1) - lag(bs1$ebit1, 2) - lag(bs1$ebit1, 3)
    #     
    #     bs1$ebit1[is.na(bs1$ebit1)] <- bs1$ebit2[is.na(bs1$ebit1)]
    #     
    #     bs1 <- bs1 %>% subset(select = -c(ebit2)) %>% mutate(C = ebit1*4/Assets)
    #     bs1$endDate <- as.Date(bs1$endDate)
    #     #bs1 <- bs1 %>% filter(!is.na(C))
    #     
    #     sP <- getSymbols(comp.ticker, from = min(bs1$endDate) %m+% months(-1),
    #                      to = max(bs1$endDate),warnings = FALSE,
    #                      auto.assign = F)
    #     
    #     names(sP) <- word(names(sP), 2, sep = fixed('.'))
    #     
    #     splits <- getSplits(comp.ticker, from =  min(bs1$endDate) %m+% months(-1), to = max(bs1$endDate))
    #     
    #     
    #     if(!is.na(splits)){
    #       names(splits) <- word(names(splits), 2, sep = fixed('.'))
    #       splits <- as.data.frame(splits) %>% mutate(endDate = date(splits))
    #       splits <- splits[nrow(splits):1,]
    #       splits$spl1 <- splits$spl
    #       i <- 2
    #       while(i <= nrow(splits)){
    #         
    #         splits$spl1[i] <- splits$spl1[i]*splits$spl1[i-1]
    #         
    #         i <- i + 1
    #       }
    #       
    #       
    #       sP <- as.data.frame(sP) %>% mutate(endDate = date(sP)) %>% left_join(splits %>% select(endDate, spl1))
    #       sP <- sP %>% arrange(desc(endDate))
    #       sP$spl1[1] <- 1
    #       sP$spl1<- na.locf(sP$spl1)
    #       sP$Adjusted <- sP$Adjusted/sP$spl1
    #     } else {
    #       sP <- as.data.frame(sP) %>% mutate(endDate = date(sP)) 
    #     }
    #     
    #     sP <- sP %>% group_by(year(endDate), month(endDate)) %>% filter(day(endDate) == max(day(endDate))) %>% ungroup()
    #     sP$endDate1 <- as.Date(paste0(year(sP$endDate), '-', month(sP$endDate), '-', days_in_month(sP$endDate)))
    #     #sP$endDate <- as.Date(sP$endDate)
    #     
    #     bs1 <- bs1 %>% mutate(endDate1 = as.Date(paste0(year(endDate), '-', month(endDate), '-', days_in_month(endDate)))) %>%
    #       left_join(sP %>% select(endDate1, sharePrice = Adjusted))
    #     
    #     
    #     
    #     
    #     shares <- df1$fact %>% filter(grepl('OfDilutedShares', elementId)) %>% 
    #       left_join(df1$context) %>%
    #       filter(is.na(value1)) %>% mutate(monthsOn = elapsed_months(endDate, startDate))  %>%
    #       group_by(elementId,startDate, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange((yr), (qtr)) %>%
    #       filter(!duplicated(paste0(elementId,startDate, endDate))) %>%
    #       group_by(endDate) %>% filter(monthsOn == min(monthsOn)) %>% ungroup() %>%
    #       mutate(endDate = as.Date(endDate)) %>%
    #       select(endDate, shares = fact) %>% group_by(endDate) %>% filter(shares == max(shares)) %>% ungroup()
    #     
    #     shares$shares[shares$shares >209817169000 ] <- shares$shares[shares$shares >209817169000 ]/1000
    #     
    #     if(nrow(shares) == 0){
    #       shares <- df1$fact %>% filter(grepl('WeightedAverageNumber', elementId)) %>% 
    #         left_join(df1$context) %>%
    #         filter(is.na(value1)) %>% mutate(monthsOn = elapsed_months(endDate, startDate))  %>%
    #         group_by(elementId,startDate, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #         mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #                yr = as.numeric(substr(yr,2,5))) %>% arrange((yr), (qtr)) %>%
    #         filter(!duplicated(paste0(elementId,startDate, endDate))) %>%
    #         group_by(endDate) %>% filter(monthsOn == min(monthsOn)) %>% ungroup() %>%
    #         mutate(endDate = as.Date(endDate)) %>%
    #         select(endDate, shares = fact) %>% group_by(endDate) %>% filter(shares == max(shares)) %>% ungroup()
    #     }
    #     
    #     
    #     bs1 <- bs1 %>% left_join(shares %>% mutate(endDate1 = as.Date(paste0(year(endDate), '-', month(endDate), '-', days_in_month(endDate)))) %>%
    #                                subset(select = -c(endDate))) %>%
    #       mutate(marketCap = shares*sharePrice) %>%
    #       mutate(D = marketCap/libs)
    #     
    #     
    #     # role1 <- df1$role %>% filter(type == 'Statement') %>% filter(!grepl('PARENT', toupper(roleId))) %>%
    #     #   filter(!grepl('BALANCE', toupper(roleId))) %>%
    #     #   filter(!grepl('COMPREHENSIVEINC', toupper(roleId))) %>%
    #     #   filter(!grepl('COMPREHENSIVELOS', toupper(roleId))) %>%
    #     #   filter(!grepl('CASHFLOWS', toupper(roleId))) %>%
    #     #   filter(!grepl('FINANCIALPOS', toupper(roleId)))%>%
    #     #   filter(!grepl('EQUIT', toupper(roleId)))%>%
    #     #   filter(!grepl('SUPPLEMENT', toupper(roleId)))
    #     # 
    #     # 
    #     # if(nrow(role1) == 0|comp.ticker == 'CLR'){
    #     #   role2 <- df1$role %>% filter(type == 'Statement') %>% filter(!grepl('PARENT', toupper(roleId))) %>%
    #     #     filter(!grepl('BALANCE', toupper(roleId))) %>%
    #     #     #filter(!grepl('COMPREHENSIVEINC', toupper(roleId))) %>%
    #     #     #filter(!grepl('COMPREHENSIVELOS', toupper(roleId))) %>%
    #     #     filter(!grepl('CASHFLOWS', toupper(roleId))) %>%
    #     #     filter(!grepl('FINANCIALPOS', toupper(roleId)))%>%
    #     #     filter(!grepl('EQUIT', toupper(roleId)))%>%
    #     #     filter(!grepl('SUPPLEMENT', toupper(roleId))) 
    #     #   role1 <- role1 %>% rbind(role2) %>% distinct()
    #     # }
    #     
    #     
    #     sales <- df1$presentation %>% filter(roleId %in% role1$roleId) %>% filter(grepl('Revenue', fromElementId)|grepl('Sales', fromElementId)|
    #                                                                                 grepl('Revenue', toElementId)|grepl('Sales', toElementId)) %>%
    #       filter(!grepl('Cost', toElementId)) %>%
    #       group_by(fromElementId, period) %>% filter(as.numeric(order) == max(as.numeric(order))) %>% mutate(order = 1) %>%
    #       filter(!duplicated(paste0(order, period))) %>% rename(elementId = toElementId) %>%
    #       left_join(df1$fact) %>% left_join(df1$context) %>%
    #       filter(is.na(value1)) %>% mutate(monthsOn = elapsed_months(endDate, startDate))  %>%
    #       mutate(monthsOn = round(monthsOn/3,0)*3) %>%
    #       group_by(elementId,startDate, endDate) %>% filter(decimals == max(decimals)) %>% ungroup() %>%
    #       mutate(elementId = 'sales') %>%
    #       mutate(yr = readr::parse_number(period), qtr = as.numeric(substr(yr,1,1)),
    #              yr = as.numeric(substr(yr,2,5))) %>% arrange(desc(yr), desc(qtr)) %>%
    #       filter(!duplicated(paste0(elementId,startDate, endDate))) 
    #     
    #     
    #     sales1 <- sales %>% filter(monthsOn == 3) %>% select(endDate, sales1 = fact)
    #     
    #     bs1 <- bs1 %>% left_join(sales1 %>% mutate(endDate = as.Date(endDate)))
    #     
    #     sales2 <- sales %>% filter(monthsOn == 12) %>% select(endDate, sales2 = fact)
    #     
    #     bs1 <- bs1 %>% left_join(sales2%>% mutate(endDate = as.Date(endDate)))
    #     
    #     bs1$sales2 <- bs1$sales2-lag(bs1$sales1,1) - lag(bs1$sales1, 2) - lag(bs1$sales1, 3)
    #     
    #     bs1$sales1[is.na(bs1$sales1)] <- bs1$sales2[is.na(bs1$sales1)]
    #     
    #     bs1 <- bs1 %>% subset(select = -c(sales2))  %>% mutate(E = sales1*4/Assets)
    #     
    #     bs1 <- bs1 %>% mutate(z = 1.2*A + 1.4*B + 3.3*C + 0.6*D+E)
    #     
    #     bs1 <- bs1 %>% mutate(C = (ebit1 + lag(ebit1, 1) + lag(ebit1, 2) + lag(ebit1, 3))/Assets,
    #                           E = (sales1 + lag(sales1, 1) + lag(sales1, 2) + lag(sales1, 3))/Assets)
    #     bs1 <- bs1 %>% mutate(z1 = 1.2*A + 1.4*B + 3.3*C + 0.6*D+E)
    #     bs1$ticker <- comp.ticker
    #     bs1 <- bs1 %>% filter(!is.na(z))
    #   
    #   min1 <- min(bs1$z)
    #   
    #   if(min1 > 0){
    #     min1 <- 0
    #   }
    #   
    #   if(nrow(bs1) == 0){
    #     NULL
    #   } else {
    #   
    #   highchart() %>%
    #     hc_colors(cols) %>%
    #     hc_add_series(bs1, type = 'line',
    #                   hcaes(x = (endDate), y = round(z, 2)), 
    #                   color = cols[3], name = 'Annualized', tooltip = list(pointFormat = "Annualized Z-Score: {point.y}"),
    #                   marker = list(enabled = F)) %>%
    #     hc_add_series(bs1, type = 'line',
    #                   hcaes(x = (endDate), y = round(z1, 2)), 
    #                   color = cols[2], name = 'Trailing 12 Mo', tooltip = list(pointFormat = "TTM Z-Score: {point.y}"),
    #                   marker = list(enabled = F)) %>%
    #     hc_xAxis(type = 'datetime', labels = list(style = list(fontSize = '10pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                            color =  cols[1]))) %>%
    #     hc_yAxis(gridLineColor = 'transparent', min = min1, labels = list(format = "{value}",
    #                                                           style = list(fontSize = '10pt', fontWeight = 'bold', 
    #                                                                        fontFamily = 'Arial', color =  cols[1])),
    #              title = list(text = 'Altman Z-Score',
    #                           style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                        color =  cols[1])),
    #              plotBands = list(
    #                list(
    #                  label = list(text = "Grey Area", style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                color =  cols[1])),
    #                  color = cols[4],
    #                  from = 1.8,
    #                  to = 3
    #                )
    #              ),
    #              plotLines = list(
    #                list(
    #                  label = list(text = "Distress", y= 20, style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                                      color =  cols[1])),
    #                  color = cols[10],
    #                  width = 5,
    #                  value = 1.8
    #                ),
    #                list(
    #                  label = list(text = "Safe", style = list(fontSize = '12pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                                                           color =  cols[1])),
    #                  color = cols[11],
    #                  width = 5,
    #                  value = 3
    #                )
    #              )) %>%
    #     hc_title(text =glue::glue('{df1$fact$ticker[1]} Altman Z-Score'),align = 'left',
    #              style = list(fontSize = '20pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                           color =  cols[2]) )%>%
    #     hc_subtitle(text =glue::glue('Source: SEC/Yahoo Finance/Tidyquant'),align = 'left',
    #                 style = list(fontSize = '14pt', fontFamily = 'Arial', fontWeight = 'bold',
    #                              color =  cols[3]) )%>%
    #     hc_legend(floating = T,
    #               verticalAlign = 'top',
    #               align = 'right',
    #               layout = 'horizontal',
    #               itemStyle = list(fontFamily = 'Arial', color = cols[2], fontSize = '12pt'))
    #   }
    #   
    #   
    #   }
      
      
      ### Log Analysis
      
      observe({
        if(is.null(values$logFile)||nrow(values$logFile) == 0){
          shinyjs::disable('addFormula')
        } else {
          if(input$userColumn %in% names(values$logFile)||is.null(input$userFormula)||
             input$userFormula == ''||is.null(input$userColumn)||input$userColumn == ''){
            shinyjs::disable('addFormula')
          } else {
            shinyjs::enable('addFormula')
          }
        }
      })
      
      observeEvent(input$addFormula, {
        
        #logFile <- values$logFile
        #txt1 <- input$userColumn
        tryCatch({
          
          values$logFile <- values$logFile %>% mutate(txt1= eval(parse(text = input$userFormula)))
          names(values$logFile)[length(values$logFile)] <- input$userColumn
          tst2 <- which(names(values$logFile) == input$depth)
          names1 <- names(values$logFile[,-tst2])
          output$formCheck <- renderText('Success')
          #print(names1)
          updateSelectizeInput(session, 'track1', choices =names1)
          updateSelectizeInput(session, 'track2', choices =names1)
          updateSelectizeInput(session, 'track3', choices =names1)
          updateSelectizeInput(session, 'track4', choices =names1)
          
          
        },
        error = function(e) {
          e
          output$formCheck <- renderText('Error in Formula')
        })
        
      })
      
      output$track1Scale <- rhandsontable::renderRHandsontable({
        if(is.null(input$track1)||input$track1 == ''){
          NULL
        } else {
          
          DF <- data.frame(Component = input$track1, min = 0, max = 150) 
          # print(head(DF))
          # print(head(values$units))
          DF <- DF %>% left_join(values$units)
          
          rhandsontable(DF, rowHeaders = NULL, width = '100%', stretchH = "all") %>%
            hot_col("Component", readOnly = TRUE)
          
        }
      })
      
      output$track2Scale <- rhandsontable::renderRHandsontable({
        if(is.null(input$track2)||input$track2 == ''){
          NULL
        } else {
          
          DF <- data.frame(Component = input$track2, min = 0, max = 150)
          
          DF <- DF %>% left_join(values$units)
          
          rhandsontable(DF, rowHeaders = NULL, width = '100%', stretchH = "all") %>%
            hot_col("Component", readOnly = TRUE)
          
        }
      })
      
      output$track3Scale <- rhandsontable::renderRHandsontable({
        if(is.null(input$track3)||input$track3 == ''){
          NULL
        } else {
          
          DF <- data.frame(Component = input$track3, min = 0, max = 150)
          DF <- DF %>% left_join(values$units)
          
          rhandsontable(DF, rowHeaders = NULL, width = '100%', stretchH = "all") %>%
            hot_col("Component", readOnly = TRUE)
          
        }
      })
      
      output$track4Scale <- rhandsontable::renderRHandsontable({
        if(is.null(input$track4)||input$track4 == ''){
          NULL
        } else {
          
          DF <- data.frame(Component = input$track4, min = 0, max = 150) 
          DF <- DF %>% left_join(values$units)
          
          rhandsontable(DF, rowHeaders = NULL, width = '100%', stretchH = "all") %>%
            hot_col("Component", readOnly = TRUE)
          
        }
      })
      
      
      observeEvent(event_data("plotly_brushed"), {
        
        updateSliderInput(session, 'depthSlide',    value = c(event_data("plotly_brushed")$y[1],  event_data("plotly_brushed")$y[2]))
        
      })
      
      observe({
        if(is.null(input$file1$datapath)){
          NULL
        } else {
          
          tryCatch({
            
            values$logFile <- SDAR::read.LAS(input$file1$datapath)
            values$tops <- data.frame(formation = NA, top = NA)
            units <- read_delim(input$file1$datapath, delim = '  ')
            units <- units[,1:2]
            names(units)[1:2] <- c('Component', 'Unit')
            units$Component <- trimws(units$Component)
            units$Unit <- trimws(units$Unit)
            units <- units %>% filter(!is.na(Unit))
            units <- units %>% filter(Component %in% names(values$logFile)) %>% filter(!duplicated(Component))
            #print(head(units))
            values$units <- units
            output$formCheck1 <- renderText('Success')
          },
          error = function(e) {
            e
            output$formCheck1 <- renderText('Error in Upload')
          })
          
          
        }
        
      }
      )
      
      observeEvent(input$file1, {
        values$tops <- data.frame(formation = NA, top = NA)
      })
      
      
      
      
      observe({
        if(is.null(values$logFile)||nrow(values$logFile) == 0){
          updateSelectInput(session, 'depth', choices = '')
        } else {
          updateSelectInput(session, 'depth', choices = unique(names(values$logFile)))
        }
      })
      
      
      
      observeEvent(input$depth, {
        
        if(is.null(values$logFile)||nrow(values$logFile) == 0||is.null(input$depth)||input$depth == ''){
          updateSliderInput(session, 'depthSlide', min = 0, max = 100, value= c(40,60))
          updateSelectizeInput(session, 'track1', choices = '')
          updateSelectizeInput(session, 'track2', choices = '')
          updateSelectizeInput(session, 'track3', choices = '')
          updateSelectizeInput(session, 'track4', choices = '')
          
        } else {
          
          tst1 <- values$logFile %>%data.frame() %>% select(input$depth)# %>% na.omit
          names(tst1)[1] <- 'depth'
          
          tst1 <- tst1 %>% filter(depth != -999) %>% filter(!is.na(depth))
          tst1$depth <- abs(tst1$depth)
          #print(head(tst1))
          updateSliderInput(session, 'depthSlide', min = min(tst1$depth, na.rm=T), max = max(tst1$depth, na.rm=T),
                            value = c(min(tst1$depth, na.rm=T), max(tst1$depth, na.rm = T)))
          
          tst2 <- which(names(values$logFile) == input$depth)
          names1 <- names(values$logFile[,-tst2])
          #print(names1)
          updateSelectizeInput(session, 'track1', choices =names1)
          updateSelectizeInput(session, 'track2', choices =names1)
          updateSelectizeInput(session, 'track3', choices =names1)
          updateSelectizeInput(session, 'track4', choices =names1)
          
        }
        
      })
      
      
      
      
      output$track1Plot <- renderPlotly({
        if(is.null(input$track1)||input$track1 == ''||is.null(input$track1Scale)){
          NULL
        } else {
          tst2 <- which(names(values$logFile) == input$depth|
                          names(values$logFile) %in% input$track1)
          
          tst1 <- values$logFile[,tst2]
          
          names1 <- which(names(tst1) == input$depth)
          
          names(tst1)[names1] <- 'depth'
          
          dfx <- data.frame(hot_to_r(input$track1Scale))
          
          tst1 <- tst1 %>% gather(Component, value, -depth) %>%
            left_join(dfx)
          
          tst1$plot <- 1+(tst1$value-tst1$min)/(tst1$max-tst1$min)*100
          tst1$depth <- abs(tst1$depth)
          
          
          plot_ly(
            type = 'scatter',
            x = tst1$plot,
            y = tst1$depth,
            group_by = tst1$Component,
            color = tst1$Component,
            colors = cols,
            text = paste(tst1$Component,
                         "<br>Depth: ", tst1$depth,
                         "<br>Value: ", tst1$value),
            hoverinfo = 'text',
            mode = 'lines'
          ) %>%
            layout(dragmode = "select", yaxis =list( range = c(max(tst1$depth), min(tst1$depth))),
                   xaxis = list( range = c(1, 100),
                                 title = "",
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = FALSE,
                                 showgrid = FALSE)) %>%
            event_register(event = "plotly_brushed") %>%
            event_register(event = 'plotly_click')
        }
      })
      
      
      dataModal <- function(failed = FALSE) {
        modalDialog(
          textInput("dataset", "Formation",
                    placeholder = 'Wolfcamp A'
          ),
          numericInput('top1', 'Formation Top', value = event_data("plotly_click")$y),
          
          footer = tagList(
            actionButton('addTop', 'Add Top'),
            modalButton("Close")
          )
        )
      }
      
      observe({
        if(is.null(input$dateset)||input$dateset == ''||is.null(input$top1)||is.na(as.numeric(input$top1))){
          shinyjs::disable('addTop')
        } else {
          shinyjs::enable('addTop')
        }
      })
      
      observeEvent(input$addTop, {
        
        values$tops <- values$tops %>% filter(!is.na(formation)) %>%
          filter(!formation %in% input$dataset) %>%
          rbind(data.frame(formation = input$dataset, top = input$top1))
        
        
      })
      # Show modal when button is clicked.
      
      observeEvent(event_data("plotly_click"), {
        
        showModal(dataModal())
        
      })
      
      
      
      observe({
        if(input$track1Type == 'log'){
          
          plotlyProxy("track1Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'log', title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(log10(1), log10(100)))))
        } else {
          plotlyProxy("track1Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'linear', title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(1, 100))))
        }
        
        if(input$track2Type == 'log'){
          
          plotlyProxy("track2Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'log',title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(log10(1), log10(100)))))
        } else {
          plotlyProxy("track2Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'linear',title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(1, 100))))
        }
        
        
        if(input$track3Type == 'log'){
          
          plotlyProxy("track3Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'log',title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(log10(1), log10(100)))))
        } else {
          plotlyProxy("track3Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'linear',title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(1, 100))))
        }
        
        if(input$track4Type == 'log'){
          
          plotlyProxy("track4Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'log',title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(log10(1), log10(100)))))
        } else {
          plotlyProxy("track4Plot", session) %>%
            plotlyProxyInvoke("relayout", list(yaxis =list( range = c(max(input$depthSlide), min(input$depthSlide))),
                                               xaxis =list(type = 'linear',title = "",
                                                           zeroline = FALSE,
                                                           showline = FALSE,
                                                           showticklabels = FALSE,
                                                           showgrid = FALSE,
                                                           autorange = F, range = c(1, 100))))
        }
        
        
        
      })
      
      observe({
        if(is.null(values$tops)||nrow(values$tops %>% filter(!is.na(formation)))==0){
          NULL
        } else {
          
          tops <- values$tops
          
          line_list <- list()
          for(i in 1:nrow(tops)){ 
            line_color <- 'black'
            line_list[[i]] <- 
              list(type = "line", fillcolor = line_color,text = tops[[1]][i], line = list(color = line_color), opacity = 0.8, x0 = 1, x1 = 100, xref = "x", y0 = tops[[2]][i], y1 = tops[[2]][i], yref = "y")
          }
          
          plotlyProxy("track1Plot", session) %>%
            plotlyProxyInvoke('relayout', list(shapes = line_list))
          
          line_list <- list()
          for(i in 1:nrow(tops)){ 
            line_color <- 'black'
            line_list[[i]] <- 
              list(type = "line", fillcolor = line_color,text = tops[[1]][i], line = list(color = line_color), opacity = 0.8, x0 = 1, x1 = 100, xref = "x", y0 = tops[[2]][i], y1 = tops[[2]][i], yref = "y")
          }
          
          plotlyProxy("track2Plot", session) %>%
            plotlyProxyInvoke('relayout', list(shapes = line_list))
          
          
          line_list <- list()
          for(i in 1:nrow(tops)){ 
            line_color <- 'black'
            line_list[[i]] <- 
              list(type = "line", fillcolor = line_color,text = tops[[1]][i], line = list(color = line_color), opacity = 0.8, x0 = 1, x1 = 100, xref = "x", y0 = tops[[2]][i], y1 = tops[[2]][i], yref = "y")
          }
          
          plotlyProxy("track3Plot", session) %>%
            plotlyProxyInvoke('relayout', list(shapes = line_list))
          
          line_list <- list()
          for(i in 1:nrow(tops)){ 
            line_color <- 'black'
            line_list[[i]] <- 
              list(type = "line", fillcolor = line_color,text = tops[[1]][i], line = list(color = line_color), opacity = 0.8, x0 = 1, x1 = 100, xref = "x", y0 = tops[[2]][i], y1 = tops[[2]][i], yref = "y")
          }
          
          plotlyProxy("track4Plot", session) %>%
            plotlyProxyInvoke('relayout', list(shapes = line_list))
          
        }
      })
      
      
      output$track2Plot <- renderPlotly({
        if(is.null(input$track2)||input$track2 == ''||is.null(input$track2Scale)){
          NULL
        } else {
          tst2 <- which(names(values$logFile) == input$depth|
                          names(values$logFile) %in% input$track2)
          
          tst1 <- values$logFile[,tst2]
          
          names1 <- which(names(tst1) == input$depth)
          
          names(tst1)[names1] <- 'depth'
          dfx <- data.frame(hot_to_r(input$track2Scale))
          
          tst1 <- tst1 %>% gather(Component, value, -depth) %>%
            left_join(dfx)
          
          tst1$plot <- 1+(tst1$value-tst1$min)/(tst1$max-tst1$min)*100
          tst1$depth <- abs(tst1$depth)
          
          
          
          plot_ly(
            type = 'scatter',
            x = tst1$plot,
            y = tst1$depth,
            group_by = tst1$Component,
            color = tst1$Component,
            colors = cols[4:length(cols)],
            text = paste(tst1$Component,
                         "<br>Depth: ", tst1$depth,
                         "<br>Value: ", tst1$value),
            hoverinfo = 'text',
            mode = 'lines'
          ) %>%
            layout(dragmode = "select", yaxis =list( range = c(max(tst1$depth), min(tst1$depth))),
                   xaxis = list( range = c(1, 100),title = "",
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = FALSE,
                                 showgrid = FALSE)) %>%
            event_register(event = "plotly_brushed") %>%
            event_register(event = 'plotly_click')
        }
      })
      
      
      output$track3Plot <- renderPlotly({
        if(is.null(input$track3)||input$track3 == ''||is.null(input$track3Scale)){
          NULL
        } else {
          tst2 <- which(names(values$logFile) == input$depth|
                          names(values$logFile) %in% input$track3)
          
          tst1 <- values$logFile[,tst2]
          
          names1 <- which(names(tst1) == input$depth)
          
          names(tst1)[names1] <- 'depth'
          
          dfx <- data.frame(hot_to_r(input$track3Scale))
          
          tst1 <- tst1 %>% gather(Component, value, -depth) %>%
            left_join(dfx)
          
          tst1$plot <- 1+(tst1$value-tst1$min)/(tst1$max-tst1$min)*100
          tst1$depth <- abs(tst1$depth)
          
          
          
          plot_ly(
            type = 'scatter',
            x = tst1$plot,
            y = tst1$depth,
            group_by = tst1$Component,
            color = tst1$Component,
            colors =cols[7:length(cols)],
            text = paste(tst1$Component,
                         "<br>Depth: ", tst1$depth,
                         "<br>Value: ", tst1$value),
            hoverinfo = 'text',
            mode = 'lines'
          ) %>%
            layout(dragmode = "select",  yaxis =list( range = c(max(tst1$depth), min(tst1$depth))),
                   xaxis = list( range = c(1, 100),title = "",
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = FALSE,
                                 showgrid = FALSE)) %>%
            event_register(event = "plotly_brushed") %>%
            event_register(event = 'plotly_click')
        }
      })
      
      
      
      output$track4Plot <- renderPlotly({
        if(is.null(input$track4)||input$track4 == ''||is.null(input$track4Scale)){
          NULL
        } else {
          tst2 <- which(names(values$logFile) == input$depth|
                          names(values$logFile) %in% input$track4)
          
          tst1 <- values$logFile[,tst2]
          
          names1 <- which(names(tst1) == input$depth)
          
          names(tst1)[names1] <- 'depth'
          
          dfx <- data.frame(hot_to_r(input$track4Scale))
          
          tst1 <- tst1 %>% gather(Component, value, -depth) %>%
            left_join(dfx)
          
          tst1$plot <- 1+(tst1$value-tst1$min)/(tst1$max-tst1$min)*100
          tst1$depth <- abs(tst1$depth)
          
          
          plot_ly(
            type = 'scatter',
            x = tst1$plot,
            y = tst1$depth,
            group_by = tst1$Component,
            color = tst1$Component,
            colors =cols[10:length(cols)],
            text = paste(tst1$Component,
                         "<br>Depth: ", tst1$depth,
                         "<br>Value: ", tst1$value),
            hoverinfo = 'text',
            mode = 'lines'
          ) %>%
            layout(dragmode = "select",  yaxis =list( range = c(max(tst1$depth), min(tst1$depth))),
                   xaxis = list( range = c(1, 100),title = "",
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = FALSE,
                                 showgrid = FALSE)) %>%
            event_register(event = "plotly_brushed") %>%
            event_register(event = 'plotly_click')
        }
      })
      
      
      
      output$logTable <- DT::renderDataTable({
        if(is.null(values$logFile)||nrow(values$logFile) == 0){
          NULL
        } else {
          DT::datatable(head(values$logFile), rownames=F,extensions = c('Scroller'), 
                        options = list(dom='Bfrtip', paging = FALSE,
                                       #buttons = c('copy'),
                                       scrollX=TRUE, scrollY=TRUE,
                                       info = FALSE, ordering = FALSE, searching = FALSE),
                        caption = htmltools::tags$caption(
                          style = 'caption-side: bottom; text-align: center;',
                          'Table: ', htmltools::em('Log Values - First 6 Rows')),
                        class = 'cell-border stripe')
        }
      })
      
      
      output$topsList <- DT::renderDataTable({
        if(is.null(values$tops)||nrow(values$tops %>% filter(!is.na(formation))) == 0){
          NULL
        } else {
          DT::datatable(head(values$tops), rownames=F,extensions = c('Scroller'), 
                        options = list(dom='Bfrtip', paging = FALSE,
                                       #buttons = c('copy'),
                                       scrollX=TRUE, scrollY=TRUE,
                                       info = FALSE, ordering = FALSE, searching = FALSE),
                        caption = htmltools::tags$caption(
                          style = 'caption-side: bottom; text-align: center;',
                          'Table: ', htmltools::em('Picked Tops')),
                        class = 'cell-border stripe')
        }
      })
      
      observe({
        if(is.null(values$logFile)||nrow(values$logFile) == 0){
          shinyjs::disable('downloadData')
        } else {
          shinyjs::enable('downloadData')
        }
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('logFile', ".csv", sep = "")
        },
        content = function(file) {
          write.csv(values$logFile, file, row.names = FALSE)
        }
      )
      
      output$cutoffs <- renderRHandsontable({
        
        if(is.null(values$logFile)||nrow(values$logFile)==0){
          NULL
        } else {
          tst2 <- append(input$depth, input$track1)
          tst2 <- append(tst2, input$track2)
          tst2 <- append(tst2, input$track3)
          tst2 <- append(tst2, input$track4)
          tst2 <- data.frame(components = tst2) 
          tst2 <- tst2 %>% filter(components != '') %>% filter(!duplicated(components))
          
          if(nrow(tst2) == 1){
            NULL
          } else {
            
            tst2 <- tst2$components
            
            
            
            tst1 <- values$logFile[,tst2]
            
            names1 <- which(names(tst1) == input$depth)
            
            names(tst1)[names1] <- 'depth'
            
            
            
            if(nrow(values$tops %>% filter(!is.na(top)))==0){
              tst1$formation <- 'total'
            } else {
              
              tst1 <- tst1 %>% left_join(values$tops %>% rename(depth = top) %>% mutate(depth = as.numeric(depth)))
              tst1$formation[1][is.na(tst1$formation[1])] <- 'surface'
              tst1$formation <- zoo::na.locf(tst1$formation)
              
            }
            
            
            tst1 <- tst1  %>% gather(Component, value, -c(depth, formation)) %>%
              group_by(Component, formation) %>% summarise(min = min(value, na.rm = T), max = max(value, na.rm = T)) %>%
              ungroup()
            
            
            
            
            rhandsontable(tst1, rowHeaders = NULL, width = '100%', stretchH = "all") 
          }
          
        }
        
      })
      
      
      observe({
        if(is.null(input$cutoffs)){
          NULL
        } else {
          tst2 <- append(input$depth, input$track1)
          tst2 <- append(tst2, input$track2)
          tst2 <- append(tst2, input$track3)
          tst2 <- append(tst2, input$track4)
          tst2 <- data.frame(components = tst2)
          tst2 <- tst2 %>% filter(components != '') %>% filter(!duplicated(components))
          tst2 <- tst2$components
          
          
          
          tst1 <- values$logFile[,tst2]
          
          names1 <- which(names(tst1) == input$depth)
          
          names(tst1)[names1] <- 'depth'
          
          
          
          if(nrow(values$tops %>% filter(!is.na(top)))==0){
            tst1$formation <- 'total'
          } else {
            
            tst1 <- tst1 %>% left_join(values$tops %>% rename(depth = top))
            tst1$formation[1][is.na(tst1$formation[1])] <- 'surface'
            tst1$formation <- zoo::na.locf(tst1$formation)
            
            
            
          }
          
          tst1 <- tst1 %>%  #subset(select = -c(depth)) %>%
            gather(Component, value, -c(depth, formation)) %>% left_join(hot_to_r(input$cutoffs)) %>%
            mutate(pay = if_else(value >= min & value <= max, 1, 0)) %>% subset(select = -c(min, max)) %>%
            group_by(depth, formation) %>% summarise(pay  = sum(pay)/n()) %>% ungroup() %>%
            mutate(pay = replace(pay, pay < 1, 0)) %>% mutate(step = (max(depth)-min(depth))/(n()-1)) %>%
            mutate(pay = pay*step) %>% group_by(formation) %>% summarise(pay = sum(pay)) %>% ungroup()
          
          values$pay <- tst1
          
        }
      })
      
      output$pay <- DT::renderDataTable({
        if(is.null(values$pay)||nrow(values$pay) == 0){
          NULL
        } else {
          DT::datatable(values$pay, rownames=F,extensions = c('Scroller'), 
                        options = list(dom='Bfrtip', paging = FALSE,
                                       #buttons = c('copy'),
                                       scrollX=TRUE, scrollY=TRUE,
                                       info = FALSE, ordering = FALSE, searching = FALSE),
                        caption = htmltools::tags$caption(
                          style = 'caption-side: bottom; text-align: center;',
                          'Table: ', htmltools::em('Pay Calculations')),
                        class = 'cell-border stripe')
        }
      })
      
    })
    
  }
)
