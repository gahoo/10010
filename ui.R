
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, width='30%',
                tabsetPanel(
                  tabPanel('Ctrl',
                  uiOutput('PROVINCE_filter'),
                  uiOutput('SPEED_filter'),
                  uiOutput('TARIFF_TYPE_filter'),
                  br(),
                  selectInput('show', 'Mapping:',
                              choices=c("PRICE", "SPEED.number", "SPEED_PRICE"),
                              selected='SPEED_PRICE'),
                  checkboxInput('tbl_bounds', 'Bound Map to Table', value=T),
                  checkboxInput('show_data', 'Show Data', value=T)
                  ),
                  tabPanel('About', 
                           wellPanel(
                             '把联通的宽带业务信息抓下来做了这个小玩意，然并卵。',
                             '主要用到了shiny, leaflea, DT三个包。',
                             '地图geojson文件来自百度Echarts的范例。',
                             '如果侵犯到任何人的利益，请联系本人移除。',
                             br(),
                             a('分享到微博',href='http://v.t.sina.com.cn/share/share.php?url='))
                           )
                )
                #textOutput('helper'),
  ),
  conditionalPanel(condition = "input.show_data == true",
    absolutePanel(bottom = 10, left = 10,
                  tabsetPanel(
                    tabPanel('Table',
                             dataTableOutput('broadband_tbl')),
                    tabPanel('Barplot',
                             plotOutput('cities_bar_plot', height="640px", width='480px')),
                    tabPanel('Dotplot',
                             plotOutput('cities_dot_plot', height="640px", width='480px'))
                    )
                  )
    )
))
