library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinyTree)
library(DT)

seas <- read.csv("data/seas.csv", header = T, stringsAsFactors = F)
samples <- read.csv("data/samples.csv", header = T, stringsAsFactors = F)
sample_seas <- read.csv("data/sample-seas.csv", header = T, stringsAsFactors = F)

ui <- fluidPage(
  fluidRow(column(1, actionButton("about", "About")),
           column(1, actionButton("contact", "Contact"))),
  fluidRow(column(9, leafletOutput("map", height = "700px")),
           column(3, dataTableOutput("seas")))
)

server <- function(input, output, session) {
  
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "About", footer = NULL,
      HTML("<p>Although our planet is covered with it, only 0.5% of our water needs are satisfied by desalinating seawater.
Current methods are expensive, high in energy cost and generate a significant carbon footprint. As an alternative
      to this, a new method is being developed at King’s College London aiming to desalinate water through crystal
      formation.vThe idea arose from a previous work, Spit Crystals, where a
      perfectly ordered crystal was grown from saliva. In this way crystallisation, a process that constitutes nearly
      everything that surrounds us, is recontextualised and applied to our environment addressing one key aspect of our greatest
      ecological crisis to date. Initital tests have been done with local seawater, however, in order to test the varying
      compositions of our world’s waters, we are seeking a global contribution in creating an Atlas of seas.
      We are inviting people from all over the world to send us samples of their seawater. So, if you: live near the sea, know someone who does,
      are planning on travelling somewhere near a salty body of water or are interested in the project please do get involved.
      </p><p>
      If you wish to post a sample, please collect some seawater, in any jar or bottle with a capacity of at least 10ml,
      include the date and location of where the sample was taken from and post to:
      </p><p>
      Inés Cámara Leret <br/>
      M10, Somerset House Studios, <br/>
      WC2R 1LA, London <br/>
      United Kingdom
      </p><p>To refund the postage costs please email the receipt to:
      </p><p>
      <a href='mailto:atlasofseas@gmail.com' target='_top'>atlasofseas@gmail.com</a>
      </p>"),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$contact, {
    showModal(modalDialog(
      title = "Contact", footer = NULL,
      HTML("<p>To get in touch please email: <a href='mailto:atlasofseas@gmail.com' target='_top'>atlasofseas@gmail.com</a>

</p><p>Please check this website for further updates including tracking your samples and the information from processing them.

</p><p>If you wish to post a sample, please collect some seawater, in any jar or bottle with a capacity of at least 10ml,include the date and location of where the sample was taken from and post to:

</p><p>Inés Cámara Leret<br/>
M10, Somerset House Studios,<br/>
WC2R 1LA, London<br/>
United Kingdom<br/>
</p><p>To refund the postage costs please email the receipt to the email above. Thank you!</p>"),
      easyClose = TRUE
    ))
  })
  
  output$seas <- renderDataTable({
    datatable(data.frame(Seas = seas$Structure,
                         Data = seas$Data), 
              escape = FALSE,
              options = list(scrollY = "600px",
                             dom = "ft",
                             pageLength = 157),
              selection = "single",
              rownames = FALSE)
  })

  colours <- reactive({
    ifelse(samples$id %in% sample_seas$id[sample_seas$sea == seas$Sea[input$seas_rows_selected]], "red", "blue")
  })
  
  output$map <- renderLeaflet({
    leaflet(data = samples, options = leafletOptions(minZoom = 2, maxZoom = 18)) %>%
      addTiles()
  })

  observe({

    leafletProxy("map", data = samples) %>%
      addCircleMarkers(fillOpacity = 1, color = NULL, fillColor = colours(), 
                 popup = ~as.character(id), label = "This is a label", radius = 6)
  })
  
}

shinyApp(ui, server)