rm(list=ls())
library('shiny')
library('shinythemes')
library('shinydashboard')
library('ggplot2')
library('dplyr')
library('readr')
library('fresh')
library('ggpubr')
library('DT')
library(cowplot)
options(shiny.maxRequestSize = 100*1024^2)

# 5.02.2022 Version of shiny based on the "bigout" files from initial cleaning step. Update to the force and EC figures to allow for interactive figures.  updated bug with the reactive eee() which resulted in rows being filtered out erroneously using "filter( == )" vs the corrected filter( %in% )

##################################################################################################
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#8aaeeb"
  ),
  adminlte_sidebar(
    width = "275px",
    dark_bg = "#3e5a85",
    dark_hover_bg = "#2d697d",
    dark_color = "#f0f2f5"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)
############################################################## 
# The above data is the color of main theme in web page.
{
  
  {
    header <- dashboardHeader(title = 'P4000 Interactive Cleaning')
  } #header
  
  {
    sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem('Import Data', tabName = 'Import', icon = icon('file')),
        menuItem('Dataset', tabName = 'Dataset', icon = icon('th')),
        menuItem('Optical', tabName = 'Optical', icon = icon('star')),
        # menuItem('Layer Combination', tabName = 'LayerC ', icon = icon('star')),
        menuItem('ECa and Force', tabName = 'sp', icon = icon('star')),
        div(style = 'height : 85px;', numericInput('num_input1', 'Depth Adjustment for Scan 1',
                                                   value = 0.00, min = -0.1, max = 0.1, step = 0.01)),
        div(style = 'height : 85px;', numericInput('num_input2', 'Depth Adjustment for Scan 2',
                                                   value = 0.00, min = -0.1, max = 0.1, step = 0.01)),
        div(style = 'height : 85px;', numericInput('num_input3', 'Depth Adjustment for Scan 3',
                                                   value = 0.00, min = -0.1, max = 0.1, step = 0.01)),
        div(style = 'height : 85px;', numericInput('num_input4', 'Depth Adjustment for Scan 4',
                                                   value = 0.00, min = -0.1, max = 0.1, step = 0.01)),
        div(style = 'height : 85px;', numericInput('num_input5', 'Depth Adjustment for Scan 5',
                                                   value = 0.00, min = -0.1, max = 0.1, step = 0.01)),
        
        
        #check boxes which indicate which scans to include or exclude from the data.  
        checkboxGroupInput(inputId = 'inputid',
                           label = "Deselect to remove scan(s)",
                           selected = c(1,2,3,4,5),
                           choices = c(1,2,3,4,5) ,
                           inline = TRUE)),
      actionButton("Update_button", "Update")
    )
    
  } #sidebar
  
  
  {
    body <- dashboardBody(
      use_theme(mytheme),
      tabItems(
        tabItem('Import',
                fluidPage(h1('Upload a File'),
                          fileInput(inputId = 'file',
                                    label = 'Limited to csv and xlsx only',
                                    accept = c('.xlsx', '.csv')),
                          tableOutput(outputId = 'Contents'),
                          verbatimTextOutput(output = 'data_file'))
        ),
        tabItem('Optical', 
                fluidPage(
                  fluidRow(
                    column(4, plotOutput('pltOPT', 
                                         click = "pltOPT_click",
                                         brush=brushOpts(id="pltOPT_brush"))),
                    column(4, plotOutput('pltEC',
                                         click = "pltEC_click",
                                         brush=brushOpts(id="pltEC_brush"))),
                    column(4, plotOutput('pltForce', 
                                         click = "pltForce_click",
                                         brush=brushOpts(id="pltForce_brush")))),
                  fluidRow(
                    column(12, plotOutput('plt2', width = '1200px', height = '650px'))
                    
                  )
                )
                
                
                
                # 
                # 
                # 
                # # plotOutput('plt1', width = '420px', height = '385px'),
                # # plotOutput('plt2', width = '650px', height = '650px')
                # plotOutput('plt1', width = '700', height = '385', click = "plt1_click",brush=brushOpts(id="plt1_brush")),
                # plotOutput('plt2', width = '1200px', height = '650px',
                #            click = "plt2_click",
                #            brush=brushOpts(id="plt2_brush"))
        ),
        tabItem('Dataset', 
                fluidPage(h1('Data'),
                          dataTableOutput('tab'),
                          tabItem('Export',
                                  fluidPage(downloadButton('downloadData', 'download')))
                )
        ),
        tabItem('sp', 
                plotOutput('plt3', width = '80%', height = '410px'),
        )
      )
    )
    
    
  } #Body
  
} #setting up the format of the dashbord page
# ui.r
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)


# Be aware of dashboardBody, dashboardSidebar, sidebarMenu, dashboardPage, dashboardHeader
# These are skeleton of the data structure. 

####################################################################################
# server.r
# Below code, check renderplot and renderDataTable.
server <- function(input, output, session){
  
 
  
  data <- reactive({
    req(input$file)
    infile <- input$file
    df <- read_csv(infile$datapath)
    return(df)
  })
  

  output$data_file <- renderPrint({
    if(is.null(input$file)){
      print('Please Import the .csv or .xlsx File')
    } else {
      infile <- input$file
      df <- read_csv(infile$datapath)
      glimpse(df)
    }
  })
  
  
  
  
  # zzz <- reactive({
  #   change_type <- 
  #     eee() %>% 
  #     mutate(EC = ifelse(EC_depth <0, "NA", EC),
  #            OD = ifelse(EC_depth <0, "NA", OD))
  # })
  
  #Return the data table to view with downloading options
  output$tab <- DT::renderDataTable(
    DT::datatable(eee()) %>% 
      formatRound(columns = c("OD","EC","Force"), digits=2),
    extensions = "Buttons", 
    options = list(dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'excel', 'pdf'))
  )
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste('LOC', unique(eee()$loc), '.csv', sep = '')
    },
    content = function(file){
      
      write.csv(eee(), file, row.names = F)
    }
  )
  
  
  #reactive data frame used to filter out the scans if they are clicked 
  eee <- reactive({
    change_type <- 
      data() %>% 
      dplyr::filter(scan %in% input$inputid) %>% 
      dplyr::mutate(
        depth = case_when(
          scan ==1 ~ depth - input$num_input1,
          
          scan ==2 ~ depth - input$num_input2,
          
          scan ==3 ~ depth - input$num_input3,
          
          scan ==4 ~ depth - input$num_input4,
          
          scan ==5 ~ depth - input$num_input5
        )) %>% 
          dplyr::mutate(          depthINV = depth *-1,
                    depthFRC = depthINV,
                    depthEC = depthINV +0.02,
                    depthOPT = depthINV + 0.09)

    
    change_type

  })
  #   
  # vals <- reactiveValues(
  #   keepOPT = rep(TRUE, nrow(eee())),
  #   keepEC = rep(TRUE, nrow(eee())),
  #   keepForce = rep(TRUE, nrow(eee()))
  # )
  #   
  
  #combined figure containing 1) optical data from all scans by depth, 
  # 2) EC by depth, 
  # and 3) Force by depth
  #all figures will be reactive and based on OD_depth that changes based on user adjustments 
  
  
  OpticalAll <- eventReactive(input$Update_button, {
    
    ggplot(eee(), aes(x = wavelen, y = depthOPT, color = OD)) +
      geom_point(size = 0.1, alpha = 0.75) + 
      ggtitle('Combined Layer') +
      theme_bw() + 
      binned_scale(aesthetics = 'color',
                   scale_name = 'stepsn',
                   palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", 
                                           "#00FF7F","#FFFF00","#FF6633","#FF0000"),
                   breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 
                              1.072371,1.28377, 2),
                   limits = c(-1, 2),
                   name = 'Reflectance') +
      ylab("Depth Optical") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      ylim(-1, 0)
    
    
    
  })
  
  output$pltOPT <- renderPlot({ 
    
    plotOPT <- OpticalAll()
    return(plotOPT)
    
    
    
  })
  
 
  output$pltEC <- renderPlot({ 
    #EC figure by depth, colored by the scan number
    # q <- eee() %>%
    
    eee() %>%   
      ggplot(aes(x = EC , y = depthEC, color = as.factor(scan), fill = as.factor(scan))) +
      geom_point(size = 2, alpha = 0.5) +
      theme_classic() +
      ylim(-1,0) +
      theme(axis.text.x = element_text(size = 10),
            legend.position = "none")
    
    
  })
  
  output$pltForce <- renderPlot({  
    #force figure, colored by the scan number
    # w <- eee() %>%
    eee() %>% 
      mutate(scan = as.factor(scan)) %>% 
      ggplot(aes(x = Force , y = depthFRC, color= scan, fill = scan)) +
      geom_point(size = 2, alpha = 0.5) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 10, angle = -90, hjust = 0),
            legend.position = c(0.2, 0),
            legend.direction="horizontal") 
    
    #plotting all three next to each other
    # plot_grid(zz,q,w, nrow=1, ncol =3, rel_widths = c(0.5,0.25,0.25))
    
    
    
  })
  
  Wav1 <- eventReactive(input$num_input1, {
    data() %>% 
      dplyr::filter(scan ==1) %>% 
      dplyr::mutate(depth = depth - input$num_input1,
                    depthINV = depth *-1,
                    depthFRC = depthINV,
                    depthEC = depthINV +0.02,
                    depthOPT = depthINV + 0.09) %>% 
      filter(depthOPT <0)
      
    
    
      
      filter(scan ==1, depthOPT <0) %>% 
      ggplot(
        # ggplot(data()[data()$scan == 1,], 
        aes(x = wavelen, y = OD, color = depthOPT)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 1') +
      theme_bw() +
      ylab("OD") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=10),
            legend.position = "none")
    
    
    
    
    
    
  })

   
  
  output$plt2 <- renderPlot({
    a <- ggplot(data()[data()$scan == 1,], aes(x = wavelen, y = depthOPT, color = OD)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 1') +
      theme_bw() + 
      binned_scale(aesthetics = 'color',
                   scale_name = 'stepsn',
                   palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", 
                                           "#00FF7F","#FFFF00","#FF6633","#FF0000"),
                   breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 
                              1.072371,1.28377, 2),
                   limits = c(-1, 2),
                   name = 'Reflectance') +
      ylab("Depth Optical") +       
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      ylim(-1, 0)
    
    b <- ggplot(data()[data()$scan == 2,], aes(x = wavelen, y = depthOPT, color = OD)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 2') +
      theme_bw() + 
      binned_scale(aesthetics = 'color',
                   scale_name = 'stepsn',
                   palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", 
                                           "#00FF7F","#FFFF00","#FF6633","#FF0000"),
                   breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 
                              1.072371,1.28377, 2),
                   limits = c(-1, 2),
                   name = 'Reflectance') +
      ylab("Depth Optical") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      ylim(-1, 0)
    
    c <- ggplot(data()[data()$scan == 3,], aes(x = wavelen, y = depthOPT, color = OD)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 3') +
      theme_bw() + 
      binned_scale(aesthetics = 'color',
                   scale_name = 'stepsn',
                   palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", 
                                           "#00FF7F","#FFFF00","#FF6633","#FF0000"),
                   breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 
                              1.072371,1.28377, 2),
                   limits = c(-1, 2),
                   name = 'Reflectance') +
      ylab("Depth Optical") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      ylim(-1, 0)
    
    d <- ggplot(data()[data()$scan == 4,], aes(x = wavelen, y = depthOPT, color = OD)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 4') +
      theme_bw() + 
      binned_scale(aesthetics = 'color',
                   scale_name = 'stepsn',
                   palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", 
                                           "#00FF7F","#FFFF00","#FF6633","#FF0000"),
                   breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 
                              1.072371,1.28377, 2),
                   limits = c(-1, 2),
                   name = 'Reflectance') +
      ylab("Depth Optical") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      ylim(-1, 0)
    
    
    e <- ggplot(data()[data()$scan == 5,], aes(x = wavelen, y = depthOPT, color = OD)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 5') +
      theme_bw() + 
      binned_scale(aesthetics = 'color',
                   scale_name = 'stepsn',
                   palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", 
                                           "#00FF7F","#FFFF00","#FF6633","#FF0000"),
                   breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 
                              1.072371,1.28377, 2),
                   limits = c(-1, 2),
                   name = 'Reflectance') +
      ylab("Depth Optical") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none") +
      ylim(-1, 0)
    
    Wav1 <- 
      eee() %>% 
      filter(scan ==1, depthOPT <0) %>% 
      ggplot(
        # ggplot(data()[data()$scan == 1,], 
        aes(x = wavelen, y = OD, color = depthOPT)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 1') +
      theme_bw() +
      ylab("OD") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=10),
            legend.position = "none")
    
    Wav2 <-
      data() %>% 
      filter(scan ==2, depthOPT <0) %>% 
      ggplot(
        aes(x = wavelen, y = OD, color = depthOPT)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 2') +
      theme_bw() +
      ylab("OD") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=10),
            legend.position = "none")
    Wav3 <-  
      data() %>% 
      filter(scan ==3, depthOPT <0) %>% 
      ggplot(
        aes(x = wavelen, y = OD, color = depthOPT)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 3') +
      theme_bw() +
      ylab("OD") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=10),
            legend.position = "none")
    Wav4 <-  
      data() %>% 
      filter(scan ==4, depthOPT <0) %>% 
      ggplot(
        aes(x = wavelen, y = OD, color = depthOPT)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 4') +
      theme_bw() +
      ylab("OD") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=10),
            legend.position = "none")
    
    
    Wav5 <- 
      data() %>% 
      filter(scan ==5, depthOPT <0) %>% 
      ggplot(
        aes(x = wavelen, y = OD, color = depthOPT)) +
      geom_point(size = 0.1, alpha = 0.75) +
      ggtitle('Scan 5') +
      theme_bw() +
      ylab("OD") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=10),
            legend.position = "none")
    
    
    
    
    ggarrange(a,b,c,d,e,Wav1, Wav2,Wav3,Wav4,Wav5,ncol = 5,nrow = 2) 
  })
  
  
  
  #click and bursh events
  observeEvent(input$pltOPT_click, {
    res <- nearPoints(eee(), input$pltOPT_click, allRows = TRUE)
    
    eee()$keepOPT <- xor(eee()$keepOPT, res$selected_)
    
    
  })
  
  
  observeEvent(input$pltEC_click, {
    res <- nearPoints(eee(), input$pltEC_click, allRows = TRUE)
    
    eee()$keepEC <- xor(eee()$keepEC, res$selected_)
    
    
  })
  
  
  
  # Plot Combination
  # eee <- reactive({
  #   change_type <- 
  #     data() %>% 
  #     filter(scan == input$inputid)
  # })
  # 
  # output$plot1 <- renderPlot({
  #       ggplot(eee(), aes(x = wavelen, y = OD_depth*(-1), color = OD)) +
  #         geom_point(size = 0.1, alpha = 0.75) + 
  #         ggtitle('Adjusted Combined Layer') +    
  #         theme_bw() + 
  #         binned_scale(aesthetics = 'color',
  #                      scale_name = 'stepsn',
  #                      palette = function(x) c("#000066", "#0040FF", "#00AAFF", "#00FFEB", 
  #                                              "#00FF7F","#FFFF00","#FF6633","#FF0000"),
  #                      breaks = c(0.648, 0.70580, 0.7575569, 0.8130966, 0.9337078, 
  #                                 1.072371,1.28377, 2),
  #                      limits = c(-1, 2),
  #                      name = 'Reflectance') +
  #         ylab("OD_Depth") +
  #         theme(plot.title = element_text(hjust = 0.5)) + 
  #         ylim(-1, 0)
  # })
  
  # scatter plot 
  output$plt3 <- renderPlot({
    q <- data() %>%
      ggplot(aes(x = EC , y = depthEC, color = as.factor(scan), fill = as.factor(scan))) +
      geom_point(size = 2, alpha = 0.5) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 10),
            legend.position = "none")
    
    w <- data() %>%
      ggplot(aes(x = Force , y = depthFRC, color= as.factor(scan), fill = as.factor(scan))) +
      geom_point(size = 2, alpha = 0.5) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 10, angle = -90, hjust = 0),
            legend.position = c(0.1, 0.25))
    
    ggarrange(q, w)
  }) 
}


shinyApp(ui, server)


