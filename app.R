
#====================================================

rm(list=ls())
graphics.off()


mirror = 'https://cloud.r-project.org/'

if (!is.element("shiny", installed.packages()[,1])) {install.packages("shiny", dependencies = TRUE, repos = mirror)}
if (!is.element("shinyjs", installed.packages()[,1])) {install.packages("shinyjs", dependencies = TRUE, repos = mirror)}
if (!is.element("shinyWidgets", installed.packages()[,1])) {install.packages("shinyWidgets", dependencies = TRUE, repos = mirror)}
if (!is.element("DT", installed.packages()[,1])) {install.packages("DT", dependencies = TRUE, repos = mirror)}
if (!is.element("readxl", installed.packages()[,1])) {install.packages("readxl", dependencies = TRUE, repos = mirror)}
if (!is.element("writexl", installed.packages()[,1])) {install.packages("writexl", dependencies = TRUE, repos = mirror)}
if (!is.element("tidyverse", installed.packages()[,1])) {install.packages("tidyverse", dependencies = TRUE, repos= mirror)}
if (!is.element("plotly", installed.packages()[,1])) {install.packages("plotly", dependencies = TRUE, repos = mirror)}
if (!is.element("readr", installed.packages()[,1])) {install.packages("readr", dependencies = TRUE, repos= mirror)}
if (!is.element("reshape2", installed.packages()[,1])) {install.packages("reshape2", dependencies = TRUE, repos= mirror)}


#------------------------


library(shiny)
library( shinyWidgets)
library(shinyjs)

library(DT)
library(readxl)
library(writexl)
library(tidyverse)
library(plotly)
library(readr)
library(reshape2)

 
#================ Settings ========================

w_folder = "H:\\PD\\Vel25\\vel25_adaptive\\"

is_test = T
is_save = F

if (is_test) {
  
  vna_folder = paste0(w_folder, "VNA\\")
  contr_folder = paste0(w_folder, "PuttyFiles - Central\\")   
  
} else {
  
  vna_folder = paste0(w_folder, "VNA\\")
  contr_folder = paste0(w_folder, "PuttyFiles - Central\\")   
}



#================================================

infomessage = function(title, txt)
{
  showModal(
    modalDialog( title = title,   txt,   easyClose = TRUE,   footer = NULL))        
}


myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}



# function finds non-numeric elements of vector x
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}






ui <- fluidPage(
  

  # Application title
  titlePanel(h3("Data Import")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("data_up_butt", "Load data", width='100%'),
      
      br(), br(),
      pickerInput("chip_input", "Chip ID:",  choices = c(''), 
                  options = list('actions-box' = TRUE), multiple = FALSE) 
      
      # pickerInput("mode_input", "Mode:",  choices = c(''), 
      #             options = list('actions-box' = TRUE), multiple = FALSE) 
      
     # checkboxInput("inters_input", "Intersection only", FALSE)
      , width = 2
    ),

    # Show a plot of the generated distribution
    mainPanel(
    
      

      tabsetPanel(
        id = 'tabs',

        #----------------------------- TCF tab -----------------------
        
        tabPanel("TCF",
                 br(),
                
                 fluidRow(width=14,
                          DT::dataTableOutput("contr_table"),
                          br(),
                          column(width=12,  plotlyOutput("plot_tcf", height = 300  ) ),
                          br(), 

                          column(width=12,  plotlyOutput("plot_power", height = 300  )                                 
                          )


                     )
        ),

        #----------------------------- VNA tab -----------------------

        tabPanel("VNA",
                 
                 br(),
                 fluidRow(width=12,         
                          DT::dataTableOutput("chip_table"),
                          br(),
                          column(width=12,  plotlyOutput("plot_chip",  height = 400  )
                          )               
                 
                 
            )
        )



     )
    )
   )
)



#============================== SERVER =====================================


server <- function(input, output, session) {
  
  rea <- reactiveValues(chip_list = NULL, ds_contr = NULL, ds_vna = NULL, 
                        contr_files = NULL, ds_tcf = NULL )
  
  
  SaveData = function(data, w_folder) {
    #fdir = gsub(paste0('bin/tmp/tmp/', arcname), '', w_folder)
    fdir = w_folder
    outfilename = paste0(fdir, 'data_out.csv')

    s = 'write.table(data, outfilename, sep = ",", col.names = T, row.names = F, append = F)'
    s = myTryCatch ( eval(parse(text=s)) )
    if (!is.null(s$error)) infomessage('Error', paste('Data is not saved. ','\nCheck the file', outfilename))

  }

  UploadData = function(contr_folder) {


      # list of available files
      f_list = list.files(contr_folder, include.dirs = FALSE, recursive=TRUE)

      #  check if the files are of csv type
      ix = c()
      f = f_list[253]
      for (f in f_list) {
        s = unlist(strsplit(f, "\\."))  # split by dot '.'
        s1 = unlist(strsplit(f, "/"))  # split by slash '/'

        ix = c(ix, (tail(s, 1)=='csv' ) & !grepl('~', f, fixed = TRUE) &
                 !("OLD" %in% toupper(s1)) #&
                 # !grepl('DATA_OUT', toupper(tail(s1, 1)), fixed = TRUE) &
                 # !grepl('INFO_OUT', toupper(tail(s1, 1)), fixed = TRUE) 
               )

      }
      f_list = f_list[ix]


      if (length(f_list) == 0) {
        df_error = data.frame(c('No available csv files!'))
        colnames(df_error) = 'Error'

        s = paste0('No available csv files!')
        infomessage('Error', s)
        return()
      }

      f_list = gsub("/", "\\\\", f_list)

      #--------------------------------------------------------------------



      cols = c("Timestamp",  "Freq", #"Input_Voltage", "Input_Current", 
               "Power", "Vpp", "Temp",  #"Flood_Param",  "Fluid_State", "Fixed_Current", 
               'Parea', "Chip_id", "Mode", "Header_line", "FileName" )

      withProgress({


        out = data.frame()

        f_name = f_list[3]
        for (f_name in f_list) {


          incProgress(1/length(f_list), detail = f_name)
          setProgress(message = 'Controller data loading')

          print(paste('.................... ', f_name, '.................... ') )

          id = match(f_name, f_list)


          data <- read.table(paste0(contr_folder, "\\\\", f_name ), col.names = paste("V", 1:100),
                             fill = T, sep = ",",  quote = "", stringsAsFactors = FALSE)
          data <- data[, which(!is.na(data[1,]))]




          # positions of chip id
          s = c("chip name")
          tmp = apply(data, 1, paste, collapse=" ")
          ixc = grep(s[1], tmp)

          # positions of data table headers
          s = c("Timestamp", "Freq", "Input_Voltage")
          tmp = apply(data, 1, paste, collapse=" ")
          ixh = intersect(intersect(grep(s[1], tmp),grep(s[2], tmp)), grep(s[3], tmp))

          # positions of Press #
          s = c("Press ")
          tmp = apply(data, 1, paste, collapse=" ")
          ixp = grep(s[1], tmp)

          i = which(diff(ixp) > 1) # indexes of gaps between Press groups
          # positions of maximum items (Press 7)
          if (length(i) > 0) { ixp = ixp[i] } else { ixp = max(ixp) }


          for (i in ixh) {  # go over data headers

            # find Chip ID preceding the considered header
            ii = tail(ixc[ixc < i], 1)
            chid = apply(data[ii+1, ], 1, paste, collapse=" ")
            chid = trimws(as.character(chid))
            chid = gsub(" ", "-", chid, fixed = TRUE)
            chid = toupper( chid)

            # # find Mode
            ii = tail(ixp[ixp < i], 1)
            mode = data[ii+1, 1]
            #mode = as.numeric(mode)

            print(paste('@@Mode=', mode, 'i=', i, 'ii=', ii, 'ixp=', ixp))


            # find data table between the header and the first non-numeric row
            ii = which.nonnum(data[, 1] )
            ii = head(ii[ii > i], 1)   # end of the table

            if (ii - i < 2) {
              print(paste("Skipped empty table: ",  chid, " | ", i) )
              next
            }

            # extract the table from the data
            tmp = data[(i):(ii-1), ]
            tmp = apply(tmp, 2, function(x) gsub("^$|^ $", NA, x))   # replace " " to NA
            tmp <- tmp[, which(!is.na(tmp[1,]))]                     # drop NA in captions row
            tmp = data.frame(tmp, stringsAsFactors = FALSE)


            # check the last row in the extracted table
            k = length( which(is.na( tail(tmp, 1) )) )
            if (k  > 0) {                 # count NA elements in last row

              print(paste("Skipped last row: ",  chid, " | ", i, " | empty elements: ", k) )
              tmp = head(tmp, -1)
            }

            # standardize headers name
            s = gsub("\\s*\\([^\\)]+\\)","", head(tmp, 1))
            s = gsub(" ", "_", s )
            colnames(tmp) = s
            tmp = tmp[-1, ]

            tmp$Chip_id <- chid
            tmp$Mode <- mode

            tmp$Header_line <- i
            tmp$FileName <- f_name

            
            # area under Power line
            tmp$Timestamp = as.numeric(tmp$Timestamp)
            tmp$Power = as.numeric(tmp$Power)
            x = tmp$Timestamp
            y = tmp$Power
            sP = sum(diff(x) * (head(y,-1)+tail(y,-1)))/2                
            tmp$Parea <- sP
            
            out = rbind(out, tmp[cols])
          }


        }

      })




      out$Freq = as.numeric(out$Freq)
      out$Power10 = out$Power * 10
      out$Vpp = as.numeric(out$Vpp)
      out$Temp = as.numeric(out$Temp)

      Fr = 10.04     # from VNA
      T0 = 22       #(VNA measurement Temp)
      TCF75 = -75   #[ppm/C] (lower bound) 
      TCF90 = -90   #[ppm/C7] (upper bound)
      
      out$fTcf75 = TCF75 * Fr * (out$Temp-T0)/1e6 + Fr
      out$fTcf90 = TCF90 * Fr * (out$Temp-T0)/1e6 + Fr
      
      

      contr_files = unique(out[,c("Chip_id","Mode", "Header_line", "FileName")])
      contr_files['Contr_id'] = seq(1, nrow(contr_files) )
      
      out = merge(x = out, y = contr_files, by = c("Chip_id", "Mode", "Header_line", "FileName") )
      

      #============== START VNA =============


      # list of available files
      f_list = list.files(vna_folder, include.dirs = FALSE, recursive=TRUE)

      #  check if the files are of csv type
      ix = c()
      f = f_list[253]
      for (f in f_list) {
        s = unlist(strsplit(f, "\\."))  # split by dot '.'
        s1 = unlist(strsplit(f, "/"))  # split by slash '/'

        ix = c(ix, (tail(s, 1)=='csv' ) & !grepl('~', f, fixed = TRUE) &
                 !("OLD" %in% toupper(s1)) &
                 # !grepl('DATA_OUT', toupper(tail(s1, 1)), fixed = TRUE) &
                 # !grepl('INFO_OUT', toupper(tail(s1, 1)), fixed = TRUE) &
                  grepl('V25-', toupper(head(s1, 1)), fixed = TRUE)  )

      }
      f_list = f_list[ix]


      if (length(f_list) == 0) {
        df_error = data.frame(c('No available csv files!'))
        colnames(df_error) = 'Error'

        s = paste0('No available csv files!')
        infomessage('Error', s)
        return()
      }


      #--------- Analyze file names

      # parse Chip ID and row/col from file name
      pattern <- "([A-Z]{2})([0-9]{3})[- ]?([0-9]{1,2})[-]([0-9]{1,2})[-]"


      tmp = c()
      for (c in unique(f_list) ) {
        s0 = tail(unlist(strsplit(c, "/")), 1)  # split by slash '/'
        s = unlist(strsplit(s0, "[(]"))[1]  # ignore all after bracket "("
        s = gsub(" ", "-", s, fixed = TRUE)
        s = gsub(".", "-", s, fixed = TRUE)

        b = str_match(s, pattern)
        chip_id = substr(b[1], 1, nchar(b[1])-1)
        r = c(c, s0, chip_id, b[4], b[5])

        tmp = rbind(tmp, r)
      }
      tmp = as.data.frame(tmp)
      colnames(tmp) = c('W_fname', 'Fname', 'Chip_id', 'Col', 'Row')
      tmp['Col'] = str_pad(tmp$Col, 2, pad = "0") # add leading zeros
      tmp['Row'] = str_pad(tmp$Row, 2, pad = "0")
      tmp['Vna_id'] = seq(1, nrow(tmp))

      vna_files = tmp



      # chips presented both in VNA and controller data
      chip_list = merge(x = contr_files, y = vna_files, by = "Chip_id")     # inner join
      chip_list = chip_list[c("Contr_id", "Chip_id", "Col" , "Row" ,  "FileName",  "W_fname", "Mode", "Vna_id" )  ]
      colnames(chip_list) = c("Contr_id", "Chip_id", "Col" , "Row" ,  "File_Contr", "File_VNA", "Mode", "Vna_id")

      chip_list = data.frame(chip_list, stringsAsFactors = FALSE)
      
      
      #------------------ VNA data
      

      out2 = data.frame()

      withProgress({
      

        vna_list =  unique(chip_list$Vna_id)  
        for (vna_id in vna_list) {
          
          ix = which(chip_list$Vna_id == vna_id)
          f_name =  as.character(chip_list[ix[1], 6]) 
          f_name = gsub("/", "\\\\", f_name, fixed = TRUE)
  
          incProgress(1/length(vna_list), detail = f_name)
          setProgress(message = 'VNA data loading')
          
          print('......................................................')
          
          print(f_name)

          data  <- read_csv(paste0(vna_folder, "\\\\", f_name ), skip=4, col_types = cols())
          if (ncol(data) < 2) next
          
          # exclude columns containing non-numeric data
          tmp = apply(data, MARGIN = c(1,2), 'as.logical')
          tmp = apply(tmp, MARGIN = c(1,2), 'as.integer')
          tmp = as.data.frame(apply(tmp, 2, prod))
          
          ix = which(tmp[,1] ==1)
          rownames(tmp)[ix]
          data  <- data[ rownames(tmp)[ix] ]
          if (ncol(data) < 2) next

          # adjust target columns (add new or remove columns)
          cols = c('F', 'S', 'Z', 'R', 'X') 

          colnames(data) = cols
          
          data['Vna_id'] = vna_id
          data['F'] =  data$F / 1e6
          
          data['Smin'] = data$S==min(data$S)
          ix = which(data$S==min(data$S))
          data['Fr'] = data[ix, 1] 
          
          out2 = rbind(out2, data[c(cols, c('Fr', 'Vna_id'))])
              
        }
      })
      
      # link Fr from VNA  to Controller data
      tmp = unique(out2[, c('Vna_id', 'Fr')])
      tmp1 = unique(vna_files[, c('Vna_id', 'Chip_id')]) 
      tmp = merge(x = tmp, y = tmp1, by = "Vna_id")     # inner join
      
      
      #outv = merge(x = out, y = tmp, by = "Chip_id", all.x = TRUE)     # left join
      
      
      

      return ( list('out' = out, #'outv' = outv, 
                    'contr_files' = contr_files,
                    'vna_files' = vna_files, 'chip_list' = chip_list, 'out2' = out2) )

  }



  #-----------------  interface functions --------------------


  # load data button
  observeEvent(input$data_up_butt, {

    #LoadSetts(w_fname)

    if (!dir.exists(contr_folder)) {
      s = paste0('Folder ', contr_folder, ' not found!')
      infomessage('Error', s)
      return()
    }

    res = suppressWarnings(UploadData(contr_folder))
    if (is.null(res)) {return ()}

    contr_data <<- res$out
    contr_files <<- res$contr_files
    vna_files <<- res$vna_files
    chip_list <<- res$chip_list
    vna_data <<- res$out2
    
    rea$chip_list <- chip_list
    rea$contr_files <- contr_files
   
    
    x <- sort(as.vector(unique(chip_list$Chip_id)))
    x = choices = c('All', x)
    updatePickerInput(session, "chip_input",  choices = x   )
    
    # x <- sort(as.vector(unique(chip_list$Mode)))
    # x = choices = c('All', x)
    # updatePickerInput(session, "mode_input",  choices = x   )
    
    if (is_save) {
      withProgress({
  
        incProgress(0.7, detail = '')
        setProgress(message = 'Data saving')
  
        SaveData(contr_data, w_folder)
      })
    }

  })

  
  observeEvent(input$chip_input, {
    
    if (is.null(input$chip_input)) return()
    
    if (exists('contr_files')) {
      
      if (input$chip_input == 'All') {
        tmp = contr_files
        tmp1 = chip_list
      } else {
        tmp = contr_files %>% filter(Chip_id %in% input$chip_input)
        tmp1 = chip_list %>% filter(Chip_id %in% input$chip_input)
      }
      
      rea$contr_files = tmp
      rea$chip_list = tmp1
      
      
      # x = sort(as.vector(unique(tmp$Mode)))
      # x = choices = c('All', x)
      # updatePickerInput(session, "mode_input", choices = x, selected = x  )
      
    }
  }, ignoreNULL = TRUE)  
  
  
  
  # observeEvent(input$mode_input, {
  #   
  #   if (is.null(input$mode_input)) return()
  # 
  #   if (exists('contr_files')) {
  #     
  #     if (input$mode_input == 'All') {
  #       tmp = contr_files
  #     } else {
  #       tmp = contr_files %>% filter(Mode %in% input$mode_input)
  #     }
  # 
  #     rea$contr_files = tmp
  # 
  # 
  #   }
  # }, ignoreNULL = TRUE)    
  
  
  
  #-------------------- TCF tab ---------------
  
  output$contr_table <- DT::renderDataTable({

      if (length(rea$contr_files) > 0) {
      
      tmp = rea$contr_files
      DT::datatable(tmp, rownames = FALSE,  class="compact",
                    caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:130% ;','Table1: Controller datasets found in controller csv files'),
                    selection = list(mode = 'single', selected = 1),
                    options = list(lengthMenu = c(10, 50, 100, 500),  pageLength = 7,  dom = 'Brtip',  deferRender = TRUE,
                                   scroller = TRUE,
                                   autoWidth = TRUE ))   %>% 
                   formatStyle(columns = colnames(.), fontSize = '10%')
    }

  })
  

  
  observeEvent(input$contr_table_rows_selected, {
    print(length(rea$contr_files))
    
    if (length(rea$contr_files) > 0) {
      i = input$contr_table_rows_selected
      if (length(i) == 0) { i = 1 }

      tmp <- rea$contr_files[i, ]


      rea$ds_tcf <- contr_data %>% filter(Contr_id == tmp$Contr_id)
      print(tmp$Contr_id)
    }
  })



  output$plot_tcf <- renderPlotly({


    ds = rea$ds_tcf
    mode_id = head(ds$Mode, 1)
    print(mode_id)

    if (is.null(ds) )  return()
    if (nrow(ds) == 0) return()

    xx = ds$Timestamp

    p <- plot_ly()    %>%
      add_trace(data = ds, x = xx, y = ~Freq,
                type = 'scatter', name = 'Freq', mode = 'lines+markers') %>%
      add_trace(data = ds, x = xx, y = ~Temp, name = 'Temp', yaxis = "y2", type = 'scatter', mode = 'lines+markers') %>%
      
      add_trace(data = ds, x = xx, y = ~fTcf75, name = 'fTcf75', yaxis = "y", 
                type = 'scatter', mode = 'lines', line = list(dash = "dot", color = "darkseagreen")) %>%      
      add_trace(data = ds, x = xx, y = ~fTcf90, name = 'fTcf90', yaxis = "y",
                type = 'scatter', mode = 'lines', 
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
                line = list(dash = "dot", color = "darkseagreen")) %>%      


      layout(yaxis2 = list(title = "Temp", overlaying = "y", side = "right", automargin = T))


    p <- p %>%
      layout(yaxis = list(title = "Freq"),  xaxis = list(title = "Time, s"),
             title = paste('Controller vs. VNA/TCF  theory: Mode =', mode_id),
             legend = list(orientation = "h", xanchor = "center", x = 0.5)
             ) %>%


      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    p

  })
  
  output$plot_power <- renderPlotly({
    
    
    ds = rea$ds_tcf
    mode_id = head(ds$Mode, 1)
    p_area = round(as.numeric(head(ds$Parea, 1)), 2)
    
    print(mode_id)
    
    if (is.null(ds) )  return()
    if (nrow(ds) == 0) return()
    
    xx = ds$Timestamp
    
    p <- plot_ly()    %>%
      add_trace(data = ds, x = xx, y = ~Power,
                type = 'scatter', name = 'Power', mode = 'lines+markers') %>%
      add_trace(data = ds, x = xx, y = ~Vpp, name = 'Temp', yaxis = "y2", type = 'scatter', mode = 'lines+markers') %>%
      layout(yaxis2 = list(title = "Vpp", overlaying = "y", side = "right", automargin = T))
    
    
    
    p <- p %>%
      layout(yaxis = list(title = "Power"),  xaxis = list(title = "Time, s"),
             title = paste0('Power Savings and Peak Voltage Reduction: Mode =', mode_id,  ';  P_area = ', p_area),
             legend = list(orientation = "v", xanchor = "center", x = 0.5)
      ) %>%
      
      
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    p
    
  })
  
  
  #----------------------------- VNA tab --------------------------  
  
  
  output$chip_table <- DT::renderDataTable({
    
    if (length(rea$chip_list) > 0) {
      
      tmp = rea$chip_list
      DT::datatable(tmp, rownames = FALSE,  class="compact",
                    caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:130% ;','Table2: Chips found both in VNA and Controller data'),
                    selection = list(mode = 'single', selected = 1),
                    options = list(lengthMenu = c(10, 50, 100, 500),  pageLength = 10,  dom = 'Brtip',  deferRender = TRUE,
                                   scroller = TRUE
                    ))   %>% formatStyle(columns = colnames(.), fontSize = '20%')
    }
    
  })
  
  
  
  observeEvent(input$chip_table_rows_selected, {
    
    if (length(rea$chip_list) > 0) {
      i = input$chip_table_rows_selected
      if (length(i) == 0) { i = 1 }
      
      tmp <- rea$chip_list[i, ]

      rea$ds_contr <- contr_data %>% filter(Contr_id == tmp$Contr_id)
      rea$ds_vna <- vna_data %>% filter(Vna_id == tmp$Vna_id)      
    }
  })  
  
  
  
  output$plot_chip <- renderPlotly({
    
    
    ds = rea$ds_contr 
    ds1 = rea$ds_vna 
    
    if (is.null(ds) )  return() 
    if (nrow(ds) == 0) return()
    
    ix = which(ds$Power==min(ds$Power))
    Fr = as.numeric(ds[ix, ]['Freq'])
    Fr = round(Fr, 3)
    #print(Fr)
    
    offset = 0.75
    
    p <- plot_ly()    %>%
      add_trace(data = ds, x = ~Freq, y = ~Power,
                type = 'scatter', name = 'Power', mode = 'lines+markers') %>%

    add_trace(x = Fr-0.01,
              y = max(ds$Power)*1.3,
              mode = 'text',  text = paste0('', Fr ),
              type = 'scatter', showlegend = FALSE, yaxis = "y",
              textfont = list(color = 'cadetblue', size = 12)
    )

    lines =  list(type='line', x0 = Fr, x1 = Fr, y0=min(ds$Power), y1=max(ds$Power)*1.3, yref = "y",
                  line=list(dash='dot', width=3, color = 'lightblue',  name = 'Fr Contr'))
    
    if (!is.null(ds1) ) {
      if (nrow(ds1) > 0) {
        
        Fr1 = round(head(ds1$Fr, 1), 3)
        

                       
        p <- p %>% 
       
        add_trace(data = ds1, x = ~F, y = ~S, name = 'VNA S11', yaxis = "y2", color = 'orange', type = 'scatter', mode = 'lines+markers') %>%
          layout(yaxis2 = list(title = "VNA S11", overlaying = "y", side = "right", automargin = T))    %>%    
        
        
          
        add_trace(x = Fr1+0.01,
                    y = max(ds1$S)+offset,
                    mode = 'text',  text = paste0('', Fr1 ),
                    type = 'scatter', showlegend = FALSE, yaxis = "y2",
                    textfont = list(color = 'orange', size = 12)
         )
          
        
        lines = list(  lines,
                       list(type='line', x0 = Fr1, x1 = Fr1, y0=min(ds1$S), y1=max(ds1$S), yref = "y2",
                            line=list(dash='dot', width=3, color = 'lightpink',  name = 'Fr VNA'))
                    )
        
      }
    }  
    
    
    p <- p %>%      
      layout(yaxis = list(title = "Power"),  xaxis = list(title = "Freq, MHz"),
             title = 'Controller vs VNA data comparison',
             legend = list(orientation = "h", xanchor = "center", x = 0.8),
             shapes = lines) %>%
      

      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    p
    
  })    
  
  
  
  
  
  
  
  
  
  session$onSessionEnded(function() {
    
    stopApp()   # close command window
  })    
  

  
}


shinyApp(ui = ui, server = server)



