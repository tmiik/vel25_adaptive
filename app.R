
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


is_test = F
is_save = F



if (is_test) {
  w_folder = "H:\\PD\\Vel25\\vel25_adaptive\\"
  
  vna_folder = paste0(w_folder, "VNA\\")
  contr_folder = paste0(w_folder, "PuttyFiles - Central\\")   
  
} else {
  w_folder = "\\\\tmi21\\Private\\R&D\\MechProdDev\\Projects\\Concept Development\\1805 Velocity25\\Testing\\Electrical\\Measurements\\"
  
  vna_folder = paste0(w_folder, "")
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


# function returns TRUE for any blank values
is.blank <- function(x, false.triggers=FALSE){
  if(is.function(x)) return(FALSE) # Some of the tests below trigger
  # warnings when used on functions
  return(
    is.null(x) ||                # Actually this line is unnecessary since
      length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
      all(is.na(x)) ||
      all(x=="") ||
      (false.triggers && all(!x))
  )
}


ui <- fluidPage(
  

  # Application title
  titlePanel(h4("Data Import")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("data_up_butt", "Load data", width='100%'),
      
      br(), br(),
      pickerInput("chip_input", "Chip ID:",  choices = c(''), 
                  options = list('actions-box' = TRUE), multiple = FALSE),
      
      br(), br(),
      pickerInput("contr_input", "Y axis (Contr.):",  choices = c("Power", "Power_db", "Vpp", "Temp"), 
                  selected = "Power_db", options = list('actions-box' = TRUE), multiple = FALSE) ,
      
      
      pickerInput("vna_input", "Y axis (VNA):",  choices = c('S', 'Z', 'R', 'X'), 
                  selected = "S", options = list('actions-box' = TRUE), multiple = FALSE) 
     
      
      , width = 2
    ),

    # Show a plot of the generated distribution
    mainPanel(
    
      

      tabsetPanel(
        id = 'tabs',

        #----------------------------- Combined tab -----------------------
        
        tabPanel("Controller vs VNA",
                 br(),
                
                 fluidRow(width=14,
                          DT::dataTableOutput("contr_table"),
                          br(), 
                          DT::dataTableOutput("vna_table")
                ),
                br(), 
                textOutput('warning_text'),
                #verbatimTextOutput('warning_text'),
                br(), 
                plotlyOutput("plot_freq", height = 400  ),
                br(), br(),
                plotlyOutput("plot_tcf", height = 300  ), 
                br(), br(),
                plotlyOutput("plot_power", height = 300  ),
                
                tags$head(tags$style("#warning_text{color: red; font-size: 18px; font-style: bold; }"  ) )
 
                               
        )


     )
    )
   )
)



#============================== SERVER =====================================


server <- function(input, output, session) {
  
  rea <- reactiveValues(vna_files = NULL, ds_contr = NULL, ds_vna = NULL, 
                        contr_files = NULL, ds_tcf = NULL,
                        chip_c = NULL, chip_v = NULL, match = NULL, mode = NULL, 
                        contr_id = NULL, vna_id = NULL,
                        fr_c = NULL, fr_v = NULL, p_area = NULL)
  
  
  SaveData = function(data, w_folder, sdev) {
    fdir = paste0(w_folder, 'output/') 
    outfilename = paste0(fdir, 'data_', sdev, '.csv')

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
               "Power", "Power_db", "Vpp", "Temp",  
               #"Flood_Param",  "Fluid_State", #"Fixed_Current", 
               'Parea', "Fr", "Chip_id", "Chip_raw", "Mode", "Header_line", "FileName", "Contr_id" )

      withProgress({


        out = data.frame()
        contr_files = c()
        
        id = 42

       # f_list = head(f_list, 10)
        #f_name = f_list[1]
        for (f_name in f_list) {


          incProgress(1/length(f_list), detail = f_name)
          setProgress(message = 'Controller data loading')

          print(paste('.................... ', f_name, '.................... ') )

          #id = match(f_name, f_list)


          data <- read.table(paste0(contr_folder, "\\\\", f_name ), col.names = paste("V", 1:100),
                             fill = T, sep = ",",  quote = "", stringsAsFactors = FALSE)
          data <- data[, which(!is.na(data[1,]))]


          # positions of data table headers
          s = c("Timestamp", "Freq", "Voltage")
          tmp = apply(data, 1, paste, collapse=" ")
          ixh = intersect(intersect(grep(s[1], tmp),grep(s[2], tmp)), grep(s[3], tmp))

          # positions of chip id
          s = c("Chip name:")
          tmp = apply(data, 1, paste, collapse=" ")
          ixc = grep(s[1], tmp)

          # positions of Mode #
          s = c("Mode:")
          tmp = apply(data, 1, paste, collapse=" ")
          ixp = grep(s[1], tmp)
          
          # # old version using Press menu:
          # if (F) {
          #   # positions of Press #
          #   s = c("Press ")
          #   tmp = apply(data, 1, paste, collapse=" ")
          #   ixp = grep(s[1], tmp)
          # 
          #   i = which(diff(ixp) > 1) # indexes of gaps between Press items
          #   # positions of maximum items (Press 7)
          #   if (length(i) > 0) { ixp = ixp[i] } else { ixp = max(ixp) }
          # }

          # parse Chip ID and row/col from file name
          pattern <- "([A-Z]{2})([0-9]{3})[- ]?([0-9]{1,2})[-]([0-9]{1,2})"          

          for (i in ixh) {  # go over data headers
            
            chid = NA 
            s0 = NA
            ii = NA
            if (length(ixc) > 0) { 
              
                # find Chip ID preceding the considered header
                ii = tail(ixc[ixc < i], 1)       
                if (length(ii) > 0) { 

                  
                  s0 = data[ii+1, 1]
                  ss = toupper(trimws(as.character(s0)) )
                  ss = gsub(" ", "-", ss, fixed = TRUE)
                  ss = gsub(".", "-", ss, fixed = TRUE)
                  
                  b = str_match(ss, pattern)                           
                  
                  if ( !is.na(b[1]) ) {  # if chip name matches the pattern
                    Col = str_pad(b[4], 2, pad = "0")    # add leading zeros
                    Row = str_pad(b[5], 2, pad = "0")
                    chid = paste0(b[2], b[3], '-', Col, '-', Row)
                  }                   
                }
                
              }
            print(paste('Chip=', chid, 'i=', i, 'ii=', ii, 'ixc=', paste(ixc, collapse=" ")))
            

            mode = NA
            ii = NA
            if (length(ixp) > 0) { 
              
              # # find Mode
              ii = tail(ixp[ixp < i], 1)   
              if (length(ii) > 0) { 
                
                mode = data[ii+1, 1]
                #mode = as.numeric(mode)                 
              }
            }            

            print(paste('Mode=', mode, 'i=', i, 'ii=', ii, 'ixp=', paste(ixp, collapse=" ")))


            # find data table between the header and the first non-numeric row
            ii = which.nonnum(data[, 1] )
            ii = head(ii[ii > i], 1)   # end of the table

            if (length(ii) == 0) {ii = nrow(data)+1}
            
            if (ii - i < 2) {
              print(paste("Skipped empty table: ",  chid, " | ", i) )
              next
            }

            # extract the table from the data
            tmp = data[(i):(ii-1), ]
            tmp = apply(tmp, 2, function(x) gsub("^$|^ $", NA, x))   # replace " " to NA
            tmp <- tmp[, which(!is.na(tmp[1,]))]                     # drop NA in captions row
            tmp = data.frame(tmp, stringsAsFactors = FALSE)

            # remove rows having NA elements
            ix =  which(rowSums(is.na( tmp )) == 0)
            k = nrow(tmp) - length(ix)
            tmp = tmp[ix, ]
            
            print(paste("Imported rows: ", nrow(tmp), " | ", chid, " | ", i, " | skipped rows: ", k) )
            

            # standardize headers name
            ss = gsub("\\s*\\([^\\)]+\\)","", head(tmp, 1))
            ss = gsub(" ", "_", ss )
            colnames(tmp) = ss
            tmp = tmp[-1, ]

            tmp$Chip_raw <- s0
            tmp$Chip_id <- chid
            tmp$Mode <- mode

            tmp$Header_line <- i
            tmp$FileName <- f_name
            
            

            
            # Power in dB
            tmp$Power = as.numeric(tmp$Power)
            tmp$Power_db = 20*log10(( 1 - 0.999* min(tmp$Power) / tmp$Power   ))
            #tmp$Power_db = 20*log10(( tmp$Power / min(tmp$Power)  ))
            
            
            # area under Power line
            tmp$Timestamp = as.numeric(tmp$Timestamp)
            x = tmp$Timestamp
            y = tmp$Power
            sP = sum(diff(x) * (head(y,-1) + tail(y,-1)))/2                
            tmp$Parea <- sP

            # resonant frequency
            ix = which(tmp$Power==min(tmp$Power))
            Fr = as.numeric(tmp[ix[1], ]['Freq'])
            tmp$Fr <- round(Fr, 3)   
            
            tmp$Contr_id <- id
            
            contr_files <- rbind( contr_files, c(s0, chid, mode, f_name, nrow(tmp), id) )

            id = id + 1
            
                        
            out = rbind(out, tmp[cols])
          }


        }

        if (nrow(out) == 0) {
          df_error = data.frame(c('No available csv files!'))
          colnames(df_error) = 'Error'
          
          s = paste0('No data available csv files!')
          infomessage('Error', s)
          return()
        }
        
        incProgress(0.5, detail = '')
        setProgress(message = 'Controller data preparing')
        
        out$Freq = as.numeric(out$Freq)
        out$Vpp = as.numeric(out$Vpp)
        out$Temp = as.numeric(out$Temp)
  

        T0 = 22       #(VNA measurement Temp)
        TCF75 = -75   #[ppm/C] (lower bound)
        TCF90 = -90   #[ppm/C7] (upper bound)
        
        out$fTcf75 = TCF75  * (out$Temp-T0)/1e6 + 1
        out$fTcf90 = TCF90  * (out$Temp-T0)/1e6 + 1    
        
        
        contr_files = data.frame(contr_files)
        colnames(contr_files) = c("Chip_raw", "Chip_id", "Mode", "FileName", "Points", "Contr_id" )
        
        contr_files$Points = as.numeric(as.character(contr_files$Points))
        

        out = out[, c( "Timestamp", "Freq", "Power", "Power_db", "Vpp", "Temp",   
                       #"Flood_Param",  "Fluid_State", #"Fixed_Current",     
                       "Parea", "Fr", "fTcf75", "fTcf90", "Contr_id" )]
        

       
      })
      
      #============== START VNA =============

      withProgress({

        incProgress(0.5, detail = '')
        setProgress(message = 'VNA files analyzing')
        
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
  
      })
      
      
      if (length(f_list) == 0) {
        df_error = data.frame(c('No available csv files!'))
        colnames(df_error) = 'Error'

        s = paste0('No available csv files!')
        infomessage('Error', s)
        return()
      }
  
        
        
      withProgress({
          
        #--------- Analyze file names
  
        # parse Chip ID and row/col from file name
        pattern <- "([A-Z]{2})([0-9]{3})[- ]?([0-9]{1,2})[-]([0-9]{1,2})[-]"
  
        fn = f_list[1]
        tmp = c()
        for (fn in unique(f_list) ) {
          s0 = tail(unlist(strsplit(fn, "/")), 1)  # split by slash '/'
          s = unlist(strsplit(s0, "[(]"))[1]  # ignore all after bracket "("
          s = gsub(" ", "-", s, fixed = TRUE)
          s = gsub(".", "-", s, fixed = TRUE)
  
          b = str_match(s, pattern)
          
          if ( !is.na(b[1]) ) {  # if file name matches the pattern
            Col = str_pad(b[4], 2, pad = "0")    # add leading zeros
            Row = str_pad(b[5], 2, pad = "0")
            chip_id = paste0(b[2], b[3], '-', Col, '-', Row)
            r = c(fn, s0, chip_id, Col, Row)
          } else {
            r = c(fn, s0, NA, NA, NA)
          }
          tmp = rbind(tmp, r)
        }
        tmp = as.data.frame(tmp)
        colnames(tmp) = c('W_fname', 'Fname', 'Chip_id', 'Col', 'Row')
  
        tmp['Vna_id'] = seq(1, nrow(tmp))
  
        vna_files = tmp[, c('W_fname', 'Chip_id', 'Vna_id')]
  
  
  
        # standard chip_id only
        tmp = contr_files[which(!is.na(contr_files$Chip_id)), ]
        
        # vna chips presented in controller data
        vna_files = vna_files %>% filter(Chip_id %in% tmp$Chip_id)
        
  
      })
      
      #------------------ VNA data
      

      out2 = data.frame()

      withProgress({
      
        vna_id = 295
        vna_list =  unique(vna_files$Vna_id)  
        for (vna_id in vna_list) {
          
          ix = which(vna_files$Vna_id == vna_id)
          f_name =  as.character(vna_files[ix[1], 1]) 
          f_name = gsub("/", "\\\\", f_name, fixed = TRUE)
  
          incProgress(1/length(vna_list), detail = f_name)
          setProgress(message = 'VNA data loading')
          
          print('......................................................')
          
          print(f_name)
          # try commas as separators
          data  <- read_delim(paste0(vna_folder, "\\\\", f_name ), ',', skip=4, col_types = cols())
          
          if (ncol(data) < 2) {  # if commas do not work, try semicolons
            data  <- read_delim(paste0(vna_folder, "\\\\", f_name ), ';', skip=4, col_types = cols())
          }
          if (ncol(data) < 2) next
          
          # exclude columns containing non-numeric data
          tmp = apply(data, MARGIN = c(1,2), 'as.logical')
          tmp = apply(tmp, MARGIN = c(1,2), 'as.integer')
          tmp = as.data.frame(apply(tmp, 2, prod))
          
          ix = which(tmp[,1] ==1)
          #rownames(tmp)[ix]
          data  <- data[ rownames(tmp)[ix] ]
          if (ncol(data) < 2) next

          # adjust target columns (add new or remove columns)
          cols = c('Freq', 'S', 'Z', 'R', 'X') 

          colnames(data) = cols
          
          data['Vna_id'] = vna_id
          data['Freq'] =  data$Freq / 1e6
          
          data['Smin'] = data$S==min(data$S)
          ix = which(data$S==min(data$S))
          data['Fr'] = data[ix[1], 1] 
          
          data['Points'] = nrow(data) 
          
          out2 = rbind(out2, data[c(cols, c('Fr', 'Points', 'Vna_id'))])
              
        }
      })
      
      # link Points  to VNA file list
      if (nrow(out2) > 0) {
        tmp = unique(out2[, c('Vna_id', 'Points')]) 
        vna_files = merge(x = vna_files, y = tmp, by = "Vna_id", all.x = TRUE)     # left join
        vna_files$Points = as.numeric(vna_files$Points)
        
        vna_files = vna_files[, c('Chip_id', 'W_fname', 'Points', 'Vna_id')]
        
      }
      

      
      
      
      return ( list('out' = out, 
                    'contr_files' = contr_files,
                    'vna_files' = vna_files,  'out2' = out2) )

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
    vna_data <<- res$out2
    
    #rea$chip_list <- chip_list
    rea$contr_files <- contr_files
    rea$vna_files <- vna_files
    
    x <- sort(as.vector(unique(res$contr_files$Chip_id)))
    x = choices = c('All', x)
    updatePickerInput(session, "chip_input",  choices = x   )
    

    if (is_save) {
      withProgress({
  
        incProgress(0.3, detail = '')
        setProgress(message = 'Controller data saving')
  
        SaveData(contr_data, w_folder, 'contr')
        
        
        incProgress(0.7, detail = '')
        setProgress(message = 'VNA data saving')
        
        SaveData(vna_data, w_folder, 'vna')
      })
    }

  })

  
  observeEvent(input$chip_input, {
    
    if (is.null(input$chip_input)) return()
    
    if (exists('contr_files')) {
      
      if (input$chip_input == 'All') {
        tmp = contr_files
        tmp1 = vna_files
      } else {
        tmp = contr_files %>% filter(Chip_id %in% input$chip_input)
        tmp1 = vna_files %>% filter(Chip_id %in% input$chip_input)
      }
      
      rea$contr_files = tmp
      rea$vna_files = tmp1
      
      

    }
  }, ignoreNULL = F)  
  
  
  
  

  
  
  #-------------------- Controller interface ---------------
  
  output$contr_table <- DT::renderDataTable({

      if (length(rea$contr_files) > 0) {

      tmp = rea$contr_files
      DT::datatable(tmp, rownames = FALSE,  class="compact",
                    caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:130% ;','Table1: Controller datasets (chips found in controller csv files)'),
                    selection = list(mode = 'single', selected = 1),
                    options = list(lengthMenu = c(10, 50, 100, 500),  pageLength = 7,  dom = 'Brtip',  deferRender = TRUE,
                                   scroller = TRUE,
                                   autoWidth = TRUE ))   %>%
                   formatStyle(columns = colnames(.), fontSize = '10%')
    }

  })



  observeEvent(input$contr_table_rows_selected, {


    if (nrow(rea$contr_files) > 0) {
      i = input$contr_table_rows_selected
      if (length(i) == 0) { i = 1 }

      tmp <- rea$contr_files[i, ]
      rea$chip_c <- tmp$Chip_id
      rea$mode <- tmp$Mode
      rea$contr_id <- tmp$Contr_id
      
      rea$ds_contr <- contr_data %>% filter(Contr_id == rea$contr_id) %>% arrange(Timestamp) 
      rea$p_area = round(as.numeric(head(rea$ds_contr$Parea, 1)), 2)
      rea$fr_c = round(as.numeric(head(rea$ds_contr$Fr, 1)), 3)     

      rea$match = F
      if ( ( !is.blank(rea$chip_c)  ) & ( !is.blank(rea$chip_v) ) ) {
        rea$match <- ( rea$chip_v == rea$chip_c) 
      }   
      
            
      print( paste('contr_id=', rea$contr_id) )      
      print( paste('chip_c=', rea$chip_c) )
      print( paste('mode=', rea$mode) )
    }
  })



  output$plot_tcf <- renderPlotly({

    ds = rea$ds_contr
    #ds1 = rea$ds_vna

    if (is.null(ds) )  return()
    if (nrow(ds) == 0) return()



    xx = ds$Timestamp

    p <- plot_ly()    %>%
      add_trace(data = ds, x = xx, y = ~Freq,
                type = 'scatter', name = 'Freq', mode = 'markers') %>%
      add_trace(data = ds, x = xx, y = ~Temp, name = 'Temp', yaxis = "y2", type = 'scatter', mode = 'markers') %>%


      layout(yaxis2 = list(title = "Temp", overlaying = "y", side = "right", automargin = T))



        if (rea$match) {

            Fr1 = rea$fr_v    #round(head(ds1$Fr, 1), 3)

            b75 = ds$fTcf75 * Fr1
            b90 = ds$fTcf90 * Fr1

            p <- p %>%
              add_trace(data = ds, x = xx, y = b75, name = 'fTcf75', yaxis = "y",
                      type = 'scatter', mode = 'lines', line = list(dash = "dot", color = "darkseagreen")) %>%
              add_trace(data = ds, x = xx, y = b90, name = 'fTcf90', yaxis = "y",
                        type = 'scatter', mode = 'lines',
                        fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
                        line = list(dash = "dot", color = "darkseagreen"))


        }


    p <- p %>%
      layout(yaxis = list(title = "Freq"),  xaxis = list(title = "Time, s"),
             title = paste0('Controller vs. VNA/TCF  theory: ', ' Contr_id = ', rea$contr_id, '; Vna_id = ', rea$vna_id),
             legend = list(orientation = "h", xanchor = "center", x = 0.5)
             ) %>%


      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    p

  })

  output$plot_power <- renderPlotly({


    ds = rea$ds_contr

    if (is.null(ds) )  return()
    if (nrow(ds) == 0) return()
    

    xx = ds$Timestamp

    p <- plot_ly()    %>%
      add_trace(data = ds, x = xx, y = ~Power,
                type = 'scatter', name = 'Power', mode = 'markers') %>%
      add_trace(data = ds, x = xx, y = ~Vpp, name = 'Vpp', yaxis = "y2", type = 'scatter', mode = 'markers') %>%
      layout(yaxis2 = list(title = "Vpp", overlaying = "y", side = "right", automargin = T))



    p <- p %>%
      layout(yaxis = list(title = "Power"),  xaxis = list(title = "Time, s"),
             title = paste0('Power Savings and Peak Voltage Reduction ( P_area = ', rea$p_area, '): ', ' Contr_id = ', rea$contr_id),
             legend = list(orientation = "v", xanchor = "center", x = 0.5)
      ) %>%


      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    p

  })
  
  
  #----------------------------- VNA interface --------------------------  
  
  
  output$vna_table <- DT::renderDataTable({

    if (length(rea$vna_files) > 0) {

      tmp = rea$vna_files
      DT::datatable(tmp, rownames = FALSE,  class="compact",
                    caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:130% ;','Table2: VNA datasets (VNA chips presented in controller data only)'),
                    selection = list(mode = 'single', selected = 1),
                    options = list(lengthMenu = c(10, 50, 100, 500),  pageLength = 7,  dom = 'Brtip',  deferRender = TRUE,
                                   scroller = TRUE
                    ))   %>% formatStyle(columns = colnames(.), fontSize = '20%')
    }

  })



  observeEvent(input$vna_table_rows_selected, {

    if (nrow(rea$vna_files) > 0) {
      i = input$vna_table_rows_selected
      if (length(i) == 0) { i = 1 }

      tmp <- rea$vna_files[i, ]
      rea$chip_v <- tmp$Chip_id
      rea$vna_id <- tmp$Vna_id
      
      rea$ds_vna <- vna_data %>% filter(Vna_id == rea$vna_id) %>% arrange(Freq) 
      rea$fr_v = round(as.numeric(head(rea$ds_vna$Fr, 1)), 3)      

      rea$match = F
      if ( ( !is.blank(rea$chip_c)  ) & ( !is.blank(rea$chip_v) ) ) {
        rea$match <- ( rea$chip_v == rea$chip_c) 
      }   
      
      print( paste('vna_id=', rea$vna_id) )      
      print( paste('chip_v=', rea$chip_v) )
      
      
    }
  })



  output$plot_freq <- renderPlotly({

    ys = input$contr_input
    ys1 = input$vna_input
    
    ds = rea$ds_contr
    
    if (is.null(ys) )  return()   
    if (is.null(ys1) )  return()
    if (is.null(ds) )  return()
    if (nrow(ds) == 0) return()

    ds <- ds %>% arrange(Freq)

    yd = ds[[ys]]
    
    
    offset = 1.0
    mg = 0.005

    p <- plot_ly()    %>%
      add_trace(data = ds, x = ~Freq, y = yd,
                type = 'scatter', name = ys, mode = 'markers') %>%

    add_trace(x = rea$fr_c * (1 - mg),
              y =  max(yd)+offset, 
              mode = 'text',  text = paste0('', rea$fr_c ),
              type = 'scatter', showlegend = F, yaxis = "y",
              textfont = list(color = 'cadetblue', size = 12)
    )

    lines =  list(type='line', x0 = rea$fr_c, x1 = rea$fr_c, y0=min(yd), y1=max(yd)*1.1, yref = "y",
                  line=list(dash='dot', width=3, color = 'lightblue',  name = 'Fr Contr'))



        if (rea$match) {

            ds1 = rea$ds_vna

            yd1 = ds1[[ys1]]
            
            p <- p %>%

            add_trace(data = ds1, x = ~Freq, y = yd1, name = ys1, yaxis = "y2", color = 'orange', type = 'scatter', mode = 'markers') %>%
              layout(yaxis2 = list(title = ys1, overlaying = "y", side = "right", automargin = T))    %>%



            add_trace(x = rea$fr_v * (1 + mg),
                        y = max(yd1)+offset,
                        mode = 'text',  text = paste0('', rea$fr_v ),
                        type = 'scatter', showlegend = FALSE, yaxis = "y2",
                        textfont = list(color = 'orange', size = 12)
             )


            lines = list(  lines,
                           list(type='line', x0 = rea$fr_v, x1 = rea$fr_v, y0=min(yd1), y1=max(yd1), yref = "y2",
                                line=list(dash='dot', width=3, color = 'lightpink',  name = 'Fr VNA'))
                        )


          }



    p <- p %>%
      layout(yaxis = list(title = ys),  xaxis = list(title = "Freq, MHz"),
             title = paste('Controller vs VNA data comparison: ', ' Contr_id = ', rea$contr_id, '; Vna_id = ', rea$vna_id),
             legend = list(orientation = "h", xanchor = "center", x = 0.8),
             shapes = lines) %>%


      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("select2d", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    p

  })
  
  
  
  output$warning_text = renderText({
    
    if ( ( is.blank(rea$chip_c)  ) & ( is.blank(rea$chip_v) ) ) return()
    
    if (!rea$match)  {
      ('Selected chips do not match! ')
    } else {
      ('')
    }
  })    
  
  
  
  
  
  session$onSessionEnded(function() {
    
    stopApp()   # close command window
  })    
  

  
}


shinyApp(ui = ui, server = server)



