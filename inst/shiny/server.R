
library(shiny)
library(httr)
library(purrr)
library(pracma)
library(ggplot2)
library(shinyjs)
library(DT)



K <<- 0
ipport_list <<- {}
Jlist <<- {}
J <<- -1
Jnum <<- 0
school_list <<-{}
M <<- NULL
Mout <<- ""
updateM <- function(nM) {
  if(is.null(M)) {
    M <<- nM
  } else {
    if(M == nM) {
      return()
    } else {
      Mout <<- ", Error in M."
    }
  }
  if(max(M) > 1) {
    use_graded_mode <<- TRUE
  } else {
    use_graded_mode <<- FALSE
  }
  print(use_graded_mode)
  global$use_graded_mode = renderPrint({
    cat("Graded Mode:", use_graded_mode)
  })
}
check_J <- function(Jlist) {
  J1 <- unique(Jlist)
  if(length(J1) == 1) {
    J <<- J1
    1
  } else if(length(J1) == 0) {
    0
  }
  else{
    2
  }
}
ui <- function(req) {
  if (identical(req$REQUEST_METHOD, "GET")) {
    fluidPage(
      titlePanel("Federated IRT - server"),
      verbatimTextOutput("ipInfo"),
      uiOutput("ip"),
      uiOutput("all"),
      uiOutput("use_graded_mode"),
      actionButton("start", "start"),
      # uiOutput("result"),
      dataTableOutput("result"),
      plotOutput("plot1"),
      plotOutput("plot2"),
    )
  } else if (identical(req$REQUEST_METHOD, "POST")) {
    global$out = renderPrint({
      all_port = sapply(seq_along(ipport_list), function(idx) {
        paste0("\tIndex: ", idx, ", Address: ", ipport_list[idx])
      })
      if(Jnum == 1){
        Jout <<- paste0("\r\ndata verified, ",length(Jlist), " school uploaded data")
      } else if (Jnum == 2){
        Jout <<- "\r\nError: please check datasets"
      } else{
        Jout <<- "\r\nWait for data"
      }
      cat( K, " schools in connection.\r\n", paste(all_port, collapse = "\r\n"), Jout, Mout, collapse = "\n")

    })
    # Handle the POST
    query_params <- parseQueryString(req$QUERY_STRING)
    body_bytes <- req$rook.input$read(-1)
    if(req$PATH_INFO == "/connect"){
      portinfo <- jsonlite::fromJSON(rawToChar(body_bytes))
      port = as.numeric(unlist(portinfo[["port"]]))
      ip = unlist(portinfo[["ip"]])
      portip = paste0(ip, ":", port)
      if(ip != "") {
        if(!portip %in% ipport_list) {
          K <<- K + 1
          ipport_list <<- rbind(ipport_list, portip)
        }

        httpResponse(
          status = 200L,
          content_type = "application/json",
          content = '{"status": "ok"}'
        )
      } else{
        httpResponse(
          status = 404L,
          content_type = "application/json",
          content = '{"status": "not find ip"}'
        )
      }
    } else if(req$PATH_INFO == "/school_info"){
      response <- jsonlite::fromJSON(rawToChar(body_bytes))
      J = as.numeric(unlist(response[["J"]]))
      tM = as.numeric(unlist(response[["M"]]))
      # print(tM)
      updateM(tM)
      Jlist <<- rbind(Jlist,J)
      Jnum <<- check_J(Jlist)
      httpResponse(
        status = 200L,
        content_type = "application/json",
        content = '{"status": "ok"}'
      )
    }
    else {
      httpResponse(
        status = 200L,
        content_type = "application/json",
        content = '{"status": "ok"}'
      )
    }
  }
}
attr(ui, "http_methods_supported") <- c("GET", "POST")
getLocalIP <- function() {
  sys_name <- Sys.info()[["sysname"]]

  if (sys_name == "Windows") {
    cmd <- 'for /f "tokens=2 delims=:" %a in (\'ipconfig ^| findstr /C:"IPv4"\') do @echo %a'
    result <- shell(cmd, intern = TRUE)
  } else {
    cmd <- "ip addr show | grep 'inet ' | grep -v '127.0.0.1' | awk '{print $2}' | cut -d/ -f1"
    result <- system(cmd, intern = TRUE)
  }

  ip_addresses <- trimws(result)
  ip_addresses <- ip_addresses[ip_addresses != ""]
  print(ip_addresses[[1]])
  ip_addresses[[1]]
}

list_to_string <- function(l) {
  out <- c()
  for (name in names(l)) {
    value <- l[[name]]
    if (is.null(value)) {
      str_value <- "NULL"
    } else if (is.atomic(value) && !is.character(value)) {
      str_value <- paste(format(value), collapse = " ")
    } else if (is.character(value)) {
      str_value <- paste(value, collapse = " ")
    } else {
      str_value <- list_to_string(value)
    }
    out <- c(out, paste("$", name, "\n", str_value, sep = ""))
  }
  paste(out, collapse = "\n\n")
}

getlogL_from_index = function(ps,index){
  ipport = ipport_list[index]
  res <- POST(
    paste0("http://",ipport,"/logL"),
    body = list(ps = ps, use_graded_mode = use_graded_mode),
    encode = "json"
  )
  # print(ps)
  # print(index)
  # print(paste("getlogL_from_index", index))
  # print(res)
  result <- content(res, "parsed")[[1]]
  # print(result)
  return(result)
}
get_g_logL_from_index = function(ps,index){
  ipport = ipport_list[index]
  res <- POST(

    paste0("http://",ipport,"/g_logL"),
    body = list(ps = ps, use_graded_mode = use_graded_mode),
    encode = "json"
  )
  result <- content(res, "parsed")
  result = unlist(result)
  return(result)
}
global <<- reactiveValues()
use_graded_mode = FALSE
global$out = renderPrint({
  cat("Waiting connection")
})
global$use_graded_mode = renderPrint({
  cat("Graded Mode:", use_graded_mode)
})
server <- function(input, output, session) {
  output$ipInfo <- renderText({
    currentIP <<- getLocalIP()
    paste0("Current IP in using: ", currentIP, ":", session$clientData$url_port)
  })

  output$all <- renderUI({
    global$out
  })

  output$use_graded_mode <- renderUI({
    global$use_graded_mode
  })

  observeEvent(input$start, {
    #server
    logL_entry = function(ps) {
      # a = matrix(ps[1:J])
      # b = matrix(ps[(J+1):(2*J)])
      # print(paste0("logL_entry::", J))
      if(K==1){
        result = getlogL_from_index(ps,1)
      } else{
        result = 0
        for(index in 1:K) {
          result = result + as.numeric(getlogL_from_index(ps,index))
        }
      }

      print(result)
      result
    }
    g_logL_entry = function(ps) {
      a = matrix(ps[1:J])
      b = matrix(ps[(J+1):(2*J)])
      ga = matrix(0, nrow = J)
      gb = matrix(0, nrow = J)
      # print(ga)
      # print(gb)
      for(index in 1:K) {
        result = get_g_logL_from_index(ps, index)
        ga[, 1] = ga[, 1] + result[1:J]
        gb[, 1] = gb[, 1] + result[(J+1):(2*J)]
      }
      # print(rbind(ga,gb))
      rbind(ga, gb)
    }
    # print("start:: use_graded_mode")
    print(use_graded_mode)
    if(!use_graded_mode) {
      fedresult <<- fedirt_server(J, logL_entry,g_logL_entry)
    } else {
      fedresult <<- fedirt_gpcm_server(J,M, logL_entry,g_logL_entry)
    }
    print("fed finish")
    for(index in 1:K){
      res <- POST(
        paste0("http://",ipport_list[index],"/fedresult"),
        body = list(fedresult = fedresult),
        encode = "json"
      )
    }
    discrimination = fedresult$a
    difficulty = fedresult$b
    # print(discrimination)
    # print(difficulty)
    # print(M)

    # difficulty = array(c(-1.88028062,-1.35402076,-0.05113284,-0.58306557,-1.73361698,-2.40545207,-3.90100091,-0.86863416,-0.02684671,0.23615914))
    # discrimination = array(c(0.7518099,0.7122077,1.0925517,0.5176389,0.2559858,0.8420262,1.0672771,0.8573997))
    # M = array(c(3,1,1,1,1,1,1,1))

    # 初始化数据框
    df <- data.frame(discrimination)

    # 最大M值决定了我们将拥有多少列
    max_M <- max(M)
    difficulty_cols <- vector("list", max_M)

    # 对difficulty数组进行分割
    start_index <- 1
    for(j in 1:length(M)) {
      for (i in 1:max_M) {
        number_to_take = M[j]
        if(number_to_take >= i) {
          difficulty_cols[[i]][[j]] = difficulty[start_index]
          start_index = start_index + 1
        } else {
          difficulty_cols[[i]][[j]] = NA
        }
      }
    }
    # print(difficulty_cols)

    # 转换临时列表为数据框的列
    for (i in 1:max_M) {
      # 将列表元素加入到数据框中，并给予适当的列名
      df[[paste0("Difficulty_", i)]] <- difficulty_cols[[i]]
    }

    print(df)

    output$result <- DT::renderDataTable({
      DT::datatable(df, options = list(
        columnDefs = list(
          list(
            targets = "_all", # Targets all columns
            className = "dt-right",
            render = DT::JS(
              "function(data, type, full, meta) {
            if (meta.col === 0) { // If it is the auto ID column, do nothing
              return data;
            }
            if (type === 'display') {
              if (data === null || typeof data === 'undefined' || data === '') {
                return ''; // Return empty string for null or undefined data
              }
              if (!isNaN(data) && data !== '') {
                return parseFloat(data).toFixed(3); // Format the numbers
              }
            }
            return data; // Return unchanged data for non-numeric fields
          }"
            )
          )
        )
      ))
    })
  })
}

options(shiny.host = "0.0.0.0")
shinyApp(ui, server, uiPattern = ".*")
