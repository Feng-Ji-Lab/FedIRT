
library(shiny)
library(httr)
library(purrr)
library(pracma)
library(callr)
library(DT)
library(ggplot2)
fedirt = function(J,logL_entry, g_logL_entry) {
  get_new_ps = function(ps_old) {
    # "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"
    optim(par = ps_old, fn = logL_entry, gr = g_logL_entry, method = "BFGS", control = list(fnscale=-1, trace = 0,  maxit = 10000))
  }
  ps_init = c(rep(1, J), rep(0, J))
  ps_next = get_new_ps(ps_init)
  ps_next$loglik = logL_entry(ps_next$par)

  ps_next$b = ps_next$par[(J+1):(J+J)]
  ps_next$a = ps_next$par[1:J]

  ps_next
}

broadcast.fortmat <- function(mat1, mat2) {
  row1 <- nrow(mat1)
  col1 <- ncol(mat1)
  row2 <- nrow(mat2)
  col2 <- ncol(mat2)
  if(col1 != 1 && row2 != 1) {
    stop("illegal operation: not 1")
  }
  if(col1 == 1) {
    mat1_new <- mat1[, rep(1:col1, col2)]
  } else if(col1 != col2) {
    stop("illegal operation: col1")
  } else {
    mat1_new <- mat1
  }
  if(row2 == 1) {
    mat2_new <- mat2[rep(1:row2, each=row1), ]
  } else if(row2 != row1) {
    stop("illegal operation: row2")
  } else {
    mat2_new <- mat2
  }

  list(mat1_new, mat2_new)
}

broadcast.multiplication <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new * mat2_new)
}
broadcast.divide <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new / mat2_new)
}
broadcast.subtraction <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new - mat2_new)
}
broadcast.exponentiation <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new ^ mat2_new)
}

mem <- function(f) {
  memo <- new.env(parent = emptyenv())
  function(...) {
    key <- paste(list(...), collapse = " ,")
    if(!exists(as.character(key), envir = memo)) {
      memo[[as.character(key)]] <- f(...)
    }
    memo[[as.character(key)]]
  }
}
g = function(x) {
  return (exp(-0.5 * x * x) / sqrt(2 * pi))
}

logL = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  sum(log(matrix(apply(broadcast.multiplication(Lik(a, b), t(A)), c(1), sum))))
}

g_logL = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
  })
  Pxy = mem(function(a, b) {
    la = LA(a,b)
    sum_la = replicate(q, apply(la, c(1), sum))
    la / sum_la
  })
  Pxyr = mem(function(a, b) {
    aperm(replicate(J, Pxy(a,b)), c(1, 3, 2)) * replicate(q, data)
  })

  njk = mem(function(a, b) {
    pxy = Pxy(a, b)
    matrix(apply(pxy, c(2), sum))
  })
  rjk = mem(function(a, b) {
    pxyr = Pxyr(a, b)
    apply(pxyr, c(2, 3), sum)
  })
  da = mem(function(a, b) {
    matrix(apply(-1 * broadcast.subtraction(b, t(X)) * (rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })
  db = mem(function(a, b) {
    -1 * a * matrix(apply((rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })

  result_a = da(a, b)
  result_b = db(a, b)
  list(result_a, result_b)
}

my_personfit = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
  })
  result = list()
  ta = matrix(a, J, 1)
  tb = matrix(b, J, 1)
  result[["ability"]] = matrix(apply(broadcast.multiplication(LA(ta,tb), t(X)), c(1), sum)) / matrix(apply(LA(ta,tb), c(1), sum))

  result[["site"]] = mean(result[["ability"]])

  result[["person"]] = result[["ability"]] - result[["site"]]
  return(result)
}
fedirt_gpcm = function(J, M,logL_entry, g_logL_entry) {
  get_new_ps = function(ps_old) {
    # "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"
    optim(par = ps_old, fn = logL_entry, method = "BFGS", control = list(fnscale=-1, trace = 0,  maxit = 10000))
  }
  ps_init = c(rep(1, J), rep(0, sum(M)))
  # print("fedirt_gpcm 2::")
  # print(M)
  # print(J)
  # print(sum(M))
  # print(ps_init)
  ps_next = get_new_ps(ps_init)
  ps_next$loglik = logL_entry(ps_next$par)

  ps_next$b = ps_next$par[(J+1):(J+sum(M))]
  ps_next$a = ps_next$par[1:J]

  ps_next
}

logL_gpcm = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  M <- apply(data, 2, function(df) {
    max(df)
  })
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Px = mem(function(a, b) {
    - rbind(rep(0, length(X)), a * broadcast.subtraction(t(b), t(X)))
  })
  Px_sum = mem(function(a, b) {
    exp(apply(Px(a,b),2,cumsum))
  })

  Pjx = mem(function(a, b, j) {
    px_sum = Px_sum(a,b)
    sum_px_sum = matrix(colSums(px_sum), nrow = 1)
    return(broadcast.divide(px_sum, sum_px_sum))
  })
  log_Lik_j = mem(function(a, b, j) {
    answerP = log(Pjx(a[j], b[[j]]))

    result_matrix <- matrix(0, nrow = N, ncol = M[j] + 1)
    result_matrix[cbind(seq_len(N), data[,j] + 1)] = 1
    selected = result_matrix %*% answerP
    return(selected)
  })

  Lik_j = mem(function(a, b, j) {
    exp(log_Lik_j(a,b,j))
  })

  finalLogL = 0
  for(j in 1:J) {
    temp = log_Lik_j(a, b, j)
    finalLogL = finalLogL + temp
  }
  sum(log(matrix(apply(broadcast.multiplication(exp(finalLogL), t(A)), c(1), sum))))
}

g_logL_gpcm = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  M <- apply(data, 2, function(df) {
    max(df)
  })
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Px = mem(function(a, b) {
    - rbind(rep(0, length(X)), a * broadcast.subtraction(t(b), t(X)))
  })
  Px_sum = mem(function(a, b) {
    exp(apply(Px(a,b),2,cumsum))
  })

  Pjx = mem(function(a, b, j) {
    # 提供所有答案的概率:  4:21
    px_sum = Px_sum(a,b)
    sum_px_sum = matrix(colSums(px_sum), nrow = 1)
    # if(j==1) {
    #   ans = broadcast.divide(px_sum, sum_px_sum)
    #   # print(ans)
    # }
    return(broadcast.divide(px_sum, sum_px_sum))
  })
  log_Lik_j = mem(function(a, b, j) {

    answerP = log(Pjx(a[j], b[[j]]))

    result_matrix <- matrix(0, nrow = N, ncol = M[j] + 1)
    result_matrix[cbind(seq_len(N), data[,j] + 1)] = 1
    selected = result_matrix %*% answerP
    return(selected)
  })

  Lik_j = mem(function(a, b, j) {
    exp(log_Lik_j(a,b,j))
  })
  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
    # 79 * 21
  })
  Pxy = mem(function(a, b) {
    la = LA(a,b) # 79 * 21
    sum_la = replicate(q, apply(la, c(1), sum)) # 79 * 21
    la / sum_la # 79 * 21
  })
  Pxyr = mem(function(a, b) {
    aperm(replicate(J, Pxy(a,b)), c(1, 3, 2)) * replicate(q, data) # 10 * 79 * 21
  })

  njk = mem(function(a, b) {
    pxy = Pxy(a, b)
    matrix(apply(pxy, c(2), sum)) # 21 * 1
  })
  rjk = mem(function(a, b) {
    pxyr = Pxyr(a, b)
    apply(pxyr, c(2, 3), sum) # 10 * 21
  })
  da = mem(function(a, b) {
    matrix(apply(-1 * broadcast.subtraction(b, t(X)) * (rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })
  db = mem(function(a, b) {
    -1 * a * matrix(apply((rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })

  result_a = da(a, b)
  result_b = db(a, b)
  list(result_a, result_b)
}

my_personfit_gpcm = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
  })
  result = list()
  ta = matrix(a, J, 1)
  tb = matrix(b, J, 1)
  result[["ability"]] = matrix(apply(broadcast.multiplication(LA(ta,tb), t(X)), c(1), sum)) / matrix(apply(LA(ta,tb), c(1), sum))

  result[["site"]] = mean(result[["ability"]])

  result[["person"]] = result[["ability"]] - result[["site"]]
  return(result)
}


use_graded_mode = FALSE

ui <- function(req) {
  if (identical(req$REQUEST_METHOD, "GET")) {
    fluidPage(
      titlePanel("Federated IRT - client"),
      verbatimTextOutput("ipInfo"),
      textInput("serverInputIP","Input Server IP:Port", "127.0.0.1:8000"),
      actionButton("reconnect", "Reconnect to Server"),
      verbatimTextOutput("info"),
      fileInput("file", "Choose CSV File: responding matrix",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("receiveresult", "receive result"),
      DT::dataTableOutput("result"),
      DT::dataTableOutput("ability"),
      # tableOutput("result"),
      # tableOutput("ability"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3"),
    )
  } else if (identical(req$REQUEST_METHOD, "POST")) {
    # Handle the POST
    query_params <- parseQueryString(req$QUERY_STRING)
    body_bytes <- req$rook.input$read(-1)
    if(req$PATH_INFO == "/logL"){
      response <- jsonlite::fromJSON(rawToChar(body_bytes))
      print(response)
      ps <<- as.numeric(unlist(response[['ps']]))
      use_graded_mode <<- response[['use_graded_mode']]
      if(!use_graded_mode) {
        a = matrix(ps[1:J])
        b = matrix(ps[(J+1):(2*J)])
        # print(a)
        # print(b)
        result = logL(a,b,localdata)
        # print(result)

        httpResponse(
          status = 200L,
          content_type = "application/json",
          content = jsonlite::toJSON(result)
        )
      } else {
        a = matrix(ps[1:J])
        totalb = as.matrix(ps[(J+1):(J+sum(M))])
        listb = split(totalb, findInterval(seq_along(totalb), c(0, cumsum(M)), left.open = TRUE))
        # b = matrix(ps[(J+1):(2*J)])
        b = lapply(listb, function(vec) {
          matrix(vec, nrow = 1, byrow = TRUE) # byrow = TRUE 使得向量按行填充矩阵
        })
        result = logL_gpcm(a,b,localdata)
        print(result)
        httpResponse(
          status = 200L,
          content_type = "application/json",
          content = jsonlite::toJSON(result)
        )
      }
    }
    else if(req$PATH_INFO == "/g_logL"){
      response <- jsonlite::fromJSON(rawToChar(body_bytes))
      print(response)
      ps <<- as.numeric(unlist(response[['ps']]))
      use_graded_mode <<- response[['use_graded_mode']]
      ps = as.numeric(unlist(ps))
      if(!use_graded_mode) {
        a = matrix(ps[1:J])
        b = matrix(ps[(J+1):(2*J)])
        # print(a)
        # print(b)
        result = g_logL(a,b,localdata)
        result = as.numeric(unlist(result))

        httpResponse(
          status = 200L,
          content_type = "application/json",
          content = jsonlite::toJSON(result)
        )
      } else {

      }
      # print(result)

    } else if(req$PATH_INFO == "/fedresult"){
      port <- jsonlite::fromJSON(rawToChar(body_bytes))
      fedresult <<- port$fedresult
      # print(fedresult)
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
serveIP <<- "127.0.0.1:8000"
currentIP <<- ""
doOnceConnect <-function(session) {
  port <- session$clientData$url_port
  # print(session$clientData)
  # print(session$clientData$url_pathname)
  url_hostname <- session$clientData$url_hostname
  res <- POST(
    paste0("http://", serveIP,"/connect"),
    body = list(ip = currentIP, port = port),
    encode = "json"
  )
  result <- content(res, "parsed")[[1]]
  paste0("Conncted to Server: ", serveIP)
}

getLocalIP <- function() {
  if (sys_name == "Windows") {
    cmd <- 'for /f "tokens=2 delims=:" %a in (\'ipconfig ^| findstr /C:"IPv4"\') do @echo %a'
    result <- shell(cmd, intern = TRUE)
  } else {
    cmd <- "ip addr show | grep 'inet ' | grep -v '127.0.0.1' | awk '{print $2}' | cut -d/ -f1"
    result <- system(cmd, intern = TRUE)
  }
  ip_addresses <- gsub(" ", "", result)
  ip_addresses <- ip_addresses[ip_addresses != ""]
  print(ip_addresses[[1]])
  ip_addresses[[1]]
}
server <- function(input, output, session) {
  observe({
    file <- input$file
    if(!is.null(file)){
      data <- read.csv(file$datapath, header = FALSE)
      data = as.matrix(data)
      localdata <<- data
      J <<- dim(data)[2]
      M <<- apply(data, 2, function(df) {
        max(df)
      })
      print(M)
      res <- POST(
        paste0("http://", serveIP,"/school_info"),
        body = list(J=J, M=M),
        encode = "json"
      )
    }
  })

  output$ipInfo <- renderText({
    currentIP <<- getLocalIP()

    output$info <- renderText({
      doOnceConnect(session)
    })
    paste0("Current IP in using: ", currentIP, ":", session$clientData$url_port)
  })
  observeEvent(input$send, {
    res <- POST(
      paste0("http://", serveIP, "/school_info"),
      body = list(J=J),
      encode = "json"
    )
  })

  observeEvent(input$reconnect, {
    print("reconnect")
    print(input$serverInputIP)
    serveIP <<- input$serverInputIP
    output$info <- renderText({
      doOnceConnect(session)
    })
  })
  observeEvent(input$receiveresult, {
    req(fedresult)
    # print(fedresult)
    discrimination = fedresult$a
    difficulty = fedresult$b
    fedresult[['person']] <<- my_personfit(fedresult[["a"]], fedresult[["b"]], localdata)[['ability']]
    # print(discrimination)
    # print(difficulty)

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
    print(difficulty_cols)

    # 转换临时列表为数据框的列
    for (i in 1:max_M) {
      # 将列表元素加入到数据框中，并给予适当的列名
      df[[paste0("Difficulty_", i)]] <- difficulty_cols[[i]]
    }

    # 查看data.frame结果
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
    if(!use_graded_mode) {

      output$plot1 <- renderPlot({
        ggplot(data.frame("Discrimination" = discrimination,
                          "Index" = seq_along(discrimination)), aes(x = Index, y = Discrimination)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(x = "Item Index", y = "Discrimination", title = "Bar Plot of Discrimination")
      })

      output$plot2 <- renderPlot({
        ggplot(data.frame("Difficulty" = difficulty,
                          "Index" = seq_along(difficulty)), aes(x = Index, y = Difficulty)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(x = "Item Index", y = "Difficulty", title = "Bar Plot of Difficulty")
      })
      output$plot3 <- renderPlot({
        ggplot(data.frame("ability" = fedresult$person,
                          "Index" = seq_along(fedresult$person)), aes(x = Index, y = fedresult$person)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(x = "Student Index", y = "Ability", title = "Bar Plot of Ability")
      })
      output$ability <- DT::renderDataTable({
        DT::datatable(data.frame("Student_ID" = seq_along(fedresult$person),
                                 "Ability" = fedresult$person),
                      options = list(
                        columnDefs = list(
                          list(
                            targets = 2, # Only targets the "Ability" column (second column)
                            className = "dt-right",
                            render = DT::JS(
                              "function(data, type, full, meta) {
              if (type === 'display' || type === 'filter') {
                if (!isNaN(parseFloat(data)) && isFinite(data)) {
                  return parseFloat(data).toFixed(3); // Format numbers to three decimal places
                } else {
                  return data; // Return data as is for non-numeric fields
                }
              }
              return data;
            }"
                            )
                          ),
                          list(
                            targets = 1, # Targets the "Student ID" column, no changes needed
                            className = "dt-right"
                          )
                        )
                      )
        )
      })
    } else {

      output$plot1 <- renderPlot(NA)
      output$plot2 <- renderPlot(NA)
      output$plot3 <- renderPlot(NA)
    }
  })
}

options(shiny.host = "0.0.0.0")
shinyApp(ui, server, uiPattern = ".*")
