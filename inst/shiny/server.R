
library(shiny)
library(httr)
library(purrr)
library(pracma)
library(ggplot2)
library(shinyjs)
library(DT)
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
    # 根据答案 data 选对应的概率
    # 原来： N : 21 = N:10 * 10:21
    # 现在： N : 21 = 10 * (N:1 select 3:21)
    answerP = log(Pjx(a[j], b[[j]]))
    # 初始化一个全0的矩阵，矩阵尺寸为N行和M[j]列
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
    # 根据答案 data 选对应的概率
    # 原来： N : 21 = N:10 * 10:21
    # 现在： N : 21 = 10 * (N:1 select 3:21)
    answerP = log(Pjx(a[j], b[[j]]))
    # 初始化一个全0的矩阵，矩阵尺寸为N行和M[j]列
    result_matrix <- matrix(0, nrow = N, ncol = M[j] + 1)
    result_matrix[cbind(seq_len(N), data[,j] + 1)] = 1
    selected = result_matrix %*% answerP
    return(selected)
  })

  Lik_j = mem(function(a, b, j) {
    exp(log_Lik_j(a,b,j))
  })
  # zby 标注尺寸
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
          status = 400L,
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
      fedresult <<- fedirt(J, logL_entry,g_logL_entry)
    } else {
      fedresult <<- fedirt_gpcm(J,M, logL_entry,g_logL_entry)
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
