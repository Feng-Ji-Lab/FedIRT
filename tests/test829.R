
library(mirt)

data(LSAT6)

responses <- as.matrix(LSAT6[, 1:5])
freq <- LSAT6$Freq

expanded_data <- matrix(NA, nrow=sum(freq), ncol=ncol(responses))
current_index <- 1

for (i in 1:nrow(responses)) {
  expanded_data[current_index:(current_index + freq[i] - 1), ] <- matrix(rep(responses[i, ], freq[i]), ncol=ncol(responses), byrow=TRUE)
  current_index <- current_index + freq[i]
}

expanded_list <- list(expanded_data)
colnames(expanded_list[[1]]) <- c("X1", "X2", "X3", "X4", "X5")
expanded_list[[1]]

data = read.csv("C:\\Users\\zby15\\Downloads\\r\\testdata\\dataset1.csv")
# call the fedirt_file function
result <- fedirt_file(data, model_name = "2PL")
SE(result)

fedresult1 = fedirt_file(data)
print(fedresult1$SE)

data1 <- data[, !names(data) %in% c("site")]
mod <- mirt(data1, 1, itemtype = '2PL', SE = TRUE)

coef(mod, printSE=TRUE)
