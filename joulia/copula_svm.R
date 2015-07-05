library(copula)
library(e1071)
library(tseries)


num_parameters <- 100
num_examples <- 10
train_copulas <- list()
test_copulas <- list()

normCopRho <- list(seq(-0.9,-0.5,length=num_parameters), seq(-0.6,-0.1,length=num_parameters), seq(0.1,0.5,length=num_parameters), seq(0.6,0.9,length=num_parameters))
tCopRho <- list(seq(-0.9,-0.5,length=num_parameters), seq(-0.6,-0.1,length=num_parameters), seq(0.1,0.5,length=num_parameters), seq(0.6,0.9,length=num_parameters))
tCopDf <- list(2,5,10)
gumbelCopTheta <- list(seq(1,10, length=num_parameters), seq(11,100, length=num_parameters), seq(101,1000, length=num_parameters))
claytonCopTheta <- list(seq(-0.9,0.9, length=num_parameters), seq(1,10, length=num_parameters), seq(11,100, length=num_parameters), seq(101,1000, length=num_parameters))
frankCopTheta <- list(seq(-500,-100, length=num_parameters), seq(-101,-10, length=num_parameters), seq(10,100, length=100), seq(101,500, length=num_parameters))

#normCopRho <- list(rep(0.6, num_parameters))
#tCopRho <- list(rep(0.7, num_parameters))
#tCopDf <- list(2)
#gumbelCopTheta <- list(rep(2, num_parameters))
#claytonCopTheta <- list(rep(-0.5, num_parameters))
#frankCopTheta <- list(rep(10, num_parameters))

generate_copulas <- function(){

  #Gaussian copula
  for (rhoList in normCopRho)
  {
    x <- {}
    # generating pair of copulas for train data
    for (rho in rhoList)
    {
      norm.cop <- normalCopula(dim = 2, dispstr = "ex", param = rho)
      norm_copula <- rCopula(num_examples, norm.cop)
      norm_copula <- t(matrix(t(norm_copula)))
      x <- rbind (x, norm_copula)
    }
    train_copulas[[length(train_copulas)+1]] <- x
  }
  
  #t_Copula
  for (Df in tCopDf)
  {
    for (rhoList in tCopRho)
    {
        x <- {}
      for (rho in rhoList)
      { 
        t.cop <- tCopula (dim = 2, dispstr = "ex", param = rho, df = Df)
        t_copula <- rCopula(num_examples, t.cop)
        t_copula <- t(matrix(t(t_copula)))
        x <- rbind (x, t_copula)
      }
      train_copulas[[length(train_copulas)+1]] <- x
    }
  }
  
  #Gumbel copula
  for (thetaList in gumbelCopTheta)
  {
    x <- {}
    for (theta in thetaList)
    {
      gumbel.cop <- gumbelCopula(dim = 2, param = theta)
      gumbel_copula <- rCopula(num_examples, gumbel.cop)
      gumbel_copula <- t(matrix(t(gumbel_copula)))
      x <- rbind (x, gumbel_copula)
    }
    train_copulas[[length(train_copulas)+1]] <- x
  }
  
  #Clayton copula
  for (thetaList in claytonCopTheta)
  {
    x <- {}
    for (theta in thetaList)
    {
      clayton.cop <- claytonCopula(dim = 2, param = theta)
      clayton_copula <- rCopula(num_examples, clayton.cop)
      clayton_copula <- t(matrix(t(clayton_copula)))
      x <- rbind (x, clayton_copula)
    }
    train_copulas[[length(train_copulas)+1]] <- x    
  }
  
  #Frank copula
  for (thetaList in frankCopTheta)
  {
    x <- {}
    for (theta in thetaList)
    {
      frank.cop <- frankCopula(dim = 2, param = theta)
      frank_copula <- rCopula(num_examples, frank.cop)
      frank_copula <- t(matrix(t(frank_copula)))
      x <- rbind (x, frank_copula)
    }
    train_copulas[[length(train_copulas)+1]] <- x    
  }
  return (train_copulas)
}


generate_real_data <- function()
{
  
  #load data from yahoo.finance
  daimler_prices <- get.hist.quote("DAI.DE", quote="Adj", start="2010-08-24", end="2014-06-30", retclass="zoo")
  adidas_prices <- get.hist.quote("ADS.DE", quote="Adj",  start="2010-08-24", end="2014-06-30", retclass="zoo")
  bmw_prices <- get.hist.quote("BMW.DE", quote="Adj", start="2010-08-24", end="2014-06-30", retclass="zoo")
  length(daimler_prices)
  
  #transform data into log-returns
  log_daimler <- diff(log(daimler_prices))
  log_adidas <- diff(log(adidas_prices))
  log_bmw <- diff(log(bmw_prices))
  
  
  #transform data into pseudo-observations, values lie in [0,1]
  daimler_pobs <- pobs(log_daimler)
  adidas_pobs <- pobs(log_adidas)
  bmw_pobs <- pobs(log_bmw)
  
  stock_pobs <- list(daimler_pobs, adidas_pobs, bmw_pobs)
  stock_names <- list("Daimler", "Adidas", "Bmw")
  for (i in 1:length(stock_pobs))
  {
    for (j in 1:length(stock_pobs))
    {
      if (j>i)
      {
      a <- matrix(stock_pobs[[i]], ncol=num_examples, byrow=TRUE)
      b <- matrix(stock_pobs[[j]], ncol=num_examples, byrow=TRUE)
      c <- cbind (a,b)
      test_copulas[[paste0(stock_names[[i]], " and ", stock_names[[j]] ) ]] <- c
      }
    }
  }
  return(test_copulas)
}

classification_report <- function(pred, y)
{
  classification_matrix <- matrix(NA, ncol=3, nrow=3, 
                                  dimnames = list(c("class_1", "class_2", "Average"), 
                                                  c("Precision", "Recall", "F1_score")))
  true_positive <- sum(which(y==1) %in% which(pred==1))
  true_negative <- sum(which(y==2) %in% which(pred==2))
  
  precision_1 <- ifelse(is.nan(true_positive / sum(pred == 1)), 0.0, (true_positive / sum(pred == 1)))
  precision_2 <- ifelse(is.nan(true_negative / sum(pred == 2)), 0.0, (true_negative / sum(pred == 2)))
  recall_1 <- ifelse(is.nan(true_positive / sum(y==1)), 0.0, (true_positive  / sum(y==1)))
  recall_2 <- ifelse(is.nan(true_negative / sum(y==2)), 0.0, (true_negative  / sum(y==2)))
  f1_score_1 <- ifelse(is.nan(2*precision_1*recall_1 / (precision_1+recall_1)), 0.0, (2*precision_1*recall_1 / (precision_1+recall_1)))
  f1_score_2 <- ifelse(is.nan(2*precision_2*recall_2 / (precision_2+recall_2)), 0.0, (2*precision_2*recall_2 / (precision_2+recall_2)))
  classification_matrix[1,] <- (c(precision_1, recall_1, f1_score_1))
  classification_matrix[2,] <- (c(precision_2, recall_2, f1_score_2))              
  classification_matrix[3,] <- (c(mean(c(precision_1, precision_2)),
                                  mean(c(recall_1, recall_2)),
                                  mean(c(f1_score_1, f1_score_2))))

  return (classification_matrix)
}


#SVM
svm_apply <- function(train_copulas, test_copulas){
  write("All combinations of copulas with accuracies", file="classification_report_copula.txt", append=FALSE)
  accuracy_list <- {}
  for (i in 1:length(train_copulas))
  {
    for (j in 1:length(test_copulas))
    {
      if (j>i)
      {
        x_train <- rbind (train_copulas[[i]], train_copulas[[j]])
        x_test <- rbind (test_copulas[[i]], test_copulas[[j]])
        y <- c( rbind (rep(1, nrow(train_copulas[[i]]))), rep(2, nrow(train_copulas[[j]])))
  
        model <- svm(x_train, y, type = "C", probability=TRUE)
        svm_prediction <- predict(model, x_test)
        pred <- as.numeric(svm_prediction)
        
        accuracy <- sum(pred == y) / length(y)
        accuracy_list <- c(accuracy_list, accuracy)
        write (paste0(i, " and ", j, " copula, accuracy = ", accuracy), file="classification_report_copula.txt", append=TRUE)
        write.table(classification_report(pred, y), sep="\t", row.names=FALSE, col.names=FALSE, file="classification_report_copula.txt", append=TRUE)
        write("Predicted values", file="classification_report_copula.txt", append=TRUE)
        write(pred, file="classification_report_copula.txt", append=TRUE, ncolumns=200)
        write("Real labels", file="classification_report_copula.txt", append=TRUE)
        write(y, file="classification_report_copula.txt", append=TRUE, ncolumns=200)
      }
    }
  }
  return(accuracy_list)
}  


svm_real_data <- function(train_data, test_data){
  #write("All combinations of copulas with accuracies", file="copula_svm.txt", append=FALSE)
  accuracy_list <- {}
  for (k in names(test_data))
  {
    write (paste0("Comparison of ", k), file="classification_report_real_data.txt", append=TRUE) 
    for (i in 1:length(train_data))
    {
      for (j in 1:length(train_data))
      {
        if (j>i)
        {
          x_train <- rbind (train_data[[i]], train_data[[j]])
          y_train <- c( rbind (rep(1, nrow(train_data[[i]]))), rep(2, nrow(train_data[[j]])))
          x_test <- test_data[[k]]
          
          tuned_svm <- tune.svm(x=x_train, y = y_train ,gamma=10^(-6:-3), cost=10^(0:3))
          best_cost <- tuned_svm$best.parameters$cost
          best_gamma <- tuned_svm$best.parameters$gamma
          
          model <- svm(x_train, y_train, type = "C", cost=best_cost, gamma = best_gamma, probability=TRUE)
          svm_prediction <- predict(model, x_test)
          pred <- as.numeric(svm_prediction)
          
          y_test <- c(rep(1, nrow(x_test)))
          accuracy <- sum(pred == y_test) / length(y_test)
          accuracy_list <- c(accuracy_list, accuracy)
          write (paste0(i, " and ", j, " copula, accuracy = ", accuracy, ", labeling based on copula ", i), file="classification_report_real_data.txt", append=TRUE) 
          write.table(classification_report(pred, y_test), file="classification_report_real_data.txt", append=TRUE) 
          
          y_test <- c(rep(2, nrow(x_test)))
          accuracy <- sum(pred == y_test) / length(y_test)
          accuracy_list <- c(accuracy_list, accuracy)
          write (paste0(i, " and ", j, " copula, accuracy = ", accuracy, ", labeling based on copula ", j), file="classification_report_real_data.txt", append=TRUE) 
          write.table(classification_report(pred, y_test), file="classification_report_real_data.txt", append=TRUE) 

          #write.table(classification_report(pred, y), file="classification_report_copula.txt", append=TRUE)
        }
      }
    }
  }
  return(accuracy_list)
}


#accuracy_matrix <- {}
#test_data <- generate_real_data()
#write("Classification Report on the real data", file="classification_report_real_data.txt", append=FALSE)
#for (i in 1:10)
#{
  train_data <- generate_copulas()
  test_data <- generate_copulas()
  accuracy_list <- svm_apply(train_data, test_data)
  #accuracy_list <- svm_real_data(train_data, test_data)
#  accuracy_matrix <- rbind(accuracy_matrix, accuracy_list)
#  print(i)
#}


length(train_data)
pdf(paste0(getwd(),"/boxplot_DaimlerAdidas.pdf"))
boxplot(accuracy_matrix[,1:20], col = c(1:20), main="Daimler vs. Adidas")
dev.off()
pdf(paste0(getwd(),"/boxplot_DaimlerBmw.pdf"))
boxplot(accuracy_matrix[,21:40], col = c(1:20), main="Daimler vs. Bmw")
dev.off()
pdf(paste0(getwd(),"/boxplot_AdidasBmw.pdf"))
boxplot(accuracy_matrix[,41:60], col = c(1:20), main="Adidas vs. Bmw")
dev.off()

#boxplot(as.formula("Copula_1 ~ Copula_2"), data = accuracy_matrix,par(cex.axis=0.75),
#        las = 1,at = c(1,2,3,4,5,6, 8,9,10,11,12,13, 15,16,17,18,19,20),
#        col = c("red","sienna","palevioletred1","royalblue2","green","yellow"),
#        names = c(1:18))


pca<-prcomp(train_data[[27]])
plot(train_data[[27]],  project = pca)
pca<-prcomp(train_data[[26]])
plot(train_data[[26]],  project = pca)
pca<-prcomp(train_data[[25]])
plot(train_data[[25]],  project = pca)
pca<-prcomp(train_data[[24]])
plot(train_data[[24]],  project = pca)
pca<-prcomp(train_data[[23]])
plot(train_data[[23]],  project = pca)
pca<-prcomp(train_data[[13]])
plot(train_data[[13]],  project = pca)
pca<-prcomp(train_data[[9]])
plot(train_data[[9]],  project = pca)
pca<-prcomp(train_data[[1]])
plot(train_data[[1]],  project = pca)

