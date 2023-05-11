#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(readr)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(gbm)
library(ggsci)
library(shinythemes)
library(shiny)
library(DT)
library(knitr)
library(tree)
library(mathjaxr)


#Leave off notes:
#I HAVE SOLVED THE DATATRAIN OBJECT NOT FOUND ISSUE I DID THIS BY RETURNING EVERY OBJECT AT THE TOP
#Next will be re-adding other objects back in to see what breaks
# Re-integrate Classification Tree
# Re-integrate Random Forest
# Add kNN

#1. Obtain Data
# - For now we keep the same
# 2. Scrub
# - Date transformation and include in model?
#   3. Explore
# - 
#   4. Model
# - kNN
# - Random Forest
# - Find 3rd method
# 
# 5. Interpret
# - Check accuracy measures
# - F1 Score
# - AUC


# getData() & Clean
shinyServer(function(input, output,session) {

  getData<- reactive ({
    npData<-read_csv("NPBase.csv")
    npData$gold <- as.numeric(npData$gold)
    dataModel <- data.frame(npData)
    dataModel$win <- as.factor(dataModel$win)
    dataModel$lane <- as.factor(dataModel$lane)
    dataModel$lane_role <- as.factor(dataModel$lane_role)
    dataModel
})

## About Page
  #Logo
  output$dotaLogo <- renderImage({
    # When input$n is 3, file name is ./images/image3.jpeg
    filename <- normalizePath(file.path('/images/dotaLogo.jfif'))
    filename
    # Return a list containing the filename and alt text
     list(src = filename,
          alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  #Sources
  
  url <- a("Open Dota, an API for Dota Statistics", href="https://www.opendota.com/")
  output$url <- renderUI({
    tagList("URL link:", url)
  })
  
  url2 <- a("Dotabuff.com, for all needs on replay stats and history", href = "https://www.dotabuff.com/")
  output$url2 <- renderUI({
    tagList("URL link:",url2)
  })
  
  url3 <- a("Dota2.com - Nature's Prophet and Image Source", href = "https://www.dota2.com/hero/nature%27sprophet")
  output$url3 <- renderUI({
    tagList("URL link:",url3)
  })
  
## Numerical Summaries
    
### Sets user's chosen variables from the UI to variable 'ghettoData' for Plotting ONLY, not modeling!
  ghettoData <- reactive ({
    var <- input$pred
  })

### Sets user's chosen plot type from the UI to variable 'plotInput'
  plotInput <- reactive ({
    plotType <- input$plotType
  })
  
### Create plot and pass to output in ui file called histPlot

    # output$histPlot<- renderPlot({
    #   npData <- getData()
    #   var <- ghettoData()
    #   plotType <- plotInput()
    # 
    # 
    #   varHist <- ggplot(npData, aes(x = var, color = win, fill = win))
    #   
    #   if (plotType == 'Histogram'){
    # 
    #     varHist + geom_histogram(aes_string(x=var),alpha = .5, position = "identity")+
    #       scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    #       scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    #       #geom_vline(aes(xintercept=mean(input$pred)), linetype = "dashed", size = 1)+
    #       labs(x = paste0("Amount of ",var), y ="Count of Matches", title = paste0("Matches vs ", var))+
    #       theme_classic()
    #   }
    #   
    #   else if (plotType == 'Boxplot'){
    #     varHist <- ggplot(npData, aes_string(y = var, x = win, color = win, fill = win) )
    #     #boxPlot <- ggplot(npData, aes(x=win,y=gold), fill = gold) +
    #     varHist +                                                              #Set classic bw plot theme
    #       #geom_hline(yintercept = median(npData$gold), size = 0.8) +             #Add line for overall median shares
    #       #geom_point(size = 0.8) +                                                  #Add points
    #       geom_boxplot(lwd = 0.5, width = 0.5, outlier.size = 2.0, alpha = 0.7) +   #Create boxplot
    #       xlab("Did Nature's Prophet win?") + ylab(paste0("Amount of ", var)) +
    #       ggtitle(paste0("Distribution of ",var," across Wins")) +                                            #Set title
    #       scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    #       scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))                            #Set color theme
    #   }
    #   
    #   else if (plotType == 'Scatterplot'){
    #     
    #     
    #   }
      
#       if (var == 'gold'){
#       varHist <- ggplot(npData, aes(x = gold, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = gold, x = win, color = win, fill = win) )
#         }
#       }
#       else if ( var == 'gold_per_min'){
#         varHist <- ggplot(npData, aes(x = gold_per_min, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = gold_per_min, x = win, color = win, fill = win) )
#         }
#       }
#       else if (var == 'kills') {
#         varHist <- ggplot(npData, aes(x = kills, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = kills, x = win, color = win, fill = win) )
#         }
#       }
#       else if (var == 'net_worth'){
#         varHist <- ggplot(npData, aes(x = net_worth, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = net_worth, x = win, color = win, fill = win) )
#         }
#       }
#       else if (var == 'tower_damage') {
#         varHist <- ggplot(npData, aes(x = tower_damage, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = tower_damage, x = win, color = win, fill = win) )
#         }
#       }
#       else if (var == 'duration'){
#         varHist <- ggplot(npData, aes(x = duration, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = duration, x = win, color = win, fill = win) )
#         }
#       }
#       else if (var == 'lane') {# should be fixed since these aren't continuous data
#         varHist <- ggplot(npData, aes(x = lane, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = lane, x = win, color = win, fill = win) )
#         }
#       }
#       else if (var == 'lane_role'){ # should be fixed since these aren't continuous data
#         varHist <- ggplot(npData, aes(x = lane_role, color = win, fill = win))
#         if (plotType == 'Boxplot'){
#           varHist <- ggplot(npData, aes(y = lane_role, x = win, color = win, fill = win) )
#         }
#       }
#       
# if (plotType == 'Histogram'){
#    
#       varHist + geom_histogram(aes(y=..count..),alpha = .5, position = "identity")+
#         scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
#         scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
#         #geom_vline(aes(xintercept=mean(input$pred)), linetype = "dashed", size = 1)+
#         labs(x = paste0("Amount of ",var), y ="Count of Matches", title = paste0("Matches vs ", var))+
#         theme_classic()
# }





##### Modeling area

getTrainSetSize <- reactive({         #Variable to call training set size from UI
    trainSetSize <- input$train
    trainSetSize
    })
output$trainSetSize <- renderText({
  trainSetSize <- getTrainSetSize()
  trainSetSize
})

getModelPreds <- reactive({           # variables selected by user for modelling from UI
    keepVars <- input$preds
    keepVars
    })
output$modelPreds <- renderText({
  modelPreds <- getModelPreds()
  modelPreds
})
    
getDataModel<- eventReactive (input$modelButton,{    #When user clicks the model button
  keepVars <- getModelPreds()
  keepVars <- c(keepVars, "win")
  
  dataModel <- getData()
  dataModel <- dataModel[, names(dataModel) %in% keepVars]
  dataModel  
})
output$dataModel <- renderDataTable({
  dataModel <- getDataModel()
  dataModel
})

dataIndex <- reactive({        # Create our index for training from getDataModel()
dataModel<-getDataModel()
trainSetSize <- getTrainSetSize()
index <- createDataPartition(dataModel$win, p = trainSetSize, list = FALSE)
index
})
output$dataIndex <- renderDataTable({
  index <- dataIndex()
  index
})


dataTrain <- reactive({        #Set our training data to a variable
  dataModel <- getDataModel()
  dataIndex <- dataIndex()
  
  dataTrain <- dataModel[dataIndex, ]
  dataTrain
})
output$dataTrain <- renderDataTable({
  dataTrain <- dataTrain()
  dataTrain
})


dataTest <- reactive({          #Set our testing data to a variable
  dataModel <- getDataModel()
  dataIndex <- dataIndex()
  dataTest <- dataModel[-dataIndex, ]
  dataTest
})
output$dataTest <- renderDataTable({
  dataTest <- dataTest()
  dataTest
})

# glmModel <- eventReactive(input$modelButton,{
# ##Math Jax
#   output$ex2 <- renderUI ({
#     withMathJax(helpText('$$log((Psuccess|networth)/1-P(success/networth)) = \beta_0 + \beta_1*networth$$'))
#   })

## Logistic Reg ------------------------------------------------
# dataTrain <- dataTrain()
# glmFit <- train(win ~ ., data = dataTrain,
#                 method = "glm",
#                 family = "binomial",
#                 preProcess = c("center", "scale"),
#                 trControl = trainControl(method = "cv", number = 10))


  # predictions <- predict(glmFit, dataTrain)
  #
  # devia<-summary(glmFit)$deviance
  #
  # coef <-summary(glmFit)$coefficients
  #
  # predictions
  # devia
  # coef

# })

#output$confusMatr<-renderPrint({
#   glmFit <- glmModel()
#   dataTest <- dataTest()
#   confusMatr<-confusionMatrix(data = dataTest$win, reference = predict(glmFit, newdata = dataTest))
#   confusMatr
# })
#output$sum <- renderPrint({
#   models <- glmModel()
#   summary(models)
# })



### Classification Trees ----------

##Unpruned Classification Tree

classTree <- reactive({    # Create out model
  trainSet <- dataTrain()
  classTreeFit <- tree(win ~ ., data = trainSet)
  classTreeFit
})
output$classTreeSumm <-renderPrint({      #Output summary of model
  classTreeSum <- classTree()
  summary(classTreeSum)
  
})

#Using Model from Full Tree on Test Data
#fullPred <- eventReactive( input$modelButton, {
#   classTreeFit<- classTree()
#   dataTest <- dataTest()
#   
#   fullPred <- predict(classTreeFit, dplyr::select(dataTest, -"win"), type = "class")
# })

#Creating Confusion Matrix with Prediction-TestData results
# fullTreeCM <- reactive({
#   
#   fullPred <- fullPred()
#   dataTest <- dataTest()
#   cm <- confusionMatrix(fullPred,dataTest$win)
#   
#   cmFull <- cm$table
#   cmFull
#   
# })
# output$fullTreeCM <- renderPrint({   #Outputting table
#   table <- fullTreeCM()
#   
#   table
# })
# fullTreeAcc <- reactive({         #Creating Confusion Matrix to Output Accuracy
#   fullPred <- fullPred()
#   dataTest <- dataTest()
#   cm <- confusionMatrix(fullPred,dataTest$win)
#   
#   cmAcc <- cm$overall[1]
# })
# output$fullTreeAcc <- renderText({ #Outputting accuracy
#   text <- fullTreeAcc()
#   text
# })


#####                        Pruning Section                          ########


##Prune Tree Fitting---------------------------------------------------------------------------

pruneTree <-reactive({    #Creating the pruned model
  classTreeFit <- classTree()
  pruneFit <- cv.tree(classTreeFit, FUN = prune.misclass)
  pruneFit
})

pruneStats <-reactive({                           #Generate best pruned model
  pruneFit<- pruneTree()
  #classTreeFit<-classTree()
  #pruneFit
  
  #Ordering things so that the best value is always in the first slot of dfPruneFit$size
  dfPruneFit <- cbind(size=pruneFit$size,dev=pruneFit$dev)
  dfPruneFit <- data.frame(dfPruneFit)
  dfPruneFit <- dfPruneFit %>% group_by(size)%>%arrange(size)%>%arrange(dev)
  
  bestVal <- dfPruneFit$size[1]
  bestVal
  
  pruneFitFinal <- prune.misclass(classTreeFit, best = bestVal)
  pruneFitFinal
  #prunePred <- predict(pruneFitFinal, dplyr::select(dataTest, -"win"), type = "class")
})


pruneProgression <-reactive({     #Create the progression of nodes to show viewers how pruning works
  pruneFit<- pruneTree()
  #Ordering things so that the best value is always in the first slot of dfPruneFit$size
  dfPruneFit <- cbind(size=pruneFit$size,dev=pruneFit$dev)
  dfPruneFit <- data.frame(dfPruneFit)
  dfPruneFit <- dfPruneFit %>% group_by(size)%>%arrange(size)%>%arrange(dev)
  dfPruneFit
})
output$pruneProg <- renderDataTable({ #Output progression of the tree fit
  pruneProg <- pruneProgression()
  pruneProg
})


## Testing the Pruned Model ----------------------------------------------------------

# prunePred <- eventReactive(input$modelButton, { # Create predictions on the testing data
#   pruneFitFinal <- pruneStats()
#   dataTest <- dataTest()
#   prunePred <- predict(pruneFitFinal, dplyr::select(dataTest, -"win"), type = "class")
# })
# output$prunePred <- renderDataTable({
#   preds <- prunePred()
# })
# output$pruneTree <- renderDataTable({ # Output results of the pruned model
# 
#   prunePred <- prunePred()
#   dataTest <- dataTest()
#   
#   pruneTbl <- table(data.frame(prunePred, dataTest$win))
#   pruneTbl
#   
# })

###Comparison of Pruned Fit

# output$accPrune <- renderText({#checked for dataTrain
#   
#   prunePred <- prunePred()
#   dataTest <- dataTest()
#   
# pruneTbl <- table(data.frame(prunePred, dataTest$win))
# kable(pruneTbl)
# accPrune<-sum(diag(pruneTbl)/sum(pruneTbl))
# accPrune
# 
# })

  ##Prune Stat Render
# output$pruneStats <- renderPrint({ #checked for dataTrain
# 
#   fullPred <- fullPred()
#   dataTest <- dataTest()
#   
#   pruneStats <- pruneStats()
#   summary(pruneStats)
# })

# #----------------------Prune Graphs
# ##Prune Graph Generate
# pruneGraph <- eventReactive (input$modelButton, { #checked for dataTrain
#   #classTreeFit<-classTree()
#   pruneFit <- pruneTree()
#   prunePlot <- plot(pruneFit$size, pruneFit$dev, type = "b")
#   prunePlot
# })
# ##Prune Graph Render
# output$pruneGraph <- renderPlot({ #checked for dataTrain
#   pruneGraph <- pruneGraph()
#   pruneGraph
# })



##------------------------------- Random Forest
# 
# rfModel <- eventReactive(input$modelButton,{
#     dataModel <- getDataModel()
#     trainSetSize<- getTrainSetSize()
#     dataTrain <- dataTrain()
#   
# trainRFModel <- train(win ~ ., data = dataTrain,
#                       method = "rf",
#                       trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
#                       tuneGrid = data.frame(mtry = sqrt(ncol(dataTrain) - 1)))
# #trainConMat <- confusionMatrix(trainRFModel, newdata = dataTest)
# #testConMat <- confusionMatrix(data = dataTest$win, reference = predict(trainRFModel, newdata = dataTest))
# trainRFModel
# })
# 
# output$rfStats <- renderPrint({
#   rfModel <- rfModel()
#   summary(rfModel)
# })
# 
# output$rfConfMat <- renderPrint({
#   trainRFModel<- rfModel()
#   dataTest <- dataTest()
#   
#   trainConMat <- confusionMatrix(trainRFModel, newdata = dataTest)
#   #testConMat <- confusionMatrix(data = dataTest$win, reference = predict(trainRFModel, newdata = dataTest))
#   #testConMat
#   trainConMat
# })
# 
# output$rfTestConfMat <- renderPrint({
#   trainRFModel<- rfModel()
#   dataTest <- dataTest()
#   
#   testConMat <- confusionMatrix(data = dataTest$win, reference = predict(trainRFModel, newdata = dataTest))
#   testConMat
# })
# 
# fullPreds <- eventReactive(input$modelButton,{
# 
#   
#   fullPred <- predict(classTreeFit, dplyr::select(dataTest, -"win"), type = "class")
#   
#   
# })
# 
# 
# ## Predictions
# 
# #Grab User Inputs
# userNetworth<-reactive({ userNetworth <- input$userNetworth})
# userGPM <- reactive ({ userGPM <- input$userGPM})
# userGold<- reactive ({userGold<-input$userGold})
# userKills<- reactive ({userKills<-input$userKills})
# userTD<- reactive ({userTD<-input$userTD})
# userDuration <- reactive ({userDuration<-input$userDuration})
# userLane <- reactive ({userLane<-input$userLane})
# userLaneRole <- reactive ({userLaneRole<-input$userLaneRole})
# 
# 
# #Upon button press, execute predict function
# prediction <- eventReactive(input$predictionButton,{
#   
#   #Pass in our trained model from earlier
#   trainRFModel <- rfModel()
#   #Pass in our selected vars
#   charvec<-getModelPreds()
#   
# #Pass in user vars
#   userNetworth <- userNetworth()
#   userGPM <- userGPM()
#   userGold <- userGold()
#   userKills <- userKills()
#   userTD <- userTD()
#   userDuration <- userDuration()
#   userLane <- userLane()
#   userLaneRole <- userLaneRole()
# 
# #Build a dataframe out of values
#   userPredVals <- data.frame("gold"=userGold,"gold_per_min"=userGPM,"kills"=userKills,"tower_damage"=userTD,
#              "duration"=userDuration,"lane"=userLane,"lane_role"=userLaneRole,"net_worth"=userNetworth)
#   
#   userPredVals$lane <- as.factor(userPredVals$lane)
#   userPredVals$lane_role <- as.factor(userPredVals$lane_role)
#   
#   gold      <- "gold"       %in% charvec
#   gold_per_min    <- "gold_per_min"     %in% charvec
#   kills <- "kills"  %in% charvec
#   tower_damage <- "tower_damage" %in% charvec
#   duration <- "duration" %in% charvec
#   lane <- "lane" %in% charvec
#   lane_role <- "lane_role" %in% charvec
#   net_worth <- "net_worth" %in% charvec
#   
#   if (!(gold)){
#     userPredVals<- userPredVals %>% select(-gold) 
#   }
#   if (!(gold_per_min)){
#     userPredVals<- userPredVals %>% select(-gold_per_min) 
#   }
#   if (!(net_worth)){
#     userPredVals<- userPredVals %>% select(-net_worth) 
#   }
#   if (!(kills)){
#     userPredVals<- userPredVals %>% select(-kills) 
#   }
#   if (!(tower_damage)){
#     userPredVals<- userPredVals %>% select(-tower_damage) 
#   }
#   if (!(duration)){
#     userPredVals<- userPredVals %>% select(-duration) 
#   }
#   if (!(lane)){
#     userPredVals<- userPredVals %>% select(-lane) 
#   }
#   if (!(lane_role)){
#     userPredVals<- userPredVals %>% select(-lane_role) 
#   }
#     
# 
# 
#   
#   #Predict
#   prediction <- predict(trainRFModel, newdata = userPredVals)
#   prediction
# })
# 
# #render our prediction
# 
# output$userPrediction<- renderPrint({
#   prediction<-prediction()
#   prediction
#   
# })
# 
# 
# #Next steps,
# # 1. Figure out the predictions error on Modeling page
# # 2. Input all the other models
# # 3. Render the summary stats
# # 4. Ensure training set size user input
# # Frequency table/Set of Box plots/Contingency Table
# # Continuous Variables Scatterplots
# 
# 
# #Datatable Output
# 
# 
#   
#   # Reactive value for selected dataset ----
#   datasetInput <- reactive({
#     switch(input$dataset,
#            "rock" = rock,
#            "pressure" = pressure,
#            "cars" = cars)
#   })
#   
#   # Table of selected dataset ----
#   output$table <- renderTable({
#     datasetInput()
#   })
#   
#   # Downloadable csv of selected dataset ----
#   output$downloadData <- downloadHandler(
#     filename = "yourData.csv",
#     content = function(file) {
#       write.csv(dataTrain(), file, row.names = FALSE)
#     }
#   )
# 
# 
# 
# output$userTable <- renderDataTable({
#   dataModel <- getDataModel()
#   dataModel
# })
# 
# output$glmStats <- renderPrint({
#   predictions
#   devia
#   coef
#   predictions
# })
 })
# 
# 
#     
