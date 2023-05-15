#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
library(DT)
library(caret)
library(knitr)
library(mathjaxr)
options(shiny.reactlog = TRUE)
# Define UI for application that draws a histogram


shinyUI(fluidPage(
  
  # Application title
  titlePanel(uiOutput("title")),
  withMathJax(),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h3("Plot Options:"),
      selectInput("plotType","Pick Plot Type", selected = "Histogram", choices = c("Histogram","Boxplot","Scatterplot")),
      selectInput("pred", "Pick Variable of Interest", selected = "gold", choices = c("gold_per_min",
                                                                "net_worth",
                                                                "gold",
                                                                "kills",
                                                                "tower_damage",
                                                                "duration"
                                                                )
                  ),
      conditionalPanel(
        condition = "input.plotType == 'Scatterplot'",
                       selectInput("var", "Pick Variable #2 of Interest", selected = "net_worth", choices = c("gold_per_min",
                                                                                                         "net_worth",
                                                                                                         "gold",
                                                                                                         "kills",
                                                                                                         "tower_damage",
                                                                                                         "duration"
                                                                                                         )
                                   ),
      ),
      br(),
      h3("Modeling Options:"),
      sliderInput("train", "Choose Training Data Size",
                  min = .2, max = .8, value = .3, step = .1),
      checkboxGroupInput("preds","Choose Predictors", selected = "net_worth",choices = list("gold_per_min",
                                                               "net_worth",
                                                               "gold",
                                                               "kills",
                                                               "tower_damage",
                                                               "duration",
                                                               "lane",
                                                               "lane_role"
                                                               )
                         ),
      strong("Model Buttons"),
      br(),
      actionButton("prepButton","Partition Data"),
      br(),
      actionButton("modelButton","Train Models"),
      br(),
      actionButton("predictionButton","Make a prediction")
      #checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      
      #conditionalPanel(condition = "input.conservation",
      #                 checkboxInput("CB",label=h4("Opacity = REM"),value=FALSE,width="100%"),
     ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 div(tags$img(src = "dotaLogo.jfif",height = '200px', weidth = '200px', deleteFile = FALSE),style="text-align: center;"),
                 h2("Dota 2: Predicting Wins with Nature's Prophet"),
                 br(),
                 h3("Instructions"),
                 strong("1. Explore the variables you're interested in by using the plot options to on the left to generate a plot on the plot tab."),
                 br(),
                 br(),
                 strong("2. Pick the variables you'd like to base a model on with the modeling tab options."),
                 br(),
                 br(),
                 strong("3. Choose your training set size, view your settings on the Dataset tab, and download the partitioned dataset if you like. When you're satisfied with you're settings, click the Partition Data button."),
                 br(),
                 br(),
                 strong("4. Click the Train Models Button to train and output results of models on the Modeling page."),
                 br(),
                 br(),
                 strong("5. Move to the Predictions tab and enter some of your own values you'd like to use in prediction. These predictions will be using the Random Forest model."),
                 br(),
                 br(),
                 strong("6. Click the Predict button on the left and see if you'd win the game or not."),
                 br(),
                 br(),
                 h3("Exposition"),
                 p("This work will be using a dataset about a specific professional eSport called Dota 2, a rapidly growing business. The purpose of this app is to apply several different models to predict a win for a player."),
                 br(), 
                 p("Dota 2 has a long and rich history that can rival the best of college sports rivalries. It currently holds the top 7 highest prize pools topping out at $40 million dollars, it also has an active betting scene."),
                 p("A quick summary on how Dota 2 is played is required for better understanding of our data. Dota 2 is a 5v5 game played on a square map with 3 'lanes' serving as the main connection between players. 
                   Each player chooses 1 of these 3 lanes and fights over gaining gold to purchase items to assist in killing the enemy and ultimately destory the opponents' base.
                   Each player chooses 1 of 100+ heros, each with their own unique abilities and playstyles, to compete against each other. My favorite hero to play right now is Nature's Prophet."),
                 p("Nature's Prophet is a hero with abilities that focus on having a global presence, and using summoned units to deal damage to enemy buildings.
                   As you can imagine, that may influence certain variables to be more impactful for him than other heros."),
                 br(),
                 h3("The Variables"),
                 p("The variables used are all stats from the games themselves. The list and a short description of what each means."),
                 strong("1. gold_per_min"), p("Sometimes shortened to GPM, gold per minute represents how much gold a hero was accuring every minute. This metric can change throughout the game and is a hybrid measurement between a hero's power and player efficiency. A higher GPM means you're likely strong and efficient."),
                 br(),
                 strong("2. net_worth"), p("Another measure of gold, net worth is the final amount of gold a hero is worth at the end of the game. Gold can be spent on items, most appopriately thought of as assets, as well as consumables which disappear after use. Net worth is the 'gold' standard to measure a hero's power in the game."),
                 br(),
                 strong("3. gold"), p("Although it may seem redundant, gold here is just the amount of gold the player had left at the end of the game. Sometimes players are saving for a big item that they hope will change the next fight, other times they get it just in time and have no money left to buy a second life after they die."),
                 br(),
                 strong("4. kills"), p("This variable is a count of kills. The more kills a hero has, the more gold they would have, but it also is a representation of how much impact they had on a game. Some heros thrive on killing other heros, where some want to avoid fighting and killing for quite some time."),
                 br(),
                 strong("5. tower_damage"),p("This variable indicates how much damage a hero caused to the enemy buildings. Some heros excel at destroying buildings rather than killing.. sometimes they're good at both!"),
                 br(),
                 strong("6.duration"),p("Duration represents how long the game went in the format of 'mmss', i.e. 2412 is a 24 minutes 12 second game. Some heros are more powerful as the game goes on, while others depend on ending the game early."),
                 br(),
                 strong("7. lane"),p("Lanes are symmetrical between teams, but each lane within a team is different from another. Lane 1, the 'safelane' is typically meant for gold dependent heros. Lane 2, the 'midlane' is good for heros that do well in 1v1 scenarios and scale well with both xp and gold. Lane 3, the 'offlane' is typically saved for highly survivable heros that provide utility to the team with little gold and experience."),
                 br(),
                 strong("8. lane_role"),p("Across lanes, a team is divided up into farm priorities to identify who should be taking last hits for gold. This is denoted by the numbers 1 - 5 with 1 being the most important and 5 being the least important. These values are a bit mis-represented as its hard to capture who was prioritized with farm outside of net worth. Its best to interpret this as an indicator of farm level within their team. i.e. A value of 1 would indicate they were the most farmed on their team."),
                 br(),
                 br(),
                 h4("Some Notes"),
                 p("This app will focus on predicting wins using several variables from matches pulled from an API for the game version 7.31d. The data we are using is a collection of games gathered from 
                 a group of professional and semi-professional tournaments and leagues that all contained Nature's Prophet."),
                 br(),
                 h4("Sources & Packages"),
                 p("readr
                   dplyr
                   randomForest
                   caret
                   ggplot2
                   gbm
                   ggsci
                   shinythemes
                   shiny
                   DT
                   knitr
                   tree
                   mlr3
                   kknn"),
                 uiOutput("url"),
                 uiOutput("url2"),
                 uiOutput("url3"),
        ),
        tabPanel("Plot",plotOutput("histPlot")),
        tabPanel("Dataset",
                 br(),
                 p("Training set size:"),textOutput("trainSetSize"),
                 p("Predictors:"),textOutput("modelPreds"),

                 p("Your subsetted data is below"),
                 downloadButton("downloadSubset", "Download"),
                 dataTableOutput("dataModel"),

                 p("The index chosen for training:"),
                 dataTableOutput("dataIndex"),

                 p("The data at those indexes for training:"),
                 downloadButton("downloadTrain","Download"),
                 dataTableOutput("dataTrain"),

                 p("The data for testing"),
                 downloadButton("downloadTest","Download"),
                 dataTableOutput("dataTest")),

        tabPanel("Modeling",
                 br(),
                 #p("Training set size:"),textOutput("trainSetSize"),
                 #p("Predictors:"),textOutput("modelPreds"),
                 h4("Logistic Regression/GLM"),
                 p("We will be using logistic regression in our first model to track the log odds of whether Nature's Prophet will win. 
                   Logistic Regression models is different from linear models for a success/failure response variable in that it is accurately attempting to model the probability of success.
                   To accomplish this, responses need to be kept between 0 and 1 using the logistic function, where L = the curve's maximum value."),
                 (helpText('Logistic Function:  $$\\frac{L}{(1 + e^{-k(x-x_0)})}$$')),
                 p("By forcing our responses between 0 and 1, this changes the interpretation of our coefficients inside our mdoel. Rather than them representing the change in slope given a value of a predictor, it represents the change in log odds of success."),
                 p("For example if you only chose a predictor of netwoth for your variable of interest, your link function between the coefficients and predictors to the log odds of success would be:"),
                 helpText('$$log(P(success|networth)/1-P(success/networth)) = beta_0 + beta_1*networth$$'),
                 p("This does mean there is some loss of interpretability of the model that is returned without more calculation. Also important to note that deviance becomes a good method to evaluate accuracy rather than the standard RMSE."),
                 verbatimTextOutput("sum"),
                 verbatimTextOutput("confusMatr"),
                 br(),
                 h4("Classification Tree Summary"),
                 p("Classification trees have great strength in interpretability, but are not fully optimized. Classification trees achieve accurate predictions by using the Gini Index.
                 The basic idea behind a classification tree is that we chop our distribtuion of log odds into distinct regions of values for each variable and use the Gini index to evaluate the amount of inequality in distribution. 
                   The Gini index is as follows, where p = the probabilty of correct classification:"),
                 helpText('Gini Index : $$\\frac{number in region}{class*2p(1-p)}$$'),
                 p("We evaluate the Gini index on every single value within a region and choose the value that has the lowest Gini Index as the point to split on.
                   This continues for each region as the tree is built."),
                 p("We also want to minimize our value of deviance."),
                 helpText('Deviance: $$-2plog(p) -2(1-plog)(1-p)$$'),
                 #Insert pictures of Gini stuff from Fitting Classification Trees.Rmd
                 p("As mentioned earlier, Classification trees aren't optimized. The Gini index currently used only looks at what the best split is 'right now',
                   rather than what are the best region splits to minimize our deviance and Gini across our next 2, 3, or 4 regions. Often pruning is also needed as multiple tree nodes
                   are created without adding enough accuracy to the model's accuracy."),

                 
                 #verbatimTextOutput("pruneStats"), #sortofwork
                 #plotOutput("pruneGraph"), #work
                 
                 h5("Tree Fit without Pruning"),
                 p("Below you will see the stats from the classification tree you created with its variables. 
                   Take important note of the misclassification rate. These metrics are from the training data, we'll compare it to the testing data soon."),
                 verbatimTextOutput("classTreeSumm"),
                 verbatimTextOutput("fullTreeCM"), 
                 verbatimTextOutput("fullTreeAcc"),
                 
                 #h5("Tree Fit with Pruning"),
                 #p("Often times, classification tree's are 'pruned' by removing nodes. Tree nodes can be excessive or insignificant at times. To play to the classification tree's strength,
                 #it can be useful to reduce the nodes in a tree for easier interpretability or remove unnecessary nodes. Let's see how the number of nodes, (aka splits or branches), will affect our deviance. 
                   #We want to reduce our deviance, I've made sure our first row will contain the least amount of deviance with the least amount of branches."),
                 #dataTableOutput("pruneProg"),
                 #dataTableOutput("prunePred")
                 #dataTableOutput("pruneTree"),
                 #verbatimTextOutput("accPrune"),
                 
                 h4("Random Forest Summary"),
                 p("Random Forests are very useful to increase prediction accuracy past what a normal Classification Tree can accomplish.
                 The basic idea of Random Forests is that multiple classification trees are being averaged over to increase prediction accuracy,
                 but with this comes with a loss of interpretability. Random forests also use a random subset of predictors at each trained tree to account for particularly powerful predictors.
                 Random trees are typically outperformed by gradient boosted tree's depending on the nature of the data.
                 The number of predictors is determined by whether the tree is a classification tree or regression tree. Ours is a classification tree and will use that value when training our model."),
                 helpText('Classification : $$m = sqrt(p)$$'),
                 helpText('Regression : $$m=p/3$$'),
                 h5("Train Confusion Matrix"),
                 verbatimTextOutput("rfConfMat"),
                 h5("Test Confusion Matrix"),
                 verbatimTextOutput("rfTestConfMat"),
                 h4("kNN Summary"),
                 p("K Neartest Neighbors, kNN for short, uses predictor variables to establish similarity between other observations. Labeling each observation as a win or loss,
                   and comparing the Euclidean distance between the new observation and its neighbors allows for the distance function to be minimized for each category of observation.
                   In our example, our categories are win or lose. Using this method will predict a win or loss depending on the distance between its neighbors."),
                 verbatimTextOutput("knnConfMat"),
                 verbatimTextOutput("knnF1")
                 #textOutput("glmStats"),
        ), #end of tabpanel 2
        
        tabPanel("Predictions",
                 verbatimTextOutput("userPrediction"),
                 conditionalPanel(condition = "input.preds.includes('net_worth')",
                                  numericInput("userNetworth", label = h4("Net Worth"), value = 1)),
                                  
                 conditionalPanel(condition ="input.preds.includes('gold_per_min')",
                                  numericInput("userGPM", label = h4("Gold Per Min"), value = 1)),
                                  
                 conditionalPanel(condition ="input.preds.includes('gold')",
                                  numericInput("userGold", label = h4("Gold"), value = 1)),
                                  
                 conditionalPanel(condition ="input.preds.includes('kills')",
                                  numericInput("userKills", label = h4("Kills"), value = 1)),
                                  
                 conditionalPanel(condition ="input.preds.includes('tower_damage')",
                                  numericInput("userTD", label = h4("Tower Damage"), value = 1)),
                                  
                 conditionalPanel(condition ="input.preds.includes('duration')",
                                  numericInput("userDuration", label = h4("Duration"), value = 1)),
                                  
                 conditionalPanel(condition ="input.preds.includes('lane')",
                                  selectInput("userLane", label = h4("Lane"), 
                                              choices = list("1" = 1, "2" = 2, "3" = 3), 
                                              selected = 1)),
                                  
                 conditionalPanel(condition="input.preds.includes('lane_role')",
                                  selectInput("userLaneRole", label = h4("Lane Role"), 
                                              choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5"=5), 
                                              selected = 1))
                 )#end of tabPanel3
        
        )#end of tabsetPanel
      )#end of MainPanel
    )#end of sidebar layout
  )#end of FluidPage
)#end of shinyUI
