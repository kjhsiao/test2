library(shiny)
library(plotly)
library(ipify)
library(shinyjs)
library(DT)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  column(3,
         #lon in
         br(),
         actionButton("log_control", label = h5("µn¤J/µù¥U"),width = 100),
         hidden(
           div(id="log",
               column(12,hr(),print(h5("-Log in- \n"))),
               column(6,textInput("username", label = "username",placeholder = "")),
               column(6,passwordInput("password", label = "password",placeholder = "")),
               column(12,
                      actionButton("log_in", label = "log in"),
                      actionButton("log_out", label = "log out"),
                      div(id="welcome_panel",br(),verbatimTextOutput("welcome"))),
               #register
               column(12,hr(),print(h5("-Register- \n"))),
               column(6,textInput("username_r", label = "username",placeholder = "")),
               column(6,passwordInput("password_r", label = "password",placeholder = "")),
               column(12,actionButton("register_submit", label = "submit")),
               column(12,br(),verbatimTextOutput("welcome_register"),hr())
           )
         )
  ),
  #add item
  column(3,
         br(),
         actionButton("add_item", label = h5("Add Item"),width = 100),
         hidden(div(id="add_panel",
                    column(12,hr(),print(h5("-Item1-"))),
                    column(6,dateInput("date1", label = ("date"))),
                    column(6,selectInput("item1", label = ("item"), choices = c("grocery","gas","dine out"))),
                    column(6,numericInput("expense1", label = ("expense"),value=0)),
                    column(6,selectInput("select1", label = ("category"), 
                                         #choices = list("eating" = 1, "clothing" = 2, "living" = 3, "moving" = 4, "education" = 5, "entertainment" = 6))),
                                         choices = c("eating", "clothing", "living", "moving", "education", "entertainment"))),
                    column(12,actionButton("add_item1", label = "add")),
                    column(12,hr(),print(h5("\n-Item2-"))),
                    column(6,dateInput("date2", label = ("date"))),
                    column(6,textInput("item2", label = ("item"))),
                    column(6,numericInput("expense2", label = ("expense"),value=0)),
                    column(6,selectInput("select2", label = ("category"), 
                                         choices = list("eating" = 1, "clothing" = 2, "living" = 3, "moving" = 4, "education" = 5, "entertainment" = 6))),
                    column(12,actionButton("add_item2", label = "add"),hr())
         ))),
  
  #review
  column(3,
         br(),
         actionButton("review", label = h5("Review"),width = 100),
         hidden(div(id="review_panel",hr(),uiOutput("review_table")
         ))),
  
  #chart
  column(3,
         br(),
         actionButton("chart", label = h5("Chart"),width = 100),
         hidden(div(id="chart_panel",hr(),plotOutput("pie"),plotOutput("plot")))
         
  )
)

server <- function(input, output) {
  # log in
  log <- read.table("register.txt",header = TRUE, sep = "\t")
  shinyjs::onclick("log_control",shinyjs::toggle(id = "log", anim = FALSE)) 
  username <- eventReactive(input$log_in,{input$username})
  password <- eventReactive(input$log_in,{input$password})
  observeEvent(input$log_out, {shinyjs::reset("log")})
  shinyjs::onclick("log_out",shinyjs::hide(id = "welcome_panel", anim = FALSE))
  shinyjs::onclick("log_in",shinyjs::show(id = "welcome_panel", anim = FALSE))   
  output$welcome <- renderPrint({
    if ((input$log_in>0) & (username()=="" & password()=="")){
      cat(paste("Please log in."))
    } else if ((input$log_in>0) & username()%in%log$username & password()==log$password[which(log$username == username())]){
      cat(paste("Welcome!",username(),"! \nYou have logged in\nfrom",get_ip(),"\nat",Sys.time(),"."))
      write.table(cbind(username(),password(),get_ip(),as.character(Sys.time()),"success"), 
                  file = "log.txt",sep = "\t",append = TRUE, row.names = FALSE, col.names = FALSE)
    } else{
      cat(paste("Your password does not match your username.")) 
      write.table(cbind(username(),password(),get_ip(),as.character(Sys.time()),"false"), 
                  file = "log.txt",sep = "\t",append = TRUE, row.names = FALSE, col.names = FALSE)
    }
  })
  
  #register
  new_register <- eventReactive(input$register_submit,{cbind(input$username_r,input$password_r)})
  observeEvent(input$register_submit, {log <- read.table("register.txt",header = TRUE, sep = "\t")})
  output$welcome_register <- renderPrint({
    if(new_register()[1]==""|new_register()[2]==""){cat(paste("Please fill in your information"))}
    else if(new_register()[1]!=""&new_register()[2]!=""&!new_register()[1]%in%log$username){
      cat(paste("Thank you for registration!\nPlease refresh before loging in.\nfrom",get_ip(),"\nat",Sys.time()))
      observeEvent(input$register_submit,{write.table(cbind(new_register(),get_ip(),as.character(Sys.time())), 
                                                      file = "register.txt",sep = "\t",append = TRUE, row.names = FALSE, col.names = FALSE)})
    } else if(new_register()[1]%in%log$username){cat(paste("This username has been used."))}
  })
  observeEvent(input$register_submit, {shinyjs::reset("register")})
  
  #add item
  shinyjs::onclick("add_item",shinyjs::toggle(id = "add_panel", anim = FALSE))
  observeEvent(input$add_item1,{write.table(cbind(as.character(input$date1),input$item1,input$expense1,input$select1), 
                                            file = "account.txt",sep = "\t",append = TRUE, row.names = FALSE, col.names = FALSE)})
  observeEvent(input$add_item2,{write.table(cbind(as.character(input$date2),input$item2,input$expense2,input$select2), 
                                            file = "account.txt",sep = "\t",append = TRUE, row.names = FALSE, col.names = FALSE)})
  
  #review
  shinyjs::onclick("review",shinyjs::toggle(id = "review_panel", anim = FALSE))
  account <- eventReactive(input$review,{read.table("account.txt",header = TRUE, sep = "\t")})
  output$review_table <- renderUI({column(3,datatable(account(), width = 400, rownames = FALSE))
  })
  
  #chart
  #plot <- read.table("account.txt",header = TRUE, sep = "\t")
  shinyjs::onclick("chart",shinyjs::toggle(id = "chart_panel", anim = FALSE))
  plot <- eventReactive(input$chart,{read.table("account.txt",header = TRUE, sep = "\t")})
  #output$chart_optput <- renderUI({plotOutput("plot")})
  output$plot <- renderPlot({
    ggplot(plot(),aes(x=date,y=expense,col=as.factor(category)))+geom_point()+geom_jitter(alpha=0.5)+
      theme(axis.title.x=element_blank())+labs(colour = "category")
  })
  
  pie <- eventReactive(input$chart,{aggregate(expense ~ category, plot(), sum)})
  output$pie <- renderPlot({
    ggplot(pie(),aes(x="",y=expense,fill=factor(category)))+geom_bar(width = 1, stat = "identity")+
      coord_polar("y",start=0)+
      theme(axis.text = element_text(size = 12),axis.title.x=element_blank(),axis.title.y=element_blank())+
      theme(legend.text = element_text(size = 12),legend.title = element_text(size=12))      
  })
  
}
shinyApp(ui, server)
