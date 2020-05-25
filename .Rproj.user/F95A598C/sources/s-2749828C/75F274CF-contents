library(shiny)
library(shinyalert)

library(DT)

library(flexdashboard)

library(shinydashboard)
library(shinydashboardPlus)

source("monthInput.R")
source("monthdf.R")
library(dplyr)
library(purrr)
library(tidyr)
library(plotly)
library(forcats)
library(ggplot2)
library(lubridate)

library(survival)

valueBox <- shinydashboard::valueBox


# Define UI for application that draws a histogram
ui = dashboardPage(
    dashboardHeader(title = "KTRA"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
            menuItem("Compatability", tabName = "compatability", icon = icon("puzzle-piece")),
            menuItem("Risk Calculator", tabName = "risk_calculator", icon = icon("calculator")),
            menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
            menuItem("About", tabName = "about", icon = icon("info-circle"))
        )
    ),
    dashboardBody(
      
        tabItems(
          
          # Login tab
          tabItem(tabName = "login",
                  
                  h1("Kidney Transplantation Recipient Aid (KTRA)"),
                  
                  br(),
                  br(),
                  br(),
                  br(),
                  
                  fluidRow(
                    # Center Login Box
                    column(width = 12, offset = 3,
                           
                    box(
                      status = "primary",
                      width = 6,
                      align = "center",
                      
                      br(),
                      
                      h1("Login"),
                      
                      br(),
                      br(),
                      selectInput("username", span(tagList(icon("user"), "  Username")), choices = c("Hoon","Ricky C", "Ricky Y", "Sherry", "Patient (Before Match)", "Patient (After Match)"), width = 250),
                      passwordInput("password", span(tagList(icon("lock"), "  Password")), value = "", width = 250, placeholder = ""),
                      
                      br(),
                      
                      useShinyalert(),
                      actionButton('switchtab', 'Login'),
                    )
                  )
                )
          ),
          
          # Compatability tab
          tabItem(tabName = "compatability",
                  h1("Compatability"),
                  
                  fluidRow(
                    box(
                      width = 12,
                      status = "primary",
                      
                      h2("Recipient vs. Donor"),
                      
                      br(),
                      
                      fluidRow(
                        widgetUserBox(
                          title = textOutput('username'),
                          type = NULL,
                          src = "https://soulcore.com/wp-content/uploads/2018/01/profile-placeholder-300x300@2x.png",
                          color = "blue",
                          
                          tags$b("DETAILS"),
                          
                          uiOutput("recipient_comparison"),
                          
                        ),
                          
                        widgetUserBox(
                          title = "Donor",
                          type = NULL,
                          src = "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSHLfkrlPPv7ZfAvNXe5AM4fV_m6ESbGpmo8IKlp3REM0Yswyts&usqp=CAU",
                          color = "red",
                          
                          tags$b("DETAILS"),
                          
                          uiOutput("donor_comparison")
                          
                        )
                      )
                    )
                  )
          ),
          
          # Risk Calculator Tab
          tabItem(tabName = "risk_calculator",
                  
                  h1("Risk Calculator"),
                  
                  # Show Not Available Screen
                  conditionalPanel(condition = "input.username == 'Patient (Before Match)'",
                    fluidRow(  
                      box(
                        width = 12,
                          status = "primary",
                          h3("Risk Calculator will be available once a donor kidney has been matched.")
                      )
                    )
                  ),
                  
                  conditionalPanel(condition = "input.username != 'Patient (Before Match)'",
                  
                    fluidRow(
                        box(
                          width = 12,
                          status = "primary",
                          
                          h2("ElliPro Scores"),
                          
                          h4("The Ellipro score is a measure of immunogenicity, the ability of the eplets to induce specific antibody responses."),
                          h4("The score indicates the level of mismatches between a recipient and a donor."),
                          h4("A high ElliPro score indicates a high level of mismatch between kidney recipient and donor."),
                          
                          br(),
                          
                          box(
                            width = 4,
                            
                            h3("ElliPro Class 1 DSA"),
                            
                            br(),
                            valueBoxOutput("ellipro_class1", width = 12),
                            
                            h4(htmlOutput("class1_risk_explanation")),
                          ),
                          
                          box(
                            width = 4,
                            
                            h3("ElliPro Class 2 DSA"),
                            
                            br(),
                            valueBoxOutput("ellipro_class2", width = 12),
                            
                            h4(htmlOutput("class2_risk_explanation")),
                          ),
                          
                          box(
                            width = 4,
                            
                            h3("Pre DSA"),
                            
                            br(),
                            valueBoxOutput("pre_dsa", width = 12),
                            
                            h4(htmlOutput("pre_dsa_explanation"))
                          )
                        )
                    ),
                    
                    fluidRow(
                      box(
                        width = 12,
                        status = "primary",
                        
                        h2("Risk Scores"),
                        br(),
                        
                        box(
                          width = 4,
                        
                          h3("AMR"),
                          
                          br(),
                          valueBoxOutput("amr_valuebox", width = 12),
                          
                          h4(htmlOutput("amr_risk_explanation"))
                        ),
                        
                        box(
                          width = 4,
                        
                          h3("BMI and Gender"),
                          
                          br(),
                          valueBoxOutput("bmi_valuebox", width = 12),
                          
                          br(),
                          h4(htmlOutput("bmi_explanation")),
                          
                          br(),
                          h4(htmlOutput("gender_explanation"))
                        ),
                        
                        box(
                          width = 4,
                          
                          h3("Cold Ischaemia Time"),
                          
                          br(),
                          valueBoxOutput("ischaemia_valuebox", width = 12),
                          
                          br(),
                          h4(htmlOutput("ischaemia_explanation"))
                          
                        )
                        
                      )
                    )
                  )
          ),
            # Calendar Tab
            tabItem(tabName = "calendar",
                    
                    h1("Calendar"),
        
                fluidRow(
                    box(
                        width = 12,
                        status = "primary",
                        
                        monthInput('month_selection', 'Month and Year:', minviewmode = 'months', format = 'M yyyy'),
                        br(),
                        
                        fluidRow(
                            box(
                                width = 8,
                                status = "primary",
                        
                                h2(tags$b(textOutput("selected_month"))),
                                plotlyOutput("plot1")
                            ),
                            
                            box(
                              width = 4,
                              status = "success",
                              
                              h3(tags$b("Next Appointment"), align = "center"),
                              h4(textOutput("next_appointment"), align = "center"),
                            ),
                            
                            box(
                                width = 4,
                                status = "success",
                                
                                h3(tags$b(textOutput("Heading"),align = "center")),
                                h4(textOutput("date_selected"),align = "center"),
                                br(),
                                #h5(textOutput("clin"),align = "left"),
                               # h5(textOutput("dia"),align = "left"),
                                
                               box(
                                 width = 12,
                                 status = "primary",
                                 
                                 icon("user-md"),
                                 h4(tags$b("Clinical Appointments")),
                                 hr(),
                                 h5(htmlOutput("clin"),align = "left")
                               ),
                               box(
                                 width = 12,
                                 status = "primary",
                                 
                                 icon("calendar-plus"),
                                 h4(tags$b("Dialysis Sessions")),
                                 hr(),
                                 h5(htmlOutput("dia"),align = "left")
                               ),
                               
                               box(
                                 width = 12,
                                 status = "primary",
                                 
                                 icon("prescription-bottle-alt"),
                                 h4(tags$b("Medication Schedule")),
                                 hr(),
                                 h5(htmlOutput("medication"),align = "left")
                               ),
                            )
                        )
                    )
                )
            ),
          # Login tab
          tabItem(tabName = "about",
                  
                  h1("About"),
                  
                  
                  fluidRow(
                    box(
                      width = 12,
                      status = "primary",
                      
                      h2("Privacy Policy"),
                      
                      br(),
                      
                      
                      h4("At G20 Inc., your privacy is our highest priority which includes being transparent about how we deal with personal data."),
                      
                      h4("By using the KTRA App, you agree to your data being collected. The data collected will not be shared with any third parties, but will be used to improve the models within the KTRA app."),
                      
                      h4("The information stored about you is only accessible by you and your nephrologist."),
                      
                      h4("Currently, your data is stored centrally on a csv file. In the near future, we will be storing all data on a server with strong encryption to further ensure security."),
                      
                    )
                  )
          )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # Load Eplet Data
  get_eplet_data = reactive({
    
    df <- read.csv("final_eplet_data_with_donor_bmi.csv")
    df <- na.omit(df)
    
    # Sum class 1 and 2 ElliPro scores
    df <- df %>% mutate(class_all.total_score = class1.total_score + class2.total_score)
    
    return(df)
  })
  
  # Assign username to a patient
  username_to_id <- reactive({
    if (input$username == "Hoon") {
      return("13209KR")
    }
    else if (input$username == "Ricky C") {
      return("13209KR")
    }
    else if (input$username == "Ricky Y") {
      return("43893KR")
    }
    else if (input$username == "Sherry") {
      return("12519KR")
    }
    
    else if (input$username == "Patient (Before Match)") {
      return("44482KR")
    }
    
    else if (input$username == "Patient (After Match)") {
      return("44482KR")
    }
  })
  
  user_data <- reactive({
    return(get_eplet_data() %>% filter(Lab_Ref == username_to_id()))
  })
  
  
  
  # Login Section:
  
  observeEvent(input$switchtab, {
    
    # Fake delay
    Sys.sleep(0.5)
    
    # Switch Tab
    updateTabItems(session, "tabs", "compatability")
    
    # Pop up 
    shinyalert("Success", paste("Welcome back, ", input$username), type = "success")
    
    # Clear password
    updateTextInput(session,"password", value="")
    
  })
  
  
  
  
  # Compatability Section
  output$username <- renderText({ 
    return(input$username)
    })
  
  
  output$recipient_comparison <- renderUI({  
    
    # Recipient Details
    recipient_age = (user_data() %>% select(agetxn))[1,1]
    recipient_id = (user_data() %>% select(Lab_Ref))[1,1]
    recipient_bmi = (user_data() %>% select(recipient_bmi))[1,1]
    recipient_gender = if((user_data() %>% select(Sex_Cat))[1,1] == 0) "Female" else "Male"
    recipient_status = if (input$username == "Patient (Before Match)") "Unmatched" else "Kidney Donor Matched"
    
    # Donor Details
    donor_age = (user_data() %>% select(donorage))[1,1]
    donor_bmi = (user_data() %>% select(donor_bmi))[1,1]
    donor_gender = if((user_data() %>% select(DonorGenderCode))[1,1] == 0) "Female" else "Male"
    donor_status = "Kidney Donor Matched"
 
    navPills(
      navPillsItem(
        pillName = "Age", 
        pillColor = "black",
        pillIcon = NULL, 
        pillText = round(recipient_age, 0),
      ),
      navPillsItem(
        pillName = "ID", 
        pillColor = "black",
        pillIcon = NULL, 
        pillText = recipient_id
      ),
      navPillsItem(
        pillName = "Location", 
        pillColor = if (recipient_status != "Unmatched") "green" else "black",
        pillIcon = NULL, 
        pillText = "NSW"
      ),
      navPillsItem(
        pillName = "Blood Type", 
        pillColor = if (recipient_status != "Unmatched") "green" else "black",
        pillIcon = NULL, 
        pillText = "A+"
      ),
      navPillsItem(
        pillName = "BMI", 
        pillColor = if (recipient_status != "Unmatched") (if(recipient_bmi < donor_bmi) "green" else "yellow") else "black",
        pillIcon = NULL, 
        pillText = round(recipient_bmi, 2),
      ),
      navPillsItem(
        pillName = "Gender", 
        pillColor = if (recipient_status != "Unmatched") (if(recipient_gender == donor_gender) "green" else "red") else "black",
        pillIcon = NULL, 
        pillText = recipient_gender
      ),
      navPillsItem(
        pillName = "Status", 
        pillColor = if (recipient_status != "Unmatched") "green" else "red",
        pillIcon = if (recipient_status != "Unmatched") "fa fa-check" else "fa fa-times", 
        pillText = recipient_status,
        active = TRUE
      )
    )
  })
  
  output$donor_comparison <- renderUI({  
    
    # Only show if patient has match
    if (input$username != "Patient (Before Match)") {
    
    # Recipient Details
    recipient_age = (user_data() %>% select(agetxn))[1,1]
    recipient_id = (user_data() %>% select(Lab_Ref))[1,1]
    recipient_bmi = (user_data() %>% select(recipient_bmi))[1,1]
    recipient_gender = if((user_data() %>% select(Sex_Cat))[1,1] == 0) "Female" else "Male"
    recipient_status = "Kidney Donor Matched"
    
    # Donor Details
    donor_age = (user_data() %>% select(donorage))[1,1]
    donor_bmi = (user_data() %>% select(donor_bmi))[1,1]
    donor_gender = if((user_data() %>% select(DonorGenderCode))[1,1] == 0) "Female" else "Male"
    donor_status = "Kidney Donor Matched"
    
    navPills(
      navPillsItem(
        pillName = "Age", 
        pillColor = "black",
        pillIcon = NULL, 
        pillText = donor_age,
      ),
      navPillsItem(
        pillName = "ID", 
        pillColor = "black",
        pillIcon = NULL, 
        pillText = ""
      ),
      navPillsItem(
        pillName = "Location", 
        pillColor = "green",
        pillIcon = NULL, 
        pillText = "NSW"
      ),
      navPillsItem(
        pillName = "Blood Type", 
        pillColor = "green",
        pillIcon = NULL, 
        pillText = "A+"
      ),
      navPillsItem(
        pillName = "BMI", 
        pillColor = if(recipient_bmi < donor_bmi) "green" else "yellow",
        pillIcon = NULL, 
        pillText = round(donor_bmi, 2),
      ),
      navPillsItem(
        pillName = "Gender", 
        pillColor = if(recipient_gender == donor_gender) "green" else "red",
        pillIcon = NULL, 
        pillText = donor_gender
      ),
      navPillsItem(
        pillName = "Status", 
        pillColor = "green",
        pillIcon = "fa fa-check", 
        pillText = donor_status,
        active = TRUE
      )
    )
    }
    else {
      h3("Awaiting Kidney Match", align = "center")
    }
  })
  
  
  # Risk Calculator Section
  
  # ElliPro Scores Section
  output$ellipro_class1 <- renderValueBox({
    
    # Get Class 1 ElliPro Score
    class1_ElliPro_Score = (user_data() %>% select(class1.total_score))[1,1]
    
    # Predict Risk
    risk = predict(dsa_class1_model() , newdata = data.frame(class1.total_score = class1_ElliPro_Score))
    
    if (risk > 1) {
      valueBox(class1_ElliPro_Score, "Class 1 ElliPro Score", icon("times"), color = "red", width = 12)
    }
    else if (risk < 0.80) {
      valueBox(class1_ElliPro_Score, "Class 1 ElliPro Score", icon("check"), color = "green", width = 12)
    }
    else { 
      valueBox(class1_ElliPro_Score, "Class 1 ElliPro Score", icon("minus"), color = "yellow", width = 12)
    }
  })
  
  output$class1_risk_explanation = renderUI({
    
    # Get Class 1 ElliPro Score
    class1_ElliPro_Score = (user_data() %>% select(class1.total_score))[1,1]
    
    # Predict Risk
    risk = predict(dsa_class1_model() , newdata = data.frame(class1.total_score = class1_ElliPro_Score))
    
    if (risk > 1) {
      HTML(paste("Your risk for Class 1 DSA is <b>High Risk</b>"))
    }
    else if (risk < 0.80) {
      HTML(paste("Your risk for Class 1 DSA is <b>Low Risk</b>"))
    }
    else { 
      HTML(paste("Your risk for Class 1 DSA is <b>Risky</b>"))
    }
  })
  
  output$ellipro_class2 <- renderValueBox({
    
    # Get Class 2 ElliPro Score
    class2_ElliPro_Score = (user_data() %>% select(class2.total_score))[1,1]
    
    # Predict Risk
    risk = predict(dsa_class2_model() , newdata = data.frame(class2.total_score = class2_ElliPro_Score))
    
    if (risk > 1) {
      valueBox(class2_ElliPro_Score, "Class 2 ElliPro Score", icon("times"), color = "red", width = 12)
    }
    else if (risk < 0.80) {
      valueBox(class2_ElliPro_Score, "Class 2 ElliPro Score", icon("check"), color = "green", width = 12)
    }
    else { 
      valueBox(class2_ElliPro_Score, "Class 2 ElliPro Score", icon("minus"), color = "yellow", width = 12)
    }
  })
  
  
  output$class2_risk_explanation = renderUI({
    
    # Get Class 2 ElliPro Score
    class2_ElliPro_Score = (user_data() %>% select(class2.total_score))[1,1]
    
    # Predict Risk
    risk = predict(dsa_class2_model() , newdata = data.frame(class2.total_score = class2_ElliPro_Score))
    
    if (risk > 1) {
      HTML(paste("Your risk for Class 2 DSA is <b>High Risk</b>"))
    }
    else if (risk < 0.80) {
      HTML(paste("Your risk for Class 2 DSA is <b>Low Risk</b>"))
    }
    else { 
      HTML(paste("Your risk for Class 2 DSA is <b>Risky</b>"))
    }
  })
  
  output$pre_dsa <- renderValueBox({
    
    pre_dsa_value = (user_data() %>% select(Pre_DSA))[1,1]
    
    if (pre_dsa_value == 0) {
      valueBox("Low Risk", "Pre DSA", icon("check"), color = "green", width = 12)
    }
    else {
    valueBox("High Risk", "Pre DSA", icon("times"), color = "red", width = 12)
    }
  })
  
  output$pre_dsa_explanation = renderUI({
    
    pre_dsa = (user_data() %>% select(Pre_DSA))[1,1]
    
    if (pre_dsa == 0) {
      HTML(paste("Biopsy revealed <b>no preformed DSA</b>"))
    } 
    else {
      HTML(paste("Biopsy revealed <b>presence of preformed DSA</b>"))
    }
  })
  
  output$amr_valuebox = renderValueBox({
    
    # Get Class 2 ElliPro Score
    class2_ElliPro_Score = (user_data() %>% select(class2.total_score))[1,1]
    
    # Predict Risk
    risk = predict(amr_model() , newdata = data.frame(class2.total_score = class2_ElliPro_Score))
    
    if (risk > 1) {
      valueBox("High Risk", "AMR", icon = icon("times"), color = "red", width = 12)
    }
    else if (risk < 0.80) {
      valueBox("Low Risk", "AMR", icon = icon("check"), color = "green", width = 12)
    }
    else { 
      valueBox("Risky", "AMR", icon = icon("minus"), color = "yellow", width = 12)
    }
  })
  
  output$amr_risk_explanation = renderUI({
    
    # Get Class 2 ElliPro Score
    class2_ElliPro_Score = (user_data() %>% select(class2.total_score))[1,1]
    
    # Predict Risk
    risk = predict(amr_model() , newdata = data.frame(class2.total_score = class2_ElliPro_Score))
    
    if (risk > 1) {
      HTML(paste("Your risk for AMR is <b>High Risk</b> <br><br> AMR is the highest risk factor leading to graft loss."))
    }
    else if (risk < 0.80) {
      HTML(paste("Your risk for AMR is <b>Low Risk</b> <br><br> AMR is the highest risk factor leading to graft loss."))
    }
    else { 
      HTML(paste("Your risk for AMR is <b>Risky</b>  <br><br> AMR is the highest risk factor leading to graft loss."))
    }
  })
  
  output$bmi_valuebox = renderValueBox({
    
    # Get BMI and Genders
    recipient_bmi = (user_data() %>% select(recipient_bmi))[1,1]
    donor_bmi = (user_data() %>% select(donor_bmi))[1,1]
    
    recipient_gender = (user_data() %>% select(Sex_Cat))[1,1]
    donor_gender = (user_data() %>% select(DonorGenderCode))[1,1]
    
    # High BMI is risky
    bmi_risk = if(recipient_bmi < donor_bmi) 1 else 2
    
    # Gender mismatch = 2, else 1
    gender_risk = if(recipient_gender == donor_gender) 1 else 2
    
    risk = bmi_risk * gender_risk
    
    if (risk == 4) {
      valueBox("High Risk", "BMI and Gender", icon = icon("times"), color = "red", width = 12)
    }
    else if (risk == 2) { 
      valueBox("Risky", "BMI and Gender", icon = icon("minus"), color = "yellow", width = 12)
    }
    else if (risk == 1) {
      valueBox("Low Risk", "BMI and Gender", icon = icon("check"), color = "green", width = 12)
    }
  })
  
  
  
  output$bmi_explanation = renderUI({
    
    recipient_bmi = (user_data() %>% select(recipient_bmi))[1,1]
    donor_bmi_bmi = (user_data() %>% select(donor_bmi))[1,1]
    
    if (recipient_bmi < donor_bmi_bmi) {
      str1 <- paste("Your BMI is <span style=\"color:green\"><b>lower than</b></span> donor BMI.")
      str2 <- paste("Patients receiving <b>larger kidneys</b> relative to their body size may have a <b>graft survival advantage</b>.")
      HTML(paste(str1, str2, sep = '<br/><br/>'))
    } 
    else {
      str1 <- paste("Your BMI is <span style=\"color:red\"><b>higher than</b></span> donor BMI.")
      str2 <- paste("Patients receiving <b>smaller kidneys</b> relative to their body size may have a <b>graft survival disadvantage</b>.")
      HTML(paste(str1, str2, sep = '<br/><br/>'))
    }
    
  })
  
  output$gender_explanation = renderUI({
    
    # Get gender
    recipient_gender = (user_data() %>% select(Sex_Cat))[1,1]
    donor_gender = (user_data() %>% select(DonorGenderCode))[1,1]
    
    # Gender mismatch = 2, else 1
    gender_risk = if(recipient_gender==donor_gender) 1 else 2
    
    if (gender_risk == 1) {
      HTML(paste("Your gender <span style=\"color:green\"><b>matches</b></span> with donor gender."))
    } 
    else {
      HTML(paste("Your gender <span style=\"color:red\"><b>does not match</b></span> with donor gender."))
    }
  })
  
  output$ischaemia_valuebox = renderValueBox({
    
    #Ischaemia(t=0) = 0.9285714 (Low Risk)
    #Ischaemia(t=1) = 0.9428571 (Low Risk)
    #Ischaemia(t=2) = 0.9571429 (Low Risk)
    #Ischaemia(t=3) = 0.9714286 (Low Risk)
    #Ischaemia(t=4) = 0.9857143 (Low Risk)
    #Ischaemia(t=5) = 1         (Risky)
    #Ischaemia(t=6) = 1.014286  (High Risk)
    #Ischaemia(t=7) = 1.028571  (High Risk)
    #Ischaemia(t=8) = 1.042857  (High Risk)
    #Ischaemia(t=9) = 1.057143  (High Risk)
    #...
    
    ischaemia_time =  (user_data() %>% select(Ischaemia))[1,1]
    risk = (1/70)*ischaemia_time + (13/14)
    
    if (risk > 1.05) {
      valueBox("High Risk", "Cold Ischameia Time", icon = icon("times"), color = "red", width = 12)
    }
    else if (risk < 1) {
      valueBox("Low Risk", "Cold Ischameia Time", icon = icon("check"), color = "green", width = 12)
    }
    else { 
      valueBox("Risky", "Cold Ischameia Time", icon = icon("minus"), color = "yellow", width = 12)
    }
    
  })
  
  output$ischaemia_explanation = renderUI({
    
    ischaemia_time =  (user_data() %>% select(Ischaemia))[1,1]
    risk = (1/70)*ischaemia_time + (13/14)
    
    if (risk > 1) {
      str1 <- paste("Cold Ischaemia Time for donor kidney: <span style=\"color:red\"><b>",ischaemia_time, "hours</b></span>.")
      str2 <- paste("Cold Ischaemia Time refers to chilling of the kidney after its blood supply has been cut off.")
      str3 <- paste("A <b>high Cold Ischaemia Time</b> may result in an <b>increased risk of graft failure</b>.")
      HTML(paste(str1, str2, str3, sep = '<br/><br/>'))
    } 
    
    else {
      str1 <- paste("Cold Ischaemia Time for donor kidney: <span style=\"color:green\"><b>",ischaemia_time, "hours</b></span>.")
      str2 <- paste("Cold Ischaemia Time refers to chilling of the kidney after its blood supply has been cut off.")
      HTML(paste(str1, str2, sep = '<br/><br/>'))
    }
    
  })
  

  
  # AMR Model
  
  amr_model <- reactive({
    
    df = get_eplet_data()
    
    df_amr_c2 <- df %>% 
      select(class2.total_score)
    df_amr_c2 <- data.frame(df_amr_c2)
    colzero <- which(colSums(df_amr_c2)==0) 
    if (length(colzero)> 0){
      df_amr_c2 <-df_amr_c2[,-which(colSums(df_amr_c2)==0)] #get rid of those always have 0
    }
    
    df_amr_c2 <- cbind(df$DaystoAMRrej,df$AMR_0619,df_amr_c2)
    df_amr_c2 <- rename(df_amr_c2, DaystoAMRrej = 'df$DaystoAMRrej',AMR_0619 = 'df$AMR_0619')
    X <- df_amr_c2
    cvK = 5  # number of CV folds
    n_sim = 25
    cv.means = cv_eval = NA
    for(i in 1:n_sim){
      cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
      cv_eval = NA  # initialise results vector
      for (j in 1:cvK) {
        test_id = cvSets$subsets[cvSets$which == j]
        X_test = X[test_id, ]
        X_train = X[-test_id, ]
        
        amr_model <- coxph(Surv(DaystoAMRrej, AMR_0619 ) ~ class2.total_score, data = X_train)
        cv_eval[j] <- concordance(amr_model, newdata = X_test)$concordance
      }
      cv.means[i] <- mean(cv_eval)
    }
    
    return(amr_model)
  })
  
  dsa_class1_model <- reactive({
    
    df = get_eplet_data()
    
    df_dsa_class1 <- df %>% 
      select(class1.total_score)
    df_dsa_class1 <- data.frame(df_dsa_class1)
    colzero <- which(colSums(df_dsa_class1)==0) 
    if (length(colzero)> 0){
      df_dsa_class1 <-df_dsa_class1[,-which(colSums(df_dsa_class1)==0)] #get rid of those always have 0
    }
    
    df_dsa_class1 <- cbind(df$C1daystodnDSA,df$C1dnDSA ,df_dsa_class1)
    df_dsa_class1 <- rename(df_dsa_class1, C1daystodnDSA = 'df$C1daystodnDSA', C1dnDSA = 'df$C1dnDSA')
    X <- df_dsa_class1
    cvK = 5  # number of CV folds
    n_sim = 25
    cv.means = cv_eval = NA
    for(i in 1:n_sim){
      cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
      cv_eval = NA  # initialise results vector
      for (j in 1:cvK) {
        test_id = cvSets$subsets[cvSets$which == j]
        X_test = X[test_id, ]
        X_train = X[-test_id, ]
        
        dsa_class1_model <- coxph(Surv(C1daystodnDSA, C1dnDSA) ~ class1.total_score, data = X_train)
        cv_eval[j] <- concordance(dsa_class1_model, newdata = X_test)$concordance
      }
      cv.means[i] <- mean(cv_eval)
    }
    
    return(dsa_class1_model)
  })
  
  dsa_class2_model <- reactive({
    
    df = get_eplet_data()
    
    df_dsa_class2 <- df %>% 
      select(class2.total_score)
    df_dsa_class2 <- data.frame(df_dsa_class2)
    colzero <- which(colSums(df_dsa_class2)==0) 
    if (length(colzero)> 0){
      df_dsa_class2 <-df_dsa_class2[,-which(colSums(df_dsa_class2)==0)] #get rid of those always have 0
    }
    
    df_dsa_class2 <- cbind(df$C2daystodnDSA,df$C2dnDSA ,df_dsa_class2)
    df_dsa_class2 <- rename(df_dsa_class2, C2daystodnDSA = 'df$C2daystodnDSA', C2dnDSA = 'df$C2dnDSA')
    X <- df_dsa_class2
    cvK = 5  # number of CV folds
    n_sim = 25
    cv.means = cv_eval = NA
    for(i in 1:n_sim){
      cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
      cv_eval = NA  # initialise results vector
      for (j in 1:cvK) {
        test_id = cvSets$subsets[cvSets$which == j]
        X_test = X[test_id, ]
        X_train = X[-test_id, ]
        
        dsa_class2_model <- coxph(Surv(C2daystodnDSA, C2dnDSA) ~ class2.total_score, data = X_train)
        cv_eval[j] <- concordance(dsa_class2_model, newdata = X_test)$concordance
      }
      cv.means[i] <- mean(cv_eval)
    }
    
    return(dsa_class2_model)
  })
  
  # Calendar Server Section: 
    
    output$selected_month <- renderText({ 
      
      if (is.null(v$month)){
        return("Please Select a date")
      }
      else {
        date = format(as.Date(v$month, '%Y-%m-%d'), format='%B, %Y')
        return(date)
      }
    })
    
    
    date_selected <- reactive({ 
      if (is.null(v$month)){
        return("Please Select a date")
      }
      else {
        date = as_date(v$monthdata[[2]][v$idx,][["day"]],origin = lubridate::origin)
        return(date)
      }
    })
    
    output$date_selected <- renderText({ 
      date = format(as.Date(date_selected(), '%Y-%m-%d'), format='%A %B %d, %Y')
      return(date)
    })
    
    output$next_appointment <- renderText({ 
      
      # Get next appointment date after today
      date = as.Date((v$monthdata[[2]] %>% filter(clin_visit == "TRUE" & day > Sys.Date()) %>% select(day))[1,1])
      date = format(as.Date(date, '%Y-%m-%d'), format='%A %B %d, %Y')
      
      print("Appointments this month:")
      print(v$monthdata[[2]] %>% filter(clin_visit == "TRUE"))
      print("Appointments after today:")
      print(v$monthdata[[2]] %>% filter(clin_visit == "TRUE" & day > Sys.Date()))
      
      if (input$username == "Patient (After Match)") {
        # 4 months from today is next appointment
        date = as.Date((v$monthdata[[2]] %>% filter(clin_visit == "TRUE" & day > Sys.Date() + 120) %>% select(day))[1,1])
        date = format(as.Date(date, '%Y-%m-%d'), format='%A %B %d, %Y')
        
        if (is.na(date)) {
          date = format(as.Date(Sys.Date() + 120, '%Y-%m-%d'), format='%B')
        }
      }
      
      return(date)
    })

    v <- reactiveValues(idx = NULL,monthdata =NULL, month = NULL)

    observe({
        if (!is.null(input$month_selection)){
            v$month = input$month_selection
        }
        click_data <- event_data("plotly_click", source = "call")
        if (is.null(click_data)){
            v$idx <- 8
        }else{
            v$idx <- which(v$monthdata[[2]][['wday_name']] == click_data[["x"]] & v$monthdata[[2]][['week_year']] == click_data[["y"]])
        }
    })
    
    
    # Calender
    output$plot1 <- renderPlotly({
        if (!is.null(v$month)){
            v$monthdata <-  monthdf(v$month)
            #print(v$monthdata)
            colorScale <- data.frame(z=c(0,0.5,0.5,1),col=c("#CDCDCD", "#CDCDCD", "#3C8DBC", "#3C8DBC")) # Changed 
            colorScale$col <- as.character(colorScale$col)
            
            plot_ly(source = "call", z = as.matrix(v$monthdata[[1]]), zmin = 0,type = "heatmap", zmax = 1, x= colnames(v$monthdata[[1]]), y= rownames(v$monthdata[[1]]),
                    xgap = 1, ygap = 1,colorscale = colorScale, colorbar=list(title = "Events", ypad = 30, tickvals=c(0.25,0.75),
                                                                             ticktext = list( "Free Day","Scheduled Events")))  %>% 
                layout(xaxis = list(side ="top", title = ""),yaxis = list(title = "")) %>%  
                style(hoverinfo = 'none') %>%
                add_annotations(x = v$monthdata[[2]][['wday_name']],
                                y = v$monthdata[[2]][['week_year']],
                                text = paste(month(v$monthdata[[2]][['day']],label = TRUE, abbr = TRUE),"-",day(v$monthdata[[2]][['day']])),
                                showarrow = FALSE,
                                ax = 10,
                                ay = -10)
        }
        })
    output$Heading <- renderText({
        if (is.null(v$month)){
            return(NULL)
        }else{
            x = as_date(v$monthdata[[2]][v$idx,][["day"]],origin = lubridate::origin)
            paste("Daily Schedule")
        }
    })
    
    output$clin <- renderText({
        if (is.null(v$month)){
            return(NULL)
        }else{
            if (!is.na(v$monthdata[[2]][v$idx,][["clin_visit"]])) {
                time = format(strptime(v$monthdata[[2]][v$idx,][["clin_time"]], "%H:%M:%S"), "%I:%M %P") # Convert 24 hour time to AM/PM  
                doc = v$monthdata[[2]][v$idx,][["dr_name"]]
                type = v$monthdata[[2]][v$idx,][["clin_visit_type"]]
                
                # If they have not yet been matched
                if (input$username != "Patient (After Match)") {
                  str1 <- paste("Your Clinical visit for today is at <b>", time,"</b>. You will be seeing <b>", doc, "</b> <br>") 
                  str2 <- paste("<b>Appointment Details</b>") 
                  str3 <- paste("<li>Blood Test</li>") 
                  str4<- paste("<li>Chest X-Ray</li>") 
                  str5 <- paste("<li>ECHO (Echocardiogram)</li>")
                  
                  HTML(paste(str1, str2, str3, str4, str5, sep = '<br/><br/>'))
                }
                
                # Else matched - Show Blood Test or Physical Therapy
                else {
                  # Only show appintments 4 months in the future
                  if (as.Date(date_selected(), '%Y-%m-%d') > as.Date(Sys.Date() + 120, '%Y-%m-%d')) {
                    str1 <- paste("Your Clinical visit for today is at <b>", time,"</b>. You will be seeing <b>", doc, "</b> <br>")
                    str2 <- paste("<b>Appointment Details</b>") 
                    str3 <- paste("<li>",type,"</li>") 
                    
                    HTML(paste(str1, str2, str3, sep = '<br/><br/>'))
                  }
                  else {
                    paste("<span style=\"color:red\">You have no Clinical Appointments Today!</span>")
                  }
                }
                
                
            } else {
              paste("<span style=\"color:red\">You have no Clinical Appointments Today!</span>")
            }
        }
        
    })
    
    output$dia <- renderText({
        if (is.null(v$month)){
            return(NULL)
        }else{
            if (!is.na(v$monthdata[[2]][v$idx,][["dial"]])) {
                time = v$monthdata[[2]][v$idx,][["dial_time"]]
                paste("You have a <b>", time, " hour </b> Dialysis Session today.")
            } else {
              paste("<span style=\"color:red\">You have no required Dialysis Sessions Today!</span>")
            }
        }
    })
    
    output$medication <- renderUI({
      if (is.null(v$month)){
        return(NULL)
      }else{
        if (TRUE) {
          
          str1 <- paste("Take Sirolimus <b> ONCE a day</b>")
          str2 <- paste("<b>    9:00am: </b> Take Sirolimus <br>")
          
          str3<- paste("Take Tacrolimus <b> TWICE a day</b>")
          str4 <- paste("<b>    9:00am: </b> Take Tacrolimus")
          str5 <- paste("<b>    9:00pm: </b> Take Tacrolimus")
          
          HTML(paste(str1, str2, str3, str4, str5, sep = '<br/><br/>'))
          
        } else {
          paste("No medications are needed today")
        }
      }
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
