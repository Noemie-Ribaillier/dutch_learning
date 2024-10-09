## DUTCH TO ENGLISH TRANSLATION APPLICATION

# Load the needed libraries
library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(readODS)


# Verbs with their translations
verbs_excel <- data.frame(read_ods('C:/Users/Admin/Documents/Projets R/data/translation_dutch.ods',sheet ='verbs'))
colnames(verbs_excel) <- c('dutch_verb','english_verb')
verbs_excel[,c('attempts','successes')] <- 0
verbs_excel$success_rate <- NA
head(verbs_excel)


# Define UI
ui <- fluidPage(
  # title of the app
  titlePanel("Dutch to English Translation Application"),
  # left side of the page
  sidebarLayout(
    sidebarPanel(
      # verb to translate, chosen randomly by the app (htmlOutput allows to change the font)
      htmlOutput("verb_to_translate"),
      # text box where the user enters the translation
      textInput("verb_translated", NULL), 
      # button to check the translation
      actionButton("check", "Check Translation",icon('circle-check'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      # button to display a verb in the 1st use then to go to the next verb for the next uses
      actionButton("next_verb", "Display a verb",icon('circle-arrow-right'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      # check if the translation is correct or not
      htmlOutput("translation_check"),
      # break line
      br(),
      # display the global statistics: total number of attempts and successes, and the total success rate
      htmlOutput("global_statistics"),
      # break line
      br(),
      # download button, to download the output table with statistics per verb
      downloadButton('download',label = 'Download the statistics',icon('download'),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    # main side of the page
    mainPanel(
      # title of the main page
      h3('All verbs - statistics'),
      # options to remove the search zone
      tags$head(tags$style(type="text/css", ".dataTables_filter {display: none;    }")),
      # output table, summing up for each verb the translation, number of attempts, successes and success rate
      DTOutput("table_verbs_statistics")
    )
  )
)


# Define server 
server <- function(input, output, session) {
  
  # Reactive value to store verbs data
  verbs_data <- reactiveVal(verbs_excel)
  
  # Reactive value to store the verb to translate
  verb_to_translate <- reactiveVal(NULL)
  
  # When clicking on the button "next", select a random verb
  observeEvent(input$next_verb, {
    # Get a random index, between 1 and the number of verbs
    random_index <- sample(nrow(verbs_data()), 1)
    # Pick the verb, based on the index
    verb_to_translate(verbs_data()$english_verb[random_index])
    # Display the verb to translate in the output
    output$verb_to_translate <- renderText(paste("Translate:<b>", verb_to_translate(),'</b>'))
    # Re-new the text box (to have it back to blank)
    updateTextInput(session, "verb_translated", value = "")
  })
  
  # When clicking on the button "check", check for the translation and update the statistics
  observeEvent(input$check, {
    ################################################# TO CHECK IF NEEDED
    # req(verb_to_translate())
    # 
    # user_translation <- input$verb_translated
    current_data <- verbs_data()
    
    # Update attempts for the selected verb (verb to translate) [+1]
    current_data[current_data$english_verb == verb_to_translate(), "attempts"] <- current_data[current_data$english_verb == verb_to_translate(), "attempts"] + 1
    
    # Check translation
    correct_translation <- current_data[current_data$english_verb == verb_to_translate(), "dutch_verb"]
    if (input$verb_translated == correct_translation) {
      # Update the successes for the selected verb (verb to translate), if correct [+1]
      current_data[current_data$english_verb == verb_to_translate(), "successes"] <- current_data[current_data$english_verb == verb_to_translate(), "successes"] + 1
      # Update the output with correct and green if correct
      output$translation_check <- renderText({ paste("<font color=\"#008000\"><b>", 'Correct!', "</b></font>") })
    } else {
      # Update the output with incorrect and red if not correct
      output$translation_check <- renderText({ paste("<font color=\"#FF0000\"><b>", 'Incorrect!', "</b></font>",' It should be <b>',correct_translation,'</b>') })
    }
    
    # Update the success rate
    current_data[current_data$english_verb == verb_to_translate(), "success_rate"] <- round(current_data[current_data$english_verb == verb_to_translate(), "successes"]*100/current_data[current_data$english_verb == verb_to_translate(), "attempts"],0)
    
    # Update the global statistics by doing the sum over the statistics per verb
    output$global_statistics <- renderText({ paste('Successes: ','<b>',sum(current_data$successes),
                                         '</b>',' | Attempts: ','<b>',sum(current_data$attempts),
                                         '</b>',' | Success Rate: ','<b>',round(sum(current_data$successes)*100/sum(current_data$attempts)),'%</b>') })
    
    # Update initial verbs data
    verbs_data(current_data)
  })
  
  # Set up the output dataframe (in the main page)
  output$table_verbs_statistics <- renderDataTable({
    # We change the column names and remove the rownames
    datatable(verbs_data(), colnames = c('Dutch','English','Attempts','Successes','Success Rate'),rownames = NULL) %>% 
      # Success rate is blank if the verb hasn't been selected yet, red if the success rate is between 0 and 60%, green above
      formatStyle('success_rate',backgroundColor=styleInterval(cuts = c(60),values = c('red','green')))
  })
  
  # Set up the download button
  output$download <- downloadHandler(
    # Filename is the date pasted with some name
    filename = function() {paste(gsub(pattern = '-',replacement = '_',x = Sys.Date()),'translation_verbs_statistics.csv', sep='_')},
    # Content is the verbs table (with statistics per verb)
    content = function(con) {write.csv(verbs_data(), con,row.names=F)}
    )
  
  # Update the button: "display" for the 1st use then for the next uses "next"
  observeEvent(input$next_verb, {
    updateActionButton(session, "next_verb", label = 'Next verb')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




# IMPROVEMENTS
# put a higher proba to verbs not succeeded
# themes
