############################################################################################################
#####                                                                                                  #####
#####                            APP DUTCH - ENGLISH TRANSLATION/CONJUGATION                           #####
#####                                         RIBAILLIER NOEMIE                                        #####
#####                                      Created on: 2024-10-15                                      #####
#####                                      Updated on: 2025-03-06                                      #####
#####                                                                                                  #####
############################################################################################################

# Clear the whole environment 
rm(list=ls())

# Load the libraries
library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(readODS)

# Set up the right directory
setwd("C:/Users/Admin/Documents/Projets R/dutch_learning")

# Select the file of the Dutch-English translations/conjugations
file <- "translation_conjugation_dutch.ods"

# Define UI
ui <- fluidPage(
  # Set up the title of the app
  titlePanel("Dutch to English Translation/Conjugation Application"),
  # Set up the left side of the page
  sidebarLayout(
    sidebarPanel(
      # Set up the list (to select the theme)
      selectizeInput(inputId = "theme", label = "Theme:", choices ='Rest words'),
      # Display the word to translate/conjugate (htmlOutput allows to change the font)
      htmlOutput("word_to_translate_conjugate"),
      # Set up the text box (where the user enters the answer, empty by default)
      textInput("answer", NULL), 
      # Set up the button to check the translation (with a specific color and icon)
      actionButton("check", "Check", icon('circle-check'), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      # Set up the button to display a word in the 1st use then to go to the next word for the next uses (with a specific color and icon)
      actionButton("next_word", "Display a word",icon('circle-arrow-right'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      # Display if the answer is correct or not
      htmlOutput("answer_check"),
      # Display a break line
      br(),
      # Display the global statistics: total number of attempts and successes, and the total success rate 
      htmlOutput("global_statistics"),
      # Display a break line
      br(),
      # Set up the download button (with a specific icon and color)
      downloadButton('download', label = 'Download the statistics', icon('download'),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    # Set up the main side of the page
    mainPanel(
      # Set up the title of the main page
      h3('All words - statistics'),
      # Set up options to remove the search zone
      tags$head(tags$style(type="text/css", ".dataTables_filter {display: none;    }")),
      # Display the output table, summing up for each word the translation/conjugation, number of attempts, successes and success rate
      DTOutput("table_words_statistics")
    )
  )
)


# Define server
server <- function(input, output, session) {
  
  # Read the sheet names from the Excel file and update the selectInput (with the list of themes)
  themes <- sort(list_ods_sheets(file))
  updateSelectInput(session, "theme", choices = themes)
  
  # Reactive value to store words data
  words_data <- reactiveVal(NULL)

  # Reactive value to store the word to translate/conjugate
  word_to_translate_conjugate <- reactiveVal(NULL)
  
  # Re-execute whenever the input theme changes
  observe({
    # Read the data from the selected sheet
    data <- read_ods(file,sheet  = input$theme)
    # Set up the right column names
    colnames(data) <- c('dutch_word','english_word')
    # Set up the statistics (attempts and successes) to 0
    data[,c('attempts','successes')] <- 0
    # Set up the statistics (success rate) to NA
    data$success_rate <- NA
    words_data(data)
  })
  
  # Select a random word when clicking on the button "next_word"
  observeEvent(input$next_word, {
    # Get a random index, between 1 and the number of words (higher probability for people not yet selected or failed)
    random_index <- sample(x = nrow(words_data()), size = 1,prob = ifelse(is.na(words_data()$success_rate),100,100-words_data()$success_rate))
    # Pick the word, based on the index
    word_to_translate_conjugate(words_data()$english_word[random_index])
    # Display the word to translate/conjugate in the output
    output$word_to_translate_conjugate <- renderText(ifelse(input$theme=='verbs_present',paste("Conjugate:<b>", word_to_translate_conjugate(),'</b>'),paste("Translate:<b>", word_to_translate_conjugate(),'</b>')))
    # Re-new the text box (to have it back to blank)
    updateTextInput(session, "answer", value = "")
  })
  
  # Check for the translation/conjugation and update the statistics after clicking on the button "check"
  observeEvent(input$check, {
    
    # Get the selected words data/statistics
    current_data <- words_data()
    
    # Add 1 to the attempts variable for the selected word (word to translate/conjugate)
    current_data[current_data$english_word == word_to_translate_conjugate(), "attempts"] <- current_data[current_data$english_word == word_to_translate_conjugate(), "attempts"] + 1
    
    # Get the correct answer
    correct_answer <- current_data[current_data$english_word == word_to_translate_conjugate(), "dutch_word"]
    # Check the answer
    if (input$answer == correct_answer) {
      # Add 1 to successes variable for the selected word (if correct)
      current_data[current_data$english_word == word_to_translate_conjugate(), "successes"] <- current_data[current_data$english_word == word_to_translate_conjugate(), "successes"] + 1
      # Update the output with correct and green if correct
      output$answer_check <- renderText({ paste("<font color=\"#008000\"><b>", 'Correct!', "</b></font>") })
    } else {
      # Update the output with incorrect (and the correct answer) and red if not correct
      output$answer_check <- renderText({ paste("<font color=\"#FF0000\"><b>", 'Incorrect!', "</b></font>",' It should be <b>',correct_answer,'</b>') })
    }
    
    # Update the success rate
    current_data[current_data$english_word == word_to_translate_conjugate(), "success_rate"] <- round(current_data[current_data$english_word == word_to_translate_conjugate(), "successes"]*100/current_data[current_data$english_word == word_to_translate_conjugate(), "attempts"],0)
    
    # Update the global statistics by doing the sum over the statistics for all words
    output$global_statistics <- renderText({ paste('Successes: ','<b>',sum(current_data$successes),
                                                   '</b>',' | Attempts: ','<b>',sum(current_data$attempts),
                                                   '</b>',' | Success Rate: ','<b>',round(sum(current_data$successes)*100/sum(current_data$attempts)),'%</b>') })
    
    # Update initial words data
    words_data(current_data)
  })

  # Set up the output dataframe (in the main page)
  output$table_words_statistics <- renderDataTable({
    # Change the column names and remove the rownames
    datatable(words_data(), colnames = c('Dutch','English','Attempts','Successes','Success Rate'),rownames = NULL) %>% 
      # Success rate is blank if the word hasn't been selected yet, red if the success rate is between 0 and 60%, green above
      formatStyle('success_rate',backgroundColor=styleInterval(cuts = c(60),values = c('red','green')))
  })

  # Create the download button
  output$download <- downloadHandler(
    # Filename is the date pasted with "words_statistics.csv"
    filename = function() {paste(gsub(pattern = '-',replacement = '_',x = Sys.Date()),'words_statistics.csv', sep='_')},
    # Content is the words table (with statistics per word)
    content = function(con) {write.csv(words_data(), con, row.names=F)}
  )
  
  # Update the button after it's been clicked once ("Display a word" for the 1st use then for the next uses "Next word")
  observeEvent(input$next_word, {
    updateActionButton(session, "next_word", label = 'Next word')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
