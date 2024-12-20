
####################################################################################################################################
#####                                                                                                                          #####
#####                                                   APP DUTCH - ENGLISH                                                    #####
#####                                                    RIBAILLIER NOEMIE                                                     #####
#####                                              LAST MODIFICATION: 2024-12-20                                              `#####
#####                                                                                                                          #####
####################################################################################################################################


# Load the needed libraries
library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(readODS)

# Set the Excel file to a library
file <- "C:/Users/Admin/Documents/Projets R/dutch_learning/translation_dutch.ods"


# Define UI
ui <- fluidPage(
  # title of the app
  titlePanel("Dutch to English Translation Application"),
  # left side of the page
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "theme", label = "Theme:",choices ='Verbs'),
      # word to translate, chosen randomly by the app (htmlOutput allows to change the font)
      htmlOutput("word_to_translate"),
      # text box where the user enters the translation
      textInput("word_translated", NULL), 
      # button to check the translation
      actionButton("check", "Check",icon('circle-check'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      # button to display a word in the 1st use then to go to the next word for the next uses
      actionButton("next_word", "Display a word",icon('circle-arrow-right'),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      # check if the translation is correct or not
      htmlOutput("translation_check"),
      # break line
      br(),
      # display the global statistics: total number of attempts and successes, and the total success rate
      htmlOutput("global_statistics"),
      # break line
      br(),
      # download button, to download the output table with statistics per word
      downloadButton('download',label = 'Download the statistics',icon('download'),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    # main side of the page
    mainPanel(
      # title of the main page
      h3('All words - statistics'),
      # options to remove the search zone
      tags$head(tags$style(type="text/css", ".dataTables_filter {display: none;    }")),
      # output table, summing up for each word the translation, number of attempts, successes and success rate
      DTOutput("table_words_statistics")
    )
  )
)


# Define server 
server <- function(input, output, session) {
  
  # Read the sheet names from the Excel file and update the selectInput
  themes <- sort(list_ods_sheets(file))
  updateSelectInput(session, "theme", choices = themes)
  
  words_data <- reactive({

    data <- read_ods(file,sheet  = input$theme)  # Read the data from the selected sheet
    colnames(data) <- c('dutch_word','english_word')
    data[,c('attempts','successes')] <- 0
    data$success_rate <- NA
    return(data)
  })
    
  # Reactive value to store words data
  words_data <- reactiveVal(NULL)

  # Reactive value to store the word to translate
  word_to_translate <- reactiveVal(NULL)
  
  observe({

    data <- read_ods('C:/Users/Admin/Documents/Projets R/dutch_learning/translation_dutch.ods',sheet  = input$theme)  # Read the data from the selected sheet
    colnames(data) <- c('dutch_word','english_word')
    data[,c('attempts','successes')] <- 0
    data$success_rate <- NA
    words_data(data)
  })
  
  
  # When clicking on the button "next", select a random word
  observeEvent(input$next_word, {
    # Get a random index, between 1 and the number of words
    random_index <- sample(x = nrow(words_data()), size = 1,prob = ifelse(is.na(words_data()$success_rate),100,100-words_data()$success_rate))
    # Pick the word, based on the index
    word_to_translate(words_data()$english_word[random_index])
    # Display the word to translate in the output
    output$word_to_translate <- renderText(ifelse(input$theme=='verbs_present',paste("Conjugate:<b>", word_to_translate(),'</b>'),paste("Translate:<b>", word_to_translate(),'</b>')))
    # Re-new the text box (to have it back to blank)
    updateTextInput(session, "word_translated", value = "")
  })
  
  # When clicking on the button "check", check for the translation and update the statistics
  observeEvent(input$check, {
    
    current_data <- words_data()
    
    # Update attempts for the selected word (word to translate) [+1]
    current_data[current_data$english_word == word_to_translate(), "attempts"] <- current_data[current_data$english_word == word_to_translate(), "attempts"] + 1
    
    # Check translation
    correct_translation <- current_data[current_data$english_word == word_to_translate(), "dutch_word"]
    if (input$word_translated == correct_translation) {
      # Update the successes for the selected word (word to translate), if correct [+1]
      current_data[current_data$english_word == word_to_translate(), "successes"] <- current_data[current_data$english_word == word_to_translate(), "successes"] + 1
      # Update the output with correct and green if correct
      output$translation_check <- renderText({ paste("<font color=\"#008000\"><b>", 'Correct!', "</b></font>") })
    } else {
      # Update the output with incorrect and red if not correct
      output$translation_check <- renderText({ paste("<font color=\"#FF0000\"><b>", 'Incorrect!', "</b></font>",' It should be <b>',correct_translation,'</b>') })
    }
    
    # Update the success rate
    current_data[current_data$english_word == word_to_translate(), "success_rate"] <- round(current_data[current_data$english_word == word_to_translate(), "successes"]*100/current_data[current_data$english_word == word_to_translate(), "attempts"],0)
    
    # Update the global statistics by doing the sum over the statistics per word
    output$global_statistics <- renderText({ paste('Successes: ','<b>',sum(current_data$successes),
                                                   '</b>',' | Attempts: ','<b>',sum(current_data$attempts),
                                                   '</b>',' | Success Rate: ','<b>',round(sum(current_data$successes)*100/sum(current_data$attempts)),'%</b>') })
    
    # Update initial words data
    words_data(current_data)
  })
  
  # Set up the output dataframe (in the main page)
  output$table_words_statistics <- renderDataTable({
    # We change the column names and remove the rownames
    datatable(words_data(), colnames = c('Dutch','English','Attempts','Successes','Success Rate'),rownames = NULL) %>% 
      # Success rate is blank if the word hasn't been selected yet, red if the success rate is between 0 and 60%, green above
      formatStyle('success_rate',backgroundColor=styleInterval(cuts = c(60),values = c('red','green')))
  })
  
  # Set up the download button
  output$download <- downloadHandler(
    # Filename is the date pasted with some name
    filename = function() {paste(gsub(pattern = '-',replacement = '_',x = Sys.Date()),'translation_words_statistics.csv', sep='_')},
    # Content is the words table (with statistics per word)
    content = function(con) {write.csv(words_data(), con,row.names=F)}
  )
  
  # Update the button: "display" for the 1st use then for the next uses "next"
  observeEvent(input$next_word, {
    updateActionButton(session, "next_word", label = 'Next word')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



