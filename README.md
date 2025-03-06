# Dutch-English learning application 

## Project description
This project aims at building an R Shiny application aiming at learning Dutch language (translation and conjugation).

## Data
We use an Excel file gathering Dutch-English translation and conjugation.

## Script details
We build the UI (front end) and the server (back end).

The application is split into the left part and the main part. The UI sets up the following:
* set up the title of the application: "Dutch to English Translation/Conjugation Application"
* left part of the application: it displays the word to translate/conjugate, the user writes its answer and the app verifies the answer. The left part of the app:
    * sets up the list to select the themes (for example colors, verbs, numbers etc)
    * displays the word to translate/conjugate. The word is chosen randomly (but words that have not been selected during this session yet or which have been selected and have been failed get a higher probability to be chosen).
    * sets up the text box where the user enters the answer. It's empty by default.
    * sets up the button to verify the answer. It's called "Check", it has a specific "circle-check" icon and specific blue color
    * sets up the button to display a word in the 1st use or to go to the next word for the next uses. It's called "Display a word" or "Next word", it has a specific "circle-arrow-right" icon and a specific blue color
    * displays if the answer is correct or not
    * displays the global statistics: the total number of attempts and successes, and the total success rate 
    * sets up a download button. It's called "Download the statistics" and it has a specific "download" icon and a specific blue color. It aims at downloading the statistics per word if the user wants to keep track of the words he succeeded and the words he didn't succeed
* main part of the application: it displays the statistics per word. The main part of the app:
    * sets up the title: "All words - statistics"
    * displays the table summing up the statistics (word, answer, number of attempts, successes and success rate) per word

The server has the following specificities:
* the user selects first the theme he/she wants to study. These themes are read from the Excel file and gathered in a drop down list.
* when clicking on the button "Display a word" (1st time) or "Next word" (next times), the app selects a random word to ask to the user. It's either a translation exercise or a conjugation (when the theme is about verbs) exercise. The word is picked randomly but the probability to be picked increase if the word has never been selected or depending on the fail rate.
* when clicking on "Check" button, the app checks for the correct translation/conjugation. It displays a green "Correct!" in case of success and a red "Incorrect!" with the right answer in case of failure
* the global statistics and statistics per word are then updated: attempts always increase by 1, number of successes increase by 1 when the answer is correct and the success rate is computed as the number of successes divided by the number of attempts. There are 2 types of statistics:
    * the global statistics (per theme): displayed on the left part. They are put back to 0 when the user changes theme
    * the statistics per word, displayed on the main page (table with all the words, their translation or conjugation [depending on the theme], and the statistics per word).
* a download button is available if the user wishes to download the statistics per word (to keep track of the words he/she succeeded and the words he/she didn't succeed)
