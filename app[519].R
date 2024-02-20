#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyMobile)
library(yonder)
library(stringr)
library(NLP) ##Natural Language Processing 
library(tm) ##Pour le traitement de du dictionaire
library("RWeka")

### Fonctions utiles
#n_gram_tokenizer <- function(x, min, max) NGramTokenizer(x, Weka_control(min = min, max = max))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Les données
v_corpus = VCorpus(DirSource("C:\Users\ALOYS AYMRICK\Desktop\Dossier de Stage\Projet R 2022\Bibliotheque")) ## Changer le chemin d'accès
data_lower_case <- tm_map(v_corpus, content_transformer(tolower))
data <- tm_map(data_lower_case, removePunctuation)

# n-grams 1 <= n <= 3
#n_grams <- DocumentTermMatrix(data, control = list(tokenize = function(x) n_gram_tokenizer(x, 1, 3)))
Data_bigrams <- DocumentTermMatrix(data, control = list(tokenize = BigramTokenizer))

# Les fréquences
#freq <- sort(colSums(as.matrix(n_grams)), decreasing=TRUE)
bigram_frequency <- sort(colSums(as.matrix(Data_bigrams)), 
                         decreasing=TRUE)

# Dictionnaire
dict = names(bigram_frequency)
dict
##Todo
# Les mots initiaux sont les plus fréquents dans le dictionnaire,
# rangés dans l'ordre décroisant des fréquences.
initialWords = names(bigram_frequency[1:3])
words = initialWords

# Define UI for application that draws a histogram
ui <- f7Page(
  # Le titre de l'application
  title = 'Predict the next word',
  
  # Pour une application à une page
  f7SingleLayout(
    navbar = f7Navbar(
      title = 'Predict next word',
      hairline = FALSE,
      shadow = TRUE
    ),
    
    # Les mots proposés
    f7Card(
      title = 'Proposed words',
      f7Row(
        gap = TRUE,
        
        # Bouton du premier mot
        f7Col(
          f7Button(
            inputId = 'bt1', 
            label = words[1], 
            fill = FALSE, 
            outline = TRUE, 
            size = 'small',
            rounded = TRUE,
          ),
        ),
        
        # Bouton du second mot
        f7Col(
          f7Button(
            inputId = 'bt2', 
            label = words[2], 
            fill = FALSE, 
            outline=TRUE, 
            size='small',
            rounded = TRUE,
          ),
        ),
        
        # Buton du troisième mot
        f7Col(
          f7Button(
            inputId = 'bt3', 
            label = words[3], 
            fill = FALSE, 
            outline=TRUE, 
            size="small",
            rounded = TRUE,
          ),
        ),
      ),
    ),
    
    # Zone de saisi du text
    f7Card(
      f7TextArea(inputId = 'text', label = 'Input a text', value="", placeholder = 'Enter your text'),
    ),
  ),
)

##Todo
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Réagir aux changements du texte saisi
  observeEvent(input$text, {
    
    x <- input$text
    
    ##Todo
    # Vous pouvez aussi analyser le mot en cours de saisi
    # pour propser en premier le mot le plus proche
    {
      # Le dernier mot en cours de saisi
      lastWord = stringr::word(x, -1)
      
      # Faire le traitement ici
      if(lastWord != ""){
        print(lastWord) # A remplacer
      }
    }
    
    # Détecter l'espace, qui déclanche la recherche des 3 mots à proposer
    if(str_sub(x, start = -1) == " "){
      ##Todo
      # 2 derniers mots si plus de deux mots
      # Le dernier mot si un seul mot
      last2words = stringr::word(trimws(x), start = -2, end = -1)
      
      if(is.na(last2words)){
        last2words = stringr::word(trimws(x), start = -1, end = -1)
      }
      
      # Fréquences des expressions avec les mots précédents
      expressions = sort(bigram_frequency[dict[str_starts(dict, paste(last2words, " ", sep=""))]], decreasing = TRUE)
      n_expenssions = length(expressions)
      
      # Nouveaux mots à proposer
      new_words = sapply(names(expressions)[1:min(3, n_expenssions)], function(x) stringr::word(x, start=-1, end=-1))
      
      # Compléter si nécessaire avec les mots les plus fréquents
      
      if(length(new_words) < 3){
        if(is.na(new_words[1])){
          new_words = c()
        }
        new_words = c(new_words, initialWords)[1:3]
      }
      
      print(as.vector(new_words))
      
      ##Todo
      # Déterminer les trois mots les plus probables qui suivent last2words
      # et les affecter dans 'words'
      # <<- permet de modifier la variable globale
      words <<- as.vector(new_words)
      
      # Mettre à jour les label des bouttons
      updateActionButton(session, "bt1", label = words[1])
      updateActionButton(session, "bt2", label = words[2])
      updateActionButton(session, "bt3", label = words[3])
    }
  })
  
  # Ajoute le premier mot proposé au text en cours de saisi
  observeEvent(input$bt1, {
    x <- input$text
    updateTextInput("text", value = paste(x, words[1]), session=session)
  })
  
  # Ajoute le second mot proposé au text en cours de saisi
  observeEvent(input$bt2, {
    x <- input$text
    updateTextInput("text", value = paste(x, words[2]), session = session)
  })
  
  # Ajoute le troisième mot proposé au text en cours de saisi
  observeEvent(input$bt3, {
    x <- input$text
    updateTextInput("text", value = paste(x, words[3]), session = session)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
