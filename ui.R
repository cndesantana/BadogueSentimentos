fluidPage(
   titlePanel("Badogue Sentimentos"),
   
   # Sidebar with controls to select the random distribution type
   # and number of observations to generate. Note the use of the
   # br() element to introduce extra vertical spacing
   sidebarLayout(
      sidebarPanel(
         textInput("workdir", 
                   "Escreva o endereço do Diretório onde guardar as saídas:", 
                   value = "C:/..."
         ),
         fileInput('file', 'Escolha o Arquivo EXCEL',
                   accept=c('.xlsx')),
         tags$hr(),
         actionButton("do", "Badogar")
      ),
      mainPanel(
         tabsetPanel(type = "tabs",
                     tabPanel("Distribuição de Sentimentos", plotOutput("plotSentimentos")),
                     tabPanel("Lista de Palavras", plotOutput("plotLista")),
                     tabPanel("Palavras Positivas", plotOutput("plotPalavrasPositivas")),                     
                     tabPanel("Palavras Negativas", plotOutput("plotPalavrasNegativas")),
                     tabPanel("Palavras Neutras", plotOutput("plotPalavrasNeutras")),
                     tabPanel("Nuvem de Palavras", plotOutput("plotNuvem")),
                     tabPanel("Gênero da Audiência", plotOutput("plotGenero"))
         )
      )
   )
)


