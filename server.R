library(xlsx)
library(gdata)
library(readxl)
library(tidyverse)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(tm)
library(wordcloud)
library(stylo)
library(tidytext)
library(xlsx)
library(gdata)
library(readxl)
library(lubridate)
source("./auxFunctions.R")


function(input, output) {
   
   # Reactive expression to generate the requested distribution.
   # This is called whenever the inputs change. The output
   # functions defined below then all use the value computed from
   # this expression
   observeEvent(input$do, {
      resultsdir <- file.path(workdir,paste("results_",format(Sys.Date(), "%Y%m%d"),sep=""));
      if(!file.exists(resultsdir))
         dir.create(resultsdir)
      
      cat(paste('Thank you for clicking ',input$datapath,sep=""))
      filepath <- input$file$datapath
      cat(filepath,sep="\n")
      withProgress(message = 'Badogando post...', value = 0, {

#Plot all the words
      incProgress(1/7, detail = "Plotando todas as palavras...")

      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      allmessages <- toupper(file$Conteúdo)
         
      stblUnigrm <- fromCommentsToUnigram(allmessages)
      top20unig<-stblUnigrm[1:20,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "gray50" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
         
      png(file.path(resultsdir,"todaspalavras.png"),width=1980,height=1280,res=300)
      print(p)
      dev.off();
      cat(paste("Criado arquivo ", file.path(resultsdir,"todaspalavras.png"),sep=""),sep="\n")

      
      
#Plot the Positive words      
      incProgress(2/7, detail = "Plotando palavras mais positivas...")

      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      positivas <- which(toupper(file$Polaridade) == "POSITIVO")
      allmessages <- toupper(file$Conteúdo)[positivas]
      
      stblUnigrm <- fromCommentsToUnigram(allmessages)
      top20unig<-stblUnigrm[1:15,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "green" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      png(file.path(resultsdir,"palavraspositivas.png"),width=1980,height=1280,res=300)
      print(p)      
      
      dev.off();
      cat(paste("Criado arquivo ", file.path(resultsdir,"palavraspositivas.png"),sep=""),sep="\n")

      
#Plot the Negative words
      incProgress(3/7, detail = "Plotando palavras mais negativas...")
      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      neutras <- which(toupper(file$Polaridade) == "NEGATIVO")
      allmessages <- toupper(file$Conteúdo)[neutras]
      
      stblUnigrm <- fromCommentsToUnigram(allmessages)
      maxUnigrm <- ifelse(length(stblUnigrm[,1]) >= 15, 15, length(stblUnigrm[,1]))
      top20unig<-stblUnigrm[1:maxUnigrm,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "red" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      png(file.path(resultsdir,"palavrasnegativas.png"),width=1980,height=1280,res=300)
      print(p)
      dev.off();
      cat(paste("Criado arquivo ", file.path(resultsdir,"palavrasnegativas.png"),sep=""),sep="\n")   

      
      
#Plot the Neutral words      
      incProgress(4/7, detail = "Plotando palavras mais neutras...") 
      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      neutras <- which(toupper(file$Polaridade) == "NEUTRO")
      allmessages <- toupper(file$Conteúdo)[neutras]
      
      stblUnigrm <- fromCommentsToUnigram(allmessages)
      top20unig<-stblUnigrm[1:15,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "blue" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      png(file.path(resultsdir,"palavrasneutras.png"),width=1980,height=1280,res=300)
      print(p)
      dev.off();
      cat(paste("Criado arquivo ", file.path(resultsdir,"palavrasneutras.png"),sep=""),sep="\n")      

      
#Plot the Wordcloud
      incProgress(5/7, detail = "Plotando Wordcloud...") 
      filepath <- input$file$datapath
      cat(str(input),sep="\n")
      file <- read_xlsx(filepath)
      allmessages <- toupper(file$Conteúdo)
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      tdm <- TermDocumentMatrix(SampCrps)
      m <- as.matrix(tdm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      pal <- brewer.pal(9, "BuGn")
      pal <- pal[-(1:2)]
      pal2 <- brewer.pal(8,"Dark2")
      png(file.path(resultsdir,"wordcloud.png"),width=1980,height=1280,res=300)
      wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.75, colors=pal2, vfont=c("sans serif","plain"))
      dev.off();
      cat(paste("Criado arquivo ", file.path(resultsdir,"wordcloud.png"),sep=""),sep="\n")


      
#Plot the Sentiment Distribution            
      incProgress(6/7, detail = "Plotando Distribuição de Sentimentos...")       
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      sentimentos <- toupper(file$Polaridade)
      
      numsentimentos <- sentimentos[order(sentimentos)];
      mr <- table(numsentimentos)[1];
      mn <- table(numsentimentos)[2];
      mp <- table(numsentimentos)[3];
      mt <- mr + mn + mp;
      
      indicesentimento <- ifelse(mn == 0, 0, as.numeric((mp-mr)/(mt-mn)))
      
      p <- ggplot() + geom_bar(stat="identity", aes(x = unique(sort(sentimentos)),y=100/mt*as.numeric(table(numsentimentos))), fill = c("red","blue","green")) + xlab("Sentimentos") + ylab("Número de Comentários") + coord_flip() 
      png(file.path(resultsdir,"sentimentos.png"),width=1980,height=1280,res=300)
      print(p)
      dev.off();
      cat(signif(indicesentimento,3), file = file.path(resultsdir,"indicedesentimentos.txt"),sep="\n")         

      incProgress(7/7, detail = "Badogada Perfeita (7/7)! :)")
      
      Sys.sleep(3)
      })
   })

   # Generate a plot of the data. Also uses the inputs to build
   # the plot label. Note that the dependencies on both the inputs
   # and the data reactive expression are both tracked, and
   # all expressions are called in the sequence implied by the
   # dependency graph

   output$plotNuvem <- renderPlot({
      filepath <- input$file$datapath
      cat(str(input),sep="\n")
      file <- read_xlsx(filepath)
      allmessages <- toupper(file$Conteúdo)
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      tdm <- TermDocumentMatrix(SampCrps)
      m <- as.matrix(tdm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      pal <- brewer.pal(9, "BuGn")
      pal <- pal[-(1:2)]
      pal2 <- brewer.pal(8,"Dark2")
      p <- wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal2, vfont=c("sans serif","plain"))
      print(p)

   })

   output$plotSentimentos <- renderPlot({
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      sentimentos <- toupper(file$Polaridade)
      
      numsentimentos <- sentimentos[order(sentimentos)];
      mr <- table(numsentimentos)[1];
      mn <- table(numsentimentos)[2];
      mp <- table(numsentimentos)[3];
      mt <- mr + mn + mp;
      
      indicesentimento <- ifelse(mn == 0, 0, as.numeric((mp-mr)/(mt-mn)))
      
      p <- ggplot() + geom_bar(stat="identity", aes(x = unique(sort(sentimentos)),y=100/mt*as.numeric(table(numsentimentos))), fill = c("red","blue","green")) + xlab("Sentimentos") + ylab("Número de Comentários") + coord_flip() 
      print(p)
      cat(paste("Sentiment Index = ", indicesentimento,sep=""),sep="\n")

   })
   
      
   output$plotLista <- renderPlot({
      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      allmessages <- toupper(file$Conteúdo)
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      myCrps<- txt.to.words(SampCrps)
      tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
      stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
      top20unig<-stblUnigrm[1:20,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "gray50" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      print(p)
   })

   output$plotPalavrasNeutras <- renderPlot({
      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      neutras <- which(toupper(file$Polaridade) == "NEUTRO")
      allmessages <- toupper(file$Conteúdo)[neutras]
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      myCrps<- txt.to.words(SampCrps)
      tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
      stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
      top20unig<-stblUnigrm[1:15,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "blue" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      print(p)
   })
   
   output$plotPalavrasNegativas <- renderPlot({
      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      negativas <- which(toupper(file$Polaridade) == "NEGATIVO")
      allmessages <- toupper(file$Conteúdo)[negativas]
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      myCrps<- txt.to.words(SampCrps)
      tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
      stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
      top20unig<-stblUnigrm[1:15,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "red" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      print(p)
   })
   
   output$plotPalavrasPositivas <- renderPlot({
      allmessages<-""
      filepath <- input$file$datapath
      file <- read_xlsx(filepath)
      positivas <- which(toupper(file$Polaridade) == "POSITIVO")
      allmessages <- toupper(file$Conteúdo)[positivas]
      
      spSamp<- unlist(strsplit(allmessages, split=", "))
      nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
      ascVec<- spSamp[ - nonAscIDX]
      ascSamp<- paste(ascVec, collapse = ", ")
      clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
      clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
      clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
      clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
      SampCrps<- Corpus(VectorSource(clnSamp))
      SampCrps<- tm_map(SampCrps, tolower)
      SampCrps<- tm_map(SampCrps, removePunctuation)
      SampCrps<- tm_map(SampCrps, removeNumbers)
      urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
      SampCrps<-tm_map(SampCrps, urlPat)
      emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
      SampCrps<- tm_map(SampCrps, emlPat)
      tt<-function(x) gsub("RT |via", "", x)
      SampCrps<- tm_map(SampCrps, tt)
      tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
      SampCrps<- tm_map(SampCrps, tun)
      SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
      myCrps<- txt.to.words(SampCrps)
      tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
      stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
      top20unig<-stblUnigrm[1:15,]
      colnames(top20unig)<-c("UniGram","Frequency")
      p <- ggplot (top20unig, aes(x = reorder(UniGram, Frequency), y= Frequency )) + 
         geom_bar( stat = "Identity" , fill = "green" ) +  
         geom_text( aes (label = Frequency ) , vjust = - 0.10, hjust = -0.8, size = 2 ) +
         xlab( "Termos" ) +
         ylab( "Frequência" ) +
         theme ( axis.text.x = element_text ( angle = 45 , hjust = 1 ) ) + coord_flip()
      print(p)
   })
   
   output$plotTeste <- renderPlot({
      plot(1:1000,log(1:1000))
   })
   
   output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file
      
      if (is.null(inFile))
         return(NULL)
      
      read.csv(inFile$datapath, header = input$header)
   })
   
}

