library(shiny)
library(stringr)
library(stringi)
library(data.table)
library(qdap)
shinyServer(function(input, output) {
  
  observeEvent(input$Submit,{
    word <- reactiveVal("good")
    prob <- reactiveVal(0)
    output$inputText <- renderText({
      #read in 1-gram, 2-gram and 3-gram
      gram_1 <- read.csv("gram_1.csv")
      gram_2 <- read.csv("gram_2.csv")
      gram_3 <- read.csv("gram_3.csv")
      gram_1 <- data.table(gram_1)
      gram_2 <- data.table(gram_2)
      gram_3 <- data.table(gram_3)
      gram_1[,c("X"):=NULL]
      gram_2[,c("X"):=NULL]
      gram_3[,c("X"):=NULL]
      #functions 
      get_obs_ngram <- function(wordseq, Ngram){
        PreTxt <- sprintf("%s%s%s", "^", wordseq, "_")
        quest <- grep(PreTxt, Ngram$Word, perl=T, useBytes = T)
        Ngram[quest,]
      }
      get_unobs_ngram_tails <- function(obsngram, N){
        obstails <- str_split_fixed(obsngram$Word, "_", N)[,N]
        return(data.table(Word=gram_1[!obstails,Word,on="Word"]))
      }
      cal_obs_prob <- function(ObsNgrams, Nm1Grams, wordseq) {
        PreCount <- Nm1Grams[wordseq, Counts, on=.(Word)]
        ObsNgrams[,Prob:=ObsNgrams[,cDis]/PreCount]  # c_dis/c
      }
      cal_alpha <- function(ObsNGrams, Nm1Grams, wordseq) {
        if (dim(ObsNGrams)[1] != 0) {
          # return(1-sum(ObsNGrams[,.(Qbo)]))  # We don't use this formular because End Of Sentence is not counted
          return(sum(ObsNGrams[,Counts-cDis]/Nm1Grams[wordseq, Counts, on=.(Word)]))
        } else {
          return(1)
        }
      }
      Find_Next_word  <- function(xy, words_num){
        xy <- gsub(" ", "_", xy)
        if (length(which(gram_2$Word == xy)) > 0) {  # C(x,y) > 0
          ## N-grams preparation
          # Retrieve all observed trigrams beginning with xy: OT
          ObsTriG <- get_obs_ngram(xy, gram_3)
          y <- str_split_fixed(xy,"_", 2)[,2]
          # Retrieve all observed bigrams beginning with y: OB
          ObsBiG <- get_obs_ngram(y, gram_2)
          # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0, UOB in UOT
          UnObsBiTails <- get_unobs_ngram_tails(ObsBiG, 2)
          # Exclude observed bigrams that also appear in observed trigrams: OB in UOT
          ObsBiG <- ObsBiG[!str_split_fixed(ObsTriG[,Word], "_", 2)[,2], on="Word"]
          
          ## Calculation part
          # Calculate probabilities of all observed trigrams: P^*(z|x,y)
          ObsTriG <- cal_obs_prob(ObsTriG, gram_2, xy)
          # Calculate Alpha(x,y)
          Alpha_xy <- cal_alpha(ObsTriG, gram_2, xy)
          # Calculate probabilities of all observed bigrams: P^*(z|y), (y,z) in UOT
          ObsBiG <- cal_obs_prob(ObsBiG, gram_1, y)
          # Calculate Alpha(y)
          Alpha_y <- cal_alpha(ObsBiG, gram_1, y)
          # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
          UnObsBiTails[, Prob:=gram_1[UnObsBiTails, Counts, on=.(Word)]/gram_1[UnObsBiTails, sum(Counts), on=.(Word)]]
          UnObsBiTails[, Prob:=Alpha_xy*Alpha_y*Prob]
          # Remove unused column in ObsTriG and ObsBiG
          ObsTriG[, c("Counts", "cDis"):=NULL]
          ObsBiG[, c("Counts", "cDis"):=NULL]
          #remove xy, only keep predict word
          ObsTriG[, Word:=str_remove(ObsTriG[, Word], "([^_]+_)+")]
          #remove y, only keep predict word
          ObsBiG[, Word:=str_remove(ObsBiG[, Word], "([^_]+_)+")]
          # Compare OT, Alpha_xy * P_{Katz}(z|y)
          # P_{Katz}(z|y) = 1. P^*(z|y), 2. Alpha_y * P_{ML}(z)
          ObsBiG[,Prob:=Alpha_xy*Prob]
          AllTriG <- setorder(rbind(ObsTriG, ObsBiG, UnObsBiTails), -Prob)
          return(AllTriG[Prob!=0][1:min(dim(AllTriG[Prob!=0])[1], words_num)])
        } else {  # C(x,y) = 0, or xy is not in N-gram, goes down to (N-1)-gram
          y <- str_split_fixed(xy,"_", 2)[,2]
          # c(y>0)
          if (length(which(gram_1$Word == y)) > 0) {
            # Retrieve all observed bigrams beginning with y: OB
            ObsBiG <- get_obs_ngram(y, gram_2)
            # Calculate probabilities of all observed bigrams: P^*(z|y)
            ObsBiG <- cal_obs_prob(ObsBiG, gram_1, y)
            # Calculate Alpha(y)
            Alpha_y <- cal_alpha(ObsBiG, gram_1, y)
            # Retrieve all unigrams end the unobserved bigrams UOBT: z where C(y,z) = 0
            UnObsBiTails <- get_unobs_ngram_tails(ObsBiG, 2)
            # Calculate P_{ML}(z), where c(y,z) in UOB: Alpha_y * P_{ML}(z)
            UnObsBiTails[, Prob:=gram_1[UnObsBiTails, Counts, on=.(Word)]/gram_1[UnObsBiTails, sum(Counts), on=.(Word)]]
            UnObsBiTails[, Prob:=Alpha_y*Prob]
            # Remove unused column in ObsBiG
            ObsBiG[, c("Counts", "cDis"):=NULL]
            ObsBiG[, Word:=str_remove(ObsBiG[, Word], "([^_]+_)+")]
            AllBiG <- setorder(rbind(ObsBiG, UnObsBiTails), -Prob)
            return(AllBiG[Prob!=0][1:words_num])
          } else {  # c(y=0) or y is not in (N-1)-gram, goes down to (N-2)-gram
            # P^*z
            return(setorder(gram_1, -cDis)[1:words_num,.(Word, Prob=cDis/gram_1[,sum(Counts)])])  
          }
        }
      }
      pre_precess <- function(wordseq){
        wordseq <- tolower(wordseq)
        tok <- strsplit(gsub("[^[:alnum:] ]", "", wordseq), " +")
        return(paste(tail(tok[[1]], 2), collapse = " "))
      }
      Next_word_pred <- function(prephrase, num=5){
        temp <- pre_precess(prephrase)
        result <- Find_Next_word(temp, num)
        if (dim(result)[1]==0){
          rbind(result, list("<Please input more words",1))
        }
        return(result)
      }
      
      intxt <- input$in_txt
      result <- Next_word_pred(intxt,1)
      word(result$Word)
      prob(result$Prob)
      intxt <- input$in_txt
      
    })
    output$next_word <- renderText({
      word()
    })
    output$prob <- renderText({prob()})
  })
  

      
})
