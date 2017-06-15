# Set Seed
set.seed (100)

# Load Libraries
library(stylo)
library(data.table)
library(stringr) 
library(stringi)
library(dplyr)
library(wordcloud)

# Load Bigrams
bigram <- read.csv("bigram.csv",stringsAsFactors=FALSE)
bigram$X <- NULL
bigramDataTable <- data.table(bigram) # 502449

# Load Trigrams
trigram <- read.csv("trigram.csv", stringsAsFactors=FALSE)
trigram$X <- NULL
trigramDataTable <- data.table(trigram) # 369415

# Load Quadgrams
quadgram <- read.csv("quadgram.csv", stringsAsFactors=FALSE)
quadgram$X <- NULL
quadgramDataTable <- data.table(quadgram)  # 276357

# Load Unigrams
unigram <- read.csv("unigram.csv", stringsAsFactors=FALSE)
unigram$X <- NULL
unigramDataTable <- data.table(unigram)   # 45627

rm(bigram,trigram,quadgram,unigram)

#gc()


# Set Keys for N-grams
setkey(unigramDataTable,word) 
setkey(bigramDataTable,word)
setkey(trigramDataTable,word)
setkey(quadgramDataTable,word)

# Build Prediction Function
# Implementing t-sequences of Typology Lauguage Models ...

predictNextWord <- function(phrase) {

	# get number of words
	numWords <- length(unlist(strsplit(phrase," ")))
	
	if (numWords >= 3) {
		initialPhrase <- tolower(word(phrase,-3,-1))
	} else {
		initialPhrase <- tolower(phrase)
	}
	
	startPhrase <- initialPhrase

	startPattern <- paste("^",startPhrase,"\\b", sep="")
	
	totalWords <- length(unlist(strsplit(startPhrase," ")))
	
	all.words <- c()
	
	while (totalWords != 0) {
		if (totalWords == 3) {
			all.words <- grep(startPattern,quadgramDataTable$word,value=T)
		} else if (totalWords == 2) {
			all.words <- grep(startPattern,trigramDataTable$word,value=T)
		} else if (totalWords == 1) {
			all.words <- grep(startPattern,bigramDataTable$word,value=T)
		}
		
		if (length(all.words) > 0) break
	
		if (length(all.words) == 0) {
			startPhrase <- word(startPhrase,-totalWords+1, -1)
			if (!is.na(startPhrase)) {
				startPattern <- paste("^",startPhrase,"\\b", sep="")
				totalWords <- length(unlist(strsplit(startPhrase," ")))
			} else {
				totalWords <- 0
			}
		}
	}	
	
	
	if (length(all.words) > 0) {    
		probwords <- unique(word(all.words,-1))  # [1] "to"     "a"      "better" "it"     "some"   "your" 

		wordProbs <- data.frame(word=as.character(probwords),probs=0, stringsAsFactors=FALSE)	

		for (i in (1:length(probwords))) {
		
			probQuadgram <- 0
			probTrigram  <- 0
			probBigram   <- 0 
		
			# This is processing quadgramDataTable ...
			if (totalWords == 3) {
				# Initiate (newEndPhrase) and (newEndPattern)
					newEndPhrase   <- probwords[i]           # eg "to"	
					newEndPattern  <- paste("\\b", newEndPhrase, "$",sep="")   # "to$"				
		
				# Get P(to|hope * *) = c(hope * * to)/c(hope * *), This is processing in (quadgramDataTable)
					newStartPhrase <- word(startPhrase,1)    # eg "hope"
		
					newStartPattern <- paste("^",newStartPhrase,"\\b",sep="")             # "^hope"
					all.quad.start.words <- grep(newStartPattern,quadgramDataTable$word,value=T)   # 371
		
					newAll.quad.Words   <- grep(newEndPattern,all.quad.start.words,value=T)      # Count = 8
										# "hope i get to"        "hope we get to"       "hope you get to"
										# "hope you continue to" "hope to get to"       "hope you decide to"
										# "hoped to go to"       "hope to run into"
							 
					all.tri.start.words <- grep(newStartPattern,trigramDataTable$word,value=T)  # Count = 349
										# "hope to see"           "hope you have"         "hope you re"   ...
		
					if (length(all.tri.start.words) > 0) {
						probQuadgram <- quadgramDataTable[quadgramDataTable$word %in% newAll.quad.Words,sum(freq)]/
					                            trigramDataTable[trigramDataTable$word %in% all.tri.start.words,sum(freq)]   
					} else {
						probQuadgram <- 0    
					}
					
				startPhrase <- word(startPhrase,-totalWords+1, -1)    # eg original "hope you get", now it is "you get"
				
				totalWords <- length(unlist(strsplit(startPhrase," ")))
				
			}
			
			# This is trigramDataTable processing ...
			if (totalWords == 2) {
				# Initiate (newEndPhrase) and (newEndPattern)
					newEndPhrase   <- probwords[i]           # eg "to"	
					newEndPattern  <- paste("\\b", newEndPhrase, "$",sep="")   # "to$"
					
				# Get P(to|you *) = c(you * to)/c(you *), This is processing in (trigramDataTable)
					newStartPhrase <- word(startPhrase,1)    # eg "you"
		
					newStartPattern <- paste("^",newStartPhrase,"\\b",sep="")           # "^you"
					all.tri.start.words <- grep(newStartPattern,trigramDataTable$word,value=T)   # 6741
		
					newAll.tri.Words   <- grep(newEndPattern,all.tri.start.words,value=T)      # Count = 255
										# "you want to"  "you have to"  "you need to"  "you going to" "you get to" ...
							 
					all.bi.start.words <- grep(newStartPattern,bigramDataTable$word,value=T)  # Count = 4650
										# "you can"  "you re"   "you are"  "you have" "you know" "you want" ...
		
					if (length(all.bi.start.words) > 0) {
						probTrigram <-   trigramDataTable[trigramDataTable$word %in% newAll.tri.Words,sum(freq)]/
												bigramDataTable[bigramDataTable$word %in% all.bi.start.words,sum(freq)] 
					} else {
						probTrigram <- 0    
					}
					
				startPhrase <- word(startPhrase,-totalWords+1, -1)   # eg "get"
				
				totalWords <- length(unlist(strsplit(startPhrase," ")))					
			}
			
			# This is bigramDataTable processing ...
			if (totalWords == 1) {
				# Initiate (newEndPhrase) and (newEndPattern)
					newEndPhrase   <- probwords[i]           # eg "to"	
					newEndPattern  <- paste("\\b", newEndPhrase, "$",sep="")   # "to$"
					
				# Get p(to|get) = c(get to)/c(get), This is processing (bigramDataTable)
					newStartPhrase <- word(startPhrase,1)    # eg "get"
		
					newStartPattern <- paste("^",newStartPhrase,"\\b",sep="")         # "^get"
					all.bi.start.words <- grep(newStartPattern,bigramDataTable$word,value=T)   # 1095
		
					newAll.bi.Words   <- grep(newEndPattern,all.bi.start.words,value=T)      # Count = 10
										# [1] "get to"       "get into"     "getting to"   "gets to"      "getting into" ...
							 
					all.start.words   <- grep(newStartPattern,unigramDataTable$word,value=T)  # Count = 19
										# [1] "get"        "getting"    "gets"       "gettin"     "getaway"  ...
		
					if (length(all.start.words) > 0) {
						probBigram <-   bigramDataTable[bigramDataTable$word %in% newAll.bi.Words,sum(freq)]/
											unigramDataTable[unigramDataTable$word %in% all.start.words,sum(freq)] 
					} else {
						probBigram <- 0    
					}
			}
	
			# Take average of 3 probabilities
				prob <- (probQuadgram+probTrigram+probBigram)/3
			
				wordProbs[wordProbs$word==probwords[i],2] <- prob
			
				wordProbs <- arrange(wordProbs,desc(probs))
		}
	} else {
	    # Return most frequent words ...
		wordProbs <- data.frame(word=as.character("UnKnown"),probs=100, stringsAsFactors=FALSE)
	}
	
	finalWordProbs <- data.frame(head(wordProbs,5), stringsAsFactors=FALSE)
	
	finalWordProbs <- finalWordProbs %>%  mutate(percents=round(probs*100/sum(probs)))
	
	return(finalWordProbs)
}



shinyServer(
  function(input, output) {
 
	# get the reactive output, since you will need it multiple times
	x <- reactive({
			if (input$go == 0) return()
			else {
				isolate(
						withProgress(message="Running Prediction Logic ...",
							predictNextWord(input$idText)
						)
				)
			}
	})
	
	# print label
	output$outputIdText <- renderText({
			if (input$go == 0) return()
			else {
					isolate(
						("Here are the Predicted Words ...")
					)
			}
	})

	# print wordcloud
	output$outputWordCloud <- renderPlot({
			if (input$go == 0) return()
			else {
					isolate(
							wordcloud(x()$word, x()$percents, scale=c(4,0.5), max.words=5,colors=brewer.pal(6,"Dark2"))
					)
			}
	})
	
	# print data-table		
    output$outputIdTable <- renderDataTable({
			if (input$go == 0) return()
			else {
					isolate(
							x()
						#(x(), options = list(searching = FALSE, paging = FALSE))
					)
			}
	})

  }     # end function
)     # end shiny server
