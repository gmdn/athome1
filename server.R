
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  
  ## group identifier
  groupID <- "ims_unipd.z9PhCK"
  
  # The color-blind-free palette with grey
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  ## define smoothing parameters (R: relevant, N: non relevant)
  alpha_relevant <- 1
  beta_relevant  <- 1
  alpha_nonrelevant <- 1
  beta_nonrelevant  <- 1
  
  ## initialize relevance vector
  #relevance <- reactiveValues(judgement = rep.int(0, times = num_of_documents))
  relevance_judgement <- rep.int(0, times = num_of_documents)
  
  ## initialize documents to judge
  judge <- reactiveValues(doc_idx = integer())
  
  ## initialize feedback rounds
  #feedback <- reactiveValues(round = 0)
  feedback_round <- 0
  
  ## use reactive value to update results
  results <- reactiveValues(new = 1)
  
  ## update topic 
  observe({
    
    query <- topics$need[topics$topid == input$topics]
    
    ## update query
    updateTextInput(session,
                    inputId = "query",
                    value = query)
    
  })
  
  ## observe event click on search button
  initSearch <- observeEvent(input$search, {
    
    ## create directory to save data (if it does not exist)
    if(!file.exists(file.path("..", "data", input$runid))) {
      dir.create(file.path("..", "data", input$runid))
    }
    
    if(!file.exists(file.path("..", "data", input$runid, input$topics))) {
      dir.create(file.path("..", "data", input$runid, input$topics))
    }
    
    ## split terms
    query_terms <- unlist(strsplit(input$query, split = " "))
    
    ## lowercase
    query_terms <- tolower(x = query_terms)
    
    ## remove stopwords
    query_terms <- setdiff(query_terms, stopwords("english"))
    
    ## stem words
    query_stems <- wordStem(query_terms)
    
    ## find the indexes of the terms (stems) involved (query + expansion)
    idx_query_terms <- which(is.element((dtmTf$dimnames$Terms), query_stems))
    
    ## set the number of top k documents to assess
    #k <- input$topk
    
    ## initialize docids and indexes
    #docids_topk <- NULL
    idx_doc_topk <- NULL
    
    ## if there are query terms
    if (length(idx_query_terms) > 0) {
      
      ## get the number of rel docs
      num_of_rel_documents <- sum(relevance_judgement == 1)
      
      if(num_of_rel_documents > 1) {
        theta_relevant <- (colSums(dtmBinSparse[relevance_judgement == 1, ]) + alpha_relevant) / (num_of_rel_documents + alpha_relevant + beta_relevant)  
      } else if (num_of_rel_documents == 1) {
        theta_relevant <- (dtmBinSparse[relevance_judgement == 1, ] + alpha_relevant) / (num_of_rel_documents + alpha_relevant + beta_relevant)  
      } else {
        theta_relevant <- rep(alpha_relevant / (alpha_relevant + beta_relevant), times = dim(dtmBinSparse)[2])
      }
      
      ## update theta negative
      theta_nonrelevant <- (colSums(dtmBinSparse) + alpha_nonrelevant) / (num_of_documents - num_of_rel_documents + alpha_nonrelevant + beta_nonrelevant)
      
      ## build coordinates
      features <- idx_query_terms
      
      if(feedback_round > 0) {
        ## select features
        p_q <- theta_relevant - theta_nonrelevant
        features <- union(idx_query_terms, head(order(p_q, decreasing = TRUE), 10))
        
      }
      
      ## get BIM scores
      bim_relevant <- log(theta_relevant[features]) - log(1 - theta_relevant[features])
      bim_nonrelevant <- log(theta_nonrelevant[features]) - log(1 - theta_nonrelevant[features])
      
      #x <- as.vector(bm25_sparse[, idx_query_terms] %*% bim_relevant)
      #y <- as.vector(bm25_sparse[, idx_query_terms] %*% bim_nonrelevant)
      x <- as.vector(bm25_sparse[, features] %*% bim_relevant)
      y <- as.vector(bm25_sparse[, features] %*% bim_nonrelevant)
      
      idx_doc_tojudge <- which(relevance_judgement == 0)
      
      ## update coordinate y
      ## for the element-wise moltiplication between a matrix A (m x n) and
      ## a vector B (n), we first transpose the matrix, t(A) * B.
      ## We sum the columns to obtain the weight for each document.
      #y <- colSums(t(bm25_sparse[, idx_query_terms]) * bim_negative[idx_query_terms])
      
      ## rank the documents (since order() it's in ascending order,
      ## the smallest is the first in the ranking list)
      ranking <- order(y[idx_doc_tojudge] - x[idx_doc_tojudge])
      #ranking <- order(y)
      #ranking <- order(y - x)
      
      ## select documents with a score different from zero.
      # no_zero_docs <- which(y != 0)
      
      ## check if k is largest than the number of no_zero_docs
      ## then get the ids of the top documents
#       if(input$topk > length(no_zero_docs)) {
#         #docids_topk <- dtmTf$dimnames$Docs[no_zero_docs]
#         idx_doc_topk <- no_zero_docs
#       } else {
#         #docids_topk <- dtmTf$dimnames$Docs[ranking[1:input$topk]]
#         idx_doc_topk <- ranking[1:input$topk]
#       }
      idx_doc_topk <- idx_doc_tojudge[ranking[1:input$topk]]
      
    }
    
    #judge$doc_idx <- idx_doc_topk
    
    ## exclude documents that have already been judged
    #judge$doc_idx <- setdiff(idx_doc_topk, which(relevance_judgement != 0))
    judge$doc_idx <- idx_doc_topk
    
  })
  
  
  output$outSearch <- renderText(({
    
    paste(input$search, length(judge$doc_idx), sep = " ")
    
  }))
  
  
  callJudge <- eventReactive(input$judge, {
    
    ## update feedback round
    feedback_round <<- feedback_round + 1
    
    ## use this line to test the code without calling the server
    #load("../data/9nkm9nhDx6HC/athome101/1.RData")
    
    ## initialize qrels
    qrels <- data.frame(docids = character(), judgement = integer())
    
    ## call the server if there is any document to judge
    if(length(judge$doc_idx) > 0) {
      
      ## build the ids of the docs to judge
      docids_to_judge <- dtmTf$dimnames$Docs[judge$doc_idx]
      
      ## connect to server and request assessments
      r <- POST(url = paste("http://quaid.uwaterloo.ca:33333/judge/", input$runid, "/", input$topics, "/", sep = ""),
                body = as.list(docids_to_judge),
                encode = "json")
      
      ## transform JSON to data.frame
      qrels <- fromJSON(content(r, "text", encoding = "ISO-8859-1"))
      
      ## save qrels on disk
      save(qrels,
           file = file.path("..", "data", input$runid, input$topics, paste(feedback_round, "RData", sep = ".")))
      
    }
    
    #load("../data/ZkC36NYlNfiw/athome101/1.RData")
    
    return(qrels)
    
  })
  
  
  ## iterate over docs to judge
  callAutomaticJudge <- observeEvent(input$auto_judge, {
    
    ## update feedback round
    feedback_round <<- feedback_round + 1
    
    ## number of sub-iterations
    feedback_sub <- 1
    
    ## initialize qrels
    qrels <- data.frame(docids = character(), judgement = integer())
    
    for(rotation in seq(input$rotate[2], input$rotate[1], by = -0.05)) {
      
      print(paste("rotation = ", rotation))
      print(paste("shift = ", input$shift))
      
      ## call the server if there is any document to judge
      repeat {
        
        ## we need to recompute the coordinates (even though we already have done it)
        ## compute total number of relevant documents judged so far
        num_of_rel_documents <- sum(relevance_judgement == 1)
        
        ## estimate the parameters for the relevant set
        #theta_relevant <- (colSums(dtmBinSparse[relevance_judgement == 1, ]) + alpha_relevant) / (num_of_rel_documents + alpha_relevant + beta_relevant)
        ## estimate the parameters for the relevant set
        if(num_of_rel_documents > 1) {
          theta_relevant <- (colSums(dtmBinSparse[relevance_judgement == 1, ]) + alpha_relevant) / (num_of_rel_documents + alpha_relevant + beta_relevant)  
        } else if (num_of_rel_documents == 1) {
          theta_relevant <- (dtmBinSparse[relevance_judgement == 1, ] + alpha_relevant) / (num_of_rel_documents + alpha_relevant + beta_relevant)  
        } else {
          theta_relevant <- alpha_relevant / (alpha_relevant + beta_relevant)
        }

        theta_nonrelevant <- (colSums(dtmBinSparse[relevance_judgement < 1, ]) + alpha_nonrelevant) / (num_of_documents - num_of_rel_documents + alpha_nonrelevant + beta_nonrelevant)
        
        ## select features
        p_q <- theta_relevant - theta_nonrelevant
        features <- head(order(p_q, decreasing = TRUE), input$features)
        
        ## BIM weight for relevant and nonrelevant set
        bim_relevant <- log(theta_relevant[features]) - log(1 - theta_relevant[features])
        bim_nonrelevant <- log(theta_nonrelevant[features]) - log(1 - theta_nonrelevant[features])
        
        x <- as.vector(bm25_sparse[, features] %*% bim_relevant)
        y <- as.vector(bm25_sparse[, features] %*% bim_nonrelevant)
        
        ## get coordinates
        coords <- data.frame(x = x, y = y)
        
        ## select idx of docs to judge
        idx_doc_tojudge <- which(relevance_judgement == 0)
        
        ## update docs to judge (potentially)
        judge$doc_idx <- idx_doc_tojudge[which(coords$y[idx_doc_tojudge] < rotation * coords$x[idx_doc_tojudge] + input$shift)]
        
        print(paste("docs to judge= ", length(judge$doc_idx)))
        
        doc_to_judge <- integer()
        
        #if(length(docids_to_judge) > 0) {
        if(length(judge$doc_idx) > 0) {
          
          ## build the ids of the docs to judge
          docids_to_judge <- dtmTf$dimnames$Docs[judge$doc_idx]
          
          ## connect to server and request assessments
          r <- POST(url = paste("http://quaid.uwaterloo.ca:33333/judge/", input$runid, "/", input$topics, "/", sep = ""),
                    body = as.list(docids_to_judge),
                    encode = "json")
          
          ## transform JSON to data.frame
          qrels <- fromJSON(content(r, "text", encoding = "ISO-8859-1"))
          
          ## add info about rotation and shift
          qrels$rotation <- rep(rotation, times = dim(qrels)[1])
          qrels$shift <- rep(input$shift, times = dim(qrels)[1])
          
          ## save qrels on disk
          save(qrels,
               file = file.path("..", "data", input$runid, input$topics,
                                paste(feedback_round, feedback_sub, "RData", sep = ".")))
          
          print(paste("new rel docs = ", sum(qrels$judgement == 1)))
          
          ## find rel and non rel docs
          idx_doc_rel <- which(is.element(dtmTf$dimnames$Docs, qrels$docid[qrels$judgement == 1]))
          idx_doc_nonrel <- which(is.element(dtmTf$dimnames$Docs, qrels$docid[qrels$judgement == -1]))
          
          ## update relevance vector
          relevance_judgement[idx_doc_rel] <<- 1
          relevance_judgement[idx_doc_nonrel] <<- -1
          
          ## save relevance on disk
          save(relevance_judgement,
               file = file.path("..", "data", input$runid, input$topics, paste("relevance_judgement", "RData", sep = ".")))
          
          ## update feedback 
          feedback_sub <- feedback_sub + 1
          
        }
        
        if(length(judge$doc_idx) == 0) { break }
        
      } # end-repeat
      
    } # end-for
    
    ## send signal to update results and plot
    results$new <- results$new + 1
    
  })
  
  
  output$outJudge <- renderTable({
    
    qrels <- callJudge()
    
    data.frame(judged = dim(qrels)[1], rel = sum(qrels$judgement == 1))
    
  })
  
  
  estimates <- reactive({
    
    ## get relevance assessments
    qrels <- callJudge()
    
    ## update if new results are available (auto-judge)
    results$new
    
    ## find indexes of documents judged relevant and non-relevant
    ## THIS IS NOT SAFE AS judge$doc_idx shortcuts some code
    #idx_doc_rel <- judge$doc_idx[qrels$judgement == 1]
    #idx_doc_nonrel <- judge$doc_idx[qrels$judgement == -1]
    
    idx_doc_rel <- which(is.element(dtmTf$dimnames$Docs, qrels$docid[qrels$judgement == 1]))
    idx_doc_nonrel <- which(is.element(dtmTf$dimnames$Docs, qrels$docid[qrels$judgement == -1]))
    
    #print(head(qrels$docids))
    #print(head(qrels[qrels$judgement == 1, ]))
    
    ## update relevance vector
    relevance_judgement[idx_doc_rel] <<- 1
    relevance_judgement[idx_doc_nonrel] <<- -1
    
    ## save relevance on disk
    save(relevance_judgement,
         file = file.path("..", "data", input$runid, input$topics, paste("relevance_judgement", "RData", sep = ".")))
    
    ## compute total number of relevant documents judged
    num_of_rel_documents <- sum(relevance_judgement == 1)
    
    ## estimate the parameters for the relevant set
    if(num_of_rel_documents > 1) {
      theta_relevant <- (colSums(dtmBinSparse[relevance_judgement == 1, ]) + alpha_relevant) / (num_of_rel_documents + alpha_relevant + beta_relevant)  
    } else if (num_of_rel_documents == 1) {
      theta_relevant <- (dtmBinSparse[relevance_judgement == 1, ] + alpha_relevant) / (num_of_rel_documents + alpha_relevant + beta_relevant)  
    } else {
      theta_relevant <- alpha_relevant / (num_of_rel_documents + alpha_relevant + beta_relevant)
    }
    
    theta_nonrelevant <- (colSums(dtmBinSparse[relevance_judgement < 1, ]) + alpha_nonrelevant) / (num_of_documents - num_of_rel_documents + alpha_nonrelevant + beta_nonrelevant)
    
    return(list(relevant = theta_relevant,
                nonrelevant = theta_nonrelevant))
    
    
  })
  
  output$features <- renderTable({
    
    ## estimate parameters
    theta <- estimates()
    
    ## select features
    p_q <- theta$relevant - theta$nonrelevant
    
    ## select top 20 to show
    features <- head(order(p_q, decreasing = TRUE), 20)
    
    as.data.frame(dtmTf$dimnames$Terms[features])
    
  })
  
  coordinates <- reactive({
    
    ## estimate parameters
    theta <- estimates()
    
    ## select features
    p_q <- theta$relevant - theta$nonrelevant
    features <- head(order(p_q, decreasing = TRUE), input$features)
    
    ## BIM weight for relevant and nonrelevant set
    bim_relevant <- log(theta$relevant[features]) - log(1 - theta$relevant[features])
    bim_nonrelevant <- log(theta$nonrelevant[features]) - log(1 - theta$nonrelevant[features])
    
    ## update coordinates
    #x <- as.vector(bm25_sparse %*% bim_relevant)
    #y <- as.vector(bm25_sparse %*% bim_nonrelevant)
    x <- as.vector(bm25_sparse[, features] %*% bim_relevant)
    y <- as.vector(bm25_sparse[, features] %*% bim_nonrelevant)
    
    return(data.frame(x = x, y = y))
    
  })
  
  ## compute 
  classify <- reactive({
    
    ## get coordinates
    coords <- coordinates()
    
    idx_doc_rel <- which(relevance_judgement == 1)
    idx_doc_nonrel <- which(relevance_judgement == -1)
    idx_doc_tojudge <- which(relevance_judgement == 0)
    
    ## compute total number of relevant docs
    relevant_found <- sum(relevance_judgement == 1)
    
    ## compute total number of non relevant docs
    nonrelevant_found <- sum(relevance_judgement == -1)
    
    ## find true positives
    true_positives <- sum((coords$y[idx_doc_rel] < input$rotate[2] * coords$x[idx_doc_rel] + input$shift))
    
    ## find false positives
    false_positives <- sum((coords$y[idx_doc_nonrel] < input$rotate[2] * coords$x[idx_doc_nonrel] + input$shift))
    
    ## compute temporary recall
    temp_recall <- true_positives / relevant_found
    
    ## compute precision
    temp_precision <- true_positives / (true_positives + false_positives)
    
    ## update docs to judge (potentially)
    doc_to_judge1 <- idx_doc_tojudge[which(coords$y[idx_doc_tojudge] < input$rotate[1] * coords$x[idx_doc_tojudge] + input$shift)]
    doc_to_judge2 <- idx_doc_tojudge[which(coords$y[idx_doc_tojudge] < input$rotate[2] * coords$x[idx_doc_tojudge] + input$shift)]

    ## documents to judge
    #potentially_true_positives <- length(judge$doc_idx)
    potentially_true_positives <- union(doc_to_judge1, doc_to_judge2)
    
    ## update list of 
    judge$doc_idx <- potentially_true_positives
    
    ## already judged 
    already_judged <- relevant_found + nonrelevant_found
    
    
    return(list(judged = already_judged,
                rel = relevant_found,
                nonrel = nonrelevant_found,
                true_pos = true_positives, 
                false_pos = false_positives,
                temp_rec = temp_recall,
                temp_pre = temp_precision,
                to_judge = length(potentially_true_positives),
                remaning = num_of_documents - already_judged
    ))
    
  })
  
  
  output$measures <- renderTable({
    
    as.data.frame(classify())
    
  })
  
  
  output$plot <- renderPlot({
    
    ## get coordinates
    coords <- coordinates()
    
    ## subset collection to speed-up plots
    subset_docs <- sample(x = 1:num_of_documents,
                          size = 0.01 * num_of_documents,
                          replace = F)
    
    non_rel <- which(relevance_judgement == -1)
    
    ## if there are too many non relevant docs to display
    if(length(non_rel) > 5000) {
      
      non_rel <- sample(non_rel,
                        size = 5000, 
                        replace = FALSE)
    }
    
    ## merge subset and judged documents
    subset_docs <- union(subset_docs, c(non_rel, which(relevance_judgement == 1)))
    
    ## build data frame for ggplot
    coords <- data.frame(x = coords$x[subset_docs],
                         y = coords$y[subset_docs],
                         classes = factor(relevance_judgement[subset_docs], levels = c("-1", "0", "1")))
    
    ## build plot
    ggp <- ggplot(coords, aes(x = x, y = y, colour = classes))
    
    if(sum(relevance_judgement == -1) > 0) {
      
      ## add points and scale 
      ggp <- ggp +
        geom_point(aes(shape = classes,
                       size = classes),
                   alpha = 0.6) +
        scale_color_manual(values = cbPalette[c(3, 1, 7)]) +
        scale_size_manual(values = c(1.5, 0.5, 2)) + 
        scale_x_continuous(limits = c(input$range[1], input$range[2])) +
        scale_y_continuous(limits = c(input$range[1], input$range[2])) +
        geom_polygon(data = data.frame(x1 = c(0,
                                              input$range[1]/input$rotate[1]-input$shift/input$rotate[1],
                                              input$range[1]/input$rotate[2]-input$shift/input$rotate[2]), 
                                       y2 = c(input$shift,
                                              input$range[1],
                                              input$range[1]),
                                       c2 = factor(c(0, 0, 0))),
                     mapping = aes(x = x1, y = y2, colour = c2),
                     alpha = 0.1)
      
      
    } else {
      
      ## add points and scale 
      ggp <- ggp +
        geom_point(aes(shape = classes,
                       size = classes),
                   alpha = 0.6) +
        scale_color_manual(values = cbPalette[c(1, 7)]) +
        scale_size_manual(values = c(0.5, 2)) + 
        scale_x_continuous(limits = c(input$range[1], input$range[2])) +
        scale_y_continuous(limits = c(input$range[1], input$range[2]))
      
    }
    
    print(ggp +
            geom_abline(slope = input$rotate[2],
                            intercept = input$shift,
                            colour = "blue") + 
            geom_abline(slope = input$rotate[1],
                        intercept = input$shift,
                        colour = "grey"))
    
    
  })
  
  
})
