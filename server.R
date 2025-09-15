options(shiny.maxRequestSize=30*1024^2) 
options(xtable.include.colnames=F)
options(xtable.include.rownames=T)

source("global.R")
shinyServer(function(input, output,session){    
  output$modelUploaded <- reactive({
    return(!is.null(input$modelfile))
  })
  outputOptions(output, 'modelUploaded', suspendWhenHidden=FALSE)
  
  output$filepredUploaded <- reactive({
    return(!is.null(input$predictionfile))
  })
  outputOptions(output, 'filepredUploaded', suspendWhenHidden=FALSE) 
  
  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           alt="I2MC logo"))},deleteFile = F)
  
  output$structuredata<-renderImage({return(list(src="pictures/structurdata prediction application.jpg",
                                                 contentType="image/jpeg",alt="structure of the table",width=400,height=300))},deleteFile = F)
  
  
  output$predictionfile<-renderText({
    namelearn<-input$predictionfile$name
  })
  
  output$predictionfile2<-renderText({
    namelearn<-input$predictionfile$name
  })
  output$predictionfile3<-renderText({
    namelearn<-input$predictionfile$name
  })  
  MODEL<-reactive({
    if(is.null(input$modelfile)){return(NULL)}
    else{
      load(file = input$modelfile$datapath)
    }
    res<<-state
  })
  
  # UI dynamique: choix de la colonne des labels (à partir du fichier de prédiction)
  output$validationLabelColumnUI <- renderUI({
    req(input$predictionfile)
    validateData <- tryCatch({
      ext <- if (is.null(input$filetypepred)) "csv" else input$filetypepred
      importfile(datapath = input$predictionfile$datapath,
                 extension = ext,
                 NAstring = if (is.null(input$NAstringpred)) "NA" else input$NAstringpred,
                 sheet = if (is.null(input$sheetnpred)) 1 else input$sheetnpred,
                 skiplines = if (is.null(input$skipnpred)) 0 else input$skipnpred,
                 dec = if (is.null(input$decpred)) "." else input$decpred,
                 sep = if (is.null(input$seppred)) "," else input$seppred)
    }, error=function(e) NULL)
    if (is.null(validateData)) return(NULL)
    # selectInput("validationLabelColumn", 
    #             "Validation label column", 
    #             choices = colnames(validateData), 
    #             selected = colnames(validateData)[1])
    
     shiny::selectizeInput("validationLabelColumn", 
                   "Validation label column :", 
                   choices = colnames(validateData), , 
                   multiple = FALSE,  # Changez à TRUE si vous voulez permettre plusieurs sélections
                   options = list(
                     placeholder = 'Sélectionnez une option...',
                     maxItems = 1
                   ))
  })
  
  # fonction de prediction qui importe et transforme les données de prédiction et fait la prédiction
  PREDICT<-reactive({
    resultsmodel<<-MODEL()
    predictionparameters<<-list(confirmdatabuttonpred=input$confirmdatabuttonpred,filetypepred=input$filetypepred,predictionfile=input$predictionfile,
                                NAstringpred=input$NAstringpred,sheetnpred=input$sheetnpred,skipnpred=input$skipnpred,decpred=input$decpred,seppred=input$seppred,
                                transposepred=input$transposepred,zeroegalNApred=input$zeroegalNApred,
                                log=resultsmodel$parameters$transformdataparameters$log,logtype=resultsmodel$parameters$transformdataparameters$logtype,
                                arcsin=resultsmodel$parameters$transformdataparameters$arcsin, 
                                standardization=resultsmodel$parameters$transformdataparameters$standardization,rempNA=resultsmodel$parameters$transformdataparameters$rempNA,
                                modeltype=resultsmodel$parameters$modelparameters$modeltype,invers=resultsmodel$parameters$modelparameters$invers,
                                thresholdmodel=resultsmodel$parameters$modelparameters$thresholdmodel)
    model<-resultsmodel$model$MODEL
    learningmodel<-resultsmodel$model$DATALEARNINGMODEL$learningmodel 
    if(is.null(input$predictionfile)){return(data.frame())}#Pas de fichier
    if(!is.null(input$predictionfile)  ){
      
      datapath<<- predictionparameters$predictionfile$datapath
      prediction<<-importfile(datapath = datapath,extension = predictionparameters$filetypepred,
                              NAstring=predictionparameters$NAstringpred,sheet=predictionparameters$sheetnpred,
                              skiplines=predictionparameters$skipnpred,dec=predictionparameters$decpred,sep=predictionparameters$seppred)
      prediction<-transformdata(toto = prediction,transpose=predictionparameters$transposepred,zeroegalNA=predictionparameters$zeroegalNApred)
      
      true_labels <- NULL
      if(input$confirmdatabuttonpred!=0){
        # Extraire et retirer la colonne de labels si l'utilisateur l'a choisie
        true_labels <- NULL
        if (!is.null(input$validationLabelColumn) && input$validationLabelColumn %in% colnames(prediction)){
          true_labels <- prediction[, input$validationLabelColumn]
          prediction <- prediction[, setdiff(colnames(prediction), input$validationLabelColumn), drop=FALSE]
        }
        prediction<-confirmdata(toto=prediction)
        datastructuresfeatures<-resultsmodel$selectdata$DATASTRUCTUREDFEATURES
        structuredfeatures<-resultsmodel$selectdata$STRUCTUREDFEATURES
        predictiondiff<-prediction[,which(colnames(prediction)%in%colnames(learningmodel))]
        learningselect<-resultsmodel$selectdata$LEARNINGSELECT
        if(resultsmodel$transformdata$transformdataparameters$log) { 
          predictiondiff<-transformationlog(x = predictiondiff+1,logtype =resultsmodel$transformdata$transformdataparameters$logtype )
          learningselect[,-1]<-transformationlog(x = learningselect[,-1]+1,logtype=resultsmodel$transformdata$transformdataparameters$logtype)}
        
        if(resultsmodel$transformdata$transformdataparameters$arcsin){
          maxlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = max,na.rm=T)
          minlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = min,na.rm=T)
          for (i in 2:dim(predictiondiff)[2]){
            predictiondiff[,i]<-(predictiondiff[,i]-minlearn[i-1])/(maxlearn[i-1]-minlearn[i-1])
            predictiondiff[which(predictiondiff[,i]>1),i]<-1
            predictiondiff[which(predictiondiff[,i]<0),i]<-0
            predictiondiff[,i]<-asin(sqrt(predictiondiff[,i]))
            
          }
          learningselect[,-1]<-apply(X = learningselect[,-1],MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
          learningselect[,-1]<-asin(sqrt(learningselect[,-1]))
          
        }
        if(resultsmodel$transformdata$transformdataparameters$standardization){
          learningselectval<<-learningselect
          sdselect<-apply(learningselect[,which(colnames(learningselect)%in%colnames(predictiondiff))], 2, sd,na.rm=T)
          predictiondiff<-scale(predictiondiff,center=F,scale=sdselect[-1])
        }
        #NAstructure if NA ->0
        if(!is.null(datastructuresfeatures)){
          predictiondiff[which(is.na(predictiondiff),arr.ind = T)[which(which(is.na(predictiondiff),arr.ind = T)[,2]%in%which(colnames(predictiondiff)%in%datastructuresfeatures$names)),]]<-0
        }
        #
        predictionmodel<- replaceNAvalidation(predictiondiff,toto=learningmodel[,-1],rempNA=predictionparameters$rempNA)
        
        lev<-levels(x = learningmodel[,1])
        names(lev)<-c("positif","negatif")
        
        
        #prediction a partir du model
        if(predictionparameters$modeltype=="randomforest"){
          scoreprediction <- randomForest:::predict.randomForest(object=model,type="prob",newdata = predictionmodel)[,lev["positif"]]
          predictclassprediction<-vector(length = length(scoreprediction) ) 
          predictclassprediction[which(scoreprediction>=predictionparameters$thresholdmodel)]<-lev["positif"]
          predictclassprediction[which(scoreprediction<predictionparameters$thresholdmodel)]<-lev["negatif"]
          predictclassprediction<-as.factor(predictclassprediction)
          
        }
        
        if(predictionparameters$modeltype=="svm"){
          scoreprediction =attr(predict(model,newdata =  predictionmodel,decision.values=T),"decision.values")
          if(sum(lev==(strsplit(colnames(scoreprediction),split = "/")[[1]]))==0){scoreprediction<-scoreprediction*(-1)}
          predictclassprediction<-vector(length = length(scoreprediction) ) 
          predictclassprediction[which(scoreprediction>=predictionparameters$thresholdmodel)]<-lev["positif"]
          predictclassprediction[which(scoreprediction<predictionparameters$thresholdmodel)]<-lev["negatif"]
          predictclassprediction<-as.factor(predictclassprediction)
        }
        
        respredictiontionmodel<<-data.frame(predictclassprediction,scoreprediction)
        colnames(respredictiontionmodel) <-c("predictclassprediction","scoreprediction") 
        datapredictionmodel<-list("predictionmodel"=predictionmodel,"respredictiontionmodel"=respredictiontionmodel,"groups"=lev)
      }
      else{datapredictionmodel=NULL
      predictiondiff=NULL}
      
    }
    
    list(prediction=prediction,
         predictiondiff=predictiondiff,
         predictionparameters=predictionparameters,
         predictionmodel=datapredictionmodel$predictionmodel,
         respredictiontionmodel=datapredictionmodel$respredictiontionmodel,
         groups=datapredictionmodel$groups,
         true_labels=true_labels
    )
    
    
  }) 
  
  #####
  # Calcul des métriques de validation (optionnelles)
  validationMetrics <- reactive({
    # req(input$confirmdatabuttonval)
    resultsmodel<-MODEL()
    if (is.null(resultsmodel)) return(NULL)
    learningmodel<-resultsmodel$model$DATALEARNINGMODEL$learningmodel
    lev<-levels(x = learningmodel[,1])
    names(lev)<-c("positif","negatif")
    
    # 1) Cas où l'utilisateur a choisi la colonne de labels dans le fichier de prédiction
    labels_from_pred <- PREDICT()$true_labels
    if (!is.null(labels_from_pred)){
      y_true<-factor(as.character(labels_from_pred), levels = lev)
      score <- PREDICT()$respredictiontionmodel$scoreprediction
      threshold<-resultsmodel$parameters$modelparameters$thresholdmodel
      pred<-ifelse(score>=threshold, lev["positif"], lev["negatif"]) 
      pred<-factor(pred, levels = lev)
      auc_val<-tryCatch({
        pROC::roc(response = y_true, predictor = as.numeric(score), levels = lev, direction = ">")$auc[1]
      }, error=function(e) NA)
      tp<-sum(y_true==lev["positif"] & pred==lev["positif"])
      tn<-sum(y_true==lev["negatif"] & pred==lev["negatif"])
      fp<-sum(y_true==lev["negatif"] & pred==lev["positif"])
      fn<-sum(y_true==lev["positif"] & pred==lev["negatif"])
      sens<-ifelse((tp+fn)==0, NA, tp/(tp+fn))
      spec<-ifelse((tn+fp)==0, NA, tn/(tn+fp))
      return(list(auc=as.numeric(auc_val), sensibility=sens, specificity=spec))
    }
    
    # 2) Sinon: fallback sur le fichier de validation séparé s'il est fourni
    if (!is.null(input$validationfile)){
      valraw<-importfile(datapath = input$validationfile$datapath,
                         extension = if (is.null(input$filetypeval)) "csv" else input$filetypeval,
                         NAstring = if (is.null(input$NAstringval)) "NA" else input$NAstringval,
                         sheet = if (is.null(input$sheetnval)) 1 else input$sheetnval,
                         skiplines = if (is.null(input$skipnval)) 0 else input$skipnval,
                         dec = if (is.null(input$decval)) "." else input$decval,
                         sep = if (is.null(input$sepval)) "," else input$sepval)
      valraw<-transformdata(toto = valraw,transpose = isTRUE(input$transposeval), zeroegalNA = isTRUE(input$zeroegalNAval))
      if (is.null(input$validationLabelColumn) || !(input$validationLabelColumn %in% colnames(valraw))) return(NULL)
      y_true<-as.factor(valraw[, input$validationLabelColumn])
      valfeatures<-valraw[, setdiff(colnames(valraw), input$validationLabelColumn), drop=FALSE]
      valfeatures<-valfeatures[, which(colnames(valfeatures)%in%colnames(learningmodel)), drop=FALSE]
      learningselect<-resultsmodel$selectdata$LEARNINGSELECT
      if(resultsmodel$transformdata$transformdataparameters$log) { 
        valfeatures<-transformationlog(x = valfeatures+1,logtype =resultsmodel$transformdata$transformdataparameters$logtype )
        learningselect[,-1]<-transformationlog(x = learningselect[,-1]+1,logtype=resultsmodel$transformdata$transformdataparameters$logtype)
      }
      if(resultsmodel$transformdata$transformdataparameters$arcsin){
        maxlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = max,na.rm=T)
        minlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = min,na.rm=T)
        for (i in 2:dim(valfeatures)[2]){
          valfeatures[,i]<-(valfeatures[,i]-minlearn[i-1])/(maxlearn[i-1]-minlearn[i-1])
          valfeatures[which(valfeatures[,i]>1),i]<-1
          valfeatures[which(valfeatures[,i]<0),i]<-0
          valfeatures[,i]<-asin(sqrt(valfeatures[,i]))
        }
        learningselect[,-1]<-apply(X = learningselect[,-1],MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        learningselect[,-1]<-asin(sqrt(learningselect[,-1]))
      }
      if(resultsmodel$transformdata$transformdataparameters$standardization){
        sdselect<-apply(learningselect[,which(colnames(learningselect)%in%colnames(valfeatures))], 2, sd,na.rm=T)
        valfeatures<-scale(valfeatures,center=F,scale=sdselect[-1])
      }
      datastructuresfeatures<-resultsmodel$selectdata$DATASTRUCTUREDFEATURES
      if(!is.null(datastructuresfeatures)){
        valfeatures[which(is.na(valfeatures),arr.ind = T)[which(which(is.na(valfeatures),arr.ind = T)[,2]%in%which(colnames(valfeatures)%in%datastructuresfeatures$names)),]]<-0
      }
      valfeatures<- replaceNAvalidation(valfeatures,toto=learningmodel[,-1],rempNA=resultsmodel$parameters$transformdataparameters$rempNA)
      model<-resultsmodel$model$MODEL
      if(resultsmodel$parameters$modelparameters$modeltype=="randomforest"){
        score <- randomForest:::predict.randomForest(object=model,type="prob",newdata = valfeatures)[,lev["positif"]]
      } else {
        score <- attr(predict(model,newdata =  valfeatures,decision.values=T),"decision.values")
        if(sum(lev==(strsplit(colnames(score),split = "/")[[1]]))==0){score<-score*(-1)}
      }
      threshold<-resultsmodel$parameters$modelparameters$thresholdmodel
      pred<-ifelse(score>=threshold, lev["positif"], lev["negatif"]) 
      pred<-factor(pred, levels = lev)
      y_true<-factor(as.character(y_true), levels = lev)
      auc_val<-tryCatch({
        pROC::roc(response = y_true, predictor = as.numeric(score), levels = lev, direction = ">")$auc[1]
      }, error=function(e) NA)
      tp<-sum(y_true==lev["positif"] & pred==lev["positif"])
      tn<-sum(y_true==lev["negatif"] & pred==lev["negatif"])
      fp<-sum(y_true==lev["negatif"] & pred==lev["positif"])
      fn<-sum(y_true==lev["positif"] & pred==lev["negatif"])
      sens<-ifelse((tp+fn)==0, NA, tp/(tp+fn))
      spec<-ifelse((tn+fp)==0, NA, tn/(tn+fp))
      return(list(auc=as.numeric(auc_val), sensibility=sens, specificity=spec))
    }
    NULL
  })
  
  #####
  output$JDDprediction=renderDataTable({
    prediction<-PREDICT()$prediction
    colmin<-min(ncol(prediction),100)
    rowmin<-min(nrow(prediction),100)
    cbind(Names=rownames(prediction[1:rowmin,1:colmin]),prediction[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10,
                       "colnames"=FALSE))
  
  output$downloaddataJDDprediction <- downloadHandler(
    filename = function() { paste('dataset', '.','csv', sep='') },
    content = function(file) {
      downloaddataset(   PREDICT()$prediction, file) })
  
  output$JDDpredictiondiff = renderDT({
    predictiondiff<-PREDICT()$predictiondiff
    colmin<-min(ncol(predictiondiff),100)
    rowmin<-min(nrow(predictiondiff),100)
    cbind(Names=rownames(predictiondiff[1:rowmin,1:colmin]),predictiondiff[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10,
                       "scrollX"= TRUE,
                       "colnames"=FALSE)
  )
  
  output$downloaddataJDDpredictiondiff <- downloadHandler(
    filename = function() { paste('dataset', '.','xlsx', sep='') },
    content = function(file) {
      downloaddataset(   PREDICT()$predictiondiff, file) }
  )
  
  output$modelparameters=renderTable({
    if(!is.null(input$modelfile)){
      settingstable<<-MODEL()$settingstable
      modelparameters<-cbind(settingstable[3:4,c(2,7)],settingstable[c(17,18),2:3])
      modelparameters<-t(modelparameters)
      modelparameters[,1]<-paste("<strong>",modelparameters[,1],"</strong>")
      modelparameters<-as.data.frame(modelparameters,row.names=modelparameters[,1])
    }
  },sanitize.text.function=function(x){x},include.colnames=F)
  
  output$modelparameters2=renderTable({
    if(!is.null(input$modelfile)){
      settingstable<<-MODEL()$settingstable
      modelparameters<-cbind(settingstable[3:4,c(2,7)],settingstable[c(17,18),2:3])
      modelparameters<-t(modelparameters)
      modelparameters[,1]<-paste("<strong>",modelparameters[,1],"</strong>")
      modelparameters<-as.data.frame(modelparameters,row.names=modelparameters[,1])
    }
  },include.colnames=F,rownames=F,sanitize.text.function=function(x){x})
  
  
  output$modelmainresults = renderTable({
    if(!is.null(input$modelfile)){
      modelmainresults<-MODEL()$settingstable[c(19,20),3:8]
      modelmainresults<-as.data.frame(modelmainresults, stringsAsFactors = FALSE)
      modelmainresults<-t(modelmainresults)
      # Colonnes (1:2) = [label train, valeur train], (3:4) = [label val, valeur val]
      modelmainresults<-cbind(modelmainresults[1:3,1:2],modelmainresults[c(4:6),1:2])
      modelmainresults<-as.data.frame(modelmainresults, stringsAsFactors = FALSE)
      cat("modelmainresults\n")
      print(modelmainresults)
      # Remplace uniquement les valeurs validation si calculées
      vm<-validationMetrics()
      print( vm)
      if (!is.null(vm)){
        modelmainresults[1,4] <- round(vm$auc,3)
        modelmainresults[2,4] <- round(vm$sensibility,3)
        modelmainresults[3,4] <- round(vm$specificity,3)
      }
      modelmainresults[,1]<-paste("<strong>",as.character(modelmainresults[,1]),"</strong>")
      modelmainresults[,3]<-paste("<strong>",as.character(modelmainresults[,3]),"</strong>")
      modelmainresults
    }},include.colnames=F,
    sanitize.text.function=function(x){x},include.rownames=F
  )
  
  output$resprediction=renderTable({
    if(input$confirmdatabuttonpred!=0){
      res<<-cbind("Names"=rownames(PREDICT()$respredictiontionmodel),"Prediction"=as.character(PREDICT()$respredictiontionmodel[,1]),"Score"=PREDICT()$respredictiontionmodel[,2])
      res[,1]<-paste("<strong>",res[,1],"</strong>")
      res
    }
  },sanitize.text.function=function(x){x},include.rownames=F) 
  
  output$downloaddataresprediction <- downloadHandler(
    filename = function() { paste('data of score and predictions values ', '.','xlsx', sep='') },
    content = function(file) {
      downloaddataset(PREDICT()$respredictiontionmodel, file) 
    })
  
  
  output$plotscorepred <- renderPlot({
    if(input$confirmdatabuttonpred!=0){
      scorepredict<<-PREDICT()$respredictiontionmodel$scoreprediction
      score<<-MODEL()$model$DATALEARNINGMODEL$reslearningmodel$scorelearning
      thresholdmodel<<-MODEL()$parameters$modelparameters$thresholdmodel
      groups<<-PREDICT()$groups
      densityscore(score = score,
                   scorepredict = scorepredict,
                   maintitle="Density learning's score and prediction score",
                   threshold=thresholdmodel,
                   groups=groups)
    }
  })
  
  reac_plotscorepred = reactive({
    if(input$confirmdatabuttonpred!=0){
      scorepredict<<-PREDICT()$respredictiontionmodel$scoreprediction
      score<<-MODEL()$model$DATALEARNINGMODEL$reslearningmodel$scorelearning
      thresholdmodel<<-MODEL()$parameters$modelparameters$thresholdmodel
      groups<<-PREDICT()$groups
      densityscore(score = score,scorepredict = scorepredict,maintitle="Density learning's score and prediction score",threshold=thresholdmodel,groups=groups)
    }
  })
  
  
  output$downloadplotscorepred = downloadHandler(
    filename = function() {
      paste('graph',input$paramdownplot, ".jpg", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = densityscore(score = MODEL()$model$DATALEARNINGMODEL$reslearningmodel$scorelearning,
                                       scorepredict = PREDICT()$datapredictionmodel$respredictiontionmodel$scoreprediction,
                                       groups=PREDICT()$groups,
                                       maintitle="Density learning's score and prediction score",threshold=MODEL()$parameters$modelparameters$thresholdmodel),
             device = 'jpg')
      
    },
    contentType=NA)
  
  output$downloaddatascorepred <- downloadHandler(
    filename = function() { paste('dataset', '.',"xlsx", sep='') },
    content = function(file) {
      downloaddataset(densityscore(score = MODEL()$model$DATALEARNINGMODEL$reslearningmodel$scorelearning,
                                   scorepredict = PREDICT()$respredictiontionmodel$scoreprediction,
                                   groups=PREDICT()$groups,
                                   maintitle="Density learning's score and prediction score",threshold=MODEL()$parameters$modelparameters$thresholdmodel,graph=F)
                      , file)
    }
  )
  
}) 
