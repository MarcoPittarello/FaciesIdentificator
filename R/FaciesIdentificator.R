#' Facies Identificator
#'
#' @description Identifica le facies pastorali sensu Cavallero et al (2007) di rilievi vegetazionali  
#' @param tbd Dataframe con rilievi vegetazionali da identificare (tbd=To be defined). Righe = Specie (nomenclatura Aeschimann et al 2004), Colonne = Rilievi
#' @param ref Dataframe con facies di riferimento. Righe = Specie (nomenclatura Aeschimann et al 2004), Colonne = Rilievi
#' @param prime10tbd LOGICAL. Se TRUE utilizza le prime 10 specie in ordine di abbondanza decrescente del database con rilievi da identificare
#' @param soglia LOGICAL.Se TRUE saranno filtrate le facies con un indice di somiglianza (Jaccard o Bray) minore della soglia impostata con *valore.soglia*
#' @param valore.soglia Valore numerico (0-1) da impostare se *SOGLIA=TRUE*. Valore default=0.5
#' @return ...
#' @examples ...
#' @export

FaciesIdentificator=function(tbd,ref,prime10tbd=T,soglia=F,valore.soglia=0.5){
  
  colnames(tbd)[1]<-"specie"
  colnames(ref)[1]<-"specie"
  
  # ten species TBD 
  tdf.list10<-lapply(2:ncol(tbd), function(i){
    tbd.sel<-data.frame(tbd[,c(1,i)])
    name.tbd.sel<-names(tbd.sel)[-1]
    tbd.sel<-tbd.sel[order(tbd.sel[,2],decreasing = T),]
    tbd.sel<-tbd.sel[1:10,]
    tbd.sel<-subset(tbd.sel,tbd.sel[,2]>0)
  })
  tbd10 <- Reduce(function(x, y) merge(x, y, by = "specie", all = TRUE), tdf.list10)
  tbd10[is.na(tbd10)] <- 0
  
  # ten species REF 
  tenSpe.Ref<-do.call(rbind,
                      lapply(2:ncol(ref), function(i){
                        ref.sel<-data.frame(ref[,c(1,i)])
                        name.ref.sel<-names(ref.sel)[-1]
                        ref.sel<-ref.sel[order(ref.sel[,2],decreasing = T),]
                        ref.sel$spe.abb<-paste(make.cepnames(ref.sel[,1]),round(ref.sel[,2],1),sep = " ")
                        colnames(ref.sel)[2]<-"abbondanza"
                        ref.sel<-subset(ref.sel,abbondanza>0)
                        df.ref.sel<-data.frame(Facies=name.ref.sel,
                                               Spe.Abb=paste(ref.sel$spe.abb,collapse = "| "))
                      })
  )
  
  # cep names
  
  
  # processing
  if(prime10tbd==T){
    data=tbd10
  }else if (prime10tbd==F){
    data=tbd
  }
  
  risultato<-lapply(2:ncol(data), function(i){
    
    tbd.sel<-data[,c(1,i)]
    
    ril.sel<-names(tbd.sel)[-1]
    
    mrgd<-merge(ref,tbd.sel,by="specie")
    rownames(mrgd)<-mrgd$specie
    mrgd<-mrgd[,-1]
    t.mrgd<-t(mrgd)
    
    # bray
    brayDist<-data.frame(as.matrix(vegdist(t.mrgd,method = "bray",upper = T)))
    brayDist$Facies<-rownames(brayDist)
    brayDist<-brayDist[,names(brayDist) %in% c("Facies",ril.sel)]
    brayDist<-brayDist[brayDist$Facies!=ril.sel,]
    brayDist<-merge(brayDist,tenSpe.Ref,by="Facies")
    brayDist<-brayDist[order(brayDist[,2],decreasing = F),]
    brayDist$Rilievo<-names(brayDist)[2]
    
    # Jaccard
    Jacc<-data.frame(as.matrix(vegdist(t.mrgd,binary=T,method = "jaccard",upper = T)))
    Jacc$Facies<-rownames(Jacc)
    Jacc<-Jacc[,names(Jacc) %in% c("Facies",ril.sel)]
    Jacc<-Jacc[Jacc$Facies!=ril.sel,]
    Jacc<-merge(Jacc,tenSpe.Ref,by="Facies")
    Jacc<-Jacc[order(Jacc[,2],decreasing = F),]
    Jacc$Rilievo<-names(Jacc)[2]
    
    
    # output complessivo (info per rilievo)
    output.info.rilievo<-list(Bray=brayDist,
                              Jaccard=Jacc)
    
    #output sintetico
    brayDist$Facies.BrayDist<-paste(brayDist$Facies,round(brayDist[,2],3),sep = ": ")
    Jacc$Facies.Jacc<-paste(Jacc$Facies,round(Jacc[,2],3),sep = ": ")
    
    if(soglia==T){
      brayDist.soglia<-subset(brayDist,brayDist[,2]<=valore.soglia)
      Jacc.soglia<-subset(Jacc,Jacc[,2]<=valore.soglia)
      
      elencoFaciesBray<-t(data.frame(Facies=brayDist.soglia$Facies.BrayDist))
      rownames(elencoFaciesBray)<-names(brayDist)[2]
      
      elencoFaciesJacc<-t(data.frame(Facies=Jacc.soglia$Facies.Jacc))
      rownames(elencoFaciesJacc)<-names(Jacc)[2]
      
      output.sintetico<-list(Bray=elencoFaciesBray,
                             Jaccard=elencoFaciesJacc)
      
      output<-list(info.rilievo=output.info.rilievo,
                   sintetico=output.sintetico)
      
      
    } else if (soglia==F){
      elencoFaciesBray<-t(data.frame(Facies=brayDist$Facies.BrayDist))
      rownames(elencoFaciesBray)<-names(brayDist)[2]
      
      elencoFaciesJacc<-t(data.frame(Facies=Jacc$Facies.Jacc))
      rownames(elencoFaciesJacc)<-names(Jacc)[2]
      
      output.sintetico<-list(Bray=elencoFaciesBray,
                             Jaccard=elencoFaciesJacc)
      
      output<-list(info.rilievo=output.info.rilievo,
                   sintetico=output.sintetico)
    }
  })
  
  
  # unione output sintetici 
  ris.bray <- do.call(rbind, lapply(1:(ncol(data)-1), function(i) {
    mat <- risultato[[i]]$sintetico$Bray
    n<-length(mat)
    n.max <- max(sapply(risultato, function(x) ncol(x$sintetico$Bray))) #numero colonne massimo nei risultati
    # Se ha meno del numero max di colonne, aggiunge colonne vuote
    if (n < n.max) {
      mat <- cbind(mat, matrix(NA, nrow = 1, ncol = n.max - n))
    }
    colnames(mat) <- paste0("Facies_", 1:n.max)  # nomi coerenti
    mat
  }))
  
  ris.Jacc <- do.call(rbind, lapply(1:(ncol(data)-1), function(i) {
    mat <- risultato[[i]]$sintetico$Jaccard
    n<-length(mat)
    n.max <- max(sapply(risultato, function(x) ncol(x$sintetico$Jaccard))) #numero colonne massimo nei risultati
    # Se ha meno del numero max di colonne, aggiunge colonne vuote
    if (n < n.max) {
      mat <- cbind(mat, matrix(NA, nrow = 1, ncol = n.max - n))
    }
    colnames(mat) <- paste0("Facies_", 1:n.max)  # nomi coerenti
    mat
  }))
  
  # unione output complessivi 
  ris.bray.TOT<-do.call(rbind,lapply(1:(ncol(data)-1), function(i){
    mat<-risultato[[i]]$info.rilievo$Bray
    colnames(mat)[2]<-"BrayDistance"
    mat
  }))
  
  ris.Jacc.TOT<-do.call(rbind,lapply(1:(ncol(data)-1), function(i){
    mat<-risultato[[i]]$info.rilievo$Jaccard
    colnames(mat)[2]<-"JaccardDistance"
    mat
  }))
  
  
  # output complessivo 
  output.list<-list(Jaccard=list(complessivo=ris.Jacc.TOT,
                                 sintetico=ris.Jacc),
                    Bray=list(complessivo=ris.bray.TOT,
                              sintetico=ris.bray))
  
  return(output.list)
}
