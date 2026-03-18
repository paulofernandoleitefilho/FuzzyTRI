rm(list=ls())
# Artigo 3

funcGraphic<-function(sistema1,ValMetricG,ValPLG){
  nameGrafico<-paste0('Graphic of Metric',ValMetricG,"LP",ValPLG,".tiff");
  tituloGrafico<-paste0('Graphic of Metric',ValMetricG,"LP",ValPLG,".tiff");
  
  path<-setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
  setwd(path); getwd();
  tiff(nameGrafico, units="in", width=5, height=5, res=300)
  #plotG1(vetDifficult,metric)
  par(mfrow=c(1,1))
  gensurf(sistema1)
  dev.off()
  
} # gerar graficos

FuzzyModel<-function(){

#install.packages("FuzzyR")
library(FuzzyR,shiny)
### -----------------

# Criar o Sistema Fuzzy
sistema1<-newfis("sistemaOne",fisType = "mamdani",andMethod="min", defuzzMethod="centroid")

# Add variaveis - entrada

sistema1<-addvar(sistema1,"input","In1", c(-6,6))     # niveis de dificuldade

sistema1<-addvar(sistema1,"input","In2", c(0,4))    # niveis da tabela de confusão

sistema1<-addvar(sistema1,"output","In3", c(0,100))    # Niveis de qualidade dos algoritmos


# membership function (mf)
sistema1<-addmf(sistema1,"input",1,"XE","trapmf", c(-6,-6,-4,-3))
sistema1<-addmf(sistema1,"input",1,"VE","trapmf", c(-4,-3,-2,-1))
sistema1<-addmf(sistema1,"input",1,"E","trapmf", c(-2,0,1,2))
sistema1<-addmf(sistema1,"input",1,"H","trapmf", c(0,1,2,3))
sistema1<-addmf(sistema1,"input",1,"VH","trapmf", c(2,3,4,5))
sistema1<-addmf(sistema1,"input",1,"XH","trapmf", c(4,5,6,6))

sistema1<-addmf(sistema1,"input",2,"FN","trimf", c(0,0,2))
sistema1<-addmf(sistema1,"input",2,"FP","trimf", c(1,2,3))
sistema1<-addmf(sistema1,"input",2,"TN","trimf", c(2,3,4))
sistema1<-addmf(sistema1,"input",2,"TP","trimf", c(3,4,4))

sistema1<-addmf(sistema1,"output",1,"Muito Ruim","trapmf", c(0,10,20,30))
sistema1<-addmf(sistema1,"output",1,"Ruim","trapmf", c(20,30,40,50))
sistema1<-addmf(sistema1,"output",1,"Regular","trapmf", c(40,50,60,70))
sistema1<-addmf(sistema1,"output",1,"Bom","trapmf", c(60,70,80,90))
sistema1<-addmf(sistema1,"output",1,"Melhor","trapmf", c(80,90,100,100))


# Criação das regras
regras<-rbind(c(1,1,1,1,2),
              c(2,1,1,1,2),
              c(3,1,1,1,2),
              c(4,1,1,1,2),
              c(5,1,1,1,2),
              c(6,1,1,1,2),
              c(1,2,1,1,2),
              c(2,2,1,1,2),
              c(3,2,1,1,2),
              c(4,2,1,1,2),
              c(5,2,1,1,2),
              c(6,2,1,1,2), # 1 a 12
              c(2,1,2,1,2),
              c(3,1,2,1,2),
              c(4,1,2,1,2),
              c(5,1,2,1,2),
              c(6,1,2,1,2),
              c(2,2,2,1,2),
              c(3,2,2,1,2),
              c(4,2,2,1,2),
              c(5,2,2,1,2),
              c(6,2,2,1,2), # 13 a 22
              c(1,3,3,1,2),
              c(2,3,3,1,2),
              c(3,3,3,1,2),
              c(4,3,3,1,2), # 23 a 26
              c(1,4,3,1,2),
              c(2,4,3,1,2),
              c(3,4,3,1,2),
              c(4,4,3,1,2), # 27 a 30
              c(4,3,4,1,2),
              c(5,3,4,1,2),
              c(6,3,4,1,2), # 31 a 33
              c(4,4,4,1,2),
              c(5,4,4,1,2),
              c(6,4,4,1,2), # 34 a 36
              c(4,3,5,1,2),
              c(5,3,5,1,2),
              c(6,3,5,1,2),
              c(4,4,5,1,2),
              c(5,4,5,1,2),
              c(6,4,5,1,2)) # 37 a 42
              


# Add regras ao sistema
sistema1<-addrule(sistema1,regras)

return(sistema1)

} # Fuzzy - parameters

AttribFuzzyModel<-function(dados,ValMetric,ValPL){
  
  sistema1<-FuzzyModel()

  saida<-evalfis(dados,sistema1)
  #zz<-showGUI(sistema1,TRUE)

  dados_normalizados <- (saida - min(saida)) / (max(saida) - min(saida))

  Comparar<-cbind(dados_normalizados,saida)

# gráfico do sistema
  ValMetricG<-ValMetric
  ValPLG<-ValPL
  
  
  funcGraphic(sistema1,ValMetricG,ValPLG)
  
  
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
  nameInputall<-paste0("SaidaMetric",ValMetric,"_PL_",ValPL,".csv")
  write.csv(saida,nameInputall)
  

return(dados_normalizados)

} # Sistem Fuzzy

AdjustLimits<-function(CurrentList){
  
  OldList<-CurrentList
  linListOrder<-dim(OldList)[1]
  
  for (indexOrder in 1:linListOrder){
    if(OldList[indexOrder,1]<(-6)){OldList[indexOrder,1]=(-6)}
    if(OldList[indexOrder,1]>(6)){OldList[indexOrder,1]=(6)}
  }
  linListOrder<-OldList
  
  return(linListOrder)
  
} # Ajusta os limietes acima de 6

StripeDifficultItens<-function(valueMetric,valuemoment){
  #SetMetric<-1
  #iDatasetCorrenteVet<-1
  #ialgorithm<-1
  #istepMoment<-1
  #valueMetric<-1
  #valuemoment<-2
  
  DatasetCorrenteVet<-c(1,3,4,5,13,14,17,19,20,21,24);#(1,3,4,5,13,14,16,17,19,20,21,24);
  algorithmList<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35)#c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
  vetPLmodels<-c(1,2,3)
  
  mtxAllDiffs<-c()
  iSetMetric<-valueMetric
  istepMoment<-valuemoment
  vetAllClassAlg<-c();contAlg<-0
  
  #for(iSetMetric in 1:1){
  for (iDatasetCorrenteVet in 1:length(DatasetCorrenteVet)){ # Dataset
    for (ialgorithm in 1:length(algorithmList)){ # algorithm
      #for(istepMoment in 1:length(vetPLmodels)){ # PL
      currentAlg<-c();
      
      if(iSetMetric=="1"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/MSE/backup_Projeto/SMOTE/Splits/Class'} # MSE
      if(iSetMetric=="2"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/ACC/backup_Projeto/SMOTE/Splits/Class'} # ACC
      if(iSetMetric=="3"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SEN/backup_Projeto/SMOTE/Splits/Class'} # SEN
      if(iSetMetric=="4"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SPE/backup_Projeto/SMOTE/Splits/Class'} # SPE
      
      setwd(pathDiffJoin); getwd(); # mtxJoinD1Alg1Difficult.csv
      setFile<-paste0('ClassmtxJoinD',DatasetCorrenteVet[iDatasetCorrenteVet],"Alg",algorithmList[ialgorithm],'Difficult',istepMoment,".csv");
      SetDataSelectedClass<-read.csv(file = setFile,header = TRUE, sep = ",")
      SetDataSelectedClass1<-SetDataSelectedClass[,2:40]
      SetDataSelectedClass<-cbind(SetDataSelectedClass1[1:25],SetDataSelectedClass1[27:39])
      szDataCurrent<-dim(SetDataSelectedClass)[1]
      
      if(istepMoment==1){vetDiff<-SetDataSelectedClass[,2]}
      if(istepMoment==2){vetDiff<-SetDataSelectedClass[,3]}
      if(istepMoment==3){vetDiff<-SetDataSelectedClass[,4]}
      
      currentAlg<-c()
      vetAllClassAlg<-c()
      for(indexAlg in 5:38){#38
        for(indexRow in 1:szDataCurrent){
          
          if((SetDataSelectedClass[indexRow,1]=="0")&&(SetDataSelectedClass[indexRow,indexAlg]=="0")){currentAlg[indexRow]<-1}
          if((SetDataSelectedClass[indexRow,1]=="1")&&(SetDataSelectedClass[indexRow,indexAlg]=="1")){currentAlg[indexRow]<-2}
          if((SetDataSelectedClass[indexRow,1]=="0")&&(SetDataSelectedClass[indexRow,indexAlg]=="1")){currentAlg[indexRow]<-3}
          if((SetDataSelectedClass[indexRow,1]=="1")&&(SetDataSelectedClass[indexRow,indexAlg]=="0")){currentAlg[indexRow]<-4}
          
          print(c(setFile,indexAlg,indexRow))
          #print("--------------------------------------------------------------------")
          #print(c("iDatasetCorrenteVet",iDatasetCorrenteVet,"ialgorithm",ialgorithm))
          #print(c("indexAlg",indexAlg,"indexRow",indexRow))
        }
        currentAlg1<-t(currentAlg);currentAlg<-currentAlg1
        vetAllClassAlg<-cbind(vetAllClassAlg,currentAlg)
        currentAlg<-c(); contAlg<-contAlg+1
        dim(vetAllClassAlg)
      }
      
      #mtxAlgByMetric<-c()
      vetSetDataSelectedClass<-rep(vetDiff,contAlg)
      vetAllClassAlg1<-t(vetAllClassAlg);vetAllClassAlg<-vetAllClassAlg1
      mtxAlgByMetric<-cbind(vetSetDataSelectedClass,vetAllClassAlg)
      dim(mtxAlgByMetric)
      
      #}
    }}
  
  mtxAlgByMetricOut<-AdjustLimits(mtxAlgByMetric)
  
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
  colnames(mtxAlgByMetricOut) <- c("In1", "In2")
  nameInputall<-paste0("VetToFuzzyInputmetric",valueMetric,"PL",valuemoment,".csv")
  write.csv(mtxAlgByMetricOut,nameInputall)
  
  return(mtxAlgByMetric)
}

CreateInputFuzzyFiles<-function(indexSelect){
  
  if(indexSelect==1){outPutFilePL1<-StripeDifficultItens(1,1)}
  if(indexSelect==2){outPutFilePL1<-StripeDifficultItens(1,2)}
  if(indexSelect==3){outPutFilePL1<-StripeDifficultItens(1,3)}
  
  if(indexSelect==4){outPutFilePL1<-StripeDifficultItens(2,1)}
  if(indexSelect==5){outPutFilePL1<-StripeDifficultItens(2,2)}
  if(indexSelect==6){outPutFilePL1<-StripeDifficultItens(2,3)}
  
  if(indexSelect==7){outPutFilePL1<-StripeDifficultItens(3,1)}
  if(indexSelect==8){outPutFilePL1<-StripeDifficultItens(3,2)}
  if(indexSelect==9){outPutFilePL1<-StripeDifficultItens(3,3)}
  
  if(indexSelect==10){outPutFilePL1<-StripeDifficultItens(4,1)}
  if(indexSelect==11){outPutFilePL1<-StripeDifficultItens(4,2)}
  if(indexSelect==12){outPutFilePL1<-StripeDifficultItens(4,3)}
  
  return(outPutFilePL1)
  
}

saveOutputFuzzy<-function(mtxOutputFuzzyAll,MetricIn){
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
  nameInputall<-paste0("MtxOutputFuzzyMetric",MetricIn,".csv")
  write.csv(mtxOutputFuzzyAll,nameInputall)
  
  if(MetricIn==1){namemetric<-"MSE"}
  if(MetricIn==2){namemetric<-"Accurcay"}
  if(MetricIn==3){namemetric<-"Sensibility"}
  if(MetricIn==4){namemetric<-"Specificity"}
  
  nameGrafico<-paste0('Boxplot - ',namemetric,".tiff");
  tituloGrafico<-paste0('Boxplot ',"Metric",namemetric,".tiff");
  
  tiff(nameGrafico, units="in", width=5, height=5, res=300)
  #plotG1(vetDifficult,metric)
  boxplot(mtxOutputFuzzyAll)
  dev.off()
  
} # create boxplot

TestAD<-function(){
  if(!require(dplyr)) install.packages("dplyr")
  library(dplyr)                                
  if(!require(RVAideMemoire)) install.packages("RVAideMemoire") 
  library(RVAideMemoire)                                        
  if(!require(car)) install.packages("car")   
  library(car)                                
  if(!require(psych)) install.packages("psych") 
  library(psych)                                
  if(!require(rstatix)) install.packages("rstatix") 
  library(rstatix)                                
  if(!require(DescTools)) install.packages("DescTools") 
  library(DescTools)
  if(!require(nortest)) install.packages("nortest") 
  library(nortest)
  if(!require(PMCMRplus)) install.packages("PMCMRplus") 
  library(PMCMRplus) 
  if(!require(PMCMR)) install.packages("PMCMR") 
  library(PMCMR)
  
  sve1All<-c()
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
  sveM1PL1<-read.csv(file = "SaidaMetric1_PL_1.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL1[,2])
  sveM1PL2<-read.csv(file = "SaidaMetric1_PL_2.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL2[,2])
  sveM1PL3<-read.csv(file = "SaidaMetric1_PL_3.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL3[,2])
  AD1All1 <- ad.test(sve1All)
  AD1All1[2]
  FT1All1 <- friedman.test(sve1All)
  FT1All1[3]
  
  y<-scale(sve1All, center = T)
  boxplot(y)
  FT1All4WT12<-wilcox.test(y[,1],y[,2], correct = FALSE,  alternative = "two.sided");FT1All4WT12
  FT1All4WT13<-wilcox.test(y[,1],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT13
  FT1All4WT23<-wilcox.test(y[,2],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT23
  
  
  sve1All<-c()
  setwd('F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff'); getwd();
  sveM1PL1<-read.csv(file = "SaidaMetric2_PL_1.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL1[,2])
  sveM1PL2<-read.csv(file = "SaidaMetric2_PL_2.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL2[,2])
  sveM1PL3<-read.csv(file = "SaidaMetric2_PL_3.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL3[,2])
  AD1All2 <- ad.test(sve1All)
  AD1All2[2]
  FT1All2 <- friedman.test(sve1All)
  FT1All2[3]
  
  y<-scale(sve1All, center = T)
  boxplot(y)
  FT1All4WT12<-wilcox.test(y[,1],y[,2], correct = FALSE,  alternative = "two.sided");FT1All4WT12
  FT1All4WT13<-wilcox.test(y[,1],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT13
  FT1All4WT23<-wilcox.test(y[,2],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT23
  
  
  sve1All<-c()
  setwd('F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff'); getwd();
  sveM1PL1<-read.csv(file = "SaidaMetric3_PL_1.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL1[,2])
  sveM1PL2<-read.csv(file = "SaidaMetric3_PL_2.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL2[,2])
  sveM1PL3<-read.csv(file = "SaidaMetric3_PL_3.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL3[,2])
  AD1All3 <- ad.test(sve1All)
  AD1All3[2]
  FT1All3 <- friedman.test(sve1All)
  FT1All3[3]
  
  y<-scale(sve1All, center = T)
  boxplot(y)
  FT1All4WT12<-wilcox.test(y[,1],y[,2], correct = FALSE,  alternative = "two.sided");FT1All4WT12
  FT1All4WT13<-wilcox.test(y[,1],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT13
  FT1All4WT23<-wilcox.test(y[,2],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT23
  
  
  sve1All<-c()
  setwd('F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff'); getwd();
  sveM1PL1<-read.csv(file = "SaidaMetric4_PL_1.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL1[,2])
  sveM1PL2<-read.csv(file = "SaidaMetric4_PL_2.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL2[,2])
  sveM1PL3<-read.csv(file = "SaidaMetric4_PL_3.csv",header = TRUE, sep = ",")
  sve1All<-cbind(sve1All,sveM1PL3[,2])
  AD1All4 <- ad.test(sve1All)
  AD1All4[2]
  FT1All4 <- friedman.test(sve1All)
  FT1All4[3]
  
  y1<-scale(sve1All, center = T)
  y<-data.frame(y)
  boxplot(y)
  
  FT1All4WT12<-wilcox.test(y[,1],y[,2], correct = FALSE,  alternative = "two.sided");FT1All4WT12
  FT1All4WT13<-wilcox.test(y[,1],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT13
  FT1All4WT23<-wilcox.test(y[,2],y[,3], correct = FALSE,  alternative = "two.sided");FT1All4WT23
  
  
  OutP-value<-c(AD1All1[2],AD1All2[2],AD1All3[2],AD1All4[2])
  
  
  frdAllPairsNemenyiTest(sveM1PL1[,2],sveM1PL2[,2],sveM1PL3[,2], p.adjust.method = "bonferroni")
  frdAllPairsNemenyiTest(y[,1] ~ y[,2], p.adjust.method = "bonferroni")
  
  
  
  frdAllPairsNemenyiTest(y)
  
  return(OutP-value)
  
} # teste de normalidade

RunOuputs<-function(){
  mtxOutputFuzzyAll<-c()
  
  dados11<-CreateInputFuzzyFiles(1)
  dataRaw11<-AttribFuzzyModel(dados11,1,1)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw11)
  boxplot(mtxOutputFuzzyAll)
  
  dados12<-CreateInputFuzzyFiles(2)
  dataRaw12<-AttribFuzzyModel(dados12,1,2)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw12)
  boxplot(mtxOutputFuzzyAll)
  
  dados13<-CreateInputFuzzyFiles(3)
  dataRaw13<-AttribFuzzyModel(dados13,1,3)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw13)
  boxplot(mtxOutputFuzzyAll)
  
  saveOutputFuzzy(mtxOutputFuzzyAll,1)
  mtxOutputFuzzyAll<-c()
  #-----------------------------
  dados21<-CreateInputFuzzyFiles(4)
  dataRaw21<-AttribFuzzyModel(dados21,2,1)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw21)
  boxplot(mtxOutputFuzzyAll)
  
  dados22<-CreateInputFuzzyFiles(5)
  dataRaw22<-AttribFuzzyModel(dados22,2,2)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw22)
  boxplot(mtxOutputFuzzyAll)
  
  dados23<-CreateInputFuzzyFiles(6)
  dataRaw23<-AttribFuzzyModel(dados23,2,3)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw23)
  boxplot(mtxOutputFuzzyAll)
  
  saveOutputFuzzy(mtxOutputFuzzyAll,2)
  mtxOutputFuzzyAll<-c()
  #-----------------------------
  dados31<-CreateInputFuzzyFiles(7)
  dataRaw31<-AttribFuzzyModel(dados31,3,1)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw31)
  boxplot(mtxOutputFuzzyAll)
  
  dados32<-CreateInputFuzzyFiles(8)
  dataRaw32<-AttribFuzzyModel(dados32,3,2)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw32)
  boxplot(mtxOutputFuzzyAll)
  
  dados33<-CreateInputFuzzyFiles(9)
  dataRaw33<-AttribFuzzyModel(dados33,3,3)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw33)
  boxplot(mtxOutputFuzzyAll)
  
  saveOutputFuzzy(mtxOutputFuzzyAll,3)
  mtxOutputFuzzyAll<-c()
  #-----------------------------
  dados41<-CreateInputFuzzyFiles(10)
  dataRaw41<-AttribFuzzyModel(dados41,4,1)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw41)
  boxplot(mtxOutputFuzzyAll)
  
  dados42<-CreateInputFuzzyFiles(11)
  dataRaw42<-AttribFuzzyModel(dados42,4,2)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw42)
  boxplot(mtxOutputFuzzyAll)
  
  dados43<-CreateInputFuzzyFiles(12)
  dataRaw43<-AttribFuzzyModel(dados43,4,3)
  mtxOutputFuzzyAll<-cbind(mtxOutputFuzzyAll,dataRaw43)
  boxplot(mtxOutputFuzzyAll)
  
  saveOutputFuzzy(mtxOutputFuzzyAll,4)
  
} # save outputFuzzy

# --------------------------
LevelDifficultItens<-function(valueMetric){
  #iSetMetric<-1
  #iDatasetCorrenteVet<-1
  #ialgorithm<-1
  #istepMoment<-1
  DatasetCorrenteVet<-c(1,3,4,5,13,14,17,19,20,21,24);#(1,3,4,5,13,14,16,17,19,20,21,24);
  algorithmList<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35)#c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
  vetPLmodels<-c(1,2,3)
  
  mtxAllDiffs<-c()
  iSetMetric<-valueMetric
  
  #for(iSetMetric in 1:1){
    for (iDatasetCorrenteVet in 1:length(DatasetCorrenteVet)){ # Dataset
      for (ialgorithm in 1:length(algorithmList)){ # algorithm
        for(istepMoment in 1:length(vetPLmodels)){ # PL
          
          if(iSetMetric=="1"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/MSE/backup_Projeto/SMOTE/Splits/Class'} # MSE
          if(iSetMetric=="2"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/ACC/backup_Projeto/SMOTE/Splits/Class'} # ACC
          if(iSetMetric=="3"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SEN/backup_Projeto/SMOTE/Splits/Class'} # SEN
          if(iSetMetric=="4"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SPE/backup_Projeto/SMOTE/Splits/Class'} # SPE
          
          setwd(pathDiffJoin); getwd(); # mtxJoinD1Alg1Difficult.csv
          setFile<-paste0('ClassmtxJoinD',DatasetCorrenteVet[iDatasetCorrenteVet],"Alg",algorithmList[ialgorithm],'Difficult',istepMoment,".csv");
          SetDataSelectedClass<-read.csv(file = setFile,header = TRUE, sep = ",")
          SetDataSelectedClass1<-SetDataSelectedClass[,2:40]
          SetDataSelectedClass<-cbind(SetDataSelectedClass1[1:25],SetDataSelectedClass1[27:39])
          
          print(setFile)
          
          
          # Difficult Levels
          countSET<-0;countVET<-0;countET<-0;countVHT<-0;countHT<-0;countXHT<-0;
          countSEF<-0;countVEF<-0;countEF<-0;countVHF<-0;countHF<-0;countXHF<-0;
          mtxTRUE<-c();mtxFALSE<-c();dataChunk<-c()
          
          dataChunk<-SetDataSelectedClass[,3]; # vector b
          szPass<-length(dataChunk);
          dataOneTarget<-SetDataSelectedClass[,1]; # Target class
          algTarget<-SetDataSelectedClass[,5:38]; # Matrix of classifications
          szalgTarget<-dim(algTarget)[2]
          
          
          for(istepDiff in 1:szalgTarget){ #Classifications
            
            for (jstepDiff in 1:szPass){ # Vector b
              
              if((dataOneTarget[jstepDiff]) == (algTarget[jstepDiff,istepDiff])){ # check if was wright
                
                if(dataChunk[jstepDiff] < -4){countSET<-countSET+1}
                if((dataChunk[jstepDiff] >= -4) && (dataChunk[jstepDiff]< -2)){countVET<-countVET+1}
                if((dataChunk[jstepDiff] >= -2)&&(dataChunk[jstepDiff]< 0)){countET<-countET+1}
                if((dataChunk[jstepDiff] >= 0)&&(dataChunk[jstepDiff]< 2)){countHT<-countHT+1}
                if((dataChunk[jstepDiff] >= 2)&&(dataChunk[jstepDiff]< 4)){countVHT<-countVHT+1}
                if(dataChunk[jstepDiff] >= 4){countXHT<-countXHT+1}
              }
              
              if((dataOneTarget[jstepDiff]) != (algTarget[jstepDiff,istepDiff])){ # check if was wrong
                
                if(dataChunk[jstepDiff]< -4){countSEF<-countSEF+1}
                if((dataChunk[jstepDiff]>= -4) && (dataChunk[jstepDiff]< -2)){countVEF<-countVEF+1}
                if((dataChunk[jstepDiff]>= -2)&&(dataChunk[jstepDiff]< 0)){countEF<-countEF+1}
                if((dataChunk[jstepDiff]>= 0)&&(dataChunk[jstepDiff]< 2)){countHF<-countHF+1}
                if((dataChunk[jstepDiff]>= 2)&&(dataChunk[jstepDiff]< 4)){countVHF<-countVHF+1}
                if(dataChunk[jstepDiff]>= 4){countXHF<-countXHF+1}
              }
              
              TotalCorrect<-(countSET+countVET+countET+countHT+countVHT+countXHT)
              TotalWrong<-(countSEF+countVEF+countEF+countHF+countVHF+countXHF) 
            }
            mtxTRUE<-c((countSET/TotalCorrect)*100,(countVET/TotalCorrect)*100,(countET/TotalCorrect)*100,(countHT/TotalCorrect)*100,(countVHT/TotalCorrect)*100,(countXHT/TotalCorrect)*100,(TotalCorrect/TotalCorrect)*100,iSetMetric,DatasetCorrenteVet[iDatasetCorrenteVet],algorithmList[ialgorithm],istepMoment)
            mtxFALSE<-c((countSEF/TotalWrong)*100,(countVEF/TotalWrong)*100,(countEF/TotalWrong)*100,(countHF/TotalWrong)*100,(countVHF/TotalWrong)*100,(countXHF/TotalWrong)*100,(TotalWrong/TotalWrong)*100,iSetMetric,DatasetCorrenteVet[iDatasetCorrenteVet],algorithmList[ialgorithm],istepMoment)
            vetTF<-rbind(mtxTRUE,mtxFALSE)
            mtxTargets<-rbind(c("CORRECT:",mtxTRUE),c("WRONG:",mtxFALSE))
            headTargets<-c("MODE","SE","VE","E","H","VH","XH","TOTAL","Metric","Dataset","Algorithm","PL")
            colnames(mtxTargets)<-headTargets
          }
          
          # Save 
          mtxAllDiffs<-rbind(mtxAllDiffs,vetTF)
          
          pathLevel<-'I:/Tese - Bkp_Projeto_Doutorado/LevelDiff'
          setwd(pathLevel); getwd(); # mtxvetDificultPLD1Alg1Difficult1_1000
          nameDiffsall<-paste0("LevelDiffmtxD",DatasetCorrenteVet[iDatasetCorrenteVet],"Alg",algorithmList[ialgorithm],'Difficult',istepMoment,"metric",iSetMetric,".csv")
          write.csv(mtxAllDiffs,nameDiffsall)
          
        }}}#}
  
  
}

LevelConfusionItens<-function(valueMetric){
  
  iSetMetric<-valueMetric
  #iDatasetCorrenteVet<-1
  #ialgorithm<-1
  #istepMoment<-1
  
  DatasetCorrenteVet<-c(1,3,4,5,13,14,17,19,20,21,24);
  algorithmList<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35)
  vetPLmodels<-c(1,2,3)
  
  mtxAllDiffs<-c();vetItem<-c(); mtxItem<-c();vetTemp<-c()
  vetMtxConfusion<-c(); mtxConfusionClassification<-c();
  
  #for(iSetMetric in 1:1){
  for (iDatasetCorrenteVet in 1:length(DatasetCorrenteVet)){ # Dataset
    for (ialgorithm in 1:length(algorithmList)){ # algorithm
      for(istepMoment in 1:length(vetPLmodels)){ # PL
        
        if(iSetMetric=="1"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/MSE/backup_Projeto/SMOTE/Splits/Class'} # MSE
        if(iSetMetric=="2"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/ACC/backup_Projeto/SMOTE/Splits/Class'} # ACC
        if(iSetMetric=="3"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SEN/backup_Projeto/SMOTE/Splits/Class'} # SEN
        if(iSetMetric=="4"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SPE/backup_Projeto/SMOTE/Splits/Class'} # SPE
        
        setwd(pathDiffJoin); getwd(); # mtxJoinD1Alg1Difficult.csv
        setFile<-paste0('ClassmtxJoinD',DatasetCorrenteVet[iDatasetCorrenteVet],"Alg",algorithmList[ialgorithm],'Difficult',istepMoment,".csv");
        SetDataSelectedClass1<-read.csv(file = setFile,header = TRUE, sep = ",")
        SetDataSelectedClass<-SetDataSelectedClass1[,6:40]
        SetDataSelectedTarget<-SetDataSelectedClass1[,2]
        
        # Difficult Levels
        szalgTarget<-dim(SetDataSelectedClass)[1]; # vector b
        dataChunk<-SetDataSelectedClass;
        szListAlg<-length(algorithmList)
        
    
        #print("Check in 1")
      for(algTarget in 1:szListAlg){
        for (step4 in 1:szalgTarget){
      # TP=1; TN=2;FN=3 AND FP=4
          
      if((SetDataSelectedTarget[step4]=="0")&&(dataChunk[step4,algTarget]=="0")){vetMtxConfusion[step4]<-1}
      if((SetDataSelectedTarget[step4]=="1")&&(dataChunk[step4,algTarget]=="1")){vetMtxConfusion[step4]<-2}
      if((SetDataSelectedTarget[step4]=="0")&&(dataChunk[step4,algTarget]=="1")){vetMtxConfusion[step4]<-3}
      if((SetDataSelectedTarget[step4]=="1")&&(dataChunk[step4,algTarget]=="0")){vetMtxConfusion[step4]<-4}
      
        vetMtxConfusion<-rbind(vetMtxConfusion,vetMtxConfusion[step4])
      }
      mtxConfusionClassification<-cbind(mtxConfusionClassification,vetMtxConfusion)
      vetMtxConfusion<-c()
      
      #print(c(iSetMetric,iDatasetCorrenteVet,ialgorithm,istepMoment))
    }
        print(c(iSetMetric,iDatasetCorrenteVet,ialgorithm,istepMoment))
      }
     
      }}
    
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); # mtxvetDificultPLD1Alg1Difficult1_1000
  nameDiffsall<-paste0("JoinALLmtxConfusionClassification",iSetMetric,".csv")
  write.csv(mtxConfusionClassification,nameDiffsall)
  
  return(mtxConfusionClassification)
}

AttibDiffAllLevels<-function(valueMetric){
  iSetMetric<-valueMetric
  #iDatasetCorrenteVet<-1
  #ialgorithm<-1
  #istepMoment<-1
  
  DatasetCorrenteVet<-c(1,3,4,5,13,14,17,19,20,21,24);
  algorithmList<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30,31,32,33,34,35)
  vetPLmodels<-c(1,2,3)
  
  mtxAllDiffs<-c();vetItem<-c(); mtxItem<-c();vetTemp<-c()
  #for(iSetMetric in 1:1){
  for (iDatasetCorrenteVet in 1:length(DatasetCorrenteVet)){ # Dataset
    for (ialgorithm in 1:length(algorithmList)){ # algorithm
      for(istepMoment in 1:length(vetPLmodels)){ # PL
        
        if(iSetMetric=="1"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/MSE/backup_Projeto/SMOTE/Splits/Class'} # MSE
        if(iSetMetric=="2"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/ACC/backup_Projeto/SMOTE/Splits/Class'} # ACC
        if(iSetMetric=="3"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SEN/backup_Projeto/SMOTE/Splits/Class'} # SEN
        if(iSetMetric=="4"){pathDiffJoin<-'F:/Tese - Bkp_Projeto_Doutorado/SPE/backup_Projeto/SMOTE/Splits/Class'} # SPE
        
        setwd(pathDiffJoin); getwd(); # mtxJoinD1Alg1Difficult.csv
        setFile<-paste0('ClassmtxJoinD',DatasetCorrenteVet[iDatasetCorrenteVet],"Alg",algorithmList[ialgorithm],'Difficult',istepMoment,".csv");
        SetDataSelectedClass<-read.csv(file = setFile,header = TRUE, sep = ",")
        SetDataSelectedClass<-SetDataSelectedClass[,3:5]
        
        # Difficult Levels
        szalgTarget<-dim(SetDataSelectedClass)[1]; # vector b
        dataChunk<-SetDataSelectedClass;
        
        for(istepDiff in 1:szalgTarget){ #Classifications
          for (jstepDiff in 1:3){ # Vector b
            # XE=1;VE=2;E=3;H=4;VH=5 AND XH=6
            
            if(dataChunk[istepDiff,jstepDiff] < -4){vetItem[jstepDiff]<-1}
            if((dataChunk[istepDiff,jstepDiff] >= -4) && (dataChunk[istepDiff,jstepDiff]< -2)){vetItem[jstepDiff]<-2}
            if((dataChunk[istepDiff,jstepDiff] >= -2)&&(dataChunk[istepDiff,jstepDiff]< 0)){vetItem[jstepDiff]<-3}
            if((dataChunk[istepDiff,jstepDiff] >= 0)&&(dataChunk[istepDiff,jstepDiff]< 2)){vetItem[jstepDiff]<-4}
            if((dataChunk[istepDiff,jstepDiff] >= 2)&&(dataChunk[istepDiff,jstepDiff]< 4)){vetItem[jstepDiff]<-5}
            if(dataChunk[istepDiff,jstepDiff] >= 4){vetItem[jstepDiff]<-6}
            
            vetTemp<-cbind(vetTemp,vetItem[jstepDiff])
            
          }
          #print(c(iDatasetCorrenteVet,ialgorithm,istepMoment,vetTemp))
          mtxItem<-rbind(mtxItem,vetTemp)
          vetTemp<-c()
          dim(mtxItem)
        }
        print(c(iSetMetric,iDatasetCorrenteVet,ialgorithm,istepMoment))
      }}
    
  }
  
  
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); # mtxvetDificultPLD1Alg1Difficult1_1000
  nameDiffsall<-paste0("JoinALLLevelDiffmtx",iSetMetric,".csv")
  write.csv(mtxItem,nameDiffsall)
  
  return(mtxItem)
  
}

JoinAllAttibLevels<-function(){
  mtxAllItens<-c(); mtxTempItens<-c();
  
  mtxTempItens<-AttibDiffAllLevels(1)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  mtxTempItens<-AttibDiffAllLevels(2)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  mtxTempItens<-AttibDiffAllLevels(3)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  mtxTempItens<-AttibDiffAllLevels(4)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); # mtxvetDificultPLD1Alg1Difficult1_1000
  nameDiffsallMetrics<-paste0("JoinALLLevelDiffmtxALLMetrics.csv")
  write.csv(mtxAllItens,nameDiffsallMetrics)
}

# executa a função LevelConfusionItens em bloco de metricas
JoinLevelConfusionItens<-function(){
  mtxAllItens<-c(); mtxTempItens<-c();
  
  mtxTempItens<-LevelConfusionItens(1)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  mtxTempItens<-LevelConfusionItens(2)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  mtxTempItens<-LevelConfusionItens(3)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  mtxTempItens<-LevelConfusionItens(4)
  mtxAllItens<-rbind(mtxAllItens,mtxTempItens)
  
  
  setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); # mtxvetDificultPLD1Alg1Difficult1_1000
  nameDiffsallMetrics<-paste0("JoinLevelConfusionItens.csv")
  write.csv(mtxAllItens,nameDiffsallMetrics)
  
  
} 

dataSelect<-function(setVal){
  
  if (setVal ==1){  
    dados1 <-read.csv("D:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff\\JoinALLLevelDiffmtx1.csv", head=TRUE, sep=",")
    dados<-as.matrix(dados1[,2:4])
    d1<-AttribFuzzyModel(dados)
    setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
    nameDiffsall<-paste0("Saida1.csv")
    write.csv(d1,nameDiffsall)
  }
  
  if (setVal ==2){
    dados1 <-read.csv("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff\\JoinALLLevelDiffmtx2.csv", head=TRUE, sep=",")
    dados<-as.matrix(dados1[,2:4])
    d2<-AttribFuzzyModel(dados)
    setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
    nameDiffsall<-paste0("Saida2.csv")
    write.csv(d2,nameDiffsall)
  }
  
  if (setVal ==3){
    dados1 <-read.csv("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff\\JoinALLLevelDiffmtx3.csv", head=TRUE, sep=",")
    dados<-as.matrix(dados1[,2:4])
    d3<-AttribFuzzyModel(dados)
    setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
    nameDiffsall<-paste0("Saida3.csv")
    write.csv(d3,nameDiffsall)
  }
  
  if (setVal ==4){
    dados1 <-read.csv("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff\\JoinALLLevelDiffmtx4.csv", head=TRUE, sep=",")
    dados<-as.matrix(dados1[,2:4])
    d4<-AttribFuzzyModel(dados)
    setwd("F:\\Tese - Bkp_Projeto_Doutorado\\JoinLevelsDiff"); getwd(); 
    nameDiffsall<-paste0("Saida4.csv")
    write.csv(d4,nameDiffsall)}
}
# -------------------------

