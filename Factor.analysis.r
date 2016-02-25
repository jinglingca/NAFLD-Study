library(FactoMineR)



test1<-test1%>% mutate(cyl=as.factor(cyl))
lapply(test1, as.numeric) %>% as.data.frame %>% PCA
FAMD(test1)
 

t<-biopsy5%>%mutate(steatosis1=factor(ifelse(steatosis==0,"steatosis0",
                                            ifelse(steatosis==1,"steatosis1",
                                                         ifelse(steatosis==2,"steatosis2",
                                                                ifelse(steatosis==3,"steatosis3",NA)))),
                                                                       
                                          levels=c("steatosis1","steatosis1","steatosis2","steatosis3")),                        
                          
                    fibrosis1=as.factor(fibrosis),
                          
                    lobinfl1=factor(ifelse(lobinfl==0,"lobinfl0",
                                        ifelse(lobinfl==1,"lobinfl1",
                                            ifelse(lobinfl==2,"lobinfl2",
                                                ifelse(lobinfl==3,"lobinfl3",NA)))),
                                          levels=c("lobinfl0","lobinfl1","lobinfl2","lobinfl3")),
                    
                    portal1=factor(ifelse(portal==0,"portal0",
                                          ifelse(portal==1,"portal1",
                                                ifelse(portal==2,"portal2",NA))),
                                         levels=c("portal0","portal1","portal2")),
                    
                    balloon1=factor(ifelse(balloon==0,"balloon0",
                                          ifelse(balloon==1,"balloon1",
                                                  ifelse(balloon==2,"balloon2",NA))),
                                         levels=c("balloon0","balloon1","balloon2")),
                          
                    nas1=factor(ifelse(nas=='2',"nas2",
                                      ifelse(nas=='3',"nas3",
                                              ifelse(nas=='4',"nas4",
                                                    ifelse(nas=='5',"nas5",
                                                          ifelse(nas=='6',"nas6",
                                                                  ifelse(nas=='7',"nas7",
                                                                ifelse(nas=='8',"nas8",NA))))))),
                                    levels=c("nas2","nas3","nas4","nas5","nas6","nas7","nas8")))

 
biopsy6<-t%>%select(gender,race,hispanic,bmi,bpsysto,bpdiasto,u_acid,hba1c,ast,alt,tot_chol,ldl,glucose,steatosis1,fibrosis1,lobinfl1,portal1,
                          balloon1,bmiz,age__yrs_dec_,zonality,nas1,ggt,waist,trig,hdl,s_insulin)  


lapply(biopsy6, as.numeric) %>% as.data.frame %>% PCA







name1<-colnames(biopsy1)
name2<-colnames(biopsy2)
name1[!name1%in%name2]

biopsy7<-biopsy1%>%select(income,diabetes_1,diabetes_2,hypertension,insulin,weight,droplet,microfat,steatodx,hyperlipidemia,cholelithiasis,fhx_liver,fhx_nafld,fhx_obesity,fhx_cholesterol,age__days_)

biopsy8<-biopsy7%>%mutate(income=as.factor(income),
                          diabetes_1_n=ifelse(is.na(diabetes_1),"no diabetes_1",
                                            ifelse(diabetes_1==1,"diabetes_1")),
                          diabetes_1_n=as.factor(diabetes_1_n),
                          
                          diabetes_2_n=ifelse(is.na(diabetes_2),"no diabetes_2",
                                              ifelse(diabetes_2==1,"diabetes_2")),
                          diabetes_2_n=as.factor(diabetes_2_n),
                          
                          hypertension_n=ifelse(is.na(hypertension),"no hypertension",
                                              ifelse(hypertension==1,"hypertension")),
                          hypertension_n=as.factor(hypertension_n),
                          
                          insulin_n=ifelse(is.na(insulin),"insulin user",
                                                ifelse(insulin==1,"insulin non")),
                          insulin_n=as.factor(insulin_n),
                          
                          hyperlipidemia_n=ifelse(is.na(hyperlipidemia)," non hyperlipidemia",
                                           ifelse(hyperlipidemia==1," hyperlipidemia")),
                          hyperlipidemia_n=as.factor( hyperlipidemia_n),
                          
                          cholelithiasis_n=ifelse(is.na(cholelithiasis)," non cholelithiasis",
                                                  ifelse(cholelithiasis==1," hyperlipidemia")),
                          cholelithiasis_n=as.factor(cholelithiasis_n),
                          
                          fhx_liver_n=ifelse(is.na(fhx_liver)," non fhx_liver",
                                                  ifelse(fhx_liver==1,"fhx_liver")),
                          fhx_liver_n=as.factor(fhx_liver_n),
                          
                          fhx_nafld_n=ifelse(is.na(fhx_nafld)," non fhx_nafld",
                                             ifelse(fhx_nafld==1,"fhx_nafld")),
                          fhx_nafld_n=as.factor(fhx_nafld_n),
                          
                          fhx_obesity_n=ifelse(is.na(fhx_obesity)," non fhx_obesity",
                                             ifelse(fhx_obesity==1,"fhx_obesity")),
                          fhx_obesity_n=as.factor(fhx_obesity_n),
                          
                          fhx_cholesterol_n=ifelse(is.na(fhx_cholesterol)," non fhx_cholesterol",
                                               ifelse(fhx_cholesterol==1,"fhx_cholesterol")),
                          fhx_cholesterol_n=as.factor(fhx_cholesterol_n),
                          
                          droplet_n=ifelse(is.na(droplet),"no result",
                                           ifelse(droplet==0,"droplet0",
                                                  ifelse(droplet==1,"droplet1",
                                                         ifelse(droplet==2,"droplet2",NA)))),
                          droplet_n=as.factor(droplet_n),
                    steatodx_n=as.factor(steatodx),
                    microfat_n=ifelse(microfat==0,'microfat0',
                                      ifelse(microfat==1,"microfat1",NA)),
                    microfat_n=as.factor(microfat_n))






 biopsy9<-biopsy8%>%select(income,diabetes_1_n,diabetes_2_n,hypertension_n,insulin_n,weight,droplet_n,microfat_n,steatodx_n,hyperlipidemia_n,cholelithiasis_n,fhx_liver_n,fhx_nafld_n,fhx_obesity_n,fhx_cholesterol_n,age__days_)
#                      
#                          
 biopsy_n<-cbind(biopsy6,biopsy9)                          
#                           
t1<-FAMD(biopsy_n)
 summary(t1)                          
# 
# sum(is.na(biopsy8$income))  
#                        
                          