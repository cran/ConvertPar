
conv.rt<-function(small.data, train.data, model="2PL",pruned=TRUE,min.inst=10){

  p<- colMeans(train.data)
  total.score<-rowSums(train.data)
  r<-c()

  for(i in 1:ncol(train.data)){
    r[i]<-cor(train.data[,i],total.score)
  }

  mod<-mirt(train.data,model=1, itemtype = model)
  par<-coef(mod,IRTpars=TRUE,simplify=TRUE)

  if(model=="2PL"){
    a <- par[[1]][,1]
  }

  else if(model=="Rasch"){
    a<-rep(1,length(r))
  }

  b <- par[[1]][,2]
  datapars<-cbind(a,b,r,p)
  datapars<-as.data.frame(datapars)

  if(model=="2PL"){
    modela<-RWeka::M5P(formula = a~ p+r,
                       data=datapars,control = Weka_control(N=pruned,M=min.inst)
    )

    modelb<-RWeka::M5P(formula = b~ p+r,
                       data=datapars,
                       control = Weka_control(N=pruned,M=min.inst))
  }

  else if(model=="Rasch"){
    modelb<-RWeka::M5P(formula = b~ p+r,
                       data=datapars,
                       control = Weka_control(N=pruned,M=min.inst))
  }

  else{
    stop("Please select one of following options as a string : Rasch, 2PL (Default model is 2PL) ")
  }

  p<-colMeans(small.data)

  total.score<-rowSums(small.data)
  r<-c()

  for(i in 1:ncol(small.data)){
    r[i]<-cor(small.data[,i],total.score)
  }

  params<-cbind(p,r)
  params<-as.data.frame(params)

  if(model=="2PL"){
    a_pre<- predict(modela,params)
    b_pre<- predict(modelb,params)

    predicted_irt_params<-cbind(a_pre,b_pre)
    colnames(predicted_irt_params)<-c("a.pre","b.pre")
    train.data.parameters<-datapars
    output<-list(predicted_irt_params,train.data.parameters, modela,modelb)
    names(output)<-c("Predicted IRT Parameters","Item Parameters of Training Data","Model for Parameter a","Model for Parameter b")
  }

  if(model=="Rasch"){

    b_pre<- predict(modelb,params)
    predicted_irt_params<-as.data.frame(b_pre)
    colnames(predicted_irt_params)<-c("b.pre")
    train.data.parameters<-datapars
    output<-list(predicted_irt_params,train.data.parameters, modelb)
    names(output)<-c("Predicted IRT Parameters","Item Parameters of Training Data","Model for Parameter b")
  }

  return(output)
}

