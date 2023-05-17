select.one.level_simpler<-function(db.model.selection,variables.included,variables.to.test,c.start.gnls.general,db.constraint,db.fixed.variables,macrovar,vif.threshold){
  
  list.variables<-list(alpha=NULL,gamma=NULL)
  
  output<-data.table(alpha=character(),gamma=character(),all.alpha=character(),all.gamma=character(),AIC=numeric(),VIF.alert=logical())
  
  if(macrovar=="alpha"){ # test only the addition of an "alpha" variable
    for(var_alpha in variables.to.test$alpha){
      
      var_gamma<-""
      VIF.alert<-F
      
      # prepare formula
      list.variables$alpha<-c(variables.included$alpha,var_alpha)
      list.variables$gamma<-c(variables.included$gamma) # important to keep this line, to avoid using a previous list.variables$gamma
      
      all.variables<-unique(unlist(list.variables))
      
      # check collinearity
      all.variables.filtered<-iterate.remove.colinear.variables(
        db=db.ready.model.selection,
        all.variables=,all.variables,
        new.variable=var_alpha,
        vif.threshold=vif.threshold
      )
      
      if(length(all.variables.filtered)!=length(all.variables)){
        VIF.alert<-T
      }
      
      list.variables$alpha<-intersect(list.variables$alpha,all.variables.filtered)
      list.variables$gamma<-intersect(list.variables$gamma,all.variables.filtered)
      
      if(var_alpha%in%list.variables$gamma){
        list.variables$gamma<-setdiff(list.variables$gamma,var_alpha)
      }
      
      # formula
      formula.info<-build.formula_Func_simplified(list.predictors=list.variables)
      
      # print(paste0("alpha : ",list.variables$alpha))
      # print(paste0("gamma : ",list.variables$gamma))
      
      # infer
      results<-run.gnls(
        db.gnls=db.model.selection,
        c.start.gnls.general=c.start.gnls.general,
        formula.info =formula.info,
        formula=formula.info$formula_aggregated,
        db.constraint=db.constraint,
        algo="nlsLM",
        use.weight = T,
        fixed.variables=db.fixed.variables
      )
      
      new.output<-data.table(
        AIC=as.numeric(results$AIC.achieved),
        alpha=var_alpha,
        gamma=var_gamma,
        all.alpha=paste(list.variables$alpha,collapse=" & "),
        all.gamma=paste(list.variables$gamma,collapse=" & "),
        VIF.alert=VIF.alert
      )
      
      output<-rbind(output,new.output)
      
    }
  }
  
  if(macrovar=="gamma"){ # test only the addition of a "gamma" variable
    for(var_gamma in variables.to.test$gamma){
      
      var_alpha<-""
      VIF.alert<-F
      
      # prepare formula (add new variables in the variables.included, which contains variables already selected)
      list.variables$alpha<-c(variables.included$alpha) # important to keep this line, to avoid using a previous list.variables$alpha
      list.variables$gamma<-c(variables.included$gamma,var_gamma)
      
      all.variables<-unique(unlist(list.variables))
      
      # check collinearity
      all.variables.filtered<-iterate.remove.colinear.variables(
        db=db.ready.model.selection,
        all.variables=,all.variables,
        new.variable=var_gamma,
        vif.threshold=vif.threshold
      )
      
      if(length(all.variables.filtered)!=length(all.variables)){
        VIF.alert<-T
      }
      
      list.variables$alpha<-intersect(list.variables$alpha,all.variables.filtered)
      list.variables$gamma<-intersect(list.variables$gamma,all.variables.filtered)
      
      if(var_gamma%in%list.variables$alpha){
        list.variables$alpha<-setdiff(list.variables$alpha,var_gamma)
      }
      
      # formula
      formula.info<-build.formula_Func_simplified(list.predictors=list.variables)
      
      # print(paste0("alpha : ",list.variables$alpha))
      # print(paste0("gamma : ",list.variables$gamma))
      
      # infer
      results<-run.gnls(
        db.gnls=db.model.selection,
        c.start.gnls.general=c.start.gnls.general,
        formula.info =formula.info,
        formula=formula.info$formula_aggregated,
        db.constraint=db.constraint,
        algo="nlsLM",
        use.weight = T,
        fixed.variables=db.fixed.variables
      )
      
      new.output<-data.table(
        AIC=as.numeric(results$AIC.achieved),
        alpha=var_alpha,
        gamma=var_gamma,
        all.alpha=paste(list.variables$alpha,collapse=" & "),
        all.gamma=paste(list.variables$gamma,collapse=" & "),
        VIF.alert=VIF.alert
      )
      
      output<-rbind(output,new.output)
      
    } 
  }
  
  return(output)
  
}