infer.and.predict <- function(
  formula, 
  db.calib, 
  db.valid
){
  
  model <- lm(data = db.calib, formula = as.formula(formula))
  
  db.mod.calib <- as.data.table(cbind(
    db.calib,
    delta.DHm_pred = fitted(model),
    res = residuals(model))
  )
  
  db.mod.valid <- as.data.table(cbind( # output db for validation
    db.valid,
    delta.DHm_pred = predict(model, newdata = db.valid.dir)
  ))
  db.mod.valid[, res := delta.DHm_obs - delta.DHm_pred]
  
  return(data.table(
    aic = round(AIC(model), 1),
    r2.calib = round(get.R2(db = db.mod.calib), 2),
    r2.valid = round(get.R2(db = db.mod.valid), 2),
    formula =  formula
  ))
  
}
