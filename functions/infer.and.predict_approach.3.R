infer.and.predict.undir <- function(
  formula, 
  db.calib, 
  db.valid
){
  
  model <- lm(data = db.calib, formula = as.formula(formula))
  
  # calibration data
  db.output.calib <- as.data.table(cbind(
    db.calib,
    ME.pred = fitted(model),
    res = residuals(model))
  )
  
  db.delta.DHm.calib <- predict.delta.DHm_ME(db = db.output.calib) # collect output in terms of delta.DHm

  # validation data
  
  db.mod.valid <- as.data.table(cbind( # output db for validation
    db.valid,
    ME.pred = predict(model, newdata = db.valid, type = "response", allow.new.levels = TRUE)
  ))
  
  db.delta.DHm.valid <- predict.delta.DHm_ME(db = db.mod.valid)

  
  return(data.table(
    aic = round(AIC(model), 1),
    r2.calib = round(get.R2(db = db.delta.DHm.calib), 2),
    r2.valid = round(get.R2(db = db.delta.DHm.valid), 2),
    formula =  formula
  ))
  
}
