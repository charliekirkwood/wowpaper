drop_predict_full <- function(...){
  rm(drop_model)
  
  load_model_weights_hdf5(object = mixture_model, filepath = paste0(getwd(), "/models/mixture_modelweights_rev.hdf5"))
  
  drop_model <- keras_model(
    inputs = c(conv_input, loc_input), 
    outputs = gaussian_output
  )
 
  droprates <- vector()
  for(l in 1:length(model$layers)){
    skip_to_next <- FALSE
    tryCatch(droprates <- append(droprates, model$layers[[l]]$rate), error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }     
  }
  
  dropweights <- weights
  
  layerrates <- data.frame(layer = head(which(lapply(lapply(dropweights, dim), length) > 1), -1), rate = droprates)
  layerrates$dims <- lapply(lapply(dropweights[layerrates$layer], dim), length)
  
  for(l in layerrates$layer){
    if(layerrates[which(layerrates$layer == l), ]$dims > 2){
      # spatial dropout for convolution
      sampl <- rep(rbinom(dim(dropweights[[l]])[4], 1, 1-layerrates[which(layerrates$layer == l), ]$rate))
      mask <- array(rep(sampl, each = 9), dim = dim(dropweights[[l]]))
      
      dropweights[[l]] <- (dropweights[[l]] * mask)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
      dropweights[[l+1]] <- (dropweights[[l+1]] * sampl)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
    } else {
      # standard dropout for fully connected layers
      mask <- rbinom(dim(dropweights[[l]])[2], 1, 1-layerrates[which(layerrates$layer == l), ]$rate)
      
      dropweights[[l]] <- t(t(dropweights[[l]]) * mask)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
      dropweights[[l+1]] <- (dropweights[[l+1]] * mask)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
    } 
  }
  
  drop_model %>% set_weights(dropweights)
  
  return(predict(drop_model, ...))
}




drop_predict_mean <- function(...){
  rm(drop_model)
  
  load_model_weights_hdf5(object = mixture_model, filepath = paste0(getwd(), "/models/mixture_modelweights_rev.hdf5"))
  
  drop_model <- keras_model(
    inputs = c(conv_input, loc_input), 
    outputs = final_output_signal
  )
  
  droprates <- vector()
  for(l in 1:length(model$layers)){
    skip_to_next <- FALSE
    tryCatch(droprates <- append(droprates, model$layers[[l]]$rate), error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }     
  }
  
  dropweights <- weights
  
  layerrates <- data.frame(layer = head(which(lapply(lapply(dropweights, dim), length) > 1), -1), rate = droprates)
  layerrates$dims <- lapply(lapply(dropweights[layerrates$layer], dim), length)
  
  for(l in layerrates$layer){
    if(layerrates[which(layerrates$layer == l), ]$dims > 2){
      # spatial dropout for convolution
      sampl <- rep(rbinom(dim(dropweights[[l]])[4], 1, 1-layerrates[which(layerrates$layer == l), ]$rate))
      mask <- array(rep(sampl, each = 9), dim = dim(dropweights[[l]]))
      
      dropweights[[l]] <- (dropweights[[l]] * mask)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
      dropweights[[l+1]] <- (dropweights[[l+1]] * sampl)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
    } else {
      # standard dropout for fully connected layers
      mask <- rbinom(dim(dropweights[[l]])[2], 1, 1-layerrates[which(layerrates$layer == l), ]$rate)
      
      dropweights[[l]] <- t(t(dropweights[[l]]) * mask)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
      dropweights[[l+1]] <- (dropweights[[l+1]] * mask)*(1/(1-layerrates[which(layerrates$layer == l), ]$rate))
    } 
  }
  
  drop_model %>% set_weights(dropweights)
  
  return(predict(drop_model, ...))
}
