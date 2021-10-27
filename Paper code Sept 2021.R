library(data.table)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(gganimate)
library(av)
library(fasttime)

library(tfprobability)
library(tfaddons)
library(keras)

library(scoringRules)
library(scoringutils)

options(scipen = 6)

dir.create(paste0(getwd(), "/plots"))
dir.create(paste0(getwd(), "/models"))

list.files("data")

wowdat <- fread("data/WOWnoID_180days.csv")[,-(1:2)]
str(wowdat)
wowdat

latlon <- SpatialPoints(wowdat[, c("LNGD","LTTD")], CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
bng <- spTransform(latlon, CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs "))
str(bng)

wowdat[, bng_easting := bng@coords[,1], ][, bng_northing := bng@coords[,2], ]
wowdat[, site_no := .GRP, by = c("bng_easting", "bng_northing")]
wowdat[, datetime_hour := fastPOSIXct(paste(YEAR,MNTH,DAY,HOUR, sep = "/")), ]
wowdat[, datetime_minute := fastPOSIXct(paste(YEAR,MNTH,DAY,HOUR,MINT, sep = "/")), ]
wowdat[, TOD_cos := cos(2*pi*((HOUR*60)+MINT)/(24*60)), ][, TOD_sin := sin(2*pi*((HOUR*60)+MINT)/(24*60)), ]
wowdat
nrow(unique(wowdat[, c("bng_easting", "bng_northing")]))

# Download UK elevation data ####
ukelev <- getData("alt", country = "GBR", mask = FALSE)
ielev <- getData("alt", country = "IE", mask = FALSE)
bielev <- projectRaster(merge(ukelev, ielev), crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs") 
names(bielev) <- "alt"
elevdat <- as.data.table(raster::as.data.frame(bielev, xy = TRUE, na.rm = TRUE))
elevdat

# Sample elevation at each observation site
wowdat[, elevation := extract(bielev, methods = "bilinear", wowdat[, c("bng_easting", "bng_northing"), with = FALSE]), ]

# Plot of UK elevation data
ggplot(elevdat) + geom_raster(aes(x = x, y = y, fill = alt)) + theme_bw() +
  scale_fill_viridis_c(option = "cividis", begin = 0.2, end = 1) + coord_equal() +
  theme(legend.position = "none") + labs(x = "Easting (BNG)", y = "Northing (BNG)") +
  scale_x_continuous(limits = c(-180000,660000)) + scale_y_continuous(limits = c(0,1200000))
# ggsave(paste0("plots/UKelevation.png"), width = 68, height = 100, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Plot of WOW weather station locations
ggplot() + geom_point(data = unique(wowdat[, c("bng_easting", "bng_northing")]), aes(x = bng_easting, y = bng_northing), col = "black", size = 0.5, alpha = 0.5) +
  scale_fill_viridis_c(option = "cividis", begin = 0.2, end = 1) + coord_equal() + theme_bw() +
  theme(legend.position = "none") + labs(x = "Easting (BNG)", y = "Northing (BNG)") +
  scale_x_continuous(limits = c(-180000,660000)) + scale_y_continuous(limits = c(0,1200000))
# ggsave(paste0("plots/UKweatherstations.png"), width = 68, height = 100, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# # Download UK outline data ####
# gbout <- spTransform(getData("GADM", country = "GBR", level = 0), crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")) 
# gbout <- fortify(gbout, region = "GID_0")
# 
# # Plot of WOW weather station sites within UK outline
# ggplot(gbout) + geom_path(aes(x = long, y = lat, group = group)) + theme_bw() +
#   geom_point(data = unique(wowdat[, c("bng_easting","bng_northing")]), aes(x = bng_easting, y = bng_northing),
#              col = "black", shape = 16, size = 0.5, alpha = 0.2) +
#   coord_equal() +
#   scale_x_continuous(limits = c(-10000,660000)) + scale_y_continuous(limits = c(0,1200000)) +
#   theme(legend.position = "none") + labs(x = "Easting (BNG)", y = "Northing (BNG)")
# ggsave(paste0("plots/UKgeochem.png"), width = 68, height = 100, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

#### Choose a variable to model ####

#convert temp to Celcius
wowdat[, SRFC_AIR_TMPR := (SRFC_AIR_TMPR - 273.15), ]

names(wowdat)
var <- "SRFC_AIR_TMPR"
#var <- "PRCTN_RATE"

# Histogram of the variable
hist(wowdat[sample(1:nrow(wowdat), 10000)][get(var) > -999, get(var)], breaks = 20)
mean(wowdat[sample(1:nrow(wowdat), 10000)][get(var) > -999, get(var)])

wowsnap <- wowdat[, lapply(.SD, mean), by = c("bng_easting", "bng_northing", "YEAR", "MNTH", "DAY", "HOUR")][datetime_hour == sample(unique(datetime_hour), 1) & get(var) > -999]
#a good snapshot - "2020-09-24 09:00:00 BST"
#a good snapshot - "2020-11-15 07:00:00 BST"
nrow(wowsnap)

# Plot the variable for a single random timestep (note outliers):
ggplot(wowsnap) + 
  geom_point(aes(x = bng_easting, y = bng_northing, col = get(var)), size = 1, alpha = 0.8) +
  scale_colour_viridis_c(option = "cividis", begin = 0.2, end = 1, name = var) + coord_equal() + theme_bw() +
  theme(legend.position = c(0.02,0.98), legend.justification = c(0,1)) + 
  labs(x = "Easting (BNG)", y = "Northing (BNG)") +
  scale_x_continuous(limits = c(-180000,660000)) + scale_y_continuous(limits = c(0,1200000))

# Plot the variable for a single random timestep (with cropped colour scale:
ggplot(wowsnap) + 
  geom_point(aes(x = bng_easting, y = bng_northing, col = get(var)), size = 1, alpha = 0.8) +
  scale_colour_viridis_c(option = "cividis", begin = 0.2, end = 1, name = var, 
                         limits = c(quantile(wowsnap[,get(var)], 0.05), quantile(wowsnap[,get(var)], 0.95))) + 
  coord_equal() + theme_bw() +
  theme(legend.position = c(0.02,0.98), legend.justification = c(0,1)) + 
  labs(x = "Easting (BNG)", y = "Northing (BNG)") +
  scale_x_continuous(limits = c(-180000,660000)) + scale_y_continuous(limits = c(0,1200000))


#### Extract sample-centred terrain images, of specified dimension and resolution ####
# NB, this will be expanded to include model forecast grids in addition to terrain #
# We could also include satellite imagery to account for albedo #
set.seed(123)

#wowsub <- wowsnap
# wowsub <- wowdat[, lapply(.SD, mean), by = c("bng_easting", "bng_northing", "YEAR", "MNTH", "DAY", "HOUR")][datetime_hour >= "2020-11-06" & datetime_hour < "2020-11-13" & get(var) > -999][order(datetime_minute)]
# wowsub <- wowdat[datetime_hour >= "2020-10-01" & datetime_hour < "2020-10-15" & get(var) > -999][, .SD[sample(.N, 1)], by = c("bng_easting", "bng_northing", "YEAR", "MNTH", "DAY", "HOUR")][order(datetime_minute)]
# wowsub <- wowdat[datetime_hour >= "2020-11-01" & datetime_hour < "2020-12-01" & get(var) > -999][, .SD[sample(.N, 1)], by = c("bng_easting", "bng_northing", "YEAR", "MNTH", "DAY", "HOUR")][order(datetime_minute)]
wowsub <- wowdat[datetime_hour >= "2020-10-26" & datetime_hour <= "2020-11-09" & get(var) > -999][, .SD[sample(.N, 1)], by = c("bng_easting", "bng_northing", "YEAR", "MNTH", "DAY", "HOUR")][order(datetime_minute)]
# wowsub <- wowsub[sample(1:nrow(wowsub), nrow(wowsub)/10)]
nrow(wowsub)
unique(wowsub$datetime_hour)

ggplot(wowsub) + geom_line(aes(x = datetime_minute, y = SRFC_AIR_TMPR, group = site_no, col = bng_northing), alpha = 0.2, size = 0.1) + 
  theme_bw() + theme(legend.justification = c(0, 0), legend.position = c(0, 0), legend.background=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_datetime(date_breaks = "1 days", guide = guide_axis(angle = 45)) +
  labs(col = "Latitude \n(metres BNG)", x = "Time", y = "Surface air temperature (°C)")

imagedim <- 32
imageres <- 250

imgs <- array(dim = c(nrow(wowsub), imagedim, imagedim))

seq.int(from = imageres/2, by = imageres, length.out = imagedim)-(imageres*imagedim)/2

cells <- as.data.table(expand.grid(x = seq.int(from = imageres/2, by = imageres, length.out = imagedim)-(imageres*imagedim)/2, 
                                   y = seq.int(from = imageres/2, by = imageres, length.out = imagedim)-(imageres*imagedim)/2))
cells[ ,coordx := rep(1:imagedim, imagedim),]
cells[ ,coordy := rep(1:imagedim, each = imagedim),]

time <- Sys.time()
for(xmeter in unique(cells$coordx)){
  for(ymeter in unique(cells$coordy)){
    imgs[,xmeter,ymeter] <- extract(bielev, method = "bilinear",
                                    cbind(wowsub$bng_easting + cells[coordx == xmeter & coordy == ymeter]$x, wowsub$bng_northing + cells[coordx == xmeter & coordy == ymeter]$y)) 
  }
}
Sys.time() - time

imgs[is.na(imgs)] = 0

# Plot an image at random:
ggplot(reshape2::melt(imgs[sample(1:nrow(wowsub), 1),,]), aes(Var1,Var2, fill=value)) +
  geom_raster() + theme_bw() + scale_fill_viridis_c(option = "B", name = "elevation") +
  coord_fixed()

# Scale sample-centered terrain images for use in neural network ####

mean = mean(as.data.frame(bielev, na.rm = TRUE)[,1])
sd =  sd(as.data.frame(bielev, na.rm = TRUE)[,1])

imgs_ann <- imgs - wowsub$elevation
imgs_ann <- (imgs_ann)/sd
imgs_ann[is.na(imgs_ann)] = 0
#imgs_ann <- imgs_ann - rowMeans(imgs_ann, na.rm = TRUE)

# Plot a normalised image at random
ggplot(reshape2::melt(imgs_ann[sample(1:nrow(wowsub), 1),,]), aes(Var1,Var2, fill=value)) + 
  geom_raster() + theme_bw() + scale_fill_viridis_c(option = "B", name = "normalised \n elevation") +
  coord_fixed()

# Extract easting, northing and elevation as location variables

#loc <- wowsub[, c("bng_easting", "bng_northing", "elevation", "DAY", "HOUR"), with = FALSE]
loc <- wowsub[, c("bng_easting", "bng_northing", "elevation", "datetime_minute", "TOD_cos", "TOD_sin"), with = FALSE]
loc[, datetime_minute := as.numeric(datetime_minute), ]

locmean <- apply(loc, 2, mean)
locsd <- apply(loc, 2, sd)

loc_ann <- t(apply(loc, 1, function(x) (x - locmean)/locsd))

loc_ann[which(is.na(loc_ann))] <- 0
loc_ann

# Assign site_no as ID for sensor embedding:

site_nos <- as.factor(unlist(wowsub[, "site_no"]))
str(site_nos)
length(unique(site_nos))

site <- model.matrix(object = ~.-1, data = data.frame(site_nos))
site
str(site)

site[1,1:10]

# Plot and save three random terrain images to use as figure in paper
set.seed(789)
img1 <- reshape2::melt(imgs_ann[sample(1:nrow(wowsub), 1),,])
img2 <- reshape2::melt(imgs_ann[sample(1:nrow(wowsub), 1),,])
img3 <- reshape2::melt(imgs_ann[sample(1:nrow(wowsub), 1),,])

ggplot(img1, aes(Var1,Var2, fill=value)) + 
  geom_raster() + theme_bw() + 
  scale_fill_viridis_c(option = "cividis", limits = c(min(rbind(img1,img2,img3)$value),max(rbind(img1,img2,img3)$value))) +
  coord_fixed() + labs(x = "", y = "") + theme(legend.position = "none")
# ggsave(paste0("plots/img1.png"), width = 45, height = 45, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

ggplot(img2, aes(Var1,Var2, fill=value)) + 
  geom_raster() + theme_bw() +
  scale_fill_viridis_c(option = "cividis", limits = c(min(rbind(img1,img2,img3)$value),max(rbind(img1,img2,img3)$value))) +
  coord_fixed() + labs(x = "", y = "") + theme(legend.position = "none")
# ggsave(paste0("plots/img2.png"), width = 45, height = 45, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

ggplot(img3, aes(Var1,Var2, fill=value)) + 
  geom_raster() + theme_bw() + 
  scale_fill_viridis_c(option = "cividis", limits = c(min(rbind(img1,img2,img3)$value),max(rbind(img1,img2,img3)$value))) +
  coord_fixed() + labs(x = "", y = "") + theme(legend.position = "none")
# ggsave(paste0("plots/img3.png"), width = 45, height = 45, units = "mm", type = "cairo", dpi = 300, scale = 1.375)


# Use neural network to learn relationship between terrain features and geochemistry ####

set.seed(321)

# # data split by observation
# fold_size = nrow(wowsub)/10
# test <- sample(1:nrow(wowsub), fold_size)
# val <- sample(which(!1:nrow(wowsub) %in% test), fold_size)
# #train <- sample(which(!1:nrow(wowsub) %in% c(test, val)), length(which(!1:nrow(wowsub) %in% c(test, val)))*0.1)
# train <- which(!1:nrow(wowsub) %in% c(test, val))

# data split by site
fold_size = nrow(unique(wowsub[, "site_no"]))/10
test <- wowsub[site_no %in% sample(1:nrow(unique(wowsub[, "site_no"])), fold_size), which = TRUE]
val <- wowsub[site_no %in% sample(1:nrow(unique(wowsub[-test][, "site_no"])), fold_size), which = TRUE]
train <- which(!1:nrow(wowsub) %in% c(test, val))

# Data Preparation --------------------------------------------------------

x_train <- list(imgs_ann[train,,], loc_ann[train, ], site[train, ])
x_val <- list(imgs_ann[val,,], loc_ann[val, ], site[val, ])
x_test <- list(imgs_ann[test,,], loc_ann[test, ], site[test, ])

dim(x_train[[1]]) <- c(nrow(x_train[[1]]), imagedim, imagedim, 1)
dim(x_val[[1]]) <- c(nrow(x_val[[1]]), imagedim, imagedim, 1)
dim(x_test[[1]]) <- c(nrow(x_test[[1]]), imagedim, imagedim, 1)

y_train <- as.numeric(wowsub[,get(var)])[train]
y_val <- as.numeric(wowsub[,get(var)])[val]
y_test <- as.numeric(wowsub[,get(var)])[test]


# Defining Model --------------------------------------------------------####

# The signal twin network (dropout rates tuned for epistemic uncertainty) ####

# Auxiliary grid image input:
conv_input <- layer_input(shape = c(imagedim, imagedim, 1), name = 'conv_input')

# Space-time input:
loc_input <- layer_input(shape = c(ncol(loc)), name = 'loc_input')

# Site ID input:

site_input <- layer_input(shape = c(ncol(site)), name = 'site_input')


conv_output_signal <- conv_input %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), dilation_rate = 1, strides = 1) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(3, 3), strides = 2) %>%
  layer_spatial_dropout_2d(rate = 0.5) %>%
  
  layer_conv_2d(filter = 32, kernel_size = c(3,3), dilation_rate = 1, strides = 1) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(3, 3), strides = 2) %>%
  layer_spatial_dropout_2d(rate = 0.5) %>%
  
  layer_conv_2d(filter = 32, kernel_size = c(3,3), dilation_rate = 1, strides = 1) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(3, 3), strides = 1) %>%
  layer_spatial_dropout_2d(rate = 0.5) %>%

  # layer_conv_2d(filter = 32, kernel_size = c(3,3), dilation_rate = 1, strides = 1) %>%
  # layer_activation("relu") %>%
  # layer_max_pooling_2d(pool_size = c(3, 3), strides = 1) %>%
  # layer_spatial_dropout_2d(rate = 0.5)%>%
  # 
  # layer_conv_2d(filter = 32, kernel_size = c(2,2), dilation_rate = 1, strides = 1) %>%
  # layer_activation("relu") %>%
  # layer_spatial_dropout_2d(rate = 0.5)%>%
  
  layer_flatten()

loc_output_signal <- loc_input %>%
  layer_dense(2016) %>%
  layer_activation("swish") %>%
  layer_dropout(rate = 0.1) %>%
  layer_flatten()

final_output_signal <- layer_concatenate(c(conv_output_signal, loc_output_signal)) %>%  
  layer_dense(256) %>%
  layer_activation("relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(128) %>%
  layer_activation("relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 2, activation = "linear", name = "dist_param_signal")

# The outlier twin network (overfit not an issue, so no dropout) ####

# site_output_outlier <- site_input %>%
#   layer_dense(256) %>%
#   layer_activation("relu") %>%
#   layer_flatten()

time_output_outlier <- layer_multiply(list(k_expand_dims(k_variable(array(c(0,0,0,1,0,0))), 1), loc_input)) %>%
  layer_dense(256) %>%
  layer_activation("relu") %>%
  layer_flatten()

final_output_outlier <- layer_concatenate(c(site_input, time_output_outlier)) %>%
  # layer_dense(256) %>%
  # layer_activation("relu") %>%
  layer_dense(units = 2, activation = "linear", name = "dist_param_outlier")

# Output stage - where twin networks contribute parameters to mixture distribution ####

#x <- cbind(rnorm(10,1,3), rnorm(10,0,2), rnorm(10,1,1), rnorm(10,-1,1))

# Mixture output (outliers modeled by separate distribution learned by outlier twin):
mixture_output <- layer_distribution_lambda(layer_concatenate(c(final_output_signal, final_output_outlier)), function(x){
  mu <-  tf$math$real(x[, 1, drop = FALSE])
  sigmasq <- 1e-6 + tf$math$softplus(x[, 2, drop = FALSE])
  
  # mutwo <- tf$math$real(x[, 3, drop = FALSE])
  return(tfd_mixture(
    cat = tfd_categorical(logits = tf$stack(list(tf$math$real(x[, 3, drop = FALSE]), tf$math$real(x[, 4, drop = FALSE])), axis = 2L)),
    components = list(
      tfd_normal(loc = mu, 
                 scale = sigmasq),
      tfd_uniform(low = mu - 50,
                  high = mu + 50)
    ),
    use_static_graph = NULL
  ))}
)

# Robust output (only Gaussian used, for prediction):
gaussian_output <- layer_distribution_lambda(final_output_signal, function(x){
  mu <-  tf$math$real(x[, 1, drop = FALSE])
  sigmasq <- 1e-6 + tf$math$softplus(x[, 2, drop = FALSE])
  return(tfd_normal(loc = mu, 
                    scale = sigmasq)
  )
}
)

rm(mixture_model)
mixture_model <- keras_model(
  inputs = c(conv_input, loc_input, site_input), 
  outputs = mixture_output
)

summary(mixture_model)

negloglik <- function(y, model) - (model %>% tfd_log_prob(y))

opt <- optimizer_adam(lr = 0.001, decay = 1e-6)

mixture_model %>% compile(
  loss = negloglik,
  optimizer = opt
)



# rm(gaussian_model)
# gaussian_model <- keras_model(
#   inputs = c(conv_input, loc_input, site_input), 
#   outputs = gaussian_output
# )
# 
# gaussian_model %>% compile(
#   loss = negloglik,
#   optimizer = opt
# )


#negloglik(y_test, model(x_test))

# weights <- get_weights(model)
# weights

# Training ----------------------------------------------------------------

time <- Sys.time()
mixture_history <- mixture_model %>% fit(
  x_train, y_train,
  batch_size = 2^12,
  epochs = 500,
  validation_data = list(x_val, y_val),
  shuffle = TRUE
  # shuffle = TRUE,
  # callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 100, restore_best_weights = TRUE),
  #                  callback_model_checkpoint(monitor = "val_loss", save_best_only = TRUE, save_weights_only = TRUE, 
  #                                            filepath = paste0(getwd(), "/models/mixture_modelweights"))) 
)
Sys.time() - time

min(mixture_history$metrics$val_loss)
which(mixture_history$metrics$val_loss == min(mixture_history$metrics$val_loss))

save_model_weights_hdf5(mixture_model, paste0(getwd(), "/models/mixture_modelweights_rev.hdf5"))
load_model_weights_hdf5(object = mixture_model, filepath = paste0(getwd(), "/models/mixture_modelweights_rev.hdf5"))

rm(robust_model)
robust_model <- keras_model(
  inputs = c(conv_input, loc_input), 
  outputs = gaussian_output
)

rm(robust_meanmodel)
robust_meanmodel <- keras_model(
  inputs = c(conv_input, loc_input),
  outputs = final_output_signal
)

rm(mixture_meanmodel)
mixture_meanmodel <- keras_model(
  inputs = c(conv_input, loc_input, site_input), 
  outputs = c(final_output_signal, final_output_outlier)
)

trainhist <- as.data.table(mixture_history$metrics)[,c("loss", "val_loss"), with = FALSE]
names(trainhist) <- c("training", "validation")
trainhist[, epoch := 1:.N, ]

# Evaluation --------------------------------------------------------------
# Deterministic

k_set_learning_phase(0)

wowsubtest <- copy(wowsub[test, ])
ggplot(wowsubtest) + geom_line(aes(x = datetime_minute, y = SRFC_AIR_TMPR, group = site_no, col = bng_northing), alpha = 0.5, size = 0.5) + 
  theme_bw() + theme(legend.justification = c(0, 0), legend.position = c(0.2, 0), legend.background=element_blank())


# Including outliers:
#holdout <- data.table(obs = y_test, preds = as.numeric(tfd_mean(robust_model(x_test[1:2]))))
holdout <- data.table(obs = y_test, preds = predict(robust_meanmodel, x_test[1:2])[,1])
#holdout <- data.table(obs = y_test, preds = as.numeric(drop_predict(robust_model, x_test[1:2])))
#holdout <- data.table(obs = y_test, preds = as.numeric(predict(mixture_model, x_test)))
ggplot(holdout) + geom_point(aes(x = obs, y = preds), shape = 16, size = 0.5, alpha = 0.2) + 
  coord_equal() + theme_bw() + geom_abline(slope = 1, intercept = 0) + theme(aspect.ratio = 1) +
  labs(x = paste("Observed", var),
       y = paste("Predicted", var), 
       subtitle = paste0("R\U00B2 = ", round(cor(holdout$preds, holdout$obs)^2, 2), "     RMSE = ", round(sqrt(mean((holdout$preds-holdout$obs)^2)), 2)))
ggsave(paste0("plots/", var, "_mean_holdout_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, ".png"), width = 89, height = 89, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Excluding outliers:
holdout_trim <- holdout[(obs - preds)^2 < quantile((obs - preds)^2, 0.99995, na.rm = TRUE)]
ggplot(holdout_trim) + geom_point(aes(x = obs, y = preds), shape = 16, size = 0.5, alpha = 0.2) + 
  coord_equal() + theme_bw() + geom_abline(slope = 1, intercept = 0) + theme(aspect.ratio = 1) +
  labs(x = paste("observed", var),
       y = paste("predicted", var), 
       subtitle = paste0("R\U00B2 = ", round(cor(holdout_trim$preds, holdout_trim$obs)^2, 2), "     RMSE = ", round(sqrt(mean((holdout_trim$preds-holdout_trim$obs)^2)), 2)),
       tag = expression(bold("a")))
ggsave(paste0("plots/", var, "_mean_holdout_trim_", round(cor(holdout_trim$preds, holdout_trim$obs)^2, 2)*100, ".png"), width = 89, height = 89, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Plot training history
ggplot(melt(trainhist, id.vars = "epoch", value.name = "NLL", variable.name = "dataset")) + 
  geom_line(aes(x = epoch, y = NLL, col = dataset)) + theme_bw() + 
  scale_y_continuous(limits = c(0,quantile(c(trainhist$training, trainhist$validation), 0.95))) # +
  # geom_vline(xintercept = trainhist[validation == min(validation)]$epoch, linetype = "dashed")
ggsave(paste0("plots/", var, "_training_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, ".png"), width = 136, height = 60, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# We can see the probabilities of assignment that the neural network has learned for each training data point:

wowsubpost <- copy(wowsub[train, ])
ggplot(wowsubpost) + geom_line(aes(x = datetime_minute, y = SRFC_AIR_TMPR, group = site_no, col = bng_northing), alpha = 0.2, size = 0.1) + 
  theme_bw() + theme(legend.justification = c(0, 0), legend.position = c(0, 0), legend.background=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_datetime(breaks =  seq(min(wowsubpost$datetime_minute), max(wowsubpost$datetime_minute), by="1 day"), guide = guide_axis(angle = 45)) +
  labs(col = "Latitude \n(metres BNG)", x = "Time", y = "Surface air temperature (°C)")
ggsave(paste0("plots/", var, "_raw_WOW_data_visualisation.png"), width = 136, height = 100, units = "mm", type = "cairo", dpi = 300, scale = 1.375)


wowsubpost <- cbind(wowsubpost, data.table(as.matrix(tf$math$softmax(predict(mixture_meanmodel, x_train, batch_size = 4096)[[2]]))))
wowsubpost[, site_V1 := mean(V1), by = site_no]
wowsubpost <- wowsubpost[order(site_V1)]

ggplot(wowsubpost[order(-site_V1)][V1 > 0.5]) + geom_line(data = wowsubpost[order(site_V1)][V1 <= 0.5], aes(x = datetime_minute, y = SRFC_AIR_TMPR, group = site_no), alpha = 0.5, size = 0.2, col = "darkred") +
  geom_line(aes(x = datetime_minute, y = SRFC_AIR_TMPR, group = site_no, col = bng_northing), alpha = 0.2, size = 0.1) + 
  theme_bw() + theme(legend.justification = c(0, 0), legend.position = c(0, 0), legend.background=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_datetime(breaks =  seq(min(wowsubpost$datetime_minute), max(wowsubpost$datetime_minute), by="1 day"), guide = guide_axis(angle = 45)) +
  labs(col = "Latitude \n(metres BNG)", x = "Time", y = "Surface air temperature (°C)")
ggsave(paste0("plots/", var, "_outlier_classified_WOW_data_visualisation.png"), width = 136, height = 100, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

ggplot(wowsubpost[order(site_V1)]) + geom_line(aes(x = datetime_minute, y = SRFC_AIR_TMPR, group = site_no, col = 1-site_V1), alpha = 0.33, size = 0.2) +
  #geom_line(aes(x = datetime_minute, y = SRFC_AIR_TMPR, group = site_no, col = bng_northing), alpha = 0.2, size = 0.1) + 
  theme_bw() + theme(legend.justification = c(0, 0), legend.position = c(0, 0), legend.background=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_datetime(breaks =  seq(min(wowsubpost$datetime_minute), max(wowsubpost$datetime_minute), by="1 day"), guide = guide_axis(angle = 45)) +
  labs(col = "Outlier probability\n(site averaged)", x = "Time", y = "Surface air temperature (°C)") +
  scale_colour_gradientn(colours = c("#4575b4", "#ffffbf", "#d73027"), limits = c(0, 1))
ggsave(paste0("plots/", var, "_outlier_probs_WOW_data_visualisation.png"), width = 136, height = 100, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Calibration, sharpness, and coverage:

covtest <- melt(as.data.table(data.frame(ID = 1:length(y_test), y = y_test, x = drop(replicate(50, drop_predict_full(x_test[1:2], batch_size = 4096))))), id.vars = c("ID", "y"))[order(ID)]
covtest

testcrps <- crps_sample(y_test, as.matrix(dcast(data = covtest,formula = ID~variable,fun.aggregate = sum,value.var = "value")[,-1]))
mean(testcrps)

testlogs <- logs_sample(y_test, as.matrix(dcast(data = covtest,formula = ID~variable,fun.aggregate = sum,value.var = "value")[,-1]))
testlogs[which(testlogs == Inf)] <- 999
mean(testlogs)

coverage <- as.data.table(data.frame(predint = seq(0, 1, by = 0.05), obsprop = 0))
coverage

for(i in coverage$predint){
  #covtest[, quant := y < quantile(value, i), by = ID]
  covtest[, quant := y > quantile(value, 0.5-(i/2)) & y < quantile(value, 0.5+(i/2)), by = ID]
  coverage[predint == i, obsprop_interval := nrow(covtest[quant == TRUE])/nrow(covtest), ]
  coverage[predint == i, obsprop_interval_SE := sqrt(obsprop_interval*(1-obsprop_interval)/length(y_test)), ]
  covtest[, quant := y < quantile(value, i), by = ID]
  coverage[predint == i, obsprop_quantile := nrow(covtest[quant == TRUE])/nrow(covtest), ]
  coverage[predint == i, obsprop_quantile_SE := sqrt(obsprop_quantile*(1-obsprop_interval)/length(y_test)), ]
}

ggplot(coverage*100) + geom_abline(slope = 1, intercept = 0) + geom_point(aes(x = predint, y = obsprop_interval), col = "darkred") + theme_bw() +
  geom_line(aes(x = predint, y = obsprop_interval), col = "darkred") +
  # geom_errorbar(aes(x = predint, ymin = obsprop_interval - obsprop_interval_SE, ymax = obsprop_interval + obsprop_interval_SE), width=2) +
  scale_x_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100, by = 20)) + scale_y_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100, by = 20)) +
  labs(x = "Prediction interval (%)", y = "Proportion of observations falling within prediction interval (%)")
ggsave(paste0("plots/", var, "_calibration_curve_interval", round(cor(holdout$preds, holdout$obs)^2, 2)*100, ".png"), width = 89, height = 89, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

ggplot(coverage*100) + geom_abline(slope = 1, intercept = 0) + geom_point(aes(x = predint, y = obsprop_quantile), col = "darkblue") + theme_bw() +
  geom_line(aes(x = predint, y = obsprop_quantile), col = "darkblue") +
  # geom_errorbar(aes(x = predint, ymin = obsprop_quantile - obsprop_quantile_SE, ymax = obsprop_quantile + obsprop_quantile_SE), width=2) +
  scale_x_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100, by = 20)) + scale_y_continuous(limits = c(0,100), expand = c(0,0), breaks = seq(0,100, by = 20)) +
  labs(x = "Predicted quantile (%)", y = "Proportion of observations falling below predicted quantile (%)")
ggsave(paste0("plots/", var, "_calibration_curve_quantile", round(cor(holdout$preds, holdout$obs)^2, 2)*100, ".png"), width = 89, height = 89, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

round(coverage, 3)*100 

ggplot() +
  stat_density(data = data.frame(x = y_test, data = "observed"), aes(x = x, col = data, linetype = data), geom = "line") +
  stat_density(data = data.frame(x = as.numeric(covtest$value), data = "predicted"), 
               aes(x = x, col = data, linetype = data), geom = "line") +
  theme_bw() + theme(legend.justification = c(0,1), legend.position = c(0.05, 0.95)) +
  labs(x = paste(var, "°C"), y = "Density",
       subtitle = paste0("CRPS = ", round(mean(testcrps), 2), #"     Log Score = ", round(mean(testlogs), 2), 
                         "     95% Coverage = ", round(coverage[predint > 0.9 & predint < 1]$obsprop_interval*100, 1))) +
  scale_x_continuous(limits = c(quantile(covtest$value, 0.000001), quantile(covtest$value, 0.999999)))
ggsave(paste0("plots/", var, "_distribution_comparison", round(cor(holdout$preds, holdout$obs)^2, 2)*100, ".png"), width = 89, height = 89, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Create covariate maps using the trained neural network
k_set_learning_phase(0)

# National scale maps ####
rm(predimgs, predimgs_ann, predloc, predloc_ann)

# Land only:
# bielevcoarse <- aggregate(bielev, fact=2)
# predgrid <- as.data.table(raster::as.data.frame(bielevcoarse, xy = TRUE, na.rm = TRUE))
# setnames(predgrid, "alt", "elevation")
# nrow(predgrid)

# Land and sea:
grid <- 2000

predgrid <- data.table(expand.grid(x = seq(-200000, 670000, grid), y = seq(0,1225000, grid)), elevation = extract(bielev, method = "bilinear", expand.grid(x = seq(-200000, 670000, grid), y = seq(0,1225000, grid))))
predgrid[is.na(predgrid)] <- 0
str(predgrid)

# # Land and sea, coarse for monte carlo posterior uncertainty
# 
# # predgrid <- data.table(expand.grid(x = seq(-200000, 670000, 5000), y = seq(0,1225000, 5000)), elevation = extract(bielev, method = "bilinear", expand.grid(x = seq(-200000, 670000, 5000), y = seq(0,1225000, 5000))))
# # predgrid[is.na(predgrid)] <- 0
# # # predgrid <- predgrid[rep(predgrid[, .I], nsim)]
# # str(predgrid)

#predgrid <- data.table(expand.grid(x = seq(-200000, 670000, 5000), y = seq(0,1225000, 5000)), elevation = 0)
nrow(predgrid)

ggplot(predgrid) + geom_raster(aes(x = x, y = y, fill = elevation)) + theme_bw() + scale_fill_viridis_c(option = "B") + coord_equal()

predimgs <- array(dim = c(nrow(predgrid), imagedim, imagedim))
str(predimgs)

time <- Sys.time()
for(xmeter in unique(cells$coordx)){
  for(ymeter in unique(cells$coordy)){
    predimgs[,xmeter,ymeter] <- extract(bielev, method = "bilinear",
                                        cbind(predgrid$x + cells[coordx == xmeter & coordy == ymeter]$x, predgrid$y + cells[coordx == xmeter & coordy == ymeter]$y)) 
  }
}
Sys.time() - time

predimgs[is.na(predimgs)] = 0

predimgs_ann <- predimgs - predgrid$elevation
predimgs_ann <- (predimgs_ann)/sd
predimgs_ann[is.na(predimgs_ann)] = 0
#predimgs_ann <- predimgs_ann - rowMeans(predimgs_ann, na.rm = TRUE)

dim(predimgs_ann) <- c(nrow(predgrid), imagedim, imagedim, 1)

# nillimgs <- predimgs_ann
# nillimgs[] <- 0

# Location information:
# predloc <- predgrid[, c("x", "y", "elevation"), with = FALSE]
#predloc <- cbind(predgrid[, c("x", "y", "elevation"), with = FALSE], wowsub[rep(sample(1:nrow(wowsub), 1), nrow(predgrid)), c("datetime_minute", "TOD_cos", "TOD_sin"), with = FALSE])

# For stochastic predictions:
predloc <- cbind(predgrid[, c("x", "y", "elevation"), with = FALSE], wowsub[datetime_minute == as.POSIXct("2020-10-30 19:00:00")][rep(1, nrow(predgrid)), c("datetime_minute", "TOD_cos", "TOD_sin"), with = FALSE])
predtime <- unique(predloc$datetime_minute)
predtime

predloc[, datetime_minute := as.numeric(datetime_minute), ]
# predloc[, DAY := 6, ][, HOUR := 11]
# predlocmean <- apply(loc, 2, function(x) mean(x, na.rm = TRUE))
# predlocsd <- apply(loc, 2, function(x) sd(x, na.rm = TRUE))
predloc_ann <- t(apply(predloc, 1, function(x) (x - locmean)/locsd))

predloc_ann[which(is.na(predloc_ann))] <- 0
predloc_ann

predimgs_ann_tf <- tf$convert_to_tensor(predimgs_ann)
predloc_ann_tf <- tf$convert_to_tensor(predloc_ann)

k_set_learning_phase(0)

# Plot predicted mean

predgrid[, paste0(var,"_mean") := predict(robust_meanmodel, list(predimgs_ann_tf, predloc_ann_tf), batch_size = 4096)[,1], ]
predgrid

ggplot(predgrid) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_mean"))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", name = paste0(var, "\npredicted mean (°C)"), limits = c(quantile(wowsub[, get(var)], 0.005), quantile(wowsub[, get(var)], 0.995)), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", predtime))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_mean_holdout_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_full.png"), 
       width = 183, height = 225, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Multiple simulations of the mean for the same timestep:

sims <- 50
rm(simgrid)
for(s in 1:sims){
  tempgrid <- copy(predgrid)
  tempgrid[, sim := s, ]
  tempgrid[, paste0(var,"_mean") := drop_predict_mean(list(predimgs_ann_tf, predloc_ann_tf), batch_size = 4096)[,1], ]
  tempgrid[, paste0(var,"_full") := drop_predict_full(list(predimgs_ann_tf, predloc_ann_tf), batch_size = 4096)[,1], ]
  ifelse(exists("simgrid"), simgrid <- rbindlist(list(simgrid, tempgrid)), simgrid <- copy(tempgrid))
}

rel <- sample(1:max(simgrid$sim), 1)

ggplot(simgrid[sim == rel]) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_mean"))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", name = paste0(var, "\none simulation of the mean"), limits = c(quantile(wowsub[, get(var)], 0.005), quantile(wowsub[, get(var)], 0.995)), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", predtime))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_one_mean_rel_", rel, "_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_half.png"), 
       width = 183/2, height = 225/2, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

ggplot(simgrid[sim == rel]) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_full"))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", name = paste0(var, "\none simulated realisation"), limits = c(quantile(wowsub[, get(var)], 0.005), quantile(wowsub[, get(var)], 0.995)), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", predtime))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_one_full_rel", rel, "_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_half.png"), 
       width = 183/2, height = 225/2, units = "mm", type = "cairo", dpi = 300, scale = 1.375)



  # Compare mean of simulations (with dropout on) to fixed prediction mean (with dropout off)
  resgrid <- copy(predgrid[,-4])
  resgrid[, paste0(var,"_mean") := (simgrid[, lapply(.SD, mean), by = c("x", "y", "elevation"), .SDcols = "SRFC_AIR_TMPR_mean"] - predgrid)[,4], ]
  
  ggplot(resgrid) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_mean"))), interpolate = FALSE) + theme_bw() + coord_equal() +
    scale_fill_viridis_c(option = "B", name = paste0("dropout simulations vs single prediction for\n", var, "\nmean")) + 
    labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
    scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
    scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
    labs(title = paste("BCNN interpolation for", predtime))
  
  # How do they compare visually?
  ggplot(simgrid[, lapply(.SD, mean), by = c("x", "y", "elevation"), .SDcols = "SRFC_AIR_TMPR_mean"]) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_mean"))), interpolate = FALSE) + theme_bw() + coord_equal() +
    scale_fill_viridis_c(option = "B", name = paste0(var,"\npredicted mean")) + 
    labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
    theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
    scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
    scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
    labs(title = paste("BCNN interpolation for", predtime))
  ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_50_stacked_realisations", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_full.png"), 
         width = 183, height = 225, units = "mm", type = "cairo", dpi = 300, scale = 1.375)
  


# Plot predicted variance

predgrid[, paste0(var,"_var") := as.numeric(tf$math$softplus(predict(robust_meanmodel, list(predimgs_ann, predloc_ann), batch_size = 4096)[,2])), ]

ggplot(predgrid) + geom_raster(aes(x = x, y = y, fill = sqrt(get(paste0(var,"_var")))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "A", begin = 0.2, name = paste0(var, "\naleatoric uncertainty\n(mean std. dev.\nof output dist.)"), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5), 
        legend.text=element_text(colour = "white"), legend.title = element_text(colour = "white")) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", predtime))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_alea_holdout_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_full.png"), 
       width = 183, height = 225, units = "mm", type = "cairo", dpi = 300, scale = 1.375)


# Plot epistemic uncertainty

predgrid[, paste0(var,"_epi") := apply(drop(replicate(250, drop_predict_mean(list(predimgs_ann_tf, predloc_ann_tf), batch_size = 4096)[,1])), 1, stats::var), ]

ggplot(predgrid) + geom_raster(aes(x = x, y = y, fill = sqrt(get(paste0(var,"_epi")))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "D", begin = 0.2, name = paste0(var, "\nepistemic uncertainty\n(std. dev. of mean)"), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5), 
        legend.text=element_text(colour = "white"), legend.title = element_text(colour = "white")) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", predtime))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_epi_holdout_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_full.png"), 
       width = 183, height = 225, units = "mm", type = "cairo", dpi = 300, scale = 1.375)


# Plot total uncertainty

predgrid[, paste0(var,"_tot") := apply(drop(replicate(250, drop_predict_full(list(predimgs_ann_tf, predloc_ann_tf), batch_size = 4096)[,1])), 1, stats::var), ]

ggplot(predgrid) + geom_raster(aes(x = x, y = y, fill = sqrt(get(paste0(var,"_tot")))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", begin = 0.2, name = paste0(var, "\npredictive uncertainty\n(total std. dev. of\npredictive distribution)"), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5), 
        legend.text=element_text(colour = "white"), legend.title = element_text(colour = "white")) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", predtime))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_tot_holdout_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_full.png"), 
       width = 183, height = 225, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Plot aleatoric uncertainty as total - epistemic

predgrid[, paste0(var,"_dif") := get(paste0(var,"_tot")) - get(paste0(var,"_epi")), ]

ggplot(predgrid) + geom_raster(aes(x = x, y = y, fill = log(get(paste0(var,"_dif")))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", begin = 0.2, name = paste0("predicted\n", var, "\nvariance\n(aleatoric, as difference)"), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5), 
        legend.text=element_text(colour = "white"), legend.title = element_text(colour = "white")) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", predtime))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", predtime)), "_dif_holdout_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_full.png"), 
       width = 183, height = 225, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Interpolation through time (time series for a single site):

single_site <- sample(unique(wowsub[test]$site_no), 1)
single_site

which(wowsub[test]$site_no == single_site)

# Mean samples - without aleatoric uncertainty

timelinepred <- copy(wowsub[test][site_no == single_site])
# timelinepred[, pred := replicate(1, drop_predict_mean(list(x_test[[1]][which(wowsub[test]$site_no == single_site),,,,drop=F], x_test[[2]][which(wowsub[test]$site_no == single_site),]))[,1]), ]
# timelinepred

timelinepred <- cbind(timelinepred, pred = replicate(50, drop_predict_mean(list(x_test[[1]][which(wowsub[test]$site_no == single_site),,,,drop=F], x_test[[2]][which(wowsub[test]$site_no == single_site),]))[,1]))
timelinepred

timelinemelt <- melt(timelinepred, id.vars = "datetime_minute", measure.vars = c("SRFC_AIR_TMPR", names(timelinepred)[grep("pred", names(timelinepred))]))
timelinemelt[, variable_num := as.numeric(as.factor(variable))]
timelinemelt

ggplot() +
  geom_line(data = timelinemelt[!variable == "SRFC_AIR_TMPR"], aes(x = datetime_minute, y = value, group = variable, col = -variable_num), alpha = 1, size = 0.25) +
  #geom_line(data = timelinemelt[variable == "pred.V99"], aes(x = datetime_minute, y = value, group = variable, col = value), alpha = 1) +
  geom_line(data = timelinemelt[variable == "SRFC_AIR_TMPR"], aes(x = datetime_minute, y = value), size = 0.5, col = "black") +
  theme_bw() + theme(legend.justification = c(0, 0), legend.position = "none", legend.background=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_datetime(breaks =  seq(min(wowsubpost$datetime_minute), max(wowsubpost$datetime_minute), by="1 day"), guide = guide_axis(angle = 45)) +
  scale_colour_viridis_c(option = "B", begin = 0.5, end = 0.9) +
  labs(x = "Time", y = "Surface air temperature (°C)") + scale_y_continuous(limits = c(-3,23))
ggsave(paste0("plots/", var, "_100_simulations_time_series_means.png"), width = 136, height = 60, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

# Full samples - with aleatoric uncertainty:

timelinepred <- copy(wowsub[test][site_no == single_site])
# timelinepred[, pred := replicate(1, drop_predict_mean(list(x_test[[1]][which(wowsub[test]$site_no == single_site),,,,drop=F], x_test[[2]][which(wowsub[test]$site_no == single_site),]))[,1]), ]
# timelinepred

timelinepred <- cbind(timelinepred, pred = replicate(50, drop_predict_full(list(x_test[[1]][which(wowsub[test]$site_no == single_site),,,,drop=F], x_test[[2]][which(wowsub[test]$site_no == single_site),]))[,1]))
timelinepred

timelinemelt <- melt(timelinepred, id.vars = "datetime_minute", measure.vars = c("SRFC_AIR_TMPR", names(timelinepred)[grep("pred", names(timelinepred))]))
timelinemelt[, variable_num := as.numeric(as.factor(variable))]
timelinemelt

ggplot() +
  geom_line(data = timelinemelt[!variable == "SRFC_AIR_TMPR"], aes(x = datetime_minute, y = value, group = variable, col = -variable_num), alpha = 1, size = 0.25) +
  #geom_line(data = timelinemelt[variable == "pred.V99"], aes(x = datetime_minute, y = value, group = variable, col = value), alpha = 1) +
  geom_line(data = timelinemelt[variable == "SRFC_AIR_TMPR"], aes(x = datetime_minute, y = value), size = 0.5, col = "black") +
  theme_bw() + theme(legend.justification = c(0, 0), legend.position = "none", legend.background=element_blank(), panel.grid.minor = element_blank()) +
  scale_x_datetime(breaks =  seq(min(wowsubpost$datetime_minute), max(wowsubpost$datetime_minute), by="1 day"), guide = guide_axis(angle = 45)) +
  scale_colour_viridis_c(option = "B", begin = 0.5, end = 0.9) +
  labs(x = "Time", y = "Surface air temperature (°C)") + scale_y_continuous(limits = c(-3,23))
ggsave(paste0("plots/", var, "_100_simulations_time_series_full.png"), width = 136, height = 60, units = "mm", type = "cairo", dpi = 300, scale = 1.375)



# For predictions at multiple timesteps:

hours <- seq(as.POSIXct("2020-10-29 00:00:00"), length.out = 120, by = "hour") 
hours

k_set_learning_phase(0)

rm(multihour)
for(h in hours){
  predloc <- cbind(predgrid[, c("x", "y", "elevation"), with = FALSE], wowsub[datetime_minute == h][rep(1, nrow(predgrid)), c("datetime_minute", "TOD_cos", "TOD_sin"), with = FALSE])
  predtime <- unique(predloc$datetime_minute)
  
  predloc[, datetime_minute := as.numeric(datetime_minute), ]
  # predloc[, DAY := 6, ][, HOUR := 11]
  # predlocmean <- apply(loc, 2, function(x) mean(x, na.rm = TRUE))
  # predlocsd <- apply(loc, 2, function(x) sd(x, na.rm = TRUE))
  predloc_ann <- t(apply(predloc, 1, function(x) (x - locmean)/locsd))
  predloc_ann[which(is.na(predloc_ann))] <- 0
  
  predloc_ann_tf <- tf$convert_to_tensor(predloc_ann)
  
  hourpred <- copy(predgrid)
  hourpred[, datetime_minute := predtime, ]
  
  hourpred[, paste0(var,"_mean") := predict(robust_meanmodel, list(predimgs_ann, predloc_ann), batch_size = 4096)[,1], ]
  
  ifelse(exists("multihour"),
     multihour <- rbind(multihour, hourpred),
     multihour <- hourpred)
}

multihour

ggplot(multihour[datetime_minute == hours[44]]) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_mean"))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", name = paste("predicted\n", var, "\nmean"), limits = c(quantile(wowsub[, get(var)], 0.005), quantile(wowsub[, get(var)], 0.995)), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000)) +
  labs(title = paste("BCNN interpolation for", hours[44]))
ggsave(paste0("plots/", var, "_", gsub("0000", "", gsub(":", "", hours[44])), "_mean_holdout_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, "_national_map_full.png"), 
       width = 183, height = 225, units = "mm", type = "cairo", dpi = 300, scale = 1.375)

gg <- ggplot(multihour[order(datetime_minute)]) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_mean"))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", name = paste0("predicted\n", var), limits = c(quantile(multihour[, get(paste0(var,"_mean"))], 0), quantile(multihour[, get(paste0(var,"_mean"))], 1)), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000))
gg

gganim <- gg + transition_manual(datetime_minute) + labs(title = "BCNN interpolation for {current_frame}")

anim <- animate(gganim, renderer = av_renderer(paste0(var, "_596k_gauss_uniform_network_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, '_animation_WOW_robust.mp4')), 
                width = 540, height = 720, res = 90, fps = 12, nframes = length(unique(multihour$datetime_minute)))

# Repeat the same process to produce animations of samples from posterior predictive distribution
k_set_learning_phase(1)

for(h in hours){
  predloc <- cbind(predgrid[, c("x", "y", "elevation"), with = FALSE], wowsub[datetime_minute == h][rep(1, nrow(predgrid)), c("datetime_minute", "TOD_cos", "TOD_sin"), with = FALSE])
  predtime <- unique(predloc$datetime_minute)
  
  predloc[, datetime_minute := as.numeric(datetime_minute), ]
  # predloc[, DAY := 6, ][, HOUR := 11]
  # predlocmean <- apply(loc, 2, function(x) mean(x, na.rm = TRUE))
  # predlocsd <- apply(loc, 2, function(x) sd(x, na.rm = TRUE))
  predloc_ann <- t(apply(predloc, 1, function(x) (x - locmean)/locsd))
  predloc_ann[which(is.na(predloc_ann))] <- 0
  
  predloc_ann_tf <- tf$convert_to_tensor(predloc_ann)
  
  hourpred <- copy(predgrid)
  hourpred[, datetime_minute := predtime, ]
  
  hourpred[, paste0(var,"_samp") := predict(robust_model, list(predimgs_ann, predloc_ann), batch_size = 4096)[,1], ]
  
  ifelse(exists("multihour"),
         multihour <- rbind(multihour, hourpred),
         multihour <- hourpred)
}

ss <- ggplot(multihour[order(datetime_minute)]) + geom_raster(aes(x = x, y = y, fill = get(paste0(var,"_samp"))), interpolate = FALSE) + theme_bw() + coord_equal() +
  scale_fill_viridis_c(option = "B", name = paste0("prost. pred. samples\n", var), limits = c(quantile(multihour[, get(paste0(var,"_samp"))], 0), quantile(multihour[, get(paste0(var,"_samp"))], 1)), oob = scales::squish) + 
  labs(x = "Easting (metres BNG)", y = "Northing (metres BNG)") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1), legend.background=element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_y_continuous(breaks = seq(0,1000000, length.out = 6), labels = paste(seq(0,1000000, length.out = 6)), expand = c(0,0), limits = c(0, 1225000)) +
  scale_x_continuous(expand = c(0,0), limits = c(-200000, 670000))
ss

ssanim <- ss + transition_manual(datetime_minute) + labs(title = "BCNN interpolation for {current_frame}")

animss <- animate(ssanim, renderer = av_renderer(paste0(var, "_596k_gauss_uniform_network_", round(cor(holdout$preds, holdout$obs)^2, 2)*100, '_samples_WOW_robust.mp4')), 
                width = 540, height = 720, res = 90, fps = 12, nframes = length(unique(multihour$datetime_minute)))