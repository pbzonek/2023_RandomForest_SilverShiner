## --------------------------------------------------------------#
## Script name: Script3-3-4_Revision_INLA_AdultLS.R 
##
## Purpose of script: 
##    1. Run an INLA model for Adult Life Stages 
##    2. Compare a null model with a spatial model
##    3. Compare models to test for spatial autocorrelation
##
## Author: Paul Bzonek
##
## Date Created: 2022-09-09
##
## --------------------------------------------------------------#  
## Modification Notes:
##   
## --------------------------------------------------------------#


#####Format data per life stage ##################################----
#-------------------------------------------------------------# 
###ALS == "Adult Life Stage"

#Only keep variables of interest
#---------------------#
m_ALS_data <- data_INLA_Full[, c("Start.Latitude", "Start.Longitude", "maturep", "Year", 
                            "AvgOfStream.depth..m.", 
                            "AvgOfWater.velocity..msec.")] %>% 
 na.omit() #Handle NAs
m_ALS_data <- m_ALS_data %>% 
 mutate(maturep = as.numeric(maturep)-1)

str(m_ALS_data)

#Identify coordinates
m_ALS_coords <- cbind(m_ALS_data$Start.Longitude, m_ALS_data$Start.Latitude)


###Inspect autocorrelation of basic model
#---------------------#
temp_var_data <- m_ALS_data
coordinates(temp_var_data) = ~ Start.Latitude + Start.Longitude

#Compare spatial autocorrelation before and after model formula
(plots_INLA$mALS_spatial$Variogram <- 
  rbind(
   variogram(maturep~1, data=temp_var_data) %>% 
    mutate(Source = "Raw Correlation"),
   variogram(maturep~Year + AvgOfStream.depth..m. + AvgOfWater.velocity..msec., data=temp_var_data) %>% 
    mutate(Source = "Lamothe Model Residuals")
  ) %>% 
  ggplot(aes(x=dist, y=gamma, colour=Source))+
  geom_point(aes(size=np))+
  geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE)+
  xlab("distance")+ylab("semi-variance")+
  scale_colour_manual(values=c("#6ab717", "#e2183d"))
)


### Make an INLA mesh to be used in the model
#---------------------#
#Make a SXM mesh for INLA models

#PB NOTE: ISSUE WITH BOUNDARY WHERE SHAPEFILE IS MISSING UPPER SXM
# SXM_mesh_ALS <- inla.mesh.2d(m_ALS_coords,
#                           boundary = SXM_bdry1,
#                           max.edge = c(1))

SXM_mesh_ALS <- inla.mesh.2d(m_ALS_coords,
                          #boundary = SXM_bdry1,
                          max.edge = c(0.005, 0.1))

plot(SXM_mesh_ALS); points(m_ALS_coords, pch = 19, col = rgb(0, 0, 0, 0.15), cex=2)

#Make a mash frame to project onto
SXM_mesh_ALS_projector <- inla.mesh.projector(SXM_mesh_ALS,  dims = c(500, 500))


### Prepare Comb Life Stage INLA parameters
#---------------------#
m_ALS_A.matrix <-inla.spde.make.A(mesh=SXM_mesh_ALS, 
                                  loc=as.matrix(m_ALS_coords)) # A-matrix maps a random field from the mesh nodes
dim(m_ALS_A.matrix)

SXM_SPDE <- inla.spde2.matern(SXM_mesh_ALS, alpha=2) #approximate spatial structure at the mesh nodes 
SXM_SPDE_index <- inla.spde.make.index(name = "spatial.field", SXM_SPDE$n.spde)



#####Build an INLA model: ALS_raw#############################----
#-------------------------------------------------------------# 

#Specify model formulas
#---------------------#
formula_ALS_raw <- y ~ -1 + Intercept + Depth + Velocity + 
                            f(Year, model = 'iid') # Base model

### Run the INLA model
#---------------------#
m_ALS_raw_stack <- inla.stack(
 data=list(y=m_ALS_data$maturep), #the response
 A=list(m_ALS_A.matrix, 1),  #the A matrix; the 1 is included to make the list(covariates)
 effects=list(
  c(list(Intercept=1) #the Intercept
  ),                 #NO SPATIAL INDEX
  #the covariates
  list(Year = m_ALS_data$Year,
       Depth = m_ALS_data$AvgOfStream.depth..m.,
       Velocity = m_ALS_data$AvgOfWater.velocity..msec.)
 ), 
 #this is a quick name so you can call upon easily
 tag='dat')


m_ALS_raw_INLA <- inla(formula = formula_ALS_raw,
                       data = inla.stack.data(m_ALS_raw_stack, spde=SXM_SPDE),  #the data stack
                       family = 'binomial',   #which family the data comes from
                       control.predictor = list(A=inla.stack.A(m_ALS_raw_stack), compute=TRUE),  #compute gives you the marginals of the linear predictor
                       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE), #model diagnostics and config = TRUE gives you the GMRF
                       verbose = FALSE) #can include verbose=TRUE to see the log of the model runs

summary(m_ALS_raw_INLA)



#####Build an INLA model: ALS_spatial#############################----
#-------------------------------------------------------------# 

#Specify model formulas
#---------------------#
formula_ALS_spatial <- y ~ -1 + Intercept + Depth + Velocity + 
                                f(Year, model = 'iid') + f(spatial.field, model = SXM_SPDE) # Base model + Spatial effects

### Run the INLA model
#---------------------#
m_ALS_spatial_stack <- inla.stack(
 data=list(y=m_ALS_data$maturep), #the response
 A=list(m_ALS_A.matrix, 1),  #the A matrix; the 1 is included to make the list(covariates)
 effects=list(
  c(list(Intercept=1), #the Intercept
    SXM_SPDE_index),  #the spatial index
  #the covariates
  list(Year = m_ALS_data$Year,
       Depth = m_ALS_data$AvgOfStream.depth..m.,
       Velocity = m_ALS_data$AvgOfWater.velocity..msec.)
 ), 
 #this is a quick name so you can call upon easily
 tag='dat')


m_ALS_spatial_INLA<-inla(formula = formula_ALS_spatial,
                         data = inla.stack.data(m_ALS_spatial_stack, spde=SXM_SPDE),  #the data stack
                         family = 'binomial',   #which family the data comes from
                         control.predictor = list(A=inla.stack.A(m_ALS_spatial_stack), compute=TRUE),  #compute gives you the marginals of the linear predictor
                         control.compute = list(dic = TRUE, waic = TRUE, config = TRUE), #model diagnostics and config = TRUE gives you the GMRF
                         verbose = TRUE) #can include verbose=TRUE to see the log of the model runs

summary(m_ALS_spatial_INLA)


###Plot the INLA outputs
#---------------------#
m_ALS_spatial_out_mean <- inla.mesh.project(SXM_mesh_ALS_projector, m_ALS_spatial_INLA$summary.random$spatial.field$mean)
m_ALS_spatial_out_sd <- inla.mesh.project(SXM_mesh_ALS_projector, m_ALS_spatial_INLA$summary.random$spatial.field$sd)

(plots_INLA$mALS_spatial$proj_mean <- 
  levelplot(m_ALS_spatial_out_mean, scales=list(draw=F), 
            xlab='Lon', ylab='Lat', 
            main='mean' , col.regions = heat.colors(16)) 
)

(plots_INLA$mALS_spatial$proj_sd <- 
  levelplot(m_ALS_spatial_out_sd, scales=list(draw=F), 
            xlab='Lon', ylab='Lat', 
            main='sd' , col.regions = heat.colors(16)) 
)



###Work towards a residual variogram
#---------------------#
#Produce some summary statistics
m_ALS_spatial_INLA_sum1 <- inla.spde.result(m_ALS_spatial_INLA, "spatial.field", SXM_SPDE)

###Modify Code from public GIT function: https://github.com/gfalbery/ggregplot/blob/master/R/INLA%20Range%20Plot.R
#plot(m_ALS_spatial_INLA_sum1$marginals.kappa[[1]], type="l") # summary values used in variogram function
###Improvising from code to guestimate Kappa
temp_Var_Kappa <- inla.emarginal(function(x) x, m_ALS_spatial_INLA_sum1$marginals.kappa[[1]])
temp_Var_seq <- seq(0, 0.025, length = 100)
### Convert Kappa estimates into semivariance estimates 
m_ALS_spatial_INLA_variogram <-lapply(temp_Var_Kappa, function(f){
 Cor.M <- as.numeric((f * temp_Var_seq) * besselK(f * temp_Var_seq, 1))
 Cor.M[1] <- 1
 return(data.frame(temp_Var_seq, Cor.M))
})

(plots_INLA$mALS_spatial$proj_variogram <- 
  as.data.frame(m_ALS_spatial_INLA_variogram) %>% 
  ggplot(aes(x=temp_Var_seq, y=Cor.M)) +
  geom_line() +
  geom_point()+
  labs(colour ="Model",x = "Distance", y = "Correlation")
)


#####Combine and COmpare models ##################################----
#-------------------------------------------------------------# 
#Clean-up
rm(list = paste(ls(pattern="temp"))) #Remove environment objects with 'temp' in name


###Compare raw and spatial ALS INLA model performance
#---------------------#
(plots_INLA$mALS_spatial$modelcomparison <- 
  ggregplot::Efxplot(list(m_ALS_raw_INLA, m_ALS_spatial_INLA)) +
  ggtitle(paste("Adult Life Stage (ALS) model comparison", "Literature model", "Raw vs Spatial INLA", 
                paste("  Raw AIC:", round(m_ALS_raw_INLA$waic$waic, 2)),  
                paste("  Spatial AIC:", round(m_ALS_spatial_INLA$waic$waic,2)), 
                paste("  Delta AIC:", round(abs(m_ALS_raw_INLA$waic$waic - m_ALS_spatial_INLA$waic$waic),2)),
                sep="\n"))
)
