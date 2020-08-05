# Load the data
main_data <- read.csv("song_data.csv")
View(main_data)

# 

# Remove duplicates
rem_dupli <- main_data %>% distinct(song_name,.keep_all = TRUE)
View(rem_dupli)

# Normalize the data :
rem_dupli <- rem_dupli[c(1,3:15,2)]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfNorm <- as.data.frame(lapply(rem_dupli[-c(1,16)], normalize))
dfNorm$song_name <- rem_dupli$song_name
dfNorm$top <- rem_dupli$top

dfNorm <- dfNorm[c(15,1:14,16)]

# Check for the missing values:
miss <- missmap(rem_dupli,c("blue","red"),legend = FALSE)

# Check the correlations in each pair of variables {corrplot function}:
correlations <- cor(dfNor[,2:15])
cor_plot <- corrplot(correlations, method = "circle")

# Add the song top ranking probability col from the top ranking dataset:
top18 <- read.csv("top2018.csv")
top17 <- read.csv("Top2017.csv")
top19 <- read.csv("top50.csv")

for (j in 1:nrow(dfNorm)){
  for (i in 1:nrow(top18)){
    if (dfNorm[j,1] == top18[i,2]  ||  dfNorm[j,1] == top17[i,2] ) {
      dfNorm[j,16] <- 1
      break
    } else {
      next
      
    }
  }
}

for (i in 1:nrow(top19)){
  for (j in 1:nrow(dfNorm)){
    if ( dfNorm[j,1] == top19[i,2]  ) {
      dfNorm[j,16] <- 1
      break
    } else {
      next
    }
  }
}

# Change NA to zeros in the TOP col:
names(dfNorm)[16] <- "top"
dfNorm$top[is.na(dfNorm$top)] <- 0
dfNorm$top <- as.factor(dfNorm$top)

# Split the data to training and validation sets {caret package}:
set.seed(123)
train_ind <- createDataPartition(dfNorm$top, p = 0.75,list = FALSE)
train_set <- dfNorm[train_ind,]
vali_set <- dfNorm[-train_ind,]

# Factorization the labels in training and validation sets:
train_set$top <- as.factor(train_set$top)
vali_set$top <- as.factor(vali_set$top)

# Logistic Regression modeling:
glm_fit <- glm(top ~ song_popularity + audio_mode + loudness+ 
                 liveness + energy + danceability +acousticness + 
                 song_duration_ms + instrumentalness + key + 
                 speechiness + tempo + time_signature + 
                 audio_valence ,data = train_set,family = binomial("logit") )

# Check the Psuedo R and cooefficents impact on the model: 
summ_gfit <- summary(glm_fit)
list(summ_gfit$coefficients,round(1-(summ_gfit$deviance / summ_gfit$null.deviance),2))

# Add predictions to train and validation sets:
train_set$prediction <- predict( glm_fit, newdata = train_set, type = "response" )
vali_set$prediction  <- predict( glm_fit, newdata = vali_set , type = "response" )
dfNorm$prediction <- predict( glm_fit, newdata = dfNorm , type = "response" )

# Plot the diff between predicitions and actuals in the training set:
ggplot(train_set, aes(prediction,color= as.factor(top)))+ 
geom_density(size = 1)+ 
ggtitle("Predictions VS Actuals - Training Set") +
scale_color_economist(name = "Prob", labels = c("TOP","NotTOP")) + 
theme_economist()

# Plot the accuracy % based on the cutoff values:
accuracy_info <- AccuracyCutoffInfo(train_set,vali_set,"prediction","top")
ggthemr("light")
accuracy_info$plot

# Plot the the confussion matrix based on the cutoff values:

cm_accuracy <- ConfusionMatrixInfo(dfNorm,"prediction","top",0.35)
ggthemr("flat")
cm_accuracy$plot
print(cm_accuracy$data)

ggthemr_reset()


cost_fp <- 100
cost_fn <- 200
roc_info <- ROCInfo( data = cm_accuracy$data, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)





##### Linear Regression:
songs_names <- dfNorm$song_name

lrdf <- dfNorm[c(-1,-16,-17)]

# break the dataset into training and validation sets:
set.seed(122)
train_ind_lr <- createDataPartition(lrdf$song_popularity, p = 0.7,list = FALSE)
train_set_lr <- as.data.frame(lrdf[train_ind_lr,])
vali_set_lr <- as.data.frame(lrdf[-train_ind_lr,])

# Start modeling:
options(scipen=999, digits = 0)
lrmodel <- lm(formula = song_popularity ~  audio_mode + loudness+ 
                liveness + energy + danceability +acousticness +
                 instrumentalness  + 
                  tempo  + 
                audio_valence,data = train_set_lr)
summ_lrmodel <- summary(lrmodel)
Rsqr <- summ_lrmodel$r.squared


# Predict the validation dataset:

vali_set_pred <- predict(lrmodel, vali_set_lr)

summary(vali_set_pred)

library(forecast)
library(leaps)

# Calculate accuracy:

residuals <- vali_set_lr$song_popularity - vali_set_pred
residuals <- ifelse(residuals<=5,0,residuals)

df <- data.frame("Predicted" = vali_set_pred, "Actual" = vali_set_lr$song_popularity,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)

df_hist <- ggplot(data=df, aes(df$Residual),alpha = I(.2)) + 
  geom_histogram()

ggthemr("flat")
df_hist

# Using regsubsets to improve the accuracy:
search <- regsubsets(song_popularity ~ ., data = train_set_lr, nbest = 1, nvmax = dim(train_set_lr)[2],
                     method = "exhaustive")
sum <- summary(search)
summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2


#par(mfrow=c(1,1))
plot(search, scale="r2")

#best 4
best_pred <- lm(song_popularity ~ danceability + instrumentalness + loudness + audio_valence ,data = train_set_lr)
summary(best_pred)

best_pred_lm <- predict(best_pred, vali_set_lr)
best
accuracy(best_pred_lm, vali_set_lr$song_popularity)

residuals_best <- vali_set_lr$song_popularity - best_pred_lm
residuals_best <- ifelse(residuals_best<= 0.05,0,residuals_best)

df <- data.frame("Predicted" = best_pred_lm, "Actual" = train_set_lr$song_popularity,
                 "Residual" = residuals_best, "Squared Residuals" = residuals_best*residuals_best)


###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Popularity" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Popularity")) 
ggthemr("flat")
p1

write.csv(df, file = "df.csv")
