require(keras)
require(tensorflow)
# require(devtools)
#library(reticulate)
library(tensorflow)
tf$config$list_physical_devices("GPU")

install.packages('devtools', dependecies=TRUE)

devtools::install_github("rstudio/tensorflow")

#devtools::install_github("rstudio/tensorflow")
#install.packages("keras") # This will pull in all R dependencies, like reticulate

# setup R (reticulate) with a Python installation it can use

#virtualenv_create("r-reticulate", python = install_python())

#install_keras(envname = "r-reticulate")

library(tensorflow)
install_tensorflow()

tensorflow::tf_config()

install.packages("keras") # This will pull in all R dependencies, like reticulate

# setup R (reticulate) with a Python installation it can use
library(reticulate)
virtualenv_create("r-reticulate", python = install_python())

library(keras)
install_keras(envname = "r-reticulate")

imdb <- dataset_imdb(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb
str(train_data[[1]])













