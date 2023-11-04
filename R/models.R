#' Deep Learning Architecture for Temporal Sleep Stage Classification model implementation in Keras.
#'
#' @description Keras implementation of the deep learning architecture described by Chambon & Al in "A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series". Consecutives polysomnography (PSG) epochs are supposed to be input to the model to fit on categorized stages as output. `write_batches_psg()` function writes files batches with the right format for `x` and `y` values. The model can then be trained using the `train_batches()` function. `score_psg()` uses this model to predict PSG epochs from a raw European Data Format (EDF) record.
#' @references Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort, A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems and Rehabilitation Engineering 26:(758-769).
#' @param channels Integer. Number of channels in each input.
#' @param samples Integer. Number of samples in each channel.
#' @return A Keras sequential model.
#' @export
chambon2018 <- function(
  channels = 6,
  samples = 6300){
  
  model <- keras::keras_model_sequential()
  
  model <- keras::layer_conv_2d(
    model,
    filters = channels,
    kernel_size = c(1,32),
    activation = 'linear',
    input_shape = c(channels,samples,1))
  
  model <- keras::layer_conv_2d(
    model, filters = 32, kernel_size = c(1,32), activation = 'relu')
  
  model <- keras::layer_max_pooling_2d(
    model, pool_size = c(1, 8))
  
  model <- keras::layer_conv_2d(
    model,filters = 32, kernel_size = c(1,32), activation = 'relu')
  
  model <- keras::layer_max_pooling_2d(model,pool_size = c(1, 8))
  
  model <- keras::layer_conv_2d(
    model,filters = 32, kernel_size = c(1,32), activation = 'relu')
  
  model <- keras::layer_max_pooling_2d(model,pool_size = c(1, 8))
  
  model <- keras::layer_flatten(model)
  model <- keras::layer_dense(model,units = 512, activation = 'relu')
  model <- keras::layer_dropout(model,rate = 0.5)
  model <- keras::layer_dense(model, units = 5, activation = 'softmax')
  
  optimizer <- keras::optimizer_adam(
    lr = 0.001,
    beta_1 = 0.9,
    beta_2 = 0.999,
    epsilon = 1e-8
  )
  
  balanced_accuracy <- function(y_true, y_pred) {
    backend =  backend()
    true_positives <- backend$sum(backend$round(backend$clip(y_true * y_pred, 0, 1)))
    true_negatives <- backend$sum(backend$round(backend$clip((1 - y_true) * (1 - y_pred), 0, 1)))
    total_positives <- backend$sum(y_true)
    total_negatives <- backend$sum(1 - y_true)
    
    balanced_accuracy <- 0.5 * (true_positives / total_positives + true_negatives / total_negatives)
    return(balanced_accuracy)
  }
  
  keras::compile(
    model,
    loss = keras::loss_categorical_crossentropy,
    optimizer = optimizer,
    metrics = list(balanced_accuracy = balanced_accuracy))
  
  model
}



#' Automated Classification of Sleep Stages in Mice with Deep Learning model implementation in Keras.
#'
#' @description Model inspired by the article "Automated Classification of Sleep Stages and EEG Artifacts in Mice with Deep Learning". Implemented using Keras. Adapted to use minimum 2 channels and to not score artifact epochs.
#' @references Schwabedal, Justus T. C., Daniel Sippel, Moritz D. Brandt, and Stephan Bialonski. “Automated Classification of Sleep Stages and EEG Artifacts in Mice with Deep Learning.” ArXiv:1809.08443 [Cs, q-Bio], September 22, 2018. http://arxiv.org/abs/1809.08443.
#' @param channels Number of channels in each input.
#' @param samples Number of samples in each channel.
#' @return A Keras sequential model.
#' @export
schwabedal2018 <- function(channels = 2,
                           samples = 8000){
  
  model <- keras::keras_model_sequential()
  
  # Layer 1
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(channels,5),
    activation = 'relu', input_shape = c(channels, samples, 1),
    strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 2
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1, 5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 3
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 4
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 5
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 6
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 7
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 8
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 9
  model <- keras::layer_flatten(model)
  
  model <- keras::layer_dense(model,units = 80, activation = 'relu')
  
  model <- keras::layer_dense(model, units = 3, activation = 'softmax')
  
  keras::compile(
    model,
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_rmsprop(lr = 0.0001),
    metrics = "categorical_accuracy")
  
  model
  
}

#' Convolutional neural network for real-time apnea-hypopnea event detection during sleep
#'
#' @description Keras implementation of the deep learning architecture described by Choi & Al in "Real-time apnea-hypopnea event detection during sleep by convolutional neural network".
#' @references Choi SH, Yoon H, Kim HS, et al. Real-time apnea-hypopnea event detection during sleep by convolutional neural networks. Computers in Biology and Medicine. 2018;100:123-131. 
#' @param segment_size Integer. The size of the segment to predict.
#' @return A Keras sequential model.
#' @export
choi2018 <- function(segment_size = 160){
  model <- keras::keras_model_sequential()
  
  model <- keras::layer_conv_1d(
    model, filters = 15, kernel_size = 4, strides = 2,
    activation = 'relu', input_shape = c(segment_size, 1))
  
  model <- keras::layer_max_pooling_1d(
    model, strides = 1, pool_size = 2)
  
  model <- keras::layer_dropout(model, rate = 0.2)
  
  model <- keras::layer_conv_1d(
    model, filters = 30, kernel_size = 4, strides = 2,
    activation = 'relu')
  
  model <- keras::layer_conv_1d(
    model, filters = 30, kernel_size = 4, strides = 2,
    activation = 'relu')
  
  model <- keras::layer_max_pooling_1d(
    model, strides = 1, pool_size = 2)
  
  model <- keras::layer_dropout(model, rate = 0.2)
  
  model <- keras::layer_flatten(model)
  
  model <- keras::layer_batch_normalization(model)
  
  model <- keras::layer_dense(model, units = 50, activation = 'relu')
  
  model <- keras::layer_dropout(model, rate = 0.2)
  
  model <- keras::layer_dense(model, units = 2, activation = 'sigmoid')
  
  model <- keras::compile(
    model,
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_adam(
      learning_rate = 0.001),
    metrics = "accuracy")
  
  model
}