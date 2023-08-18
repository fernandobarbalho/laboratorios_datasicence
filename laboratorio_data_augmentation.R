#Scripts gerados a partir de interação com chatGPT no seguinte endereço:
#https://chat.openai.com/share/ba2ad5e1-ca96-4731-b49a-42613585a7a6


#Opção de uso do SMOTE para gerar data augmentation

# Install and load the required package
install.packages("DMwR")
library(DMwR)

# Sample dataset (you can replace this with your dataset)
set.seed(123)
data <- data.frame(
  num1 = rnorm(90),
  num2 = rnorm(90),
  num3 = rnorm(90),
  cat1 = sample(letters[1:5], 90, replace = TRUE),
  output = c(rep(0, 80), rep(1, 10)) # This creates an imbalanced output
)

# Apply SMOTE
# Assuming the unbalanced output variable is named "output"
# The "perc.over" parameter controls the amount of over-sampling
# Here, it's set to generate 600% of the minority class count, 
# but you can adjust this based on your needs.
data_smote <- SMOTE(output ~ ., data, perc.over = 600, k = 5)

# Checking the distribution of the output after SMOTE
table(data_smote$output)



#Opção de uso de simulação de Monte Carlo
# Required packages
install.packages(c("data.table", "MASS"))
library(data.table)
library(MASS)

# Sample dataset (replace with your own)
set.seed(123)
data <- data.frame(
  num1 = rnorm(90),
  num2 = rnorm(90),
  num3 = rnorm(90),
  cat1 = sample(letters[1:5], 90, replace = TRUE),
  output = c(rep(0, 80), rep(1, 10)) # Imbalanced output
)

# Monte Carlo Simulation
num_samples <- 1000 - nrow(data) # Number of new samples

# For numerical variables: assuming they follow a normal distribution
num1_sim <- rnorm(num_samples, mean(data$num1), sd(data$num1))
num2_sim <- rnorm(num_samples, mean(data$num2), sd(data$num2))
num3_sim <- rnorm(num_samples, mean(data$num3), sd(data$num3))

# For categorical variable: sample based on original frequencies
cat1_levels <- levels(data$cat1)
cat1_probs <- table(data$cat1) / nrow(data)
cat1_sim <- sample(cat1_levels, num_samples, replace = TRUE, prob = cat1_probs)

# For output variable: sample based on original frequencies
output_levels <- levels(as.factor(data$output))
output_probs <- table(data$output) / nrow(data)
output_sim <- sample(output_levels, num_samples, replace = TRUE, prob = output_probs)

# Combine simulated data
simulated_data <- data.frame(num1 = num1_sim, num2 = num2_sim, num3 = num3_sim, cat1 = cat1_sim, output = output_sim)

# Append simulated data to original data
augmented_data <- rbind(data, simulated_data)


library(tidyverse)

x<- "HIVH"

todos_anagramas<-
stringr::str_c(sample( unlist(strsplit(x, split = "")), 4^4, replace = TRUE),
               sample( unlist(strsplit(x, split = "")), 4^4, replace = TRUE),
               sample( unlist(strsplit(x, split = "")), 4^4, replace = TRUE),
               sample( unlist(strsplit(x, split = "")), 4^4, replace = TRUE))

unique(unlist(strsplit(x, split = "")))


anagramas_validos<-
purrr::map_dfr(unique(unlist(strsplit(x, split = ""))), function(letra){
  
  palavra_certa<-
  purrr::map_chr(todos_anagramas, function(palavra){
    #print(letra)
    conta_letra <- str_count(x, letra)
    #print(conta_letra)
    print(stringr::str_count(palavra,letra))
    if (stringr::str_count(palavra,letra)>conta_letra){
      print(palavra)
      print("Número de letras inadequado")
      return("")
    }
    #print("Número de letras ok")
    palavra
  })
  #print(palavra_certa)
  tibble(palavra= palavra_certa)
  #palavra_certa
})

anagramas <-
  (anagramas_validos %>%
  filter(palavra!="") %>%
  distinct(palavra))$palavra
  

anagramas <- unique(anagramas)


