library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(topicmodels)
require(topicmodels)
library(textstem)
library(stopwords)
library(ggplot2)
library(shape)
library(tm)
library(Matrix)
library(reshape2)
require(pals)
library(patchwork) 
library(webshot)
library(wordcloud2) 
require(wordcloud2)

current_dir <- getwd()
MODEL <- NULL
COLLOC <- NULL

path_data <- file.path(current_dir, "..","raw_caracters_without_commands.csv" )
path_name_data <- file.path(current_dir, "..","babynames-clean.csv" )
path_lemma_data <- file.path(current_dir, "..","baseform_en.tsv" )
raw_characters <- read.csv(path_data)
name_data <- read.csv(path_name_data, encoding = "UTF-8", col.names = c("name","gender"))$name
lemma_data <- read.csv(path_lemma_data, encoding = "UTF-8")
stopwords <-  stopwords::stopwords("en", source = "stopwords-iso")
raw_characters$text_length = nchar(raw_characters$lines_cleand)
raw_characters <- raw_characters[raw_characters$text_length != 0, ]
stopwords <- c(stopwords, "cont'd", "hand", "walk", "sit", "head")
name_data <- c(name_data, "john")

clean <- function(df){
  df$lines_cleand <- gsub(paste(c("’s","'s","’ll","'ll"), collapse = "|"), "", df$lines_cleand)
  df$lines_cleand <- gsub("’", "'", df$lines_cleand)
  df <- df[df$text_length != 0, ]
  df$text_length = nchar(df$lines_cleand)
  return(df)
}
  
get_tokens <- function(df){
  tokens <- corpus(df$lines_cleand, docnames = df$X)
  
  tokens <- quanteda::tokens(tokens, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
  tokens <- tokens_tolower(tokens)
  tokens <- tokens_replace(tokens, lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") 
  tokens <- tokens_remove(tokens, pattern = stopwords, padding = TRUE)
  tokens <- tokens_remove(tokens, pattern = name_data, padding = TRUE, case_insensitive = TRUE)
  
  tokens <- tokens_remove(tokens, pattern = c("--","i’m"), padding = TRUE, case_insensitive = TRUE)
  
  collocations <- quanteda.textstats::textstat_collocations(tokens, min_count = 25)
  
  #tokens <- tokens_compound(tokens, collocations)
  #if (nrow(collocations) > 250) {
  #  collocations <- collocations[1:250, ]
  #}
  
  #COLLOC <<- collocations
  tokens <- tokens_remove(tokens, "")
  
  return(tokens)
}

df <- clean(raw_characters)
tokens <- get_tokens(df)
nrow(df)


apply_model <- function(tokens, iterations = 500, topics = 10, return_model = F){
  dtm <- dfm(tokens)
  dtm <- dfm_trim(dtm, min_docfreq = 3)
  sel_idx <- rowSums(dtm) > 0
  dtm <- dtm[sel_idx, ]
  
  topicModel <- LDA(dtm, topics, method="Gibbs", control=list(
    iter = iterations,
    seed = 1,
    verbose = 25,
    alpha = 0.02))
  MODEL <<- topicModel
  
  tmResult <- posterior(topicModel)
  
  tm_terms <- tmResult$terms
  tm_topics <- tmResult$topics
  if(return_model){
    return(dtm)
  }
  return(terms(topicModel, 3))
}

compare_m_f_over_time_multiple_models <- function(df){
  group_df <- data.frame(
    min = seq(1925, 2015, by = 10),
    max = seq(1925, 2015, by = 10) + 9
  )
  group_df$results_m <- vector("list", length = nrow(group_df))
  for (i in 1:nrow(group_df)){
    male_df <- df[df$gender == "actor" & 
                    df$year >= group_df[i,1] & 
                    df$year <= group_df[i,2] ,]
    female_df <- df[df$gender == "actress" & 
                    df$year >= group_df[i,1] & 
                    df$year <= group_df[i,2] ,]
    male_df <- clean(male_df)
    female_df <- clean(female_df)
    male_tokens <- get_tokens(male_df)
    female_tokens <- get_tokens(female_df)
    male_results <- apply_model(male_tokens, iterations = 500, topics = 3)
    tm_result_m <- posterior(MODEL)
    female_results <- apply_model(female_tokens, iterations = 500, topics = 3)
    tm_result_f <- posterior(MODEL)
    
    group_df$results_f[[i]] <- female_results
    group_df$results_m[[i]] <- male_results
  
    
    for (topicToViz in 1:3) {
      top40terms <- sort(tm_result_f$terms[topicToViz, ], decreasing = TRUE)[1:40]
      words <- names(top40terms)
      probabilities <- sort(tm_result_f$terms[topicToViz, ], decreasing = TRUE)[1:40]
      wordcloud_f <- wordcloud2(data.frame(words, probabilities), shuffle = FALSE)
      
      top40terms <- sort(tm_result_m$terms[topicToViz, ], decreasing = TRUE)[1:40]
      words <- names(top40terms)
      probabilities <- sort(tm_result_m$terms[topicToViz, ], decreasing = TRUE)[1:40]
      wordcloud_m <- wordcloud2(data.frame(words, probabilities), shuffle = FALSE)
      
      saveWidget(wordcloud_m,"m.html",selfcontained = F)
      saveWidget(wordcloud_f,"f.html",selfcontained = F)
      filename_f <- paste("wordcloud_female_", group_df[i, 1], "_to_", group_df[i, 2], "_topic_", topicToViz, ".png", sep = "")
      filename_m <- paste("wordcloud_male_", group_df[i, 1], "_to_", group_df[i, 2], "_topic_", topicToViz, ".png", sep = "")
      webshot("f.html", file = filename_f)
      webshot("m.html", file = filename_m)
  }
  
  for (i in 1:nrow(group_df)){
    print(paste(group_df[i,1], " - ", group_df[i,2], " FEMALE:"))
    print(group_df$results_f[[i]])
    print(paste(group_df[i,1], " - ", group_df[i,2], " MALE: "))
    print(group_df$results_m[[i]])  
  }
  }
  return(group_df)
}

result <- compare_m_f_over_time_multiple_models(raw_characters)

compare_m_f_over_time_one_model <-function(df){
  df <- clean(df)
  tokens <- get_tokens(df)
  K <- 10
  dtf_l <- apply_model(tokens, iterations = 500, topics = K, return_model = T)
  
  matching_rows <- rownames(dtf_l)
  df <- df[df$X %in% matching_rows, ]
  terms(MODEL, 5)
  tmResult <- posterior(MODEL)
  
  tm_terms <- tmResult$terms
  tm_topics <- tmResult$topics
  
  df$Jahrzehnt <- paste0(substr(df$year, 0, 3), "0")
  
  topic_proportion_per_decade <- aggregate(tm_topics~Jahrzehnt+gender, df, mean)
  colnames(topic_proportion_per_decade)[3:(K+2)] <- apply(terms(MODEL, 5), 2, paste, collapse = " ")
  
  vizDataFrame <- melt(topic_proportion_per_decade, id.vars = c("Jahrzehnt", "gender"))
  
  plot <- (ggplot(vizDataFrame[vizDataFrame$gender == "actor",],
          aes(x=Jahrzehnt, y=value, fill=variable)) 
    + geom_bar(stat = "identity") 
    + ylab("Anteil") 
    + theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none")
    + ggtitle(label = "männlich"))+
    (ggplot(vizDataFrame[vizDataFrame$gender == "actress",],
            aes(x=Jahrzehnt, y=value, fill=variable)) 
     + geom_bar(stat = "identity")
     + ylab(NULL)
     + theme(axis.text.x = element_text(angle = 90, hjust = 1))
     + ggtitle(label = "weiblich") 
     + labs(fill = "Topic"))
  
  show(plot)
  path <- file.path(current_dir, "..", "über_zeit_ein_model_1.png")
  
  ggsave(filename = path, plot = plot, width = 10, height = 4, units = "in", dpi = 300)
}

compare_m_f_over_time_one_model(raw_characters)

compare_m_f_two_models <-function(df){
  male_df <- df[df$gender == "actor",]
  female_df <- df[df$gender == "actress",]
  male_df <- clean(male_df)
  female_df <- clean(female_df)
  male_tokens <- get_tokens(male_df)
  female_tokens <- get_tokens(female_df)
  male_results <- apply_model(male_tokens, iterations = 500, topics = 10)
  female_results <- apply_model(female_tokens, iterations = 500, topics = 10)
  print("FEMALE TOPICS")
  print(female_results)
  print("MALE TOPICS")
  print(male_results)
}
#compare_m_f(raw_characters)

compare_m_f_one_model <-function(df = raw_characters){

  df <- clean(df)
  tokens <- get_tokens(df)
  K <- 10
  dtf_l <- apply_model(tokens, iterations = 100, topics = K, return_model = T)

  matching_rows <- rownames(dtf_l)
  df <- df[df$X %in% matching_rows, ]
  terms(MODEL, 5)
  tmResult <- posterior(MODEL)
  
  tm_terms <- tmResult$terms
  tm_topics <- tmResult$topics
  
  topics_per_gender <- aggregate(tm_topics, by = list(gender = df$gender), mean)

  colnames(topics_per_gender)[2:(K+1)] <- apply(terms(MODEL, 5), 2, paste, collapse = " ")
  
  vizDataFrame <- melt(topics_per_gender, id.vars = "gender")
  ggplot(vizDataFrame,
         aes(x=gender, y=value, fill=variable)) +
    geom_bar(stat = "identity") + ylab("proportion") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
#compare_m_f_one_model(raw_characters)

plot_gender_dist_text <- function(raw_characters = raw_characters){
  
  year_counts <- xtabs(text_length ~ year + gender, data = raw_characters)
  
  year_counts_df <- as.data.frame(year_counts)
  names(year_counts_df) <- c("Year", "Gender", "Characters")
  for (year in 1925 : 2020) {
    if (!year %in% year_counts_df$column_name) {
      new_rows <- data.frame(Year = c(as.character(year),as.character(year)), Gender = c("actor","actress"), Characters = c(0,0))
      year_counts_df <- bind_rows(year_counts_df, new_rows)
    }
  }
  plot <- ggplot(year_counts_df, aes(x = Year, y = Characters, fill = Gender)) +
    geom_bar(stat = "identity") +
    labs(title = "Zeichen", x = "Jahr", y = "Anzahl", fill = "Geschlecht") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 5 == 0, x, "")) +
    scale_fill_discrete(labels = function(gender) ifelse(gender == "actor", "männlich", "weiblich"))
  show(plot)
  path <- file.path(current_dir, "..", "geschlechterverteilung_Zeichen.png")
  ggsave(filename = path, plot = plot, width = 5, height = 4, units = "in", dpi = 300)
}
#plot_gender_dist_text(df)

plot_gender_dist_role <- function(raw_characters = raw_characters){
  
  year_counts <- table(raw_characters$year, raw_characters$gender)
  year_counts_df <- as.data.frame(year_counts)
  names(year_counts_df) <- c("Year", "Gender", "Count")
  for (year in 1925 : 2020) {
    if (!year %in% year_counts_df$column_name) {
      new_rows <- data.frame(Year = c(as.character(year),as.character(year)), Gender = c("actor","actress"), Count = c(0,0))
      year_counts_df <- bind_rows(year_counts_df, new_rows)
    }
  }
  
  plot <- ggplot(year_counts_df, aes(x = Year, y = Count, fill = Gender)) +
    geom_bar(stat = "identity") +
    labs(title = "Charaktere", x = "Jahr", y = "Anzahl", fill = "Geschlecht") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 5 == 0, x, "")) +
    scale_fill_discrete(labels = function(gender) ifelse(gender == "actor", "männlich", "weiblich"))
  show(plot)
  path <- file.path(current_dir, "..", "geschlechterverteilung_charaktere.png")
  ggsave(filename = path, plot = plot, width = 5, height = 4, units = "in", dpi = 300)
}
#plot_gender_dist_role(df)
