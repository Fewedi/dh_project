library(jsonlite)
library(readr)
library(stringi)
library(logger)
library(gdata)
library(gender)

current_dir <- getwd()

path_character_genders <- file.path(current_dir, "..", "character_genders.json" )
character_genders <- fromJSON(path_character_genders)

path_meta_data <- file.path(current_dir, "..", "archive", "movie_metadata","movie_meta_data.csv" )
movies <- read.csv(path_meta_data)

df <- data.frame(
  lines_original = character(0),
  lines_cleand = character(0),
  speaker = character(0),
  gender = character(0),
  gender_origin = character(0),
  imdbid = integer(0),
  movie_name = character(0),
  year = integer(0),
  won_oscars = logical(0),
  topics = character(0)
)

counter_missing_character_genders = 0
counter_missing_movies_in_meta_data = 0
counter_missing_gender_entry = 0


path_dialogs <- file.path(current_dir, "..", "archive", "movie_characters", "data", "movie_character_texts","movie_character_texts")
dirs_movies_with_dialogs <- list.dirs(path_dialogs, full.names = TRUE, recursive = FALSE)
for (dir in dirs_movies_with_dialogs) {
  dir_name <- strsplit(basename(dir), "_")[[1]]
  movie_name <- dir_name[1]
  imdbid <- as.integer(dir_name[2])
  gender_entry <- character_genders[[sprintf("%07d", imdbid)]]
  movie_entry <- movies[movies$imdbid == imdbid, ]
  
  
  if(nrow(movie_entry) != 1){
    log_error(paste( "discarded ", movie_name, "-", imdbid, " :expected meta data entry but none found"))
    counter_missing_movies_in_meta_data <- counter_missing_movies_in_meta_data+1
  }else{
    movie_entry <- movie_entry[1,]
    character_files <- list.files(dir, full.names = TRUE)
    if(is.null(gender_entry)){
      log_error(paste( "discarded ", movie_name, "-", imdbid, " :expected gender entry but none found"))
      counter_missing_gender_entry <- counter_missing_gender_entry+1
    }else{
      gender_df <- as.data.frame(gender_entry)
      colnames(gender_df) <- c("speaker", "gender")
      
      for (file in character_files) {
        character_name = substr(basename(file), 1, nchar(basename(file)) - 9)
        encoding_info <- guess_encoding(file)
        dialog_lines <- tryCatch({
          dialog_lines = readLines(file, encoding = encoding_info$encoding[1])
        }, error = function(e) {
          print(encoding_info$encoding)
          log_error(paste(encoding_info$encoding[1], "automatic encoding failed"))
          dialog_lines = readLines(file, encoding = "UTF8")
        })
        
        clean_dialog_lines <- ""
        for (line in dialog_lines) {
          if (!grepl("text:", line)) {
            clean_line <- gsub("^[^:]*:", "", line)
            clean_dialog_lines <- paste(clean_dialog_lines, clean_line)
          }
        }
        clean_dialog_lines <- trim(clean_dialog_lines)
        
        found_row <- gender_df[gender_df$speaker == character_name,]
        
        add_row <- function(gender_found, gender_origin){
          new_row <- list(
            lines_original = paste(dialog_lines, collapse = " "),
            lines_cleand = clean_dialog_lines,
            speaker = character_name,
            gender = gender_found,
            gender_origin = gender_origin,
            imdbid = imdbid,
            movie_name = movie_name,
            year = movie_entry$year,
            won_oscars = grepl("Oscar", movie_entry$awards),
            topics = "not yet set"
          )
          df <- rbind(df, new_row)
          return(df)
        }
        
        if(nrow(found_row) > 1) {
          log_error(paste("unusual gender results: ", found_row) )
        }else if(nrow(found_row) == 0) {
          
          cleaned_name <- gsub("[^a-zA-Z0-9 ]", "", character_name)
          
          name_list <- unlist(strsplit(cleaned_name, " "))
          filtered_list <- name_list[sapply(name_list, function(x) nchar(x) >= 3 | grepl("(Ms|Mr)", x, ignore.case = TRUE))]
          
          guess_df <- tryCatch({
            gender(filtered_list, years = as.integer(movie_entry$year), method = "ssa")
          }, error = function(e) {
            data.frame(
              proportion_male = c(0),
              proportion_female = c(0),
            )
          })
          filtered_list <- tolower(filtered_list)
          max_m <- max(guess_df$proportion_male)
          max_f <- max(guess_df$proportion_female)
          
          if (length(intersect(c("man", "boy", "mr", "father", "son", "brother"), filtered_list)) > 0 ){
            gender_found <- "actor"
            gender_origin <- "guessed_asumption"
            df <- add_row(gender_found, gender_origin)
            log_info(paste( "added", character_name," for ", movie_name, "-", imdbid,"-",gender_found,"-",gender_origin, ": successfull"))
          } else if (length(intersect(c("woman", "girl", "daughter", "mother", "mrs", "madame", "miss", "mister", "ms"), filtered_list)) > 0 ){
            gender_found <- "actress"
            gender_origin <- "guessed_asumption"
            df <- add_row(gender_found, gender_origin)
            log_info(paste( "added", character_name," for ", movie_name, "-", imdbid,"-",gender_found,"-",gender_origin, ": successfull"))
          }else if(max_m > 0.60 && max_f < 0.30){
            gender_found <- "actor"
            gender_origin <- "guessed_package"
            df <- add_row(gender_found, gender_origin)
            log_info(paste( "added", character_name," for ", movie_name, "-", imdbid,"-",gender_found,"-",gender_origin, ": successfull"))
          }else if(max_f > 0.60 && max_m < 0.30){
            gender_found <- "actress"
            gender_origin <- "guessed_package"
            df <- add_row(gender_found, gender_origin)
            log_info(paste( "added", character_name," for ", movie_name, "-", imdbid,"-",gender_found,"-",gender_origin, ": successfull"))
          }else{
            log_error(paste( "discarded ", character_name," for ", movie_name, "-", imdbid, ": expected line in gender entry but none found"))
            counter_missing_gender_entry <- counter_missing_gender_entry+1
          }
        }else if(nrow(found_row) == 1){
          gender_found <- found_row[1,2]
          gender_origin <- "data"
          df <- add_row(gender_found, gender_origin)
          log_info(paste( "added", character_name," for ", movie_name, "-", imdbid,"-",gender_found,"-",gender_origin, ": successfull"))
          
        }
      }
    }
  }
}

write.csv(df, file.path("..","raw_caracters_without_commands.csv"))

print("done")