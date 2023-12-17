library(tidyverse)
library(googledrive)

# files -------------------------------------------------------------------
habitat_files <- list.files(path = "monster-habitats", pattern = "habitat", full.names = TRUE) 

# setup -------------------------------------------------------------------
monster_list <- sapply(habitat_files, read_csv, simplify = FALSE) |> 
  bind_rows(.id = "habitat_file") |> 
  rename(
    "Author" = "Source",
    "xp" = "XP"
  ) |> 
  mutate(
    habitat = str_remove_all(sapply(strsplit(habitat_file, "_"), '[', 2), ".csv"),
    Challenge = case_when(
    Challenge == "1/8" ~ "0.125",
    Challenge == "1/4" ~ "0.25", 
    Challenge == "1/2" ~ "0.5",
    TRUE ~ Challenge
  )) |> 
  mutate(Challenge = as.numeric(Challenge)) |> 
  select(c(Name, habitat, xp, Challenge, Author))

party_size <- 4

possible_habitats <- unique(monster_list$habitat)

level_up <- data.frame(current_level = c(1:20), 
                       min_xp = c(
                         0,300,900,2700,6500,14000,23000,34000,48000,64000,85000,100000,
                         120000,140000,165000,195000,225000,265000,305000,355000)) |> 
  mutate(xp_next = lead(min_xp)-min_xp) |> 
  mutate(xp_party = xp_next*party_size)

difficulty <- data.frame(rating = c("easy", "medium", "hard", "deadly")) |> 
  mutate(times_chance = c(2, 3, 2, 1))

xp_thresholds <- data.frame(level = c(1:20), 
                            easy = c(
                              25,50,75,125,250,300,350,450,550,600,800,
                              1000,1100,1250,1400,1600,2000,2100,2400,2800), 
                            medium = c(
                              50,100,150,250,500,600,750,900,1100,1200,
                              1600,2000,2200,2500,2800,3200,3900,4200,4900,5700), 
                            hard = c(
                              75,150,225,375,750,900,1100,1400,1600,1900,2400,3000,
                              3400,3800,4300,4800,5900,6300,7300,8500), 
                            deadly = c(
                              100,200,400,500,1100,1400,1700,2100,2400,2800,3600,
                              4500,5100,5700,6400,7200,8800,9500,10900,12700))

multiplier_df <- data.frame(monsters = c(1:15)) |> 
  mutate(multiplier = case_when(monsters == 1 ~ 1, 
                                monsters == 2 ~ 1.5, 
                                monsters <= 6 & monsters >= 3 ~ 2, 
                                monsters <= 10 & monsters >= 7 ~ 2.5, 
                                monsters <= 14 & monsters >= 11 ~ 3,
                                TRUE ~ 4)) |> 
  mutate(times_chance = c(rep(9, times=2),
                          rep(7, times=3),
                          rep(4, times=2),
                          rep(3, times=2),
                          rep(1, times=6))) %>%
  {as.data.frame(lapply(., rep, .$times_chance))}

difficulty <- as.data.frame(lapply(difficulty, rep, difficulty$times_chance))

# functions ---------------------------------------------------------------
level_encounter <- function (desired_level, party_size = 4, initial_start_xp = NULL, unique_difficulties = NULL) {
  
  current_level <<- desired_level-1
  desired_level <<- desired_level
  
  if (is.null(unique_difficulties)) {
    difficulties <- 4
  } else {
    difficulties <- unique_difficulties
  }
    
  repeat {
  
  if (is.null(initial_start_xp)) {
    start_xp <- level_up[which(level_up$current_level==current_level),]$min_xp
    
    xp_next <- 
      level_up[which(level_up$current_level==current_level),]$xp_party 
  } else {
    start_xp <- initial_start_xp
    
    xp_next <- 
      level_up[which(level_up$current_level==current_level),]$xp_party - 
      start_xp
  }
  
  encounter_table <- data.frame(start_xp=numeric(), 
                                difficulty_rating=character(),
                                number_monsters=numeric(),
                                encounter_xp=numeric(),
                                remain_xp=numeric()) 
  
  
  xp_remaining <- xp_next
    
    while (xp_remaining > 0) {
      
      diff_rate <- sample(difficulty$rating, 1)
      
      if (diff_rate == "easy") {
        multiplier_df_adj <- multiplier_df |> 
          filter(monsters <= 5)
      } else {
        multiplier_df_adj <- multiplier_df
      }
      
      monster_count <- multiplier_df_adj[which(
        multiplier_df_adj$monsters == sample(multiplier_df_adj$monsters, 1)
      ),]$monsters[1]
      
      encounter_xp <- (xp_thresholds[which(xp_thresholds$level == 
                                             current_level), 
                                     diff_rate]) * party_size
      
      xp_remaining <- xp_remaining - encounter_xp
      
      
      encounter_table_row <- data.frame(start_xp = start_xp, 
                                        difficulty_rating = diff_rate,
                                        number_monsters = monster_count,
                                        encounter_xp = encounter_xp,
                                        remain_xp = xp_remaining)
      
      start_xp <- start_xp + encounter_xp/party_size
      
      encounter_table <- bind_rows(encounter_table, encounter_table_row)
      
      difficulty_test <- as.numeric(length(unique(encounter_table$difficulty_rating)))
      
      print(encounter_table)
    }
    
  start_xp <- initial_start_xp
  
  if (difficulty_test == difficulties) {
      break
    }
  } 
  
  potential_num_quests <- case_when(
    nrow(encounter_table) <= 8 ~ 4, 
    T ~ 5)
  
  encounter_table <- encounter_table |> 
    mutate(quests = sort(sample(1:potential_num_quests, nrow(encounter_table),
                                replace = TRUE))) |> 
    mutate(quests = dense_rank(quests)) |> 
    plyr::rename(list(
      "start_xp" = paste0("Individual XP, Level ", current_level),
      "difficulty_rating" = "Difficulty Rating",
      "number_monsters" = "Number of Monsters",
      "encounter_xp" = "Total Encounter XP",
      "remain_xp" = paste0("Remaining Party XP until ", desired_level),
      "quests" = "Quest Breakdown"
    ))
  
  encounter_table_view <<- encounter_table
  
  level_table <- paste0("./level_", current_level, "_to_", desired_level,".csv")
  
  write_csv(encounter_table, file = level_table)
  
  drive_folder <- 
    "https://drive.google.com/drive/folders/10llFlvxQK0kb62ymWTLKErT1dqBFSpXI/"
  
  drive_rm(substr(level_table, 3, nchar(level_table)-4))
  drive_upload(media = level_table, path = drive_folder, type = "spreadsheet")
  
  print(encounter_table)
}

random_encounter <- function (current_level, party_size=4, my_habitat=c("forest"), 
                              monsters=NULL, diff_rate=NULL) {
  random_encounter_table <- data.frame(current_level = numeric(),
                                       difficulty_rating = character(),
                                       number_monsters = numeric(), 
                                       encounter_xp = numeric(), 
                                       ind_xp = numeric())
  
  if (is.null(diff_rate)) {
    diff_rate <- sample(difficulty$rating, 1)
  } 
  
  if (diff_rate == "easy") {
    multiplier_df_adj <- multiplier_df %>% 
      filter(monsters <= 5)
  } else {
    multiplier_df_adj <- multiplier_df
  }
  
  if (is.null(monsters)) {
    monster_count <- multiplier_df_adj[which(
      multiplier_df_adj$monsters == sample(multiplier_df_adj$monsters, 1)
    ),]$monsters[1]
  } else {
    monster_count <- monsters
  }  
  
  encounter_xp_base <- (xp_thresholds[which(xp_thresholds$level == 
                                              current_level), 
                                      diff_rate]) * party_size
  
  encounter_mult <- multiplier_df %>% 
    group_by(monsters) %>% 
    slice_head() %>% 
    {.[which(.$monsters==monster_count),]$multiplier}
  
  xp_nonadj <- encounter_xp_base/encounter_mult
  
  monster_list_adj <- monster_list %>% 
    filter(xp <= xp_nonadj) %>% 
    filter(habitat %in% my_habitat) %>% 
    group_by(Name) %>% 
    slice_head() %>% 
    ungroup()
  
  sum_xp <- 0
  
  if (diff_rate != "deadly") {
    while (sum_xp > xp_nonadj*1.5 | sum_xp < xp_nonadj) {
      attempt <- sample(monster_list_adj$Name, monster_count, replace = F)
      
      sum_xp <- monster_list_adj %>% 
        filter(Name %in% attempt) %>% 
        {sum(.$xp)}
      
      print(sum_xp)
    }
  } else {
    while (sum_xp > xp_nonadj*1.25 | sum_xp < xp_nonadj) {
      attempt <- sample(monster_list_adj$Name, monster_count, replace = F)
      
      sum_xp <- monster_list_adj %>% 
        filter(Name %in% attempt) %>% 
        {sum(.$xp)}
      
      print(sum_xp)
    }
  }
  
  encounter_monster_table <- monster_list_adj %>% 
    filter(Name %in% attempt) %>% 
    subset(select = c(Name, xp, Challenge, habitat)) 
  
  random_encounter_row <- data.frame(current_level, 
                                     difficulty_rating = diff_rate, 
                                     number_monsters = monster_count, 
                                     encounter_xp = sum(encounter_monster_table$xp)*encounter_mult) %>% 
    mutate(ind_xp = ceiling(encounter_xp/party_size))
  
  random_encounter_table <- bind_rows(random_encounter_table, 
                                      random_encounter_row)
  
  random_encounter_table <<- random_encounter_table
  
  encounter_monster_table <<- encounter_monster_table
  
  print(random_encounter_table)
  
  print(encounter_monster_table)
}

encounter_diff_xp <- function (current_level, party_size=4, monster_count, 
                               unadj_xp) {
  
  xp_thresholds_adj <- xp_thresholds %>% 
    filter(level == current_level)
  
  encounter_mult <- multiplier_df %>% 
    group_by(monsters) %>% 
    slice_head() %>% 
    {.[which(.$monsters==monster_count),]$multiplier}
  
  xp_adj_ind <- encounter_mult*unadj_xp/party_size
  
  diff_rating <- xp_thresholds_adj %>% 
    pivot_longer(cols = c("easy", "medium", "hard", "deadly"), 
                 names_to = "difficulty", values_to = "xp") %>% 
    filter(xp <= xp_adj_ind) %>% 
    filter(xp == max(xp)) %>% 
    rename("xp_threshold" = "xp") %>% 
    mutate(xp_adj_ind) %>% 
    mutate(extremity = case_when(
      difficulty=="deadly" & xp_adj_ind>1.2*xp_threshold ~ "too deadly", 
      difficulty=="easy" & xp_adj_ind<0.8*xp_threshold ~ "too easy", 
      T ~ "feasible"))
  
  print(diff_rating)
}

# written_levels ----------------------------------------------------------

written_level_files <- list.files("dnd/", pattern = "level_*", full.names = T)

currently_planned <- sapply(written_level_files, read_csv, simplify = F) %>% 
  bind_rows(.id = "file") %>% 
  mutate(
    levels = str_c(sapply(strsplit(file, "_"), `[`, 2), "-", str_remove(sapply(strsplit(file, "_"), `[`, 4), ".csv"))
  ) %>% 
  select(c(-file))
# sandbox -----------------------------------------------------------------
