# mixed vs ambidextrous for mothers and fathers


# recode items
master$m.hand.1 <- as.factor(recode(master$m.hand.1, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.2 <- as.factor(recode(master$m.hand.2, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.3 <- as.factor(recode(master$m.hand.3, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.4 <- as.factor(recode(master$m.hand.4, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.5 <- as.factor(recode(master$m.hand.5, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.6 <- as.factor(recode(master$m.hand.6, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.7 <- as.factor(recode(master$m.hand.7, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.8 <- as.factor(recode(master$m.hand.8, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.9 <- as.factor(recode(master$m.hand.9, "3" = "0", "2" = "1", "1" = "2"))
master$m.hand.10 <- as.factor(recode(master$m.hand.10,"3" = "0", "2" = "1", "1" = "2"))
master$m.hand.11 <- as.factor(recode(master$m.hand.11, "3" = "0", "2" = "1", "1" = "2"))

master$p.hand.1 <- as.factor(recode(master$p.hand.1, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.2 <- as.factor(recode(master$p.hand.2, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.3 <- as.factor(recode(master$p.hand.3, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.4 <- as.factor(recode(master$p.hand.4, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.5 <- as.factor(recode(master$p.hand.5, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.6 <- as.factor(recode(master$p.hand.6, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.7 <- as.factor(recode(master$p.hand.7, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.8 <- as.factor(recode(master$p.hand.8, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.9 <- as.factor(recode(master$p.hand.9, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.10 <- as.factor(recode(master$p.hand.10, "3" = "0", "2" = "1", "1" = "2"))
master$p.hand.11 <- as.factor(recode(master$p.hand.11, "3" = "0", "2" = "1", "1" = "2"))

master$m.foot.1 <- as.factor(recode(master$m.foot.1, "3" = "0", "2" = "1", "1" = "2"))
master$m.foot.2 <- as.factor(recode(master$m.foot.2, "3" = "0", "2" = "1", "1" = "2"))
master$m.foot.3 <- as.factor(recode(master$m.foot.3, "3" = "0", "2" = "1", "1" = "2"))
master$m.foot.4 <- as.factor(recode(master$m.foot.4, "3" = "0", "2" = "1", "1" = "2"))

master$p.foot.1 <- as.factor(recode(master$p.foot.1, "3" = "0", "2" = "1", "1" = "2"))
master$p.foot.2 <- as.factor(recode(master$p.foot.2, "3" = "0", "2" = "1", "1" = "2"))
master$p.foot.3 <- as.factor(recode(master$p.foot.3, "3" = "0", "2" = "1", "1" = "2"))
master$p.foot.4 <- as.factor(recode(master$p.foot.4, "3" = "0", "2" = "1", "1" = "2"))

master$m.eye.1 <- as.factor(recode(master$m.eye.1, "3" = "0", "2" = "1", "1" = "2"))
master$m.eye.2 <- as.factor(recode(master$m.eye.2, "3" = "0", "2" = "1", "1" = "2"))

master$p.eye.1 <- as.factor(recode(master$p.eye.1, "3" = "0", "2" = "1", "1" = "2"))
master$p.eye.2 <- as.factor(recode(master$p.eye.2, "3" = "0", "2" = "1", "1" = "2"))

# filter individuals

mixed.hand <- master %>%
  filter(!is.na(hand) & !is.na(m.hand) & !is.na(p.hand)) %>%
  filter(hand == 1 | hand == 3) %>%
  dplyr::select(m.hand, m.hand.1, m.hand.2, m.hand.3, m.hand.4, m.hand.5, m.hand.6, m.hand.7, m.hand.8, m.hand.9, m.hand.10, m.hand.11,
                p.hand, p.hand.1, p.hand.2, p.hand.3, p.hand.4, p.hand.5, p.hand.6, p.hand.7, p.hand.8, p.hand.9, p.hand.10, p.hand.11)

mixed.foot <- master %>%
  filter(!is.na(foot) & !is.na(m.foot) & !is.na(p.foot)) %>%
  filter(foot == 1 | foot == 3) %>%
  dplyr::select(m.foot, m.foot.1, m.foot.2, m.foot.3, m.foot.4,
                p.foot, p.foot.1, p.foot.2, p.foot.3, p.foot.4)

mixed.eye <- master %>%
  filter(!is.na(eye) & !is.na(m.eye) & !is.na(p.eye)) %>%
  filter(eye == 1 | eye == 3) %>%
  dplyr::select(m.eye, m.eye.1, m.eye.2,
                p.eye, p.eye.1, p.eye.2)


# hand mothers

mixed.hand.mothers <- mixed.hand %>%
  filter(m.hand == 2)

mixed.hand.mothers$zeros <- rowSums(mixed.hand.mothers[2:12] == 0, na.rm = TRUE) # how many times has each individual checked 0?
mixed.hand.mothers$ones <- rowSums(mixed.hand.mothers[2:12] == 1, na.rm = TRUE) # how many times has each individual checked 1?
mixed.hand.mothers$twos <- rowSums(mixed.hand.mothers[2:12] == 2, na.rm = TRUE) # how many times has each individual checked 2?
mixed.hand.mothers$nas <- rowSums(is.na(mixed.hand.mothers[2:12])) # how many NA values for each individual?

input_names = names(mixed.hand.mothers)[25:28]

print(mixed.hand.mothers %>%
        count_(input_names) %>% 
        unite_("items", input_names, sep = ","))

# hand fathers

mixed.hand.fathers <- mixed.hand %>%
  filter(p.hand == 2)

mixed.hand.fathers$zeros <- rowSums(mixed.hand.fathers[14:24] == 0, na.rm = TRUE) # how many times has each individual checked 0?
mixed.hand.fathers$ones <- rowSums(mixed.hand.fathers[14:24] == 1, na.rm = TRUE) # how many times has each individual checked 1?
mixed.hand.fathers$twos <- rowSums(mixed.hand.fathers[14:24] == 2, na.rm = TRUE) # how many times has each individual checked 2?
mixed.hand.fathers$nas <- rowSums(is.na(mixed.hand.fathers[14:24])) # how many NA values for each individual?

input_names = names(mixed.hand.fathers)[25:28]

print(mixed.hand.fathers %>% 
        count_(input_names) %>% 
        unite_("items", input_names, sep = ","))

# foot mothers

mixed.foot.mothers <- mixed.foot %>%
  filter(m.foot == 2)

mixed.foot.mothers$zeros <- rowSums(mixed.foot.mothers[2:5] == 0, na.rm = TRUE)
mixed.foot.mothers$ones <- rowSums(mixed.foot.mothers[2:5] == 1, na.rm = TRUE)
mixed.foot.mothers$twos <- rowSums(mixed.foot.mothers[2:5] == 2, na.rm = TRUE)
mixed.foot.mothers$nas <- rowSums(is.na(mixed.foot.mothers[2:5]))

input_names = names(mixed.foot.mothers)[11:14]

print(mixed.foot.mothers %>% 
        count_(input_names) %>% 
        unite_("items", input_names, sep = ","))

# foot fathers

mixed.foot.fathers <- mixed.foot %>%
  filter(p.foot == 2)

mixed.foot.fathers$zeros <- rowSums(mixed.foot.fathers[7:10] == 0, na.rm = TRUE)
mixed.foot.fathers$ones <- rowSums(mixed.foot.fathers[7:10] == 1, na.rm = TRUE)
mixed.foot.fathers$twos <- rowSums(mixed.foot.fathers[7:10] == 2, na.rm = TRUE)
mixed.foot.fathers$nas <- rowSums(is.na(mixed.foot.fathers[7:10]))

input_names = names(mixed.foot.fathers)[11:14]

print(mixed.foot.fathers %>% 
        count_(input_names) %>% 
        unite_("items", input_names, sep = ","))

# eye mothers

mixed.eye.mothers <- mixed.eye %>%
  filter(m.eye == 2)

mixed.eye.mothers$zeros <- rowSums(mixed.eye.mothers[2:3] == 0, na.rm = TRUE)
mixed.eye.mothers$ones <- rowSums(mixed.eye.mothers[2:3] == 1, na.rm = TRUE)
mixed.eye.mothers$twos <- rowSums(mixed.eye.mothers[2:3] == 2, na.rm = TRUE)
mixed.eye.mothers$nas <- rowSums(is.na(mixed.eye.mothers[2:3]))

input_names = names(mixed.eye.mothers)[7:10]

print(mixed.eye.mothers %>% 
        count_(input_names) %>% 
        unite_("items", input_names, sep = ","))

# eye fathers

mixed.eye.fathers <- mixed.eye %>%
  filter(p.eye == 2)

mixed.eye.fathers$zeros <- rowSums(mixed.eye.fathers[5:6] == 0, na.rm = TRUE)
mixed.eye.fathers$ones <- rowSums(mixed.eye.fathers[5:6] == 1, na.rm = TRUE)
mixed.eye.fathers$twos <- rowSums(mixed.eye.fathers[5:6] == 2, na.rm = TRUE)
mixed.eye.fathers$nas <- rowSums(is.na(mixed.eye.fathers[5:6]))

input_names = names(mixed.eye.fathers)[7:10]

print(mixed.eye.fathers %>% 
        count_(input_names) %>% 
        unite_("items", input_names, sep = ","))

