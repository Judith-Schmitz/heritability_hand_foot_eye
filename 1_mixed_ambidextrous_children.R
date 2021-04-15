# mixed vs ambidextrous for unrelated children

freq(mydata$hand.cat)
freq(mydata$foot.cat)
freq(mydata$eye.cat)

mixed.hand <- mydata %>%
  filter(hand.cat == 1) %>% # filter individuals in mixed category
  select(hand.draw, hand.throw, hand.colour, hand.hold, hand.cut, hand.hit)

mixed.hand$zeros <- rowSums(mixed.hand[1:6] == 0, na.rm = TRUE) # how many times has each individual checked 0?
mixed.hand$ones <- rowSums(mixed.hand[1:6] == 1, na.rm = TRUE) # how many times has each individual checked 1?
mixed.hand$twos <- rowSums(mixed.hand[1:6] == 2, na.rm = TRUE) # how many times has each individual checked 2?
mixed.hand$nas <- rowSums(is.na(mixed.hand[1:6])) # how many NA values for each individual?

input_names = names(mixed.hand)[7:10]

mixed.hand %>% 
  count_(input_names) %>% 
  unite_("ComboVar", input_names, sep = "")



mixed.foot <- mydata %>%
  filter(foot.cat == 1) %>%
  select(foot.kick, foot.pick, foot.stamp, foot.climb)

mixed.foot$zeros <- rowSums(mixed.foot[1:4] == 0, na.rm = TRUE)
mixed.foot$ones <- rowSums(mixed.foot[1:4] == 1, na.rm = TRUE)
mixed.foot$twos <- rowSums(mixed.foot[1:4] == 2, na.rm = TRUE)
mixed.foot$nas <- rowSums(is.na(mixed.foot[1:4]))

input_names = names(mixed.foot)[5:8]

mixed.foot %>% 
  count_(input_names) %>% 
  unite_("ComboVar", input_names, sep = "")



mixed.eye <- mydata %>%
  filter(eye.cat == 1) %>%
  select(eye.hole, eye.bottle)

mixed.eye$zeros <- rowSums(mixed.eye[1:2] == 0, na.rm = TRUE)
mixed.eye$ones <- rowSums(mixed.eye[1:2] == 1, na.rm = TRUE)
mixed.eye$twos <- rowSums(mixed.eye[1:2] == 2, na.rm = TRUE)
mixed.eye$nas <- rowSums(is.na(mixed.eye[1:2]))

input_names = names(mixed.eye)[3:6]

mixed.eye %>% 
  count_(input_names) %>% 
  unite_("ComboVar", input_names, sep = "")



