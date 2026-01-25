library(dplyr)
library(readr)

State_S <- read_csv("D:/Users/Downloads/profile_data_1_All State_2024-25 (1)/100_prof1.csv")
Students_S <- read_csv("D:/Users/Downloads/enrolment_data_2_All State_2024-25/100_enr2.csv")
Teachers_S <- read_csv("D:/Users/Downloads/teacher_data_All State_2024-25 (1)/100_tch.csv")

State_S <- State_S %>%
  arrange(pseudocode)

Students_S <- Students_S %>%
  group_by(pseudocode) %>%
  summarise( 
    across(matches("^(cpp|c\\d{1,2})_(b|g)$"),
           ~ sum(.x,na.rm = TRUE)),
    .groups = "drop"
  )

Students_S <- Students_S %>%
  mutate(students_s = rowSums(
    select(., c1_b:c12_g) ,na.rm = TRUE))

State_S2 <- State_S %>% select(pseudocode,state)
#View(State_S2)
Students_S2 <- Students_S %>% select(pseudocode,students_s)
#View(Students_S2)
Teachers_S2 <- Teachers_S %>% select(pseudocode,total_tch)
#View(Teachers_S2)

State_Teachers_S <- State_S2 %>%
  full_join(Teachers_S2, by = "pseudocode")

State_Students_Teachers_S <- State_Teachers_S %>%
  full_join(Students_S2, by = "pseudocode")
#View(State_Students_Teachers_S)

State_Students_Teachers_S <- State_Students_Teachers_S %>%
  arrange(state)
#View(State_Students_Teachers_S)

No_enrolment_S <- State_Students_Teachers_S %>%
  filter(students_s == 0) 
#View(No_enrolment_S)

No_enrolment_S <- No_enrolment_S %>%
  group_by(state) %>%
  summarise(
    Total_Schools = n() ) %>%
  ungroup() 
View(No_enrolment_S)

No_enrolment <- No_enrolment %>%
  arrange(desc(Total_Schools))
#View(No_enrolment)






















