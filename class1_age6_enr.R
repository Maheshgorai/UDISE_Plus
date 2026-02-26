library(dplyr)
library(readr)
library(tidyr)
library(writexl)

State <- read_csv("D:/*****/*****/profile_data_1_All State_2024-25 (1)/100_prof1.csv")
Students <- read_csv("D:/*****/*****/enrolment_data_2_All State_2024-25/100_enr2.csv")


Students <-Students%>%
  filter(item_id==6)%>%
  mutate(
    class_1 = rowSums(select(.,c1_b:c1_g)),na.rm = TRUE)
    

State_2 <- State %>% select(pseudocode,state,school_category, managment)

Students_2 <- Students %>% select(pseudocode,c1_b,c1_g,class_1)



State_Students <- State_2 %>%
  full_join(Students_2, by = "pseudocode")%>%
  filter(managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101))
View(State_Students)


enr_ind <- State_Students%>%
  filter(school_category %in% c(1,2,3,6))%>%
  mutate(
    state = "India",
    school_category_label = case_when(
      school_category == 1 ~ "Primary only (I-V)",
      school_category == 2 ~ "Upper Primary with I-VIII",
      school_category == 3 ~ "HSS (I-XII)",                  
      school_category == 4 ~ "Upper Primary only (VI-VIII)",
      school_category == 5 ~ "HSS (VI-XII)",
      school_category == 6 ~ "SS (I-X)",
      school_category == 7 ~ "SS (VI-X)",
      school_category == 8 ~ "SS only (IX-X)",
      school_category == 10 ~ "HSS (IX-XII)",
      school_category == 11 ~ "HSS (XI-XII)",
      TRUE ~ as.character(school_category)
    ))%>%
  group_by(state,school_category_label)%>%
  summarise(
    total_sch = n(),
    total_c1 = sum(class_1, na.rm = TRUE),
    total_c1_b = sum(c1_b,na.rm = TRUE),
    total_c1_g = sum(c1_g, na.rm = TRUE),
    .groups = "drop"
  )


enr_state <- State_Students%>%
  group_by(state,school_category)%>%
  summarise(
    total_sch = n(),
    total_c1 = sum(class_1, na.rm = TRUE),
    total_c1_b = sum(c1_b,na.rm = TRUE),
    total_c1_g = sum(c1_g, na.rm = TRUE),
    .groups = "drop"
  )%>%
  filter(school_category %in% c(1,2,3,6))%>%
  mutate(
    school_category_label = case_when(
      school_category == 1 ~ "Primary only (I-V)",
      school_category == 2 ~ "Upper Primary with I-VIII",
      school_category == 3 ~ "HSS (I-XII)",                  
      school_category == 4 ~ "Upper Primary only (VI-VIII)",
      school_category == 5 ~ "HSS (VI-XII)",
      school_category == 6 ~ "SS (I-X)",
      school_category == 7 ~ "SS (VI-X)",
      school_category == 8 ~ "SS only (IX-X)",
      school_category == 10 ~ "HSS (IX-XII)",
      school_category == 11 ~ "HSS (XI-XII)",
      TRUE ~ as.character(school_category)
    ))

c1_enr <- bind_rows(enr_ind,enr_state)%>%
  select(state,school_category_label,total_sch,total_c1,total_c1_b,total_c1_g)
 








