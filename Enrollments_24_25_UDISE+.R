library(dplyr)
library(readr)
library(tidyr)
library(writexl)
State <- read_csv("D:/Users/Downloads/profile_data_1_All State_2024-25 (1)/100_prof1.csv")
Students <- read_csv("D:/Users/Downloads/enrolment_data_2_All State_2024-25/100_enr2.csv")


View(State)
View(Students)

Students <- Students %>%
  group_by(pseudocode) %>%
  summarise( 
    across(matches("^(cpp|c\\d{1,2})_(b|g)$"),
           ~ sum(.x,na.rm = TRUE)),
    .groups = "drop"
  )
View(Students)

Students <- Students %>%
  mutate(
    total_students = rowSums(select(., cpp_b:c12_g),na.rm = TRUE),
    total_boys = rowSums(select(.,matches("_(b)$")),na.rm = TRUE),
    total_girls = rowSums(select(.,matches("_(g)$")),na.rm = TRUE),
    class_1 = rowSums(select(.,c1_b:c1_g),na.rm = TRUE),
    class_5 = rowSums(select(.,c5_b:c5_g),na.rm = TRUE),
    class_8 = rowSums(select(.,c8_b:c8_g),na.rm = TRUE),
    class_10 = rowSums(select(.,c10_b:c10_g),na.rm = TRUE),
    class_12 = rowSums(select(., c12_b:c12_g)),na.rm = TRUE)
View(Students)

State_2 <- State %>% select(pseudocode,state,school_category, managment)
View(State_2)
Students_2 <- Students %>% select(pseudocode,c1_b,c1_g,c5_b,c5_g,c8_b,c8_g,c10_b
                                  ,c10_g,c12_b,c12_g,total_students,class_1,class_5
                                  ,class_8,class_10,class_12,total_boys,total_girls)
View(Students_2)

State_Students <- State_2 %>%
  full_join(Students_2, by = "pseudocode")
View(State_Students)

# GOVERNMENT CLASS 1(INDIA) ====================================================

gov_c1_india <- State_Students %>%
  filter(
    managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101),
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_1 = sum(class_1, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_c1_india)

# GOVERNMENT CLASS 1(STATE) ===========================================================

State_students_gov_c1 <- State_Students %>%
  filter(managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101))%>%
  select(pseudocode,state,school_category,managment,c1_b,c1_g,class_1,total_boys,
         total_girls,total_students)


gov_summary_by_state_c1 <- State_students_gov_c1 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_1   = sum(class_1, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_1, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_summary_by_state_c1)

# GOVERNMENT CLASS 1(COMBINED) ================================================

gov_c1_final <- bind_rows(gov_c1_india, gov_summary_by_state_c1) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_c1_final)

# GOV AIED CLASS 1(INDIA) =====================================================

gov_aided_c1_india <- State_Students %>%
  filter(
    managment %in% c(4,7),
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_1 = sum(class_1, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_aided_c1_india)

# GOV AIDED CLASS 1(STATE) ============================================================

State_students_gov_aided_c1 <- State_Students %>%
  filter(managment %in% c(4,7))%>%
  select(pseudocode,state,school_category,managment,c1_b,c1_g,class_1,total_boys,
         total_girls,total_students)

gov_aided_summary_by_state_c1 <- State_students_gov_aided_c1 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_1   = sum(class_1, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_1, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_aided_summary_by_state_c1)

# GOV AIDED CLASS 1(COMBINED) =================================================

gov_aided_c1_final <- bind_rows(gov_aided_c1_india, gov_aided_summary_by_state_c1) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_aided_c1_final)


# PRIVATE UNAIDED CLASS 1(INDIA) ==============================================

private_unaided_c1_india <- State_Students %>%
  filter(
    managment == 5,
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_1 = sum(class_1, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(private_unaided_c1_india)

# PRIVATE UNAIDED CLASS 1(STATE) ======================================================

State_students_private_unaided_c1 <- State_Students %>%
  filter(managment == 5) %>%
  select(pseudocode,state,school_category,managment,c1_b,c1_g,class_1,total_boys,
         total_girls,total_students)

private_unaided_summary_by_state_c1 <- State_students_private_unaided_c1 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_1   = sum(class_1, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_1, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(private_unaided_summary_by_state_c1)

# PRIVATE UNAIDED CLASS 1(COMBINED) ===========================================

private_unaided_c1_final <- bind_rows(private_unaided_c1_india, private_unaided_summary_by_state_c1) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(private_unaided_c1_final)

# OVERALL CLASS 1(INDIA) ======================================================

overall_c1_india <- State_Students %>%
  filter(
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_1 = sum(class_1, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(overall_c1_india)

# OVERALL CLASS 1(STATE) ==============================================================

State_students_overall_c1 <- State_Students %>%
  select(pseudocode,state,school_category,managment,c1_b,c1_g,class_1,total_boys,
         total_girls,total_students)

overall_summary_by_state_c1 <- State_students_overall_c1 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_1   = sum(class_1, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_1, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(overall_summary_by_state_c1)

# OVERALL CLASS 1(COMBINED) ===================================================

overall_c1_final <- bind_rows(overall_c1_india, overall_summary_by_state_c1) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(overall_c1_final)

# GOVERNMENT CLASS 5(INDIA) ====================================================

gov_c5_india <- State_Students %>%
  filter(
    managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101),
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_5 = sum(class_5, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_c5_india)

# GOVERNMENT CLASS 5(STATE) ===========================================================

State_students_gov_c5 <- State_Students %>%
  filter(managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101))%>%
  select(pseudocode,state,school_category,managment,c5_b,c5_g,class_5,total_boys,
         total_girls,total_students)


gov_summary_by_state_c5 <- State_students_gov_c5 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_5   = sum(class_5, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_5, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_summary_by_state_c5)

# GOVERNMENT CLASS 5(COMBINED) ================================================

gov_c5_final <- bind_rows(gov_c5_india, gov_summary_by_state_c5) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_c5_final)

# GOV AIED CLASS 5(INDIA) =====================================================

gov_aided_c5_india <- State_Students %>%
  filter(
    managment %in% c(4,7),
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_5 = sum(class_5, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_aided_c5_india)

# GOV AIDED CLASS 5(STATE) ============================================================

State_students_gov_aided_c5 <- State_Students %>%
  filter(managment %in% c(4,7))%>%
  select(pseudocode,state,school_category,managment,c5_b,c5_g,class_5,total_boys,
         total_girls,total_students)

gov_aided_summary_by_state_c5 <- State_students_gov_aided_c5 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_5   = sum(class_5, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_5, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_aided_summary_by_state_c5)

# GOV AIDED CLASS 5(COMBINED) =================================================

gov_aided_c5_final <- bind_rows(gov_aided_c5_india, gov_aided_summary_by_state_c5) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_aided_c5_final)


# PRIVATE UNAIDED CLASS 5(INDIA) ==============================================

private_unaided_c5_india <- State_Students %>%
  filter(
    managment == 5,
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_5 = sum(class_5, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(private_unaided_c5_india)

# PRIVATE UNAIDED CLASS 5(STATE) ======================================================

State_students_private_unaided_c5 <- State_Students %>%
  filter(managment == 5) %>%
  select(pseudocode,state,school_category,managment,c5_b,c5_g,class_5,total_boys,
         total_girls,total_students)

private_unaided_summary_by_state_c5 <- State_students_private_unaided_c5 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_5   = sum(class_5, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_5, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(private_unaided_summary_by_state_c5)

# PRIVATE UNAIDED CLASS 5(COMBINED) ===========================================

private_unaided_c5_final <- bind_rows(private_unaided_c5_india, private_unaided_summary_by_state_c5) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(private_unaided_c5_final)

# OVERALL CLASS 5(INDIA) ======================================================

overall_c5_india <- State_Students %>%
  filter(
    school_category %in% c(1,2,3,6)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_5 = sum(class_5, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(overall_c5_india)

# OVERALL CLASS 5(STATE) ==============================================================

State_students_overall_c5 <- State_Students %>%
  select(pseudocode,state,school_category,managment,c5_b,c5_g,class_5,total_boys,
         total_girls,total_students)

overall_summary_by_state_c5 <- State_students_overall_c5 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_5   = sum(class_5, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_5, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(overall_summary_by_state_c5)

# OVERALL CLASS 5(COMBINED) ===================================================

overall_c5_final <- bind_rows(overall_c5_india, overall_summary_by_state_c5) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(overall_c5_final)

# GOVERNMENT CLASS 8(INDIA) ===================================================

gov_c8_india <- State_Students %>%
  filter(
    managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101),
    school_category %in% c(2,3,4,5,6,7)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_8 = sum(class_8, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_c8_india)

# GOVERNMENT CLASS 8(STATE) ===========================================================

State_students_gov_c8 <- State_Students %>%
  filter(managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101))%>%
  select(pseudocode,state,school_category,managment,c8_b,c8_g,class_8,total_boys,
         total_girls,total_students)


gov_summary_by_state_c8 <- State_students_gov_c8 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_8   = sum(class_8, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(2,3,4,5,6,7))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_8, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_summary_by_state_c8)

# GOVERNMENT CLASS 8(COMBINED) ================================================

gov_c8_final <- bind_rows(gov_c8_india, gov_summary_by_state_c8) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_c8_final)

# GOV AIED CLASS 8(INDIA) =====================================================

gov_aided_c8_india <- State_Students %>%
  filter(
    managment %in% c(4,7),
    school_category %in% c(2,3,4,5,6,7)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_8 = sum(class_8, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_aided_c8_india)

# GOV AIDED CLASS 8(STATE) ============================================================

State_students_gov_aided_c8 <- State_Students %>%
  filter(managment %in% c(4,7))%>%
  select(pseudocode,state,school_category,managment,c8_b,c8_g,class_8,total_boys,
         total_girls,total_students)

gov_aided_summary_by_state_c8 <- State_students_gov_aided_c8 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_8   = sum(class_8, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(2,3,4,5,6,7))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_8, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_aided_summary_by_state_c8)

# GOV AIDED CLASS 8(COMBINED) =================================================

gov_aided_c8_final <- bind_rows(gov_aided_c8_india, gov_aided_summary_by_state_c8) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_aided_c8_final)


# PRIVATE UNAIDED CLASS 8(INDIA) ==============================================

private_unaided_c8_india <- State_Students %>%
  filter(
    managment == 5,
    school_category %in% c(2,3,4,5,6,7)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_8 = sum(class_8, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(private_unaided_c8_india)

# PRIVATE UNAIDED CLASS 8(STATE) ======================================================

State_students_private_unaided_c8 <- State_Students %>%
  filter(managment == 5) %>%
  select(pseudocode,state,school_category,managment,c8_b,c8_g,class_8,total_boys,
         total_girls,total_students)

private_unaided_summary_by_state_c8 <- State_students_private_unaided_c8 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_8   = sum(class_8, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(2,3,4,5,6,7))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_8, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(private_unaided_summary_by_state_c8)

# PRIVATE UNAIDED CLASS 8(COMBINED) ===========================================

private_unaided_c8_final <- bind_rows(private_unaided_c8_india, private_unaided_summary_by_state_c8) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(private_unaided_c8_final)

# OVERALL CLASS 8(INDIA) ======================================================

overall_c8_india <- State_Students %>%
  filter(
    school_category %in% c(2,3,4,5,6,7)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_8 = sum(class_8, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(overall_c8_india)

# OVERALL CLASS 8(STATE) ==============================================================

State_students_overall_c8 <- State_Students %>%
  select(pseudocode,state,school_category,managment,c8_b,c8_g,class_8,total_boys,
         total_girls,total_students)

overall_summary_by_state_c8 <- State_students_overall_c8 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_8   = sum(class_8, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(2,3,4,5,6,7))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_8, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(overall_summary_by_state_c8)

# OVERALL CLASS 8(COMBINED) ===================================================

overall_c8_final <- bind_rows(overall_c8_india, overall_summary_by_state_c8) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(overall_c8_final)

# GOVERNMENT CLASS 10(INDIA) ===================================================

gov_c10_india <- State_Students %>%
  filter(
    managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101),
    school_category %in% c(3,5,6,7,8,10)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_10 = sum(class_10, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_c10_india)


# GOVERNMENT CLASS 10(STATE) ===================================================

State_students_gov_c10 <- State_Students %>%
  filter(managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101))%>%
  select(pseudocode,state,school_category,managment,c10_b,c10_g,class_10,total_boys,
         total_girls,total_students)


gov_summary_by_state_c10 <- State_students_gov_c10 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_10   = sum(class_10, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,6,7,8,10))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_10, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_summary_by_state_c10)

# GOVERNMENT CLASS 10(COMBINED) ================================================

gov_c10_final <- bind_rows(gov_c10_india, gov_summary_by_state_c10) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_c10_final)

# GOV AIED CLASS 10(INDIA) =====================================================

gov_aided_c10_india <- State_Students %>%
  filter(
    managment %in% c(4,7),
    school_category %in% c(3,5,6,7,8,10)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_10 = sum(class_10, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_aided_c10_india)

# GOV AIDED CLASS 10(STATE) ====================================================

State_students_gov_aided_c10 <- State_Students %>%
  filter(managment %in% c(4,7))%>%
  select(pseudocode,state,school_category,managment,c10_b,c10_g,class_10,total_boys,
         total_girls,total_students)

gov_aided_summary_by_state_c10 <- State_students_gov_aided_c10 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_10   = sum(class_10, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,6,7,8,10))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_10, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_aided_summary_by_state_c10)

# GOV AIDED CLASS 10(COMBINED) =================================================

gov_aided_c10_final <- bind_rows(gov_aided_c10_india, gov_aided_summary_by_state_c10) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_aided_c10_final)

# PRIVATE UNAIDED CLASS 10(INDIA) ==============================================

private_unaided_c10_india <- State_Students %>%
  filter(
    managment == 5,
    school_category %in% c(3,5,6,7,8,10)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_10 = sum(class_10, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(private_unaided_c10_india)

# PRIVATE UNAIDED CLASS 10(STATE) ==============================================

State_students_private_unaided_c10 <- State_Students %>%
  filter(managment == 5) %>%
  select(pseudocode,state,school_category,managment,c10_b,c10_g,class_10,total_boys,
         total_girls,total_students)

private_unaided_summary_by_state_c10 <- State_students_private_unaided_c10 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_10   = sum(class_10, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,6,7,8,10))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_10, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(private_unaided_summary_by_state_c10)

# PRIVATE UNAIDED CLASS 10(COMBINED) ===========================================

private_unaided_c10_final <- bind_rows(private_unaided_c10_india, private_unaided_summary_by_state_c10) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(private_unaided_c10_final)

# OVERALL CLASS 10(INDIA) ======================================================

overall_c10_india <- State_Students %>%
  filter(
    school_category %in% c(3,5,6,7,8,10)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_10 = sum(class_10, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(overall_c10_india)

# OVERALL CLASS 10(STATE) ======================================================

State_students_overall_c10 <- State_Students %>%
  select(pseudocode,state,school_category,managment,c10_b,c10_g,class_10,total_boys,
         total_girls,total_students)

overall_summary_by_state_c10 <- State_students_overall_c10 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_10   = sum(class_10, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,6,7,8,10))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_10, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(overall_summary_by_state_c10)

# OVERALL CLASS 10(COMBINED) ===================================================

overall_c10_final <- bind_rows(overall_c10_india, overall_summary_by_state_c10) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(overall_c10_final)

# GOVERNMENT CLASS 12(INDIA) ===================================================

gov_c12_india <- State_Students %>%
  filter(
    managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101),
    school_category %in% c(3,5,10,11)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_12 = sum(class_12, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_c12_india)

# GOVERNMENT CLASS 12(STATE) ===================================================

State_students_gov_c12 <- State_Students %>%
  filter(managment %in% c(1,2,3,6,89,90,91,92,93,94,95,96,101))%>%
  select(pseudocode,state,school_category,managment,c12_b,c12_g,class_12,total_boys,
         total_girls,total_students)


gov_summary_by_state_c12 <- State_students_gov_c12 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_12   = sum(class_12, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,10,11))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_12, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_summary_by_state_c12)

# GOVERNMENT CLASS 12(COMBINED) ================================================

gov_c12_final <- bind_rows(gov_c12_india, gov_summary_by_state_c12) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_c12_final)

# GOV AIED CLASS 12(INDIA) =====================================================

gov_aided_c12_india <- State_Students %>%
  filter(
    managment %in% c(4,7),
    school_category %in% c(3,5,10,11)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_12 = sum(class_12, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(gov_aided_c12_india)

# GOV AIDED CLASS 12(STATE) ============================================================

State_students_gov_aided_c12 <- State_Students %>%
  filter(managment %in% c(4,7))%>%
  select(pseudocode,state,school_category,managment,c12_b,c12_g,class_12,total_boys,
         total_girls,total_students)

gov_aided_summary_by_state_c12 <- State_students_gov_aided_c12 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_12   = sum(class_12, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,10,11))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_12, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(gov_aided_summary_by_state_c12)

# GOV AIDED CLASS 12(COMBINED) =================================================

gov_aided_c12_final <- bind_rows(gov_aided_c12_india, gov_aided_summary_by_state_c12) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(gov_aided_c12_final)


# PRIVATE UNAIDED CLASS 12(INDIA) ==============================================

private_unaided_c12_india <- State_Students %>%
  filter(
    managment == 5,
    school_category %in% c(3,5,10,11)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_12 = sum(class_12, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(private_unaided_c12_india)

# PRIVATE UNAIDED CLASS 12(STATE) ======================================================

State_students_private_unaided_c12 <- State_Students %>%
  filter(managment == 5) %>%
  select(pseudocode,state,school_category,managment,c12_b,c12_g,class_12,total_boys,
         total_girls,total_students)

private_unaided_summary_by_state_c12 <- State_students_private_unaided_c12 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_12   = sum(class_12, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,10,11))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_12, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(private_unaided_summary_by_state_c12)

# PRIVATE UNAIDED CLASS 12(COMBINED) ===========================================

private_unaided_c12_final <- bind_rows(private_unaided_c12_india, private_unaided_summary_by_state_c12) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(private_unaided_c12_final)

# OVERALL CLASS 12(INDIA) ======================================================

overall_c12_india <- State_Students %>%
  filter(
    school_category %in% c(3,5,10,11)
  ) %>%
  mutate(
    state = "INDIA",
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
    )
  ) %>%
  group_by(state, school_category_label) %>%
  summarise(
    total_schools            = n(),
    total_enrollment_class_12 = sum(class_12, na.rm = TRUE),
    total_enrollment         = sum(total_students, na.rm = TRUE),
    enrolled_boys            = sum(total_boys, na.rm = TRUE),
    enrolled_girls           = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  )
View(overall_c12_india)

# OVERALL CLASS 12(STATE) ==============================================================

State_students_overall_c12 <- State_Students %>%
  select(pseudocode,state,school_category,managment,c12_b,c12_g,class_12,total_boys,
         total_girls,total_students)

overall_summary_by_state_c12 <- State_students_overall_c12 %>%
  group_by(state, school_category) %>%
  summarise(
    total_schools               = n(),                           
    total_enrollment_class_12   = sum(class_12, na.rm = TRUE),   
    total_enrollment            = sum(total_students, na.rm = TRUE),
    enrolled_boys               = sum(total_boys, na.rm = TRUE),
    enrolled_girls              = sum(total_girls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(school_category %in% c(3,5,10,11))%>%
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
    )
  ) %>%
  complete(
    state,
    nesting(school_category, school_category_label),   # To keep all the sch_category even if there are no sch 
    fill = list(
      total_schools            = 0L,
      total_enrollment_class_10 = 0L,
      total_enrollment         = 0L,
      enrolled_boys            = 0L,
      enrolled_girls           = 0L
    )
  ) %>%
  arrange(state, school_category) %>%
  select(state, school_category_label, total_schools, 
         total_enrollment_class_12, total_enrollment, 
         enrolled_boys, enrolled_girls)

View(overall_summary_by_state_c12)

# OVERALL CLASS 12(COMBINED) ===================================================

overall_c12_final <- bind_rows(overall_c12_india, overall_summary_by_state_c12) %>%
  arrange(
    if_else(state == "INDIA", 0L, 1L),   # 0 before 1 → INDIA first
    state,
    school_category_label
  )

View(overall_c12_final)

# CLASS ========================================================================

Class1 <- list(
  "Class1_Gov" = gov_summary_by_state_c1,
  "Class1_Gov_Aided" = gov_aided_summary_by_state_c1,
  "Class1_Private_Unaided" = private_unaided_summary_by_state_c1,
  "Class1_Overall" = overall_summary_by_state_c1
)

Class5 <- list(
  "Class5_Gov" = gov_summary_by_state_c5,
  "Class5_Gov_Aided" = gov_aided_summary_by_state_c5,
  "Class5_Private_Unaided" = private_unaided_summary_by_state_c5,
  "Class5_Overall" = overall_summary_by_state_c5
)

Class8 <- list(
  "Class8_Gov" = gov_summary_by_state_c8,
  "Class8_Gov_Aided" = gov_aided_summary_by_state_c8,
  "Class8_Private_Unaided" = private_unaided_summary_by_state_c8,
  "Class8_Overall" = overall_summary_by_state_c8
)

Class10 <- list(
  "Class10_Gov" = gov_summary_by_state_c10,
  "Class10_Gov_Aided" = gov_aided_summary_by_state_c10,
  "Class10_Private_Unaided" = private_unaided_summary_by_state_c10,
  "Class10_Overall" = overall_summary_by_state_c10
)

Class12 <- list(
  "Class12_Gov" = gov_summary_by_state_c12,
  "Class12_Gov_Aided" = gov_aided_summary_by_state_c12,
  "Class12_Private_Unaided" = private_unaided_summary_by_state_c12,
  "Class12_Overall" = overall_summary_by_state_c12
)

Combined_class1 <- list(
  "Combined_class1_Gov" = gov_c1_final,
  "Combined_class1_Gov_Aided" = gov_aided_c1_final,
  "Combined_class1_Private_Unaided" = private_unaided_c1_final,
  "Combined_class1_Overall" = overall_c1_final
)

Combined_class5 <- list(
  "Combined_class5_Gov" = gov_c5_final,
  "Combined_class5_Gov_Aided" = gov_aided_c5_final,
  "Combined_class5_Private_Unaided" = private_unaided_c5_final,
  "Combined_class5_Overall" = overall_c5_final
)

Combined_class8 <- list(
  "Combined_class8_Gov" = gov_c8_final,
  "Combined_class8_Gov_Aided" = gov_aided_c8_final,
  "Combined_class8_Private_Unaided" = private_unaided_c8_final,
  "Combined_class8_Overall" = overall_c8_final
)

Combined_class10 <- list(
  "Combined_class10_Gov" = gov_c10_final,
  "Combined_class10_Gov_Aided" = gov_aided_c10_final,
  "Combined_class10_Pri_Unaided" = private_unaided_c10_final,
  "Combined_class10_Overall" = overall_c10_final
)

Combined_class12 <- list(
  "Combined_class12_Gov" = gov_c12_final,
  "Combined_class12_Gov_Aided" = gov_aided_c12_final,
  "Combined_class12_Pri_Unaided" = private_unaided_c12_final,
  "Combined_class12_Overall" = overall_c12_final
)


setwd("D:/UDISE+/R")
write_xlsx(Class1, "Class_1_enrollment_24_25.xlsx")
write_xlsx(Class5, "Class_5_enrollment_24_25.xlsx")
write_xlsx(Class8, "Class_8_enrollment_24_25.xlsx")
write_xlsx(Class10, "Class_10_enrollment_24_25.xlsx")
write_xlsx(Class12, "Class_12_enrollment_24_25.xlsx")

write_xlsx(Combined_class1, "Combined_Class_1_enrollment_24_25.xlsx")
write_xlsx(Combined_class5, "Combined_Class_5_enrollment_24_25.xlsx")
write_xlsx(Combined_class8, "Combined_Class_8_enrollment_24_25.xlsx")
write_xlsx(Combined_class10, "Combined_Class_10_enrollment_24_25.xlsx")
write_xlsx(Combined_class12, "Combined_Class_12_enrollment_24_25.xlsx")
