utils::globalVariables(c(
  "Provider", "Completed", "Caseload", "Mode", "Pre_Post",
  "Participant_ID", "Service", "Score", "Difference",
  "Group_freq", "Online_freq", "Rural_freq", "Has_Multiple_Scores",
  "Pre_Date", "Post_Date", "Provider.x", "Provider.y",
  "Time_Passed_Days", "Differences_Available", "Median_Pre_Score",
  "Median_Post_Score", "Median_Difference_Score",
  "Median_Time_Passed_Days", "Final_Employment", "Age_Group",
  "Age_At_Application", "E7_Application_Date_911",
  "E1_Year_911", "E2_Quarter_911", "Occurrences_Per_Quarter",
  "Overall_Quarter", "Enroll_Length", "Max_Overall_Quarter",
  "Min_Overall_Quarter", "Enroll_Length_Grp", "Primary_Impairment_Group",
  "State_Abbrev", "State", "Has_Race", "..required_cols_pre",
  "..response_col",
  "Birth_Year",
  "Has_App_Date",
  "Income_Struggle",
  "Cultural_Struggle",
  "Support_Struggle",
  "Housing_Struggle",
  "Facing_Struggle",
  "NonMissingCount",
  "AppDateRank",
  "Median_Age_Diff_TRT"
))

# function for some reason not recognized as from data.table during checks
utils::globalVariables(c(".", "state.abb", "state.name", "count"))
