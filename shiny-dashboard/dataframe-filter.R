dataframe_filter <- function(df, input_col1, input_col2, input_col3){
  # all selected
  if (is.null(input_col1) == FALSE & is.null(input_col2) == FALSE & is.null(input_col3) == FALSE){
    within(df,rm("uuid")) %>% filter(col1 == input_col1,
                  col2 == input_col2,
                  col3 == input_col3)
    # two selected
  } else if (is.null(input_col1) != FALSE & is.null(input_col2) == FALSE & is.null(input_col3) == FALSE){
    within(df,rm("uuid")) %>% filter(col2 == input_col2,
                  col3 == input_col3)
  }else if (is.null(input_col1) == FALSE & is.null(input_col2) != FALSE & is.null(input_col3) == FALSE){
    within(df,rm("uuid")) %>% filter(col1 == input_col1,
                  col3 == input_col3)
  }else if (is.null(input_col1) == FALSE & is.null(input_col2) == FALSE & is.null(input_col3) != FALSE){
    within(df,rm("uuid")) %>% filter(col1 == input_col1,
                  col2 == input_col2)
    # one selected
  }else if (is.null(input_col1) != FALSE & is.null(input_col2) != FALSE & is.null(input_col3) == FALSE){
    within(df,rm("uuid")) %>% filter(col3 == input_col3)
  }else if (is.null(input_col1) != FALSE & is.null(input_col2) == FALSE & is.null(input_col3) != FALSE){
    within(df,rm("uuid")) %>% filter(col2 == input_col2)
  }else if (is.null(input_col1) == FALSE & is.null(input_col2) != FALSE & is.null(input_col3) != FALSE){
    within(df,rm("uuid")) %>% filter(col1 == input_col1)
    # none selected
  }else{
    return( within(df,rm("uuid")) );
  }
  
}


change_df_col <- function(df, name1, name2, name3){
  names(df)[names(df) == "col1"] <- name1
  names(df)[names(df) == "col2"] <- name2
  names(df)[names(df) == "col3"] <- name3
  df
}

