
retrieve_label <- function(vec, var_name,
                           cohorts_labels = NULL, 
                           domain_name = NULL,
                           overwrite_var_label = FALSE,
                           ...){
  existing_var_label <- attr(vec,"label",exact=TRUE)
  
  if(overwrite_var_label){
    if(!is.null(existing_var_label)){
      attr(vec,"label") <- NULL
      # print(attr(vec,"label",exact=TRUE))
    }
  }
  
  existing_var_label <- attr(vec,"label",exact=TRUE)
  
  
  if(!is.null(existing_var_label)){
    print(paste0("Variable ",var_name,": Using default label (class 'labelled')"))
    return(existing_var_label)
  }
  
  # Read nfhs labels dataframe
  if(is.null(cohorts_labels)){
    cohorts_labels_file <- "B9 Variable List.xlsx"
    print(paste0("Using default if no existing variable labels: ",cohorts_labels_file))
    
    cohorts_labels <- readxl::read_excel(paste0("C:/Cloud/OneDrive - Emory University/Box/COHORTS SES Harmonization/brazil 1993/",
                                             "/",cohorts_labels_file),
                                      sheet="pcall") 
  }
  
  
  
  if(is.null(existing_var_label)){
    # # Need to build in error catch: Possible that domain might not be specified
    # domain_labels <- cohorts_labels %>% 
    #   dplyr::filter(domain %in% domain_name) 
    
    
    if(!is.null(domain_name)){
      var_label <- cohorts_labels %>% 
        dplyr::filter(domain == domain_name,variable == var_name) %>% 
        dplyr::select(label) %>% 
        pull()
      
      
      if(length(var_label)==0){
        print(paste0("Variable ",var_name,": No label found in domain- ",domain_name))
        return(NA)
      }
      
    }
    
    if(is.null(domain_name)){
      var_label <- cohorts_labels %>% 
        dplyr::filter(variable == var_name) %>% 
        dplyr::select(label) %>% 
        pull()
      
      if(is.null(var_label)){
        print(paste0("Variable ",var_name,": No label found"))
        return(NA)
      }
      
      
    }
    
    if(length(var_label)>1){
      print(paste0("Variable name ",var_name,
                   " has ",length(var_label),
                   " occurences. Returning 1st occurence"))
    }
    
    if(length(var_label)==1){
      print(paste0("Variable name ",var_name,
                   ": ",var_label))
    }
    
    return(var_label[1])
    
    
  }
}



label_variables <- function(df,
                            domain_name = NULL,
                            cohorts_labels = NULL, 
                            overwrite_var_label = FALSE,...){
  # Add variable labels if not already present
  df_labelled <- data.frame()
  df_labelled <- map_dfc(.x=colnames(df),
                         .f = function(x){
                           labelled::var_label(df[[x]]) <- retrieve_label(vec=df[[x]],
                                                                          var_name = x,
                                                                          domain_name=domain_name,
                                                                          cohorts_labels=cohorts_labels,
                                                                          overwrite_var_label=overwrite_var_label);
                           return(df[[x]])
                         }
  ) 
  names(df_labelled) <- names(df)
  
  return(df_labelled)
  
}


label_factor <- function(vec,
                         var_label = NULL,
                         levels_vec=NULL,
                         labels_vec=NULL,
                         type="factor"){
  
  var_label <- var_label
  
  existing_var_label <- if(!is.null(attr(vec,"label"))){attr(vec,"label")} else{NULL}
  
  vec_stata <- vec
  
  if(!is.null(levels_vec) & !is.null(labels_vec)){
    
    if(type == "ordered"){
      vec_stata <- ordered(vec,levels=levels_vec,
                           labels=labels_vec)
      # Hmisc::label(vec_factor) <- var_label
    }
    if(type =="factor"){
      vec_stata <- factor(vec,levels=levels_vec,
                          labels=labels_vec)
      # Hmisc::label(vec_factor) <- var_label
    }
    
    
  }
  
  if(is.null(var_label)) {var_label <- existing_var_label}
  
  labelled::var_label(vec_stata) <- var_label
  return(vec_stata)
}

generate_labels <- function(vec){
  
  # existing_var_label <- if(!is.null(attr(vec,"label"))){attr(vec,"label")} else{NULL}
  existing_var_label <- labelled::var_label(vec)
  existing_value_labels <- if(is.factor(vec) | is.ordered(vec)){unlist(levels(vec))} else{NULL}
  
  labels_vec <- paste0(as.character(existing_var_label),existing_value_labels)
  return(labels_vec)
  
  
  
}



sum_factors <- function(df,na.rm=TRUE){
  
  
}