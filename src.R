<<<<<<< HEAD
# selected_configuration <- "macBook"
selected_configuration <- "Sherlock"
=======
selected_configuration <- "macBook"
# selected_configuration <- "Sherlock"
>>>>>>> e626a54d2a88b30b67d7b3fd948c3530d9306a3e

if(selected_configuration == "macBook"){
  config_file_path <- "~/R Language Default Dir/Github-projects/multiview-oneclass/config.yml"
  setwd(config::get("directory", 
                    file = config_file_path, 
                    config = "default"))
  source(config::get("source_scripts", 
                     file = config_file_path, 
                     config = "default"))
  use_condaenv(config::get("conda_env", 
                           file = config_file_path,
                           config = "default"))
  loaded_config_type <- "default"
}


if(selected_configuration == "Sherlock"){
  config_file_path <- "~/GitHub_projects/multiview-oneclass/config.yml"
  setwd(config::get("directory", 
                    file = config_file_path,
                    config = "production"))
  source(config::get("source_scripts", 
                     file = config_file_path,
                     config = "production"))
  loaded_config_type <- "production"
}

