
# selected_configuration <- "macBook"
selected_configuration <- "Sherlock"

if(selected_configuration == "macBook"){
  config_file_path <- "~/R Language Default Dir/Github-projects/multiview-oneclass/config.yml"
  setwd(config::get("directory", 
                    file = config_file_path, 
                    config = "default"))
  reticulate::use_python(python = "/Users/georgios.kaiafas/opt/anaconda2/bin/python", 
                         required = T)
  source(config::get("source_scripts", 
                     file = config_file_path, 
                     config = "default"))
  use_condaenv(config::get("conda_env", 
                           file = config_file_path,
                           config = "default"), conda = "/Users/georgios.kaiafas/opt/anaconda2/condabin/conda")
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

