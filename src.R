# selected_configuration <- "macBook"

if(selected_configuration == "macBook"){
  config <- config::get(file = "~/R Language Default Dir/Github-projects/multiview-oneclass/config.yml")
  setwd(config::get("directory"))
  source(config::get("source_scripts"))
  use_condaenv(config::get("conda_env"))
  }



