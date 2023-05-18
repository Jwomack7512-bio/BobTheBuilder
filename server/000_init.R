# This script will hold any server side processes that need to be run at the 
# initiation of the program.

# Generate first compartment on document load
onStart <- observe({
  shinyjs::click("createVar_add_compartment_button")
  
  # Grab file names from base_models
  base.models <- list.files(file.path("base_models"))
  
  updateSelectInput(
    session = session,
    inputId = "SI_repos_base_choices",
    choices = base.models
  )
  
  # Remove this observer after first iteration
  onStart$destroy()
})