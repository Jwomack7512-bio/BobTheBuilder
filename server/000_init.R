# This script will hold any server side processes that need to be run at the 
# initiation of the program.

# Generate first compartment on document load
onStart <- observe({
  shinyjs::click("createVar_add_compartment_button")
  onStart$destroy()
})