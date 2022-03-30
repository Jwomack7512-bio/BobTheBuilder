# need to go into css file and change .navbar-white{background-color:} to the proper value
# as fresh doesn't seem to change it.

mytheme <- create_theme(
  bs4dash_layout(
    main_bg = "#222222"
  ),
  
  bs4dash_sidebar_light(
    #color of sidebar background
    bg = "#375a7f",  
    color = "#bec5cb",
    active_color = "#FFF",
    #color of submenu background
    submenu_bg = "#375a7f",
    submenu_color = "#FFF"
  ),
  #this seems to change the background color of picker inputs
  #bs4dash_status(light = "#005475", primary = "#00755c"), 
  
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF",
    text_light = "#272c30"
  ),
  bs4dash_status(
    primary = "#0059ff"
  ),
  bs4dash_color(gray_900 = "#FFF", white = "#272c30"),
  output_file = "www/test1.css"
)


create_theme(
  bs4dash_vars(
    navbar_light_color = "#bec5cb",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 100, 
    text_dark = "#FFF", 
    text_light = "#272c30"
  ),
  bs4dash_layout(main_bg = "#353c42"),
  bs4dash_sidebar_dark(
    bg = "#272c30", 
    color = "#bec5cb", 
    hover_color = "#FFF",
    submenu_bg = "#272c30", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(dark = "#272c30"),
  bs4dash_color(gray_900 = "#FFF", white = "#272c30"),
  output_file = "www/test2.css"
)

