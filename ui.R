
# List all .R files in the directory
files <- list.files(pattern = "\\.R$",path = "components/",full.names = TRUE)

# Source each file
for (file in files) {
  source(file)
}


ui <- tagList(
  useShinyjs(),
  uiOutput("headerLayout"),
  tags$script("
    
   
  $(document).on('shiny:connected',function(){
    handlerHashChange()
    window.onhashchange = function() {
      handlerHashChange()
    }
    
     function handlerHashChange(){
      
      var currentHash = window.location.hash.substring(1);
      console.log(currentHash)
      Shiny.onInputChange('currentHash', currentHash);
    }
    
  });
"),
 
  uiOutput("mainOutputLayout") %>% withSpinner(type = 8),
  
 uiOutput("footerLayout")
)


