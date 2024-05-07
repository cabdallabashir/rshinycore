
# List all .R files in the directory
files <- list.files(pattern = "\\.R$",path = "components/",full.names = TRUE)

# Source each file
for (file in files) {
  source(file)
}


ui <- tagList(
  useShinyjs(),
  htmlTemplate("views/layout/header.html"),
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
  uiOutput("mainOutputLayout") %>% withSpinner(),
  htmlTemplate("views/layout/footer.html")
)


