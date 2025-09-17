# 
# members <- nrow(readTable("members"))
# districts <- readTable("districts")
# partners <- nrow(readTable("partners"))


homeComponent <- htmlTemplate("views/index.html",
                              members = "",
                              districts = "",
                              partners = "",
                              whereWeWorkMap = ""
                              )

initaiteWhereWeWorkMap <- function(output , input){
  
  
}