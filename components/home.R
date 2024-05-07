
members <- nrow(readTable("members"))
districts <- readTable("districts")
partners <- nrow(readTable("partners"))


homeComponent <- htmlTemplate("views/index.html",
                              members = members,
                              districts = nrow(districts),
                              partners = partners,
                              whereWeWorkMap = leafletOutput("whereWeWorkMAp")
                              )

initaiteWhereWeWorkMap <- function(output , input){
  
  
}