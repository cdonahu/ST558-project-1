base <- "https://pokeapi.co/api/v2/pokemon/"

getAll <- function(url){
  out <- NULL
  while (!is.null(url)){
    
    request <- GET(url)
    contentPokemon <- content(request)
    out <- c(out, contentPokemon$results)
    url <- contentPokemon$`next`
  }
  out
}
pokemon <- getAll("base")
str(pokemon[[1]])

lookup <- function(url){
  all <- getAll(url)
  
  url <- all %>% map_chr("url")
  name <- all %>% map_chr(name)
  #name[name == "unknown"] <- NA
  
  set_names(name)
}

nameList <- lookup(base)

df <- data.frame()

for (name in nameList){
  call1 <- paste(base, name, sep = "")
  pokemon <- GET(call1) %>% content()
  #name <- pokemon$name
  df$name <- name
}
  
base <- "https://pokeapi.co/api/v2/pokemon/"
name <- "pikachu"
call1 <- paste(base, name, sep = "")
pokemon <- GET(call1) %>% content()
pokemon$name
