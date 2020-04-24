 #Load packages
install.packages("rjson")
install.packages("RCurl")
install.packages("httr")
install.packages("reshape2")
#Register application (to be done only once)

library(rjson)
library(RCurl)
library(httr)
library(reshape2)

## Install from github if you don't have the package "rCharts".
if (FALSE){
  require(devtools)
  install_github('rCharts', 'ramnathv')
}

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)



#Request access to the user
app_name <- "secun_R_scan"
client_id <- "f7dc4b29ed534c66ac86d398b10b2835"
client_secret <- "06715a409a924d20a75600dc3b87b815"
scope <- "public_content" # no need to change this

#Provide access points
instagram <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)

#Let's authenticate'
ig_oauth <- oauth2.0_token(instagram, 
                           myapp,scope="basic",  
                           type = "application/x-www-form-urlencoded",
                           cache=FALSE)

tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]


## Specify the user name you want to explore.
## This is my account. Also check out my favotite cat breeder's account "fjarilflickans"
username <- "secunsr" 

user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")
received_profile <- user_info$data[[1]]

## Get the recent media data (last 20 pictures) and plot the number of Likes.
## Return an error message if the user does not exist.
if(grepl(received_profile$username,username))
{
  user_id <- received_profile$id
  #Get recent media 
  media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',user_id,'/media/recent/?access_token=',token,sep="")))
  
  
  df = data.frame(no = 1:length(media$data)) 
  for(i in 1:length(media$data))
  {
    #comments
    df$comments[i] <-media$data[[i]]$comments$count
    
    #likes:
    df$likes[i] <- media$data[[i]]$likes$count
    
    #date
    df$date[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01"))
  }
  
  #Visualization -- interactive plot using rCharts
  m1 <- mPlot(x = "date", y = c("likes", "comments"), data = df, type = "Line", 
              labels = c("Number of Likes", "Number of Comments"))
  m1$set(pointSize = 1, lineWidth = 2, lineColors = c("#66C2A5", "#FC8D62"))
  
}else
{
  print("Error: User not found!")
}
