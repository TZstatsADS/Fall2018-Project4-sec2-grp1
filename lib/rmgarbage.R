ifCleanToken <- function(cur_token){
  now <- 1
  if_clean <- TRUE
  
  ## in order to accelerate the computation, conduct ealy stopping
  rule_list <- c("str_count(cur_token, pattern = '[A-Za-z0-9]') <= 0.5*nchar(cur_token)", # If the number of punctuation characters in a string is greater than the number of alphanumeric characters, it is garbage
                 "length(unique(strsplit(gsub('[A-Za-z0-9]','',substr(cur_token, 2, nchar(cur_token)-1)),'')[[1]]))>1", #Ignoring the first and last characters in a string, if there are two or more different punctuation characters in thestring, it is garbage
                 "nchar(cur_token)>20", #A string composed of more than 20 symbols is garbage 
                 "str_detect(cur_token,'([a-z\\d]|[A-Z\\d])\\1{3,}')",#Three or more identical characters in a string
                 "str_count(cur_token,pattern='[A-Z]')>str_count(cur_token,pattern='[a-z]') & str_count(cur_token,pattern='[A-Z]')<nchar(cur_token)", #Uppcase number larger than lowercase number
                 "if(str_count(cur_token,'[a-zA-Z]')==nchar(cur_token)){nchar(cur_token)-str_count(cur_token,'[aeyiuoAEYIUO]')>=8*str_count(cur_token,'[aeyiuoAEYIUO]')}", #consonants greater than 8 times than vowels
                 "str_detect(cur_token,'[aeiou]{4,}') | str_detect(cur_token,'[^aeiou]{5,}')",  #four or more consecutive vowels or five or more consecutive consonants.
                 "str_detect(substr(cur_token,1,1),'[a-z]') & str_detect(substr(cur_token,nchar(cur_token),nchar(cur_token)),'[a-z]'){str_detect(substr(cur_token,2,nchar(cur_token)),'[A-Z]')}")#If first and last lowercase, detect other character uppercase or not.
  while((if_clean == TRUE)&now<=length(rule_list)){
    if(eval(parse(text = rule_list[now]))){
      if_clean <- FALSE
    }
    now <- now + 1
  }
  return(if_clean)
}




