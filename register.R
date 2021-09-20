register <- list.files('register',pattern='xls',full.names = TRUE)
reg <- readxl::read_excel(register) %>% data.frame()
names(reg)[9:10] <- c('Status','Reason')
reg <- reg %>% select(Email, Name, Input.session.date, Status, Reason) %>%
  filter(!is.na(reg$Input.session.date))
alldf <- data.frame(reg %>% select(Name, Email))
alldf <- alldf %>% filter(!duplicated(alldf))
alldf <- lapply(unique(alldf$Name), FUN = function(named){
  temp <- alldf %>% filter(Name == named)
  if(nrow(temp)>1){
    temp <- temp %>% filter(grepl('student',temp$Email))
    if(nrow(temp)==0){
      temp <- alldf %>% filter(Name == named) %>% head(1)
    }
  }
  temp
}) %>% bind_rows()

register_all <- lapply(sort(unique(reg$Input.session.date)), FUN = function(dated){
  tmp <- reg %>% filter(Input.session.date == dated)
  Statuses <- paste0(tmp$Status,
                     ifelse(!is.na(tmp$Reason),paste0(', ',tmp$Reason,''),''))
  mytmp <- data.frame(Name = tmp$Name,
                      Status = Statuses)
  mytmp <- merge(alldf, mytmp, by.x = 'Name', by.y = 'Name', all.x = TRUE)
  mytmp <- mytmp %>% filter(!duplicated(mytmp))
  mytmp <- mytmp %>% arrange(Name)
  mytmp <- mytmp %>% select(3)
  names(mytmp) <- as.character(dated)
  mytmp
}) %>% bind_cols()

register_all <- bind_cols(alldf %>% arrange(Name), register_all)
write.csv(register_all, 'register/register_all.csv',row.names = FALSE)
