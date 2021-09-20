library(dplyr)
rm(list=ls())
allSKAs <- list.files(pattern = 'xlsx')
#unlink(allSKAs)
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

allSKAsummary <- lapply(allSKAs, FUN = function(thisSKA){
  mySKA <- read_excel_allsheets(thisSKA)
  mySKA <- lapply(1:length(mySKA), FUN = function(j){
    sheet <- mySKA[[j]]
    lev <- names(mySKA)[j]
    if((ncol(sheet)>=4)|(grepl('A level',lev))){
      tmp <- data.frame(area = sheet[,1],
                        knowledge = sheet[,3],
                        pck  = sheet[,4])
      headNum <- which(is.na(tmp[,2]))
      headers <- tmp[is.na(tmp[,2]),1]
      tmp$header <- headers[1]
      for(i in 2:length(headers)){
        tmp$header[headNum[i]:nrow(tmp)] <- headers[i]
      }
      tmp <- tmp[-headNum,]
      lev <- names(mySKA)[j]
      tmp$level <- lev
      subj <- 'WS'
      if(grepl('Bio',lev)) subj <- 'Bio'
      if(grepl('Phys',lev)) subj <- 'Phys'
      if(grepl('Chem',lev)) subj <- 'Chem'
      tmp$subject <- subj
    } else {
      tmp <- NULL
    }
    tmp
  }) %>% bind_rows()
  
  mySKAdf <- lapply(unique(mySKA$header), FUN = function(lev){
    temp <- mySKA %>% filter(header == lev)
    data.frame(header = lev,
               subject = unique(temp$subject),
               level = unique(temp$level),
               score = mean(as.numeric(temp$knowledge)))
               #score = mean(as.numeric(c(temp$knowledge, temp$pck))))
  }) %>% bind_rows()
  mySKAdf <- mySKAdf[!grepl('A level',mySKAdf$level),]
  mySKAdf$name <- tools::file_path_sans_ext(thisSKA)
  mySKAdf
})
allSKAdf <- bind_rows(allSKAsummary)

experts <- lapply(unique(allSKAdf$header), FUN = function(category){
  tmp <- allSKAdf %>% filter(header == category)
  helpers <- tmp %>% filter(score == max(tmp$score)) %>%
    pull(name) %>% paste0(collapse = ', ')
  data.frame(Header = unique(tmp$header),
             Subject = unique(tmp$subject),
             Level = unique(tmp$level),
             Median_Score = median(tmp$score),
             Mean_Score = mean(tmp$score),
             Expert = helpers)
}) %>% bind_rows()
write.csv(experts, 'Experts.csv')

helperdf <- lapply(allSKAsummary, FUN = function(personhere){
  personstrug <- personhere %>% arrange(score) %>% head(30)
  helperfolk <- lapply(personstrug$header, FUN = function(category){
    helpers <- experts %>% filter(Header == category) %>% pull(Expert)
  }) %>% unlist()
  personstrug$helperfolk <- helperfolk
  write.csv(personstrug, paste0(unique(personstrug$name),'.csv'))
  personstrug <- personstrug %>% select(-score)
  personstrug
}) %>% bind_rows()
