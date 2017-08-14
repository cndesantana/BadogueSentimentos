fromCommentsToUnigram <- function(allmessages){
   
   spSamp<- unlist(strsplit(allmessages, split=", "))
#   nonAscIDX<- grep("spSamp", iconv(spSamp, "latin1", "ASCII", sub="spSamp"))
#   ascVec<- spSamp[ - nonAscIDX]
   ascVec<- spSamp;
   ascSamp<- paste(ascVec, collapse = ", ")
   clnSamp<- gsub('[[:digit:]]+', '', ascSamp)
   clnSamp<- gsub('[[:punct:]]+', '', clnSamp)
   clnSamp<- gsub("http[[:alnum:]]*", "", clnSamp)
   clnSamp<- gsub("([[:alpha:]])\1+", "", clnSamp)
   SampCrps<- Corpus(VectorSource(clnSamp))
   SampCrps<- tm_map(SampCrps, tolower)
   SampCrps<- tm_map(SampCrps, removePunctuation)
   SampCrps<- tm_map(SampCrps, removeNumbers)
   urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
   SampCrps<-tm_map(SampCrps, urlPat)
   emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
   SampCrps<- tm_map(SampCrps, emlPat)
   tt<-function(x) gsub("RT |via", "", x)
   SampCrps<- tm_map(SampCrps, tt)
   tun<-function(x) gsub("[@][a - zA - Z0 - 9_]{1,15}", "", x)
   SampCrps<- tm_map(SampCrps, tun)
   SampCrps<-tm_map(SampCrps, removeWords, stopwords("portuguese"))
   myCrps<- txt.to.words(SampCrps)
   tblUniGrm<-data.frame(table(make.ngrams(myCrps, ngram.size = 1)))
   stblUnigrm<-tblUniGrm[order(tblUniGrm$Freq, decreasing = TRUE),]
   
   return(stblUnigrm)
}
