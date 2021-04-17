imgGen <- function(cardname,cardtype,src,imgsize) {
  if (cardtype == "henchmen") {
    vals = filter(src$henchmen,
                  Name==cardname)
  }
  if (cardtype == "heroes") {
    vals = filter(src$heroes,
                  uni==cardname,
                  !duplicated(Split)|is.na(Split)|grepl("T",Split))
  }
  if (cardtype == "masterminds") {
    vals = filter(src$masterminds,
                  Name==cardname|MM==cardname)
  }
  if (cardtype == "villains") {
    vals = filter(src$villains,
                  Group==cardname)
  }
  if (cardtype == "schemes") {
    vals = filter(src$schemes,
                  Name==cardname)
    if (vals$Name%in%c("House of M",
                       "Secret Hydra Corruption",
                       "The Korvac Saga",
                       "Earthquake Drains the Ocean")) {
      vals = rbind(vals,vals[1,])
      vals$file[2] = vals$file2[1]
      vals$loc[2] = vals$loc2[1]
    }
  }
  if (cardtype == "none") {
    return(F)
  }
  n = dim(vals)[1]
  if (n>0) {
    vals$file = gsub(",",".",vals$file,fixed=T)
    imgcode = "<head><style>"
    vals$sz1 = 0
    vals$sz2 = 0
    for (i in 1:n) {
      if (!is.na(vals$file[i])) {
        loc = strsplit(vals$loc[i],split=" ")[[1]]
        if (length(loc)==1) {
          if (loc[1]=="1") {
            bg = 100
            mod2 = 409
          }
          if (loc[1]=="NW") {
            bg = 200
            mod2 = 409
          }
          if (loc[1]=="NE") {
            bg = 200
            mod2 = 409
            vals$sz1[i] = 100
          }
          if (loc[1]=="SW") {
            bg = 200
            mod2 = 409
            vals$sz2[i] = 100.8
          }
          if (loc[1]=="SE") {
            bg = 200
            mod2 = 409
            vals$sz1[i] = 100
            vals$sz2[i] = 100.8
          }
        }
        if (length(loc)==2) {
          sze = filter(imgsize,filename==vals$file[i])
          mod = 16.67 - 16.67*(1-sze$rel[1])/7
          mod2 = 397 + 397*(1-sze$rel[1])
          vals$sz1[i] = (as.numeric(loc[1])-1)*11.11
          vals$sz2[i] = (as.numeric(loc[2])-1)*mod
          bg = 1000
        }
        if (!is.na(vals$file[i])) {
          line = paste0("#home",
                        i,
                        " {width: 104.9%; height: ",
                        mod2,
                        "px; position:relative; background: url(img/",
                        vals$file[i],
                        ") ",
                        vals$sz1[i],
                        "% ",
                        vals$sz2[i],
                        "%; background-size:",
                        bg,
                        "%;}")
          imgcode = paste0(imgcode,
                           line)
        }
      }
    }
    imgcode = paste0(imgcode,
                     "</style></head><body>")
    float = "left"
    ct = ""
    for (i in 1:n) {
      if (!is.na(vals$file[i])) {
        if (i%%2==0) {
          float = "right"
        } else {
          float = "left"
        }
        ct = ""
        if (cardtype == "Villains"|
            cardtype == "Heroes") {
          ct = vals$Ct[i]
        }
        imgcode = paste0(imgcode,
                         "<div style=\"width:49%;float:",
                         float,
                         ";\"><img id=\"home",
                         i,
                         "\" src=\"empty.png\" title=\"",
                         ct,
                         " copies\"></div><div style=\"width:2%;\"> </div>")
      }
    }
    imgcode = paste0(imgcode,
                     "</body>")
    return(HTML(imgcode))
  }
}