library(plyr)
library(ggplot2)
library(reshape)
library(gridExtra)

Sys.setlocale(locale = "C")

readFile <- function(file) {
  balance <- read.csv(paste("input/", file, sep = ""),
                      sep = ";",
                      row.names = NULL,
                      skip = 1)
  balance <-
    balance[, c(
      "Datum.v..verrichting",
      "Beschrijving",
      "Bedrag.v.d.verrichting",
      "Rekening.tegenpartij",
      "Naam.v.d.tegenpartij..",
      "Mededeling.1..",
      "Mededeling.2..",
      "Ref..v.d.verrichting"
    )]

  colnames(balance) <-
    c("date", "type", "value", "targetaccount", "targetname", "desc1", "desc2", "ref")

  return(balance)
}

createReport <- function(file1, file2, title) {
  balance1 <- readFile(file1)
  if (is.null(file2)) {
    balance <- balance1
  } else {
    balance2 <- readFile(file2)

    balance <- rbind(balance1, balance2)
  }


  balance$date <- as.Date(balance$date, format = "%d/%m/%y")
  balance$year <- as.numeric(format(balance$date, "%Y"))
  balance$month <- as.Date(cut(balance$date, breaks = "month"))
  balance$value <- gsub(".", "", balance$value, fixed = TRUE)
  balance$value <- as.numeric(gsub(",", ".", balance$value))
  balance$type <- toupper(balance$type)
  balance$targetname <- toupper(balance$targetname)
  balance$desc <-
    paste(toupper(balance$desc1), toupper(balance$desc2), sep = "")
    balance$desc1 <- NULL
    balance$desc2 <- NULL


  # Build simple regexp strings
  # Do this for as many useful classes as you can think of

  categories <- list(
    c("exclude", "desc", "(VERVROEGDE TERUGBETALING)|(GEDEELTELIJKE AFREKENING)"),
    c("cash", "type", "GELDAFHALING|OPNAME"),
    c("retirement", "type", "(STORTING PS)"),
    c("transport", "type", "TANKEN"),
    c("transport", "desc", "GOLF|AUTO"),
    c("transport", "targetname", "OUTLET|ESSO|NMBS|SNCB|CAMBIO|OPTIMOBIL|PARKEREN|PARKING|(PARK ST-PIETERS GENT)|(PARK VRIJDAGMARKGENT)"),
    c("common", "desc", "(MAANDELIJKSE BIJDRAGE)|(EXTRA BIJDRAGE)|(EXTRA RIB)"),
    c("gsm", "targetname", "VIKINGS|MOBISTAR"),
    c("visa", "type", "VISA|(DEBET TEN VOORDELE VAN BCC)"),
    c("salary", "targetname", "GENOHM|HVW|(ABVV OOST-VLAANDEREN)|(TER KIMME)|(O.C.M.W.)"),
    c("salary", "desc", "(/B/ UITK.)"),
    c("insurance", "targetname", "CM|VMOB|KBC|PARTENA|ABVV|ACOD|(APB-GEZINNEN)"),
    c("rent-mortgage", "desc", "(HUUR EN LASTEN)|(BIJDRAGE ETEN)|(HUUR APPARTEMENT)"),
    c("energy", "targetname", "ENECO|ELEGANT|TMVW|(T.M.V.W.)|SEMINCK|IVAGO"),
    c("telecom", "targetname", "TELENET|SCARLET"),
    c("grocery", "targetname", "ALBERT|CARREFOUR|(GB PARTNER)|(GB AALST)|COLRUY|ALDI|LIDL|SPAR|DELH|(AD TERLINDEN)|EXPRESS|(STG ADYEN)|HELLOFRESH|MARKET|MATCH|RENMANS"),
    c("cloths", "targetname", "C&A|H&M|VERITAS|ZALANDO|PRONTI|(UNITED BRANDS)|(DI AALST)|TORFS|BENT|PARIS|(VERO MODA)|TAMARIS|HUNKEMOLLER|X.TENSION|PARFUM|(TAKKO FASH)|MEXX|CASSIS|(COOL CAT)|(COMAR SPORT)|SKOLL|BUFFALO|ETAM|FASHION|(LL RETAIL)|(CAMPING NV)|FONCE|(L&L)|BOSMANS|JBC|(VAN EYCK SPORT)|(PRO DUO)"),
    c("cloths", "desc", "SPRINGFIELD"),
    c("housing", "targetname", "MEDIAMARKT|(MEDIA MART)|SINT-DENIJS-W|IKEA|VDBORRE|KREFEL|BANIER|KRUIDVAT|2007|ACTION|AVA|BLOKKER|(DILLE&KAMILLE)|CASA|HEMA|INNO|ZOOMART|SELMUS|(NV SEDIA)|WEBA|MAKRO|DECATHLON|(BOL.COM)|ZEEMAN|(TOM & CO)|(SODEXO PASS BELGIUM NV)"),
    c("housing", "desc", "SLAAPKAMER"),
    c("renovation", "targetname", "GAMMA|SAFTI|(HAESELEER BVBA)|(DE RYCKE BVBA)|VERHOEVEN|EANDIS|(DE BOEVER)|MBM|(M.B.M)|HUBO"),
    c("recreation", "targetname", "STANDAAR|KINEPOLIS|PALACE|SANDERS|LEKKERBEKJE|(VAN GUYSE JURGEN)|PIZZA|DONALDS|QUICK|(NIEF PET@TJE)|ZWIJNTJE|EENWIGEN|TADEMA|HYDRA|KREAALST|(VIETNAM FLAVOUR)|GOLAZO|CHRONORACE|(GEIREGAT GWEN)|(STD BH)|BOEKENVOORDEEL"),
    c("recreation", "desc", "CADEAU|KADO|PROFICIAT|KERST|GELUK|OUDEJAAR|NIEUWJAAR|NYE|TOEKOMST|TROUW|CARNAVAL|VRIJGEZELLEN"),
    c("health", "targetname", "PHARM|FARMA|APO|(DR ROGGE)|(DR D'HOLLANDER)|(DE RYCKER)|APDELINDEBOOMAALST|(A.S.Z.)|ASZ|OLV|ZIEKENHUIS|PATHOLOGIE|(UZ GENT)|LABO|DIERENARTSPRAKT|(VAN MELCKEBEKE  AALST)")
  )

  # Add a class field to the data, default "other"
  balance$class <- "Other"
  # Apply the regexp and return their class
   for (cat in categories) {
       x <- grep(cat[2], colnames(balance))
     for (i in 1:nrow(balance)) {
       balance$class[i] <- ifelse(balance$class[i] == "Other" && grepl(cat[3], balance[i, x]), cat[1], balance$class[i])
     }
   }

  #head(balance, n = 3)

  pdf(paste("output/", title, ".pdf", sep = ""))


  balance_export <- subset(balance, grepl("Other", balance$class))
  write.table(balance, paste("output/all-", title, ".csv", sep = ""), sep = ";", dec = ",", row.names = FALSE)
  write.table(balance_export, paste("output/other-", title, ".csv", sep=""), sep = ";", dec = ",", row.names = FALSE)

  #Exclude some
  balance <- subset(balance, !grepl("exclude", balance$class))

  balance_small <- subset(balance, !grepl("Other", balance$class))
  expenses <- subset(balance, balance$value < 0)
  expenses_small <- subset(balance_small, balance_small$value < 0)

balance_monthly <-
    ddply(balance, .(month, class), summarise, cost = ifelse(class == "salary" || class == "common", abs(sum(value)), -sum(value)))
balance_monthly_small <-
    ddply(balance_small, .(month, class), summarise, cost = ifelse(class == "salary" || class == "common", abs(sum(value)), -sum(value)))
balance_monthly_mean <-
    ddply(balance_monthly, .(class), summarise, m = mean(cost))
balance_monthly_mean_small <-
    ddply(balance_monthly_small, .(class), summarise, m = mean(cost))

balance_yearly <-
    ddply(balance, .(year, class), summarise, cost = ifelse(class == "salary" || class == "common", abs(sum(value)), -sum(value)))

balance_yearly_pivot <- cast(balance_yearly, class ~ year)
balance_yearly_pivot_total <- transform(balance_yearly_pivot, total=rowSums(balance_yearly_pivot, na.rm = TRUE))

expenses_monthly <-
    ddply(expenses, .(month, class), summarise, cost = abs(sum(value)))
expenses_monthly_small <-
    ddply(expenses_small, .(month, class), summarise, cost = abs(sum(value)))
expenses_monthly_mean <-
    ddply(expenses_monthly, .(class), summarise, m = mean(cost))
expenses_monthly_mean_small <-
    ddply(expenses_monthly_small, .(class), summarise, m = mean(cost))

expenses_yearly <-
    ddply(expenses, .(year, class), summarise, cost = abs(sum(value)))
expenses_yearly_small <-
    ddply(expenses_small, .(year, class), summarise, cost = abs(sum(value)))
expenses_yearly_small_total <-
    ddply(expenses_yearly_small, .(class), summarise, cost = sum(cost), year = 'total')

    print(expenses_yearly_small_total)

  p1 <- ggplot(balance_monthly, aes(month, cost, col = class)) +
    facet_wrap(~ class, ncol = 2, scale = "free_y") +
    geom_smooth(method = "loess", se = F) + geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(x = "", y = "Monthly total (EUR)") +
    ggtitle(title)


  p2 <- ggplot(expenses_monthly_mean_small, aes(x = class, y = m)) +
    geom_bar(stat = "identity") +
    labs(y = "Average monthly expenses (EUR)", x = "") +
    ggtitle(title)


  p3 <- ggplot(expenses_monthly_mean, aes(x = class, y = m)) +
    geom_bar(stat = "identity") +
    labs(y = "Full average monthly expense (EUR)", x = "") +
    ggtitle(title)


  p4 <- ggplot(balance_yearly, aes(year, cost, col = class)) +
    facet_wrap(~ class, ncol = 2, scale = "free_y") +
    geom_smooth(method = "loess", se = F) + geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(x = "", y = "Yearly total (EUR)") +
    ggtitle(title)


  p5 <- ggplot(rbind(expenses_yearly_small, expenses_yearly_small_total), aes(x = "", y = cost, fill = class)) +
    facet_wrap(~ year, ncol = 2, scale = "fixed") +
    geom_bar(stat = "identity",
             color = 'black',
             position = position_fill()) +
    labs(x = "", y = "Yearly total expenses (EUR)") +
    coord_polar(theta = "y") +
    ggtitle(title)

    table1 <- grid.table(balance_yearly_pivot_total)

    return(list(p1, p2, p4, p5, table1))
}

createReport("bank-acount-1-total.csv",
            "bank-acount-2-total.csv",
            "Jeno")
createReport("bank-account-3.csv",
            as.null(),
            "Jeno")

