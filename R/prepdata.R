library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")


resubmission_path <- "/home/j/Project/IRH/DAH/RESEARCH/INTEGRATED DATABASES/DATA/FGH_2021/regional_data"
gdp_path <- "/home/j/Project/IRH/LDI_PPP/LDI/output_data/LDIpc_for_uploading_20220713.csv"

# DAH db
# problem - if we are looking at src->dest, do we want to "elim_ch" or keep them all?
#   since we are ignoring channel we might want to keep them all?
#  - yes, we do: elim_ch and elim_donor == 0 so that we have unique flows.
#
isos <- list(
    panel = c("USA", "GBR", "DEU", "FRA", "CAN", "AUS", "JPN", "NOR", "ESP",
              "NLD", "AUT", "BEL", "DNK", "FIN", "GRC", "IRL", "ITA", "KOR",
              "LUX", "NZL", "PRT", "SWE", "CHE", "CHN"),
    # other potential dac
    other_dac = c("CZE", "HUN", "ISL", "POL", "SVK", "SVN"),
    non_oecd_dac = c("OMN", "ZAF", "SAU", "RUS", "QAT", "MCO", "IND"),
    # unallocable
    qza = c("DN","EN","ERE", "ERK" ,"ERQ" ,"ERR" ,"ERS", "ERT","ERU","GA","MA",
            "TH","WA","ZA")
)

fun.checkna <- function(x, fun) {
    if (all(is.na(x))) {
        return(NA_real_)
    }
    return(fun(x, na.rm = TRUE))
}


.clean_dah_adbpdb <- function(datapath) {
    dah <- fread(datapath)

    #----double-counting and in-kind costs----
    dah[INKIND != 0,
        `:=` (ISO3_RC = "INKIND", INC_GROUP = "INKIND", gbd_region_name = "INKIND",
              gbd_superregion_name = "INKIND", WB_REGION = "INKIND",
              WB_REGIONCODE = "INKIND", RECIPIENT_COUNTRY = "INKIND")]

    #----funding source----
    dah[, source := INCOME_SECTOR]
    dah[DONOR_NAME == 'BMGF',
        source := "BMGF"]
    # dah[INCOME_SECTOR == "PUBLIC",
    #     source := ISO_CODE]
    dah[INCOME_SECTOR == "PUBLIC" & ISO_CODE %in% isos$panel,
        source := ISO_CODE]
    dah[INCOME_SECTOR == "PUBLIC" & ISO_CODE %in% isos$other_dac,
        source := "OTHERDAC"]
    dah[INCOME_SECTOR == "PUBLIC" & ISO_CODE %in% isos$non_oecd_dac,
       source := "NONOECDDAC"]
    dah[source == "PUBLIC",
        source := "OTHERPUB"]

    #----funding channel----
    dah[CHANNEL == "INTL_NGO", CHANNEL := "INTLNGO"]
    dah[CHANNEL == "NIH", CHANNEL := "BIL_USA"]

    #----funding recipient----
    dah[RECIPIENT_COUNTRY == "" & ISO3_RC %in% isos$qza, ISO3_RC := 'QZA']

    names(dah) <- tolower(names(dah))
    return(dah)
}

.filter_dah <- function(dah) {
    dah <- dah[elim_ch == 0 & elim_donor == 0]
    dah <- dah[source %in% isos$panel]
    # G (global), GRC (Greece, high-income), QZA (unalloc), QMA (region)
    dah <- dah[! iso3_rc %in% c("G", "WLD", "GRC", "QZA", "QMA", "INKIND", "")]
    dah.keep <- dah[, .(year,
                        source,
                        donor_name,
                        data_source = gov,
                        channel,
                        dest_iso = iso3_rc,
                        dest_name = recipient_country,
                        inkind,
                        dah_21)]
    return(dah.keep)
}


.gdp <- function(dah) {
    gdp <- fread(gdp_path,
                 select = list(integer=c("location_id"),
                               numeric=c("year_id", "mean"))
                 )
    locs <- get_location_metadata(location_set_id = 22, release_id = 9)
    locs <- unique(locs[, .(location_id, ihme_loc_id)])
    gdp <- merge(gdp, locs, by = "location_id", all.x = TRUE)
    gdp[, location_id := NULL]
    setnames(gdp, c("year_id", "ihme_loc_id"), c("year", "iso"))
 
    dah <- merge(dah, gdp,
                 by.x = c("source", "year"), by.y = c("iso", "year"),
                 all.x = TRUE)
    setnames(dah, "mean", "src_gdp")
    dah <- merge(dah, gdp,
                 by.x = c("dest_iso", "year"), by.y = c("iso", "year"),
                 all.x = TRUE)
    setnames(dah, "mean", "dst_gdp")
    return(dah)
}

# TODO
.colony <- function(dah) {
    # should also id if dst is (A) src's former colony or (B) anyone's former colony
    colonizer <- data.frame(
        source = c("AUT", "BEL", "DEU", "DNK", "ESP", "FRA", "GBR", "ITA", "NLD", "NOR", "PRT", "SWE"),
        colonizer = 1L
    )
    # dah <- merge(dah, colonizer, by = "source", all.x = TRUE)
    # dah[is.na(colonizer), colonizer := 0L]
}

.add_covars <- function(dah) {
    dah <- .gdp(dah)
    # dah <- .colony(dah) 
    return(dah)
}

load_dahnetwork <- function(path, rebuild = TRUE) {
    rawdir <- resubmission_path
    rawname <- "DAH_ADB_PDB_1990_2021.csv"
    if (rebuild || !file.exists(path)) {
        message("Building DAH Network...")
        dah <- .clean_dah_adbpdb(file.path(rawdir, rawname))
        dah <- .filter_dah(dah)
        dah <- dah[,.(dah_21 = fun.checkna(dah_21, sum)),
                   by = .(year, source, channel, dest_iso, dest_name)]
        setcolorder(dah, c("year", "source", "channel",
                           "dest_iso", "dest_name", "dah_21"))
        dah <- .add_covars(dah)
        data.table::fwrite(dah, path)
        message(" *** Saved to: ", path)
    } else {
        message("Loading DAH Network...")
        dah <- data.table::fread(path)
    }
    return(dah)
}

