#regressions#####

library(openxlsx)
file_path <- "04_Output/RC_by_well.xlsx"
sheet_names <- excel_sheets(file_path)
RC_df <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
}) %>%
  bind_rows()

cols <- c('DOC_flux','DIC_flux','CO2_flux','CH4_flux','qL','WT_elevations','ID.Well')
unique_sites <- unique(RC_df$ID.Well[!is.na(RC_df$ID.Well)])

RC <- setNames(
  lapply(unique_sites, function(site_id) {
    df_subset <- RC_df %>%
      filter(ID.Well == site_id) %>%
      select(all_of(cols))
    return(df_subset)
  }),
  unique_sites
)

col_names<- c("ID", "pvalue", "slope", "r2", "type")


DOC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DOC_flux", "WT_elevations")])) > 1
  DOC.elevation.p <- DOC.elevation.slope <- DOC.elevation.r2 <- NA

  if (valid_elev) {
    DOC.elevation <- lm(DOC_flux ~ WT_elevations, data = df)
    DOC.elevation.cf <- summary(DOC.elevation)
    DOC.elevation.p <- DOC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DOC.elevation.slope <- DOC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DOC.elevation.r2 <- DOC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DOC.elevation.p = as.numeric(DOC.elevation.p),
    DOC.elevation.slope = as.numeric(DOC.elevation.slope),
    DOC.elevation.r2 = as.numeric(DOC.elevation.r2)
  )
})
DOC_table <- bind_rows(DOC_relationships, .id = "ID")%>%mutate(type='DOC')
colnames(DOC_table)<-col_names

DIC_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("DIC_flux", "WT_elevations")])) > 1

  # Initialize with NA
  DIC.elevation.p <- DIC.elevation.slope <- DIC.elevation.r2 <- NA
  DIC.qL.p <- DIC.qL.slope <- DIC.qL.r2 <- NA

  if (valid_elev) {
    DIC.elevation <- lm(DIC_flux ~ WT_elevations, data = df)
    DIC.elevation.cf <- summary(DIC.elevation)
    DIC.elevation.p <- DIC.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    DIC.elevation.slope <- DIC.elevation.cf$coefficients["WT_elevations", "Estimate"]
    DIC.elevation.r2 <- DIC.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    DIC.elevation.p = as.numeric(DIC.elevation.p),
    DIC.elevation.slope = as.numeric(DIC.elevation.slope),
    DIC.elevation.r2 = as.numeric(DIC.elevation.r2)
  )
})
DIC_table <- bind_rows(DIC_relationships, .id = "ID")%>%mutate(type='DIC')
colnames(DIC_table)<-col_names

CO2_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("CO2_flux", "WT_elevations")])) > 1

  # Initialize with NA
  CO2.elevation.p <- CO2.elevation.slope <- CO2.elevation.r2 <- NA
  CO2.qL.p <- CO2.qL.slope <- CO2.qL.r2 <- NA

  if (valid_elev) {
    CO2.elevation <- lm(CO2_flux ~ WT_elevations, data = df)
    CO2.elevation.cf <- summary(CO2.elevation)
    CO2.elevation.p <- CO2.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    CO2.elevation.slope <- CO2.elevation.cf$coefficients["WT_elevations", "Estimate"]
    CO2.elevation.r2 <- CO2.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    CO2.elevation.p = as.numeric(CO2.elevation.p),
    CO2.elevation.slope = as.numeric(CO2.elevation.slope),
    CO2.elevation.r2 = as.numeric(CO2.elevation.r2)
  )
})
CO2_table <- bind_rows(CO2_relationships, .id = "ID")%>%mutate(type='CO2')
colnames(CO2_table)<-col_names

CH4_relationships <- lapply(RC, function(df) {
  # Check for sufficient data for both regressions
  valid_elev <- sum(complete.cases(df[, c("CH4_flux", "WT_elevations")])) > 1

  # Initialize with NA
  CH4.elevation.p <- CH4.elevation.slope <- CH4.elevation.r2 <- NA
  CH4.qL.p <- CH4.qL.slope <- CH4.qL.r2 <- NA

  if (valid_elev) {
    CH4.elevation <- lm(CH4_flux ~ WT_elevations, data = df)
    CH4.elevation.cf <- summary(CH4.elevation)
    CH4.elevation.p <- CH4.elevation.cf$coefficients["WT_elevations", "Pr(>|t|)"]
    CH4.elevation.slope <- CH4.elevation.cf$coefficients["WT_elevations", "Estimate"]
    CH4.elevation.r2 <- CH4.elevation.cf$r.squared
  }

  # Return a one-row data frame
  data.frame(
    CH4.elevation.p = as.numeric(CH4.elevation.p),
    CH4.elevation.slope = as.numeric(CH4.elevation.slope),
    CH4.elevation.r2 = as.numeric(CH4.elevation.r2)
  )
})
CH4_table <- bind_rows(CH4_relationships, .id = "ID")%>%mutate(type='CH4')
colnames(CH4_table)<-col_names

relationships<-rbind(DOC_table, DIC_table, CO2_table, CH4_table)

