library(dplyr)

load("./Data/Database_Full.RData")

Recipients = filter(Recipients,RecipientCode!=6000,RecipientCode!=6001)
Data = filter(Data,RecipientCode!=DonorCode,DonorCode!=6000)
Donors = filter(Donors,DonorCode!=6000,DonorCode!=6001)

Data$DonorName = iconv(Data$DonorName, "latin1", "ASCII", sub="")
Data$RecipientName = iconv(Data$RecipientName, "latin1", "ASCII", sub="")
Recipients$RecipientName = iconv(Recipients$RecipientName, "latin1", "ASCII", sub="")
Donors$DonorName = iconv(Donors$DonorName, "latin1", "ASCII", sub="")

Donors$DonorName[Donors$DonorName=="[Other donors]"] = "Other Donors"

load("./Data/Master_Code.RData")

Recipients_Map = left_join(Recipients,Master_Code,by = c("RecipientCode"="CRS_CODE"))

Donors_Map = left_join(Donors,Master_Code,by = c("DonorCode"="CRS_CODE"))

Recipients_Graphs = mutate(Recipients,
                           Recipients_Donors=1,
                           Name=RecipientCode)

Donors_Graphs = mutate(Donors,
                       Recipients_Donors=2,
                       Name=DonorCode)

Data = select(Data,
              DonorName,
              RecipientName,
              Year,
              Ind5a_P1,
              Ind5a_P2,
              Ind5a_P3,
              Ind5a_P4,
              Ind5b_G1,
              Ind5b_G2,
              Ind5b_G3,
              Ind6_G4,
              Ind9b_P5,
              Ind9b_P6,
              Ind9b_P7,
              Ind9b_P8
)

Data$DonorName = as.character(Data$DonorName)

Data$DonorName[Data$DonorName=="[Other donors]"] = "Other Donors"
Data$DonorName[is.na(Data$DonorName)] = "Other Donors"

colnames(Data) = c("Donor",
                   "Recipient",
                   "Year",
                   "Disbursed",
                   "Gov't Sector",
                   "Scheduled",
                   "Other",
                   "Fwd Exp 1",
                   "Fwd Exp 2",
                   "Fwd Exp 3",
                   "Gov't Budget",
                   "Budget Execution",
                   "Financial",
                   "Auditing",
                   "Procurement"
)

Recipients = Recipients %>%
  mutate(Indicator5a_1 = Indicator5a_1*100,
         Indicator5a_2 = Indicator5a_2*100,
         Indicator5b = Indicator5b*100,
         Indicator6_1 = Indicator6_1*100,
         Indicator6_2 = Indicator6_2*100,
         Indicator9b = Indicator9b*100)

Recipients$Indicator7[Recipients$Indicator7==1] = "Yes"
Recipients$Indicator7[Recipients$Indicator7==0] = "No"

Recipients$Indicator8[Recipients$Indicator8==1] = "Yes"
Recipients$Indicator8[Recipients$Indicator8==0] = "No"

Recipients = select(Recipients,
                    RecipientName,
                    Year,
                    Number,
                    Indicator5a_1,
                    Indicator5a_2,
                    Indicator5b,
                    Indicator6_1,
                    Indicator6_2,
                    Indicator7,
                    Indicator8,
                    Indicator9a,
                    Indicator9b
)

colnames(Recipients) = c("Recipient",
                         "Year",
                         "Number of Donors",
                         "Indicator 5a - As Scheduled",
                         "Indicator 5a - Beyond Scheduled",
                         "Indicator 5b",
                         "Indicator 6 - Of Scheduled",
                         "Indicator 6 - Beyond Scheduled",
                         "Indicator 7",
                         "Indicator 8",
                         "Indicator 9a",
                         "Indicator 9b"
)

Donors = select(Donors,
                DonorName,
                Year,
                Number,
                Indicator5a_1,
                Indicator5a_2,
                Indicator5b,
                Indicator6_1,
                Indicator6_2,
                Indicator9b
)

Donors = Donors %>%
  mutate(Indicator5a_1 = Indicator5a_1*100,
         Indicator5a_2 = Indicator5a_2*100,
         Indicator5b = Indicator5b*100,
         Indicator6_1 = Indicator6_1*100,
         Indicator6_2 = Indicator6_2*100,
         Indicator9b = Indicator9b*100)

colnames(Donors) = c("Donor",
                     "Year",
                     "Number of Recipients",
                     "Indicator 5a - As Scheduled",
                     "Indicator 5a - Beyond Scheduled",
                     "Indicator 5b",
                     "Indicator 6 - Of Scheduled",
                     "Indicator 6 - Beyond Scheduled",
                     "Indicator 9b"
)

Data = arrange(Data,Donor,Recipient,Year)
