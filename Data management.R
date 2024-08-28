setwd("/Users/lorenzomattioli/Documents/Studio/Università/Progetti/Tesi magistrale/Data Analysis")

library(tidyverse)
library(expss)
library(readxl)

# Importing data ---------------------------------------------------------------

# July '24
data310724 <- read_excel("/Users/lorenzomattioli/Documents/Studio/Università/Progetti/Tesi magistrale/Data Analysis/Data/Infojobs/Infojobs_Italia/Infojobs 3107.xlsx")
data310724$date <- as.Date("31/07/2024")

data310724 |> 
  select(!cat1:exHours) -> data310724

## Merging datasets
data <- rbind(data310724)
rm(data310724)

# Data management --------------------------------------------------------------

# deduplicating
data |> 
  distinct(url, .keep_all = T) -> df
rm(data)

# Cleaning and labelling -------------------------------------------------------

## Job listing
var_lab(df$jobList) = "Job listings"

## URL
var_lab(df$url) = "Job listing URLs"

## Firm
var_lab(df$firm) = "Firm demanding labour"

## Job location
table(df$loc)
mutate(df, loc = dplyr::recode(loc,
                               'italy' = 'Remote/relocation needed',
                               'Italia' = 'Remote/relocation needed',
                               'ITALIA' = 'Remote/relocation needed',
                               'Italy' = 'Remote/relocation needed',
                               'Isp Cannara (Pg)' = 'Cannara',
                               'Isp Massa Martana (Pg)' = 'Massa Martana',
                               'ponte felcino' = 'Ponte Felcino',
                               'Ponte Felcino (Pg)' = 'Ponte Felcino',
                               'Umbria, Perugia e provincia' = 'Perugia'
)) -> df
var_lab(df$loc) = "Job location"

## Job description
var_lab(df$desc) = "Job description"

## Type of Contract
table(df$cont)
df |> 
  mutate(cont = dplyr::recode(cont,
                              "Contratto a progetto" = 1,
                              "Contratto agente / partita iva" = 2,
                              "Contratto a tempo determinato" = 3,
                              "Contratto indeterminato" = 4,
                              "Contratto da definire" = 5,
                              "Contratto stage / internship" = 6,
                              "Contratto altro tipo di contratto" = 7
  )) -> df 
var_lab(df$cont) = "Contract type"
val_lab(df$cont) = make_labels("
                                1 Project contract
                                2 Agent contract / partita iva
                                3 Fixed-term contract
                                4 Open-ended contract
                                5 Contract to be defined
                                6 Stage / internship
                                7 Other
                                ")

## Working hours
### recoding
table(df$hour)
df |> 
  mutate(
    hour = dplyr::recode(hour, 'Giornata lavorativa completa' = 1,
                         'Giornata lavorativa indifferente' = 2,
                         'Giornata lavorativa part-time: indifferente' = 3,
                         'Giornata lavorativa part-time: mattina' = 4,
                         'Giornata lavorativa part-time: pomeriggio' = 5
    )) -> df

df |> 
  mutate(
    hourRec = dplyr::recode(hour,
                            '1' = 1,
                            '2' = 2,
                            '3' = 3,
                            '4' = 3,
                            '5' = 3
    ), .after = hour
  ) -> df
### labelling
var_lab(df$hour) = "Working hours"
val_lab(df$hour) = make_labels("
                                1 Full work day
                                2 Indifferent work day
                                3 Part-time: indifferent
                                4 Part-time: morning
                                5 Part-time: afternoon
                                ")

var_lab(df$hourRec) = "Working hours (recode)"
val_lab(df$hourRec) = make_labels("
                                  1 Full work day
                                  2 Indifferent work day
                                  3 Part-time
                                  ")

## Salary
var_lab(df$sal) = "Salary"

## Educational requisites
### recoding
table(df$minEdu)
df |> 
  mutate(minEdu = dplyr::recode(minEdu,
                                'Senza studi' = 0,
                                'Licenza media' = 1,
                                'In corso: diploma di maturità' = 2,
                                'Diploma di Maturità' = 3,
                                'Laurea breve (3 anni)' = 4,
                                'Laurea breve (3 anni) - Economia Aziendale' = 5,
                                'Laurea breve (3 anni) - Pedagogia' = 6,
                                'Laurea breve (3 anni) - Ing. Meccanica' = 7,
                                'Laurea breve (3 anni) - Ing. Altro' = 8,
                                'Laurea breve (3 anni) - Altro' = 4,
                                'Laurea specialistica (4-5 anni)' = 9,
                                'Laurea specialistica (4-5 anni) - Dottore Commercialista' = 10,
                                'Laurea specialistica (4-5 anni) - Ing. Civile' = 11,
                                'Altri titoli, certificati e patenti' = 12,
                                'Altri corsi di formazione' = 13
  )) -> df


df |> 
  mutate(Edu = dplyr::recode(minEdu,
                             '0' = 0,
                             '1' = 1,
                             '2' = 1,
                             '3' = 2,
                             '4' = 3,
                             '5' = 3,
                             '6' = 3,
                             '7' = 3,
                             '8' = 3,
                             '9' = 4,
                             '10' = 4,
                             '11' = 4,
                             '12' = 5,
                             '13' = 5
                             
  ), .after = minEdu) -> df
table(df$Edu)

### labelling
var_lab(df$minEdu) = "Educational requirements"
val_lab(df$minEdu) = make_labels("
                              0 No qualification
                              1 Middle school diploma
                              2 Ongoing: High school
                              3 High school diploma
                              4 Bachelor's degree
                              5 Bachelor's degree - Economics and Business
                              6 Bachelor's degree - Pedagogy
                              7 Bachelor's degree - Mechanical engineering
                              8 Bachelor's degree - Engineering (other)
                              9 Bachelor's degree - Other
                             10 Master's degree - Chartered accounting
                             11 Master's degree - Civil Engineering
                             12 Other qualifications, certificates and licenses
                             13 Other forms of training
                              ")
var_lab(df$Edu) = "Educational requirements (recode)"
val_lab(df$Edu) = make_labels("
                              0 No qualification
                              1 Middle school diploma
                              2 High school diploma
                              3 Bachelor's degree
                              4 Master's degree
                              5 Other
                              ")

## minExp
### recoding
table(df$minExp, useNA = "ifany")
df |> 
  mutate(minExp = dplyr::recode(minExp,
                                '1 anno' = 1,
                                '2 anni' = 2,
                                '3 anni' = 3,
                                '4 anni' = 4,
                                'Da 6 a 10 anni' = 5,
                                'Non richiesta' = 0
  )) -> df
var_lab(df$minExp) = "Experience requirements"
val_lab(df$minExp) = make_labels("
                                 0 None required
                                 1 1 year
                                 2 2 years
                                 3 3 years
                                 4 4 years
                                 5 5 to 10 years
                                 ")

## Requirements
table(df$requirements,
      useNA = "ifany") # way too many missing observations, unusable
df |> 
  select(!requirements) -> df

## Industry
table(df$industry,
            useNA = "ifany")
### recoding
df |> 
  mutate(industry = dplyr::recode(industry,
                       'Acquisti, logistica, magazzino - Acquisti' = 11,
                       'Acquisti, logistica, magazzino - Stoccaggio materiale, inventario, magazzino' = 13,
                       'Agroalimentari' = 20,
                       'Amministrazione, contabilità, segreteria - Amministrazione' = 31,
                       'Amministrazione, contabilità, segreteria - Consulenza strategica' = 32,
                       'Amministrazione, contabilità, segreteria - Contabilità' = 33,
                       'Amministrazione, contabilità, segreteria - Segreteria' = 34,
                       'Amministrazione, contabilità, segreteria - Ufficio paghe e contributi' = 35,
                       'Attività amministrative e ausiliarie in uffici' = 36,
                       'Consulenza manageriale e revisione' = 40,
                       'Architettura e progettazione' = 50,
                       'Arti grafiche, design - Grafica' = 51,
                       'Grafica' = 51,
                       'Attività audiovisive (cinematografiche, televisive, radiofoniche, sonore, edizioni musicali, ecc.)' = 52,
                       'Attenzione al cliente - Assistenza clienti, call center, data entry' = 61,
                       'Automotive' = 70,
                       'Commercio al dettaglio, GDO, Retail - Commercio al dettaglio, GDO, retail' = 80,
                       'Vendita al dettaglio' = 80,
                       'Commercio, distribuzione, GDO' = 90,
                       'Costruzioni / Edilizia' = 100,
                       'Edilizia, immobiliare - Architettura, design' = 101,
                       'Edilizia, immobiliare - Interior design' = 102,
                       'Edilizia, immobiliare - Costruzioni' = 103,
                       'Servizi infrastrutturali' = 103,
                       'Edilizia, immobiliare - Immobiliare' = 104,
                       'Settore immobiliare' = 104,
                       'Edilizia, immobiliare - Ingegneria civile' = 105,
                       'Editoria' = 110,
                       'Elettronica di consumo' = 120,
                       'Energia rinnovabile e ambiente' = 130,
                       'Fabbricazione di computer e prodotti di elettronica e ottica' = 140,
                       'Fabbricazione di mobili' = 150,
                       'Fabbricazione materiale ed equipaggiamento elettrico' = 161,
                       'Fabbricazione prodotti in metallo, ferro e acciaio' = 162,
                       'Fabricazione plastica, carta, legno, ceramica, vetro, cemento, calcestruzzo, gesso' = 163,
                       'Farmacia, medicina, salute - Farmacia' = 171,
                       'Settore farmaceutico - Commerciale e vendita' = 171,
                       'Farmacia, medicina, salute - Odontoiatria' = 172,
                       'Farmacia, medicina, salute - Altre professioni sanitarie' = 173,
                       'Sanità' = 170,
                       'Servizi finanziari' = 180,
                       'Finanza, banche e credito - Assicurazioni' = 181,
                       'Finanza, banche e credito - Credito a privati' = 182,
                       'Finanza, banche e credito - Prodotti e servizi bancari' = 183,
                       'Forniture elettriche, gas, vapore, aria, acqua, servizi igienico-sanitari, rifiuti' = 190,
                       'Hotellerie, ristorazione' = 200,
                       'Turismo' = 200,
                       'Turismo, ristorazione - Hotellerie' = 201,
                       'Turismo, ristorazione - Ristorazione' = 202,
                       'Turismo, ristorazione - Viaggi' = 203,
                       'Industria Chimica' = 210,
                       'Vetro, ceramica e cemento' = 210,
                       'Industria farmaceutica' = 220,
                       'Settore farmaceutico - Qualità, Produzione, Ricerca e Sviluppo' = 220,
                       'Industria tessile, moda e calzaturiera' = 230,
                       'Informatica, IT e telecomunicazioni - Amministrazione di sistemi' = 241,
                       'Informatica, IT e telecomunicazioni - Gestione progetti' = 242,
                       'Informatica, IT e telecomunicazioni - Sviluppo software' = 243,
                       'Informatica, IT e telecomunicazioni - Telecomunicazioni' = 244,
                       'Telecomunicazioni' = 244,
                       'Ingegneria - Altre ingegnerie' = 250,
                       'Ingegneria - Automazione industriale' = 251,
                       'Ingegneria - Elettronica' = 252,
                       'Ingegneria - Energie rinnovabili' = 253,
                       'Ingegneria - Ingegneria gestionale' = 254,
                       'Ingegneria - Ingegneria industriale' = 255,
                       'Insegnamento e formazione' = 260,
                       'Internet e servizi informatici' = 270,
                       'Programmazione, consulenza e altre attività informatiche' = 270,
                       'Logistica' = 280,
                       'Acquisti, logistica, magazzino - Distribuzione e logistica' = 280,
                       'Trasporto (merci e passeggeri) e corriere' = 280,
                       'Macchinari' = 290,
                       'Marketing e pubblicità' = 301,
                       'Sara' = 301,
                       'Marketing, comunicazione - Marketing' = 301,
                       'Marketing, comunicazione - Produzione' = 302,
                       'Media offline' = 310,
                       'Minerario e metalli' = 320,
                       'Operai, produzione, qualità - Controllo di qualità' = 331,
                       'Operai, produzione, qualità - Manutenzione' = 332,
                       'Operai, produzione, qualità - Operai, produzione' = 333,
                       'Operai, produzione, qualità - Project Management' = 334,
                       'Produzione di prodotti per la pulizia, cosmetici e profumi' = 340,
                       'Professioni e mestieri - Altri' = 350,
                       'Professioni e mestieri - Artigianato' = 351,
                       'Professioni e mestieri - Elettricità' = 352,
                       'Professioni e mestieri - Estetica' = 353,
                       'Professioni e mestieri - Falegnameria' = 354,
                       'Professioni e mestieri - Idraulica' = 355,
                       'Professioni e mestieri - Meccanico' = 356,
                       'Professioni e mestieri - Pulizie e servizi domestici' = 357,
                       'Professioni e mestieri - Sartoria' = 358,
                       'Professioni e mestieri - Sicurezza, vigilanza' = 359,
                       'Professioni e mestieri - Trasporti' = 3510,
                       'Pubblica amministrazione - Amministrazione locale' = 361,
                       'Pubblica amministrazione - Aziende pubbliche' = 362,
                       'Risorse umane, recruiting - Gestione del personale' = 371,
                       'Risorse umane, recruiting - Sviluppo carriere professionali' = 372,
                       'Sport' = 380,
                       'Vendita al dettaglio' = 391,
                       "Vendita all'ingrosso" = 392,
                       'Vendite - Agenti' = 393,
                       'Vendite - Key account' = 394,
                       'Vendite - Local / national account' = 395,
                       'Selezione e ricerca di personale' = 00,
                       'Altre - Non specificato' = 00,
                       'Altre attività di servizi per la persona (parrucchieri, estetisti, pompe funebri, mantenimento fisico)' = 00,
                       'Altre industrie e servizi / Non specificato' = 00,
                       'Altri servizi alle imprese' = 00
  )) -> df # necessario un aggressivo recode. La compilazione da parte dei datori di lavoro è poco affidabile. molti NA.

## Data collection date
var_lab(df$date) = "Data collection date"

# Git test