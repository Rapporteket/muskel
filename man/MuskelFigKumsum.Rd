% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MuskelFigKumsum.R
\name{MuskelFigKumsum}
\alias{MuskelFigKumsum}
\title{Lag søylediagram med kummulativ vekst over år}
\usage{
MuskelFigKumsum(
  RegData,
  valgtVar = "AntReg",
  datoFra = "2000-01-01",
  datoTil = "2050-01-01",
  reshID,
  diagnosegr = -1,
  minald = 0,
  maxald = 120,
  erMann = 99,
  outfile = "",
  forlop = 99,
  avdod = "",
  enhetsUtvalg = 0,
  preprosess = F,
  hentData = F,
  inkl_tittel = T
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel skal plottes}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{diagnosegr}{Diagnosegruppe
1: Muskelsykdommer
2: Spinal muskelatrofi
3: Polynevropati}

\item{minald}{Alder, fra og med (Default: 0)}

\item{maxald}{Alder, til og med (Default: 130)}

\item{erMann}{kjønn
1: menn
0: kvinner
99: begge (Default)}

\item{outfile}{Navn på fil figuren skrives til. Default: '' (Figur skrives
til systemets default output device (som regel skjerm))}

\item{forlop}{Forløpstype
1: Basisregistrering
2: Oppfølging
3: Annet}

\item{avdod}{avdød.}

\item{enhetsUtvalg}{Lag figur for
0: Hele landet (Default)
1: Egen enhet mot resten av landet
2: Egen enhet}

\item{inkl_tittel}{inkluder titel. kan være TRUE eller FALSE}
}
\value{
En figur med kummulativt vekst i registeret
}
\description{
Denne funksjonen lager et søylediagram som viser kummulativ vekst over år
filtrert på de utvalg som er gjort.
}
