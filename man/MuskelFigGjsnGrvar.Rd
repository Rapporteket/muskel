% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MuskelFigGjsnGrvar.R
\name{MuskelFigGjsnGrvar}
\alias{MuskelFigGjsnGrvar}
\title{Presenter andeler gruppert etter valgt grupperingsvariabel}
\usage{
MuskelFigGjsnGrvar(
  RegData,
  valgtVar,
  datoFra = "2000-01-01",
  datoTil = "2050-01-01",
  diagnosegr = -1,
  grvar = "SykehusNavn",
  minald = 0,
  maxald = 120,
  erMann = 99,
  outfile = "",
  forlop = 99,
  diagnose = -1,
  undergr = -1,
  skriftstr = 1,
  undergr2 = -1,
  avdod = "",
  inkl_tittel = T,
  gen_aarsak_paavist = -1,
  xtekst = "",
  graa = "",
  tittel = "",
  terskel = 10,
  decreasing = T,
  UtredningsaarFra = 1950,
  UtredningsaarTil = as.numeric(format(Sys.Date(), "\%Y"))
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel skal plottes}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'}

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

\item{diagnose}{diagnosegrupper}

\item{undergr}{undergruppe av diagnosegruppen}

\item{undergr2}{undergruppe to}

\item{avdod}{avdød.}

\item{inkl_tittel}{inkluder titel. kan være TRUE eller FALSE}

\item{UtredningsaarFra}{Utredningsår f.o.m}

\item{UtredningsaarTil}{Utredningsår t.o.m}
}
\value{
PrepData En figur med ønsket plot
}
\description{
Denne funksjonen lager grupperte andeler
}
\details{
Her kan detaljer skrives
}
