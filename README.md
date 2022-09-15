# Ruska2022 polkupyöräilytapahtuman varjoseurannan lähdekoodi

Tämä sovellus on kirjoitettu R-kielellä käyttäen Shiny-verkkosovellusframeworkkiä.

Sovellus käyttää virallisen seurannan KML-muotoista dataa, joka löytyy osoitteista: 

  - `https://www.randonneurs.fi/live/ruska2022/current.kml`
  - `https://www.randonneurs.fi/live/ruska2022/all.kml`

Käynnistääksesi sovelluksen omalla koneellasi [asenna R](), R-paketti `shiny` sekä git. Voit joko käynnistää sovelluksen komennolla 

```r
shiny::runGitHub('varjoseuranta', 'muuankarski')
```

tai kloonata repon ja käynnistää sovelluksen levyltä

```r
install.packages("gert")
library(gert)
repo <- git_clone("https://github.com/muuankarski/varjoseuranta")
setwd("varjoseuranta")
shiny::
shiny::runApp()
```

- (C) 2022 Markus Kainu
- MIT-license
