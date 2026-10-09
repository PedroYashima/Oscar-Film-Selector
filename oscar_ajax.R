library(tidyverse)
library(chromote)
library(rvest)

# a pagina le o ano que vem depois do # quando carrega, entao nao precisa clicar
url_base <- "https://www.scrapethissite.com/pages/ajax-javascript/#"
anos <- 2010:2015

b <- ChromoteSession$new()

pega_ano <- function(ano) {
  message("Baixando ", ano)

  # so mudar o # nao recarrega a pagina, entao da um refresh depois
  b$Page$navigate(paste0(url_base, ano))
  Sys.sleep(2)
  b$Page$reload()

  # o site demora uns segundos pra montar a tabela
  for (i in 1:15) {
    Sys.sleep(1)
    linhas <- b$Runtime$evaluate("document.querySelectorAll('tr.film').length")$result$value
    if (linhas > 0) break
  }
  if (linhas == 0) stop("Tabela de ", ano, " nao carregou")

  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)

  page %>%
    html_elements("tr.film") %>%
    map(~ tibble(
      ano = ano,
      titulo = .x %>% html_element(".film-title") %>% html_text2(),
      indicacoes = .x %>% html_element(".film-nominations") %>% html_text2() %>% as.integer(),
      premios = .x %>% html_element(".film-awards") %>% html_text2() %>% as.integer(),
      melhor_filme = !is.na(.x %>% html_element(".film-best-picture i"))
    )) %>%
    list_rbind()
}

oscar <- map(anos, pega_ano) %>%
  list_rbind() %>%
  arrange(desc(ano), desc(premios))

b$close()

nrow(oscar)
oscar %>% filter(melhor_filme)

write_csv(oscar, "oscar_2010_2015.csv")
openxlsx::write.xlsx(oscar, "oscar_2010_2015.xlsx")
