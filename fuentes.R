library(systemfonts)
library(dplyr)
library(stringr)

system_fonts() %>% 
  filter(family == "Lato") %>% 
  transmute(
    family, style,
    file = str_extract(path, "[\\w-]+\\.ttf$")
  )

registry_fonts()
register_fonts()
systemfonts::system_fonts()



system_fonts() %>% 
  filter(str_detect(family, "Font Awesome 5")) %>% 
  transmute(
    family, style,
    file = stringr::str_extract(path, "[\\w-]+\\.ttf$")
  )


font_hoist <- function(family, silent = FALSE) {
  font_specs <- systemfonts::system_fonts() %>%
    dplyr::filter(family == .env[["family"]]) %>%
    dplyr::mutate(family = paste(.data[["family"]], .data[["style"]])) %>%
    dplyr::select(plain = .data[["path"]], name = .data[["family"]])
  
  purrr::pwalk(as.list(font_specs), systemfonts::register_font)
  
  if (!silent)  message(paste0("Hoisted ", nrow(font_specs), " variants:\n",
                               paste(font_specs$name, collapse = "\n")))
}


font_hoist("Lato")


