
library(sysfonts)
library(showtext)

#font_add_google("Source Sans Pro")
font_add_google("Merriweather")
font_add_google("Gupter")

font_families_google()
sysfonts::font_families()

# Arial stand-ins
font_add_google("Inter",      family = "inter")
font_add_google("Roboto",     family = "roboto")
font_add_google("Open Sans",  family = "open-sans")

# Times New Roman stand-ins
font_add_google("Lora",             family = "lora")
font_add_google("Libre Baskerville", family = "libre-baskerville")

font_add_google("Source Sans 3", family = "source-sans")
font_add_google("Merriweather",    family = "merriweather")
showtext_auto()