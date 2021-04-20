
bd = tibble::tribble(
                   ~origen,              ~destino,         ~tipo_enlace,
         "Monkey D. Luffy",        "Roronoa Zoro",             "Nakama",
         "Monkey D. Luffy",               "Sanji",             "Nakama",
         "Monkey D. Luffy",               "Jinbe",             "Nakama",
         "Monkey D. Luffy",   "Tony Tony Chopper",             "Nakama",
         "Monkey D. Luffy",                "Nami",             "Nakama",
         "Monkey D. Luffy",          "Nico Robin",             "Nakama",
         "Monkey D. Luffy",                "Sabo", "Familia (adoptiva)",
         "Monkey D. Luffy",         "Boa Hancock",    "Interés amoroso",
         "Monkey D. Luffy",              "Shanks",     "Maestro/Mentor",
         "Monkey D. Luffy",          "Nico Robin",             "Nakama",
         "Monkey D. Luffy",               "Usopp",             "Nakama",
         "Monkey D. Luffy",               "Brook",             "Nakama",
         "Monkey D. Luffy",              "Franky",             "Nakama",
         "Monkey D. Luffy",               "Dadan", "Familia (adoptiva)",
         "Monkey D. Luffy",      "Monkey D. Garp",            "Familia",
         "Monkey D. Luffy",    "Monkey D. Dragon",            "Familia",
         "Monkey D. Luffy",      "Portgas D. Ace", "Familia (adoptiva)",
            "Roronoa Zoro",               "Kuina",      "Amigo cercano",
            "Roronoa Zoro",     "Koushiro-sensei",     "Maestro/Mentor",
            "Roronoa Zoro",      "Dracule Mihawk",     "Maestro/Mentor",
          "Vinsmoke Sanji", "Restaurante Baratie",     "Grupo anterior",
          "Vinsmoke Sanji",                "Zeff",     "Maestro/Mentor",
          "Vinsmoke Sanji",      "Vinsmoke Reiju",            "Familia",
          "Vinsmoke Sanji",     "Vinsmoke Ichiji",            "Familia",
          "Vinsmoke Sanji",       "Vinsmoke Niji",            "Familia",
          "Vinsmoke Sanji",      "Vinsmoke Yonji",            "Familia",
          "Vinsmoke Sanji",       "Vinsmoke Sora",            "Familia",
          "Vinsmoke Sanji",      "Vinsmoke Judge",            "Familia",
                    "Nami",   "Piratas de Arlong",     "Grupo anterior",
                    "Nami",              "Nojiko", "Familia (adoptiva)",
                    "Nami",           "Bell-mère", "Familia (adoptiva)",
              "Nico Robin",          "Nico Olvia",            "Familia",
                   "Usopp",    "Piratas de Ussop",     "Grupo anterior",
                   "Usopp",              "Yassop",            "Familia",
                   "Usopp",            "Banchina",            "Familia",
                   "Usopp",                "Kaya",    "Interés amoroso",
                  "Yassop",              "Shanks",             "Nakama",
                  "Shanks",      "Dracule Mihawk",      "Amigo cercano",
       "Tony Tony Chopper",          "Dr. Kureha", "Familia (adoptiva)",
       "Tony Tony Chopper",         "Dr. Hiruluk", "Familia (adoptiva)",
                  "Franky",                 "Tom", "Familia (adoptiva)",
                  "Franky",             "Iceburg", "Familia (adoptiva)",
                  "Franky",       "Franky family", "Familia (adoptiva)",
                  "Franky",              "Kokoro", "Familia (adoptiva)",
                   "Jinbe",     "Piratas del sol",     "Grupo anterior",
              "Nico Robin",       "Baroque Works",     "Grupo anterior",
                   "Jinbe",      "Portgas D. Ace",      "Amigo cercano",
                   "Jinbe",  "Piratas de Big Mom",     "Grupo anterior",
                   "Jinbe",         "Shichibukai",     "Grupo anterior",
                   "Brook",              "Laboon",      "Amigo cercano",
                   "Brook",      "Piratas Rumbar",     "Grupo anterior",
          "Dracule Mihawk",         "Shichibukai",     "Grupo anterior",
            "Boa Hanckock",         "Shichibukai",     "Grupo anterior",
         "Buggy el Payaso",         "Shichibukai",     "Grupo anterior",
         "Buggy el Payaso",              "Shanks",      "Amigo cercano"
       )


writeLines(unique(bd$origen))
writeLines(unique(bd$destino))

bd2 = tibble::tribble(
             ~Personaje,
               "Monkey D. Luffy",
               "Roronoa Zoro",
             "Vinsmoke Sanji",
                       "Nami",
                 "Nico Robin",
                      "Usopp",
                     "Yassop",
                     "Shanks",
          "Tony Tony Chopper",
                     "Franky",
                      "Jinbe",
                      "Brook",
             "Dracule Mihawk",
               "Boa Hanckock",
            "Buggy el Payaso",
               "Roronoa Zoro",
                      "Sanji",
                      "Jinbe",
          "Tony Tony Chopper",
                       "Nami",
                 "Nico Robin",
                       "Sabo",
                "Boa Hancock",
                     "Shanks",
                      "Usopp",
                      "Brook",
                     "Franky",
                      "Dadan",
             "Monkey D. Garp",
           "Monkey D. Dragon",
             "Portgas D. Ace",
                      "Kuina",
            "Koushiro-sensei",
             "Dracule Mihawk",
        "Restaurante Baratie",
                       "Zeff",
             "Vinsmoke Reiju",
            "Vinsmoke Ichiji",
              "Vinsmoke Niji",
             "Vinsmoke Yonji",
              "Vinsmoke Sora",
             "Vinsmoke Judge",
          "Piratas de Arlong",
                     "Nojiko",
                  "Bell-mère",
                 "Nico Olvia",
           "Piratas de Ussop",
                     "Yassop",
                   "Banchina",
                       "Kaya",
                 "Dr. Kureha",
                "Dr. Hiruluk",
                        "Tom",
                    "Iceburg",
              "Franky family",
                     "Kokoro",
            "Piratas del sol",
              "Baroque Works",
         "Piratas de Big Mom",
                "Shichibukai",
                     "Laboon",
             "Piratas Rumbar"
        )

writeLines(unique(bd2$Personaje))



permutations(n, r, v=1:n, set=TRUE, repeats.allowed=FALSE)
library(gtools)


combinations(c("a","b","c"))


combn(x = c("a","b","c"),
      m = 2)

?combn

rr = combn(c("Sabo",
             "Monkey D. Dragon",
             "Emporio Ivankov",
             "Koala",
             "Bartholomew Kuma"), 2) %>%
  t() %>%
  as_tibble()

openxlsx::write.xlsx(rr, "rr.xlsx")

mugis  = tibble::tribble(
  ~Nakamas,
     "Monkey D. Luffy",
       "Roronoa Zoro",
              "Vinsmoke Sanji",
  "Tony Tony Chopper",
               "Nami",
         "Nico Robin",
              "Usopp",
              "Brook",
             "Franky",
              "Jinbe"
  )

mugis$Nakamas

vsm = tibble::tribble(
  ~Vinsmokes,
  "Vinsmoke Reiju",
  "Vinsmoke Ichiji",
    "Vinsmoke Niji",
   "Vinsmoke Yonji",
    "Vinsmoke Sora",
   "Vinsmoke Judge",
   "Vinsmoke Sanji"
  )


vsm$Vinsmokes



