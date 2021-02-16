sigma <- matrix(c(3,1.5,1.5,2),nrow=2)

data <- as_tibble(data.frame(mvtnorm::rmvnorm(400,mean=c(0,0),sigma = sigma)))

ang <- 30

theta <- ang * pi / 180

matriz <- matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2)

# no funciona
data %>%
  mutate(matriz = matriz)

data %>%
  mutate(matriz = list(matriz))

data %>%
  mutate(v = map2(X1,X2,~c(.x,.y)),
         matriz = list(matriz))

# contar preposiciones en una palabra

## ==============================

luchadores <- tibble::tribble(
   ~codigo,           ~nombre,
  "gsf901",  "Vicente Viloni",
  "thf028",     "Hip Hop Man",
  "wpx281",         "La Masa",
  "fez195", "Fulgencio Mejía",
  "jfa348",        "Mc Floyd",
  "phb625",     "Mario Morán",
  "gky651",      "Rulo Verde",
  "yfm179",    "Steve Murphy"
  )

datos <- tibble(pelea = 1:6, cant = sample(1:4,6,replace=T)) %>%
  mutate(horario = c("20:30","20:50","19:40","21:00","19.20","20:10")) %>%
  mutate(luchadores = map_chr(cant,~paste(sample(luchadores$codigo,.x),collapse=","))) %>%
  select(pelea, horario, luchadores)

datos %>%
  mutate(luchadores = map(luchadores,~unlist(strsplit(.x,split=",")))) %>%
  unnest(luchadores) %>%
  left_join(luchadores, by = c("luchadores" = "codigo"))

tibble::tribble(
 ~pelea, ~horario,                      ~casting,
      1,  "20:30",        "thf028,fez195,yfm179",
      2,  "20:50",               "thf028,yfm179",
      3,  "19:40", "jfa348,fez195,gky651,wpx281",
      4,  "21:00",               "thf028,fez195",
      5,  "19.20",               "phb625,yfm179",
      6,  "20:10", "yfm179,gsf901,thf028,fez195"
  )


# comentar por qué es necesario el unlist!

# esto
datos %>%
  mutate(luchadores = map(luchadores,~(strsplit(.x,split=",")))) %>% pull(luchadores) %>% str

# vs
datos %>%
  mutate(luchadores = map(luchadores,~unlist(strsplit(.x,split=",")))) %>% pull(luchadores) %>% str

## ==============================

tibble::tribble(
               ~banda,                     ~cancion,                                                           ~frase,
    "Los Wachiturros",          "Este es el pasito",                               "El que no hace palmas es un gato",
            "La Base",             "Sabor sabrosón",      "Según la moraleja, el que no hace palmas se deja, se deja",
       "Damas Gratis",           "Me va a extrañar",                          "ATR perro cumbia cajeteala piola gato",
    "Altos Cumbieros",            "No voy a llorar",       "Andy, fijate que volvieron, quienes… los altos cumbieros",
  "Los Pibes Chorros", "Llegamos los Pibes Chorros", "Llegamos los pibes chorros queremos las manos de todos arriba…",
            "La Liga",               "Se re pudrió",                       "El que no hace palmas tiene fama de gato",
       "Los Palmeras",                    "La Cola",    "A la una, a la dos, a la one two three four five seven nine"
  )

  
cancion <- "Llegamo los pibes chorros, queremos las manos de todos arriba"

preposiciones <- c("a", "ante", "bajo", "cabe", "con", "contra", "de", "desde", "durante", "en", "entre", "hacia", "hasta", "mediante", "para", "por", "según", "sin", "so", "sobre", "tras", "versus", "vía")

cant_palabras <- strsplit(cancion," ") %>% unlist %>% length

palabras <- strsplit(cancion," ") %>% unlist
cant_preposiciones <- sum(palabras %in% preposiciones)

# ====================================

tibble::tribble(
  ~empresa, ~producto,       ~fecha, ~evento,
       "A",      "A1", "02/06/2018",     112,
       "A",      "A1", "06/06/2018",     141,
       "A",      "A1", "13/07/2018",     119,
       "A",      "A2", "01/05/2018",      53,
       "A",      "A2", "04/05/2018",      67,
       "B",      "B1", "01/07/2018",     127,
       "B",      "B1", "05/07/2018",     301,
       "B",      "B1", "10/07/2018",      98,
       "B",      "B1", "11/07/2018",     167
  )

