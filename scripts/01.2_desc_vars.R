##############################
# ERevision palabras comunes #
#############################


#### === LIMPIEZA === ####

# Voy a unir tpdas las descripciones y analizar a ver que info tienen

all_descs <- ""
for (i in 1:5000) {
  all_descs <- paste(all_descs, train$description[i])
}


# Puesto que nos interesa ver la frecuencia de palabras, eliminaremos 
# todos los números y espacios no simples que se hayan generado.



all_descs <- gsub("\\d+", "", all_descs) # quito \digitos (numero)

all_descs <- trimws(all_descs) # quito espacios al principio y final



#### === TOKENIZACION === ####

# Vamos a usar una nueva libreria
p_load(tokenizers, stopwords, SnowballC)

## ahora vamos a tokenizar ya que el texto esta limpio

documento_tokenizado <- tokenize_words(all_descs)

# En total tenemos 1 documento con 6703 términos únicos
length(unique(documento_tokenizado[[1]]))

# El próximo paso es eliminar lo que se conoce como las stopwords, que son aquellas 
# palabras que no le añaden ningún significado al texto, por ejemplo: 
# el, la, y, o, del, con, a, etc.

# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras

documento_tokenizado <- documento_tokenizado[[1]]
# Número de palabras iniciales
n0 <- length(documento_tokenizado)
# Vamos a eliminar las stopwords de nuestro documento
documento_tokenizado <- setdiff(documento_tokenizado, lista_palabras)
# Número de palabras finales
n1 <- length(documento_tokenizado)
# Se eliminaron 310.438 palabras! (todas stop words - ruido)
n0 - n1

# Finalmente vamos a hacer stemming para unificar términos. 

#El stemming es un método para reducir una palabra a su raíz.

# Esto se hace con el objetivo de analizar variaciones de una palabra como una sola. 
# Supongamos por ejemplo que tenemos el conjunto de palabras: 
# {moderniza, moderno, moderna, modernos, modernización}

# Para lograr esto en R utilizaremos la versión en español del algoritmo de 
# Porter a través de la función wordStem de la librería SnowballC.

documento_tokenizado <- wordStem(documento_tokenizado, "spanish")


p_load(wordcloud)

frecuencia <- documento_tokenizado %>%
  table() %>%
  data.frame() %>%
  rename("Palabra" = ".") %>%
  arrange(desc(Freq))

set.seed(666) 
png(filename = "wordcloud.png", width = 800, height = 800)
wordcloud(words = frecuencia$Palabra, freq = frecuencia$Freq, min.freq = 1,
          max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()

















