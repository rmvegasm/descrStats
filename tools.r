# Herramientas para una prueba a distancia en contexto covid-19

#-------------------------------------------------------------------------------
# Initialize
#-------------------------------------------------------------------------------

# el objeto que guardará los datos, las respuestas y la calificación

reg <- structure(list(
    name = character(),
    init_date = character(),
    resp = list(),
    done = character(),
    end_date = character()
  ),
  class = "concealed"
)

# función para registrar una respuesta:

register <- function (key, ...) {}

inicia <- function () {
  reg <- get('reg', envir = .GlobalEnv)
  reg['name'] <- readline(prompt = 'Escriba por favor su nombre y apellido: ')
  reg['init_date'] <- date()
  assign('reg', reg, envir = .GlobalEnv)
  return(invisible(NULL))
}

# No queremos que este objeto sea visible...

print.concealed <- function (x, ...) {
  print("nop, censurado...")
}

str.concealed <- function (x, ...) print(x)

summary.concealed <- function (x, ...) print(x)

#-------------------------------------------------------------------------------
# Questions object
#-------------------------------------------------------------------------------

# Todas las preguntas van en un solo objecto `questions` rotuladas con su
# identificador. Cada pregunta contiene una función `$correct()` que devuelve
# TRUE o FALSE, y un elemento `points`. Puede contener una función `$do()` que
# realiza alguna acción, y otros elementos necesarios para estas funciones

questions <- structure(list(), class = "concealed")

run_question <- function (key) {
  if (!(key %in% names(questions))) {
    cat('el identificador ', paste0('"', key, '"'), ' no existe, intenta de nuevo\n')
    return(invisible(NULL))
  }
  qs <- questions[[key]]
  if (exists('do', envir = as.environment(qs), inherits = FALSE)) {
    qs$do()
  } else {
    print('nada que hacer por aquí...')
  }
  return(invisible(NULL))
}

ans_question <- function (key, value) {
  questions <- get('questions', envir = .GlobalEnv)
  if (!(key %in% names(questions))) {
    cat('el identificador ', paste0('"', key, '"'), ' no existe, intenta otra vez\n')
    return(invisible(NULL))
  }
  qs <- questions[[key]]
  reg <- get('reg', envir = .GlobalEnv)
  if (key %in% reg[['done']]) {
    cat('lo siento, la pregunta', paste0('"', key, '"'), 'ya cuenta con una respuesta...\n')
    return(invisible(NULL))
  }
  entry <- list(
    time = date(),
    points = qs[['points']],
    value = value,
    correct = qs$correct(value)
  )
  reg[['resp']][[key]] <- entry
  reg[['done']] <- append(reg[['done']], key)
  assign('reg', reg, envir = .GlobalEnv)
  return(invisible(NULL))
}

#-------------------------------------------------------------------------------
# Question generator
#-------------------------------------------------------------------------------

# Functions to make questions based on type. Should take 'type', 'key' and
# 'content' based on question type. The `correct()` and `do()` functions should
# be part of the question type definition and contained within these
# functions...

make_question <- function (type = c('spl'), key, content, ...) {
  if (is.null(type) || !type %in% c('spl')) {
    stop('unknown question "type"')
  }
  questions <- get('questions', envir = .GlobalEnv)
  if (key %in% names(questions)) {
    stop('"key" must be unique and the one you provided is already taken...')
  }
  fun <- paste0('make_question_', type)
  args <- list(key = key, content = content, ...)
  do.call(fun, args = args, envir = .GlobalEnv)
  return(invisible(NULL))
}

make_question_spl <- function (key, content, resp_set, n = 5L) {
  questions <- get('questions', envir = .GlobalEnv)
  questions[[key]] <- list(
    do = function () {
      qt <- get('questions', envir = .GlobalEnv)
      vt <- qt[[key]][['qs']]
      qsmp <- sample(vt, n)
      pt <- 0L
      get_ans <- function (x) {
        ans <- readline(paste0(x[1L], ': '))
        if (ans %in% resp_set) {
          return(ans)
        } else { 
          cat('el valor', ans, 'no es válido! intenta otra vez...\n\n')
          get_ans(x)
        }
      }
      for (i in qsmp) {
        ans <- get_ans(i)
        if (identical(ans, i[2L])) pt <- pt + 1L
      }
      qt[[key]][['points']] <- pt
      assign('questions', qt, envir = .GlobalEnv)
      ans_question(key, ifelse(pt > 0L, TRUE, FALSE))
      invisible()
    },
    correct = function (value) {
      value
    },
    points = NULL,
    qs = content
  )
  assign('questions', questions, envir = .GlobalEnv)
  return(invisible(NULL))
}

#-------------------------------------------------------------------------------
# Submission
#-------------------------------------------------------------------------------

submit <- function () {
  # make a filename
  # serialize reg object
  # print a good bye message
}
