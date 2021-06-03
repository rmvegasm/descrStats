# Herramientas para una prueba a distancia en contexto covid-19


#-------------------------------------------------------------------------------
# Dependencies
#-------------------------------------------------------------------------------

require('rdrop2')
require('extraDistr')

# need to install this package in the students machines...
.install_dep <- function () {
  if (!require('extraDistr')) {
    cat('Antes de empezar necesitamos instalar un paquete adicional...\n')
    install.packages('extraDistr')
  }
  library('extraDistr')
}

#-------------------------------------------------------------------------------
# Initialize
#-------------------------------------------------------------------------------

# el objeto que guardará los datos, las respuestas y la calificación

.reg <- list(
    name = character(),
    init_date = character(),
    resp = list(),
    done = character(),
    end_date = character()
)

.resp2df <- function () {
  reg = get('.reg', envir = .GlobalEnv)
  resp = reg[['resp']]
  time = sapply(resp, function (x) x[['time']])
  points = sapply(resp, function (x) x[['points']])
  correct = sapply(resp, function (x) x[['correct']])
  value = sapply(resp, function (x) x[['value']])
  data.frame(time, points, correct, value)
}

inicia <- function () {
  .install_dep()
  reg <- get('.reg', envir = .GlobalEnv)
  reg['name'] <- readline(prompt = 'Escriba por favor su nombre y apellido: ')
  reg['init_date'] <- date()
  assign('.reg', reg, envir = .GlobalEnv)
  return(invisible(NULL))
}

#-------------------------------------------------------------------------------
# Questions object
#-------------------------------------------------------------------------------

# Todas las preguntas van en un solo objecto `questions` rotuladas con su
# identificador. Cada pregunta contiene una función `$correct()` que devuelve
# TRUE o FALSE, y un elemento `points`. Puede contener una función `$do()` que
# realiza alguna acción, y otros elementos necesarios para estas funciones

.questions <- list()

run_question <- function (key) {
  questions <- get('.questions', envir = .GlobalEnv)
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
  questions <- get('.questions', envir = .GlobalEnv)
  if (!(key %in% names(questions))) {
    cat('el identificador ', paste0('"', key, '"'), ' no existe, intenta otra vez\n')
    return(invisible(NULL))
  }
  qs <- questions[[key]]
  reg <- get('.reg', envir = .GlobalEnv)
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
  assign('.reg', reg, envir = .GlobalEnv)
  return(invisible(NULL))
}

.get_points <- function (key) {
  get('.questions', envir = .GlobalEnv)[[key]][['points']]
}

.get_max_points <- function (sum = TRUE) {
  qs <- get('.questions', envir = .GlobalEnv)
  tp <- sapply(qs, function (x) x[['points']])
  return(ifelse(sum, sum(tp), tp))
}

#-------------------------------------------------------------------------------
# Question generator
#-------------------------------------------------------------------------------

# Functions to make questions based on type. Should take 'type', 'key' and
# 'content' based on question type. The `correct()` and `do()` functions should
# be part of the question type definition and contained within these
# functions...

.make_question <- function (type = c('spl', 'sglval'), key, points, ...) {
  if (is.null(type) || !type %in% c('spl', 'sglval')) {
    stop('unknown question "type"')
  }
  questions <- get('.questions', envir = .GlobalEnv)
  if (key %in% names(questions)) {
    stop('"key" must be unique and the one you provided is already taken...')
  }
  fun <- paste0('.make_question_', type)
  args <- list(key = key, points = points, ...)
  do.call(fun, args = args, envir = .GlobalEnv)
  return(invisible(NULL))
}

.make_question_spl <- function (key, points, content, resp_set, n = 5L) {
  questions <- get('.questions', envir = .GlobalEnv)
  questions[[key]] <- list(
    do = function () {
      qt <- get('.questions', envir = .GlobalEnv)
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
      assign('.questions', qt, envir = .GlobalEnv)
      ans_question(key, ifelse(pt > 0L, TRUE, FALSE))
      invisible()
    },
    correct = function (value) {
      value
    },
    points = points,
    qs = content
  )
  assign('.questions', questions, envir = .GlobalEnv)
  return(invisible(NULL))
}

.make_question_sglval <- function (key, solution, points) {
  questions <- get('.questions', envir = .GlobalEnv)
  questions[[key]] <- list(
    points = points,
    solution = solution,
    correct = function (value) {
      isTRUE(all.equal(value, solution()))
    }
  )
  assign('.questions', questions, envir = .GlobalEnv)
  return(invisible(NULL))
}


#-------------------------------------------------------------------------------
# dataset generator
#-------------------------------------------------------------------------------

# generate a well balanced dataset including all variable types, different
# measurement scales and some correlation structure. All parameters will be
# stored within a (hidden) environment in order for the generating function to
# be reusable.

.sim_params <- new.env()

.sim_dset <- function () {
  # overall size
  n = sample(900:1000, size = 1L)
  
  # numeric variables
  mean_x = runif(1, 65, 80)
  mean_y = runif(1, 160, 178)
  sd_x = runif(1, 2, 5)
  sd_y = runif(1, 2, 6)
  
  # correlation rxy
  rxy = runif(1, .6, .95)
  
  # factor effects over mean and sd
  ef_Amx = runif(1, -5, 5)
  ef_Asdx = runif(1, .1, .5)
  ef_Amy = runif(1, -5, 5)
  ef_Asdy = runif(1, .1, .3)
  ef_Arxy = runif(1, .01, .04)

  ef_Bmx = runif(1, -5, 5)
  ef_Bsdx = runif(1, .1, .3)
  ef_Bmy = runif(1, -5, 5)
  ef_Bsdy = runif(1, .3, .5)
  ef_Brxy = runif(1, .01, .04) * -1

  ef_Cmx = runif(1, -5, 5)
  ef_Csdx = runif(1, .1, .5)
  ef_Cmy = runif(1, -5, 5)
  ef_Csdy = runif(1, .1, .5)
  ef_Crxy = runif(1, .01, .04) * sample(c(1, -1), size = 1L)

  # store params for inspection
  for (param in ls()) assign(param, get(param), envir = .sim_params)

  # simulate from bivariate normal (near half the dset size for each factor)
  n_sim = n %/% 2
  dset = rbind(
    # grp A
    rbvnorm(n_sim,
            mean1 = mean_x + ef_Amx,
            sd1 = sd_x + ef_Asdx,
            mean2 = mean_y + ef_Amy,
            sd2 = sd_y + ef_Asdy,
            cor = rxy + ef_Arxy),
    # grp B
    rbvnorm(n_sim,
            mean1 = mean_x + ef_Bmx,
            sd1 = sd_x + ef_Bsdx,
            mean2 = mean_y + ef_Bmy,
            sd2 = sd_y + ef_Bsdy,
            cor = rxy + ef_Brxy),
    # grp C
    rbvnorm(n_sim,
            mean1 = mean_x + ef_Cmx,
            sd1 = sd_x + ef_Csdx,
            mean2 = mean_y + ef_Cmy,
            sd2 = sd_y + ef_Csdy,
            cor = rxy + ef_Crxy)
    )
  dset = as.data.frame(dset)
  names(dset) = c('x', 'y')
  dset[['grp']] = rep(c('A', 'B', 'C'), each = n_sim)
  dset = dset[sample(1:dim(dset)[1L], size = n), ]
  return(dset)
}

.make_dset <- function (overwrite = FALSE, quiet = FALSE) {
  if (exists('dset', envir = .GlobalEnv, inherits = FALSE) && !overwrite) {
    cat('el set de datos "dset" ya fue creado, no puedo crearlo otra vez...\n\n')
    return(invisible(NULL))
  }
  assign('dset', .sim_dset(), envir = .GlobalEnv)
  if (!quiet) cat('"dset" creado en el ambiente global\n\n')
  return(invisible(NULL))
}

make_dset <- function () .make_dset()
#-------------------------------------------------------------------------------
# Submission
#-------------------------------------------------------------------------------

# read dropbox token for rdrop2:
.token = readRDS('token.rds')

submit <- function () {
  reg = get('.reg', envir = .GlobalEnv)

  # make a filename
  name = reg[['name']]
  filename = gsub(' ', '_', tolower(name))
  
  # register end_date:
  reg[['end_date']] = date()
  assign('.reg', reg, envir = .GlobalEnv)

  # serialize .reg object
  save('.reg', 'dset', file = filename)

  # upload to dropbox
  rdrop2::drop_upload(filename, path = 'tmp', dtoken = .token)

  # print a good bye message
  cat('listo!\n',
      'se ha creado un archivo con tu nombre en el directorio de trabajo,\n
       consérvalo como respaldo. Una copia ha sido enviada al profesor\n',
      'buen día!\n')
  
  # remove prueba1
  file.remove('prueba1', 'main.html')
  return(invisible(NULL))
}
