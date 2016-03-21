
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Connections.h>

#include <sys/time.h>

Rconnection getConnection(int n);

SEXP progress_tick(SEXP self, SEXP private, SEXP len, SEXP tokens);
SEXP progress_terminate(SEXP self, SEXP private);
SEXP progress_render(SEXP self, SEXP private, SEXP tokens);

void progress_refresh_line(SEXP private, ...);
double progress_ratio(SEXP private);
SEXP progress_now();
double progress_elapsed_since(SEXP start);

/* Tokens */
int progress_token_bar(SEXP private, char *buffer, char *bufend,
		       int bar_pos);
int progress_token_current(SEXP private, char *bufptr, char *bufend);
int progress_token_total(SEXP private, char *bufptr, char *bufend);
int progress_token_elapsed(SEXP private, char *bufptr, char *bufend);
int progress_token_eta(SEXP private, char *bufptr, char *bufend);
int progress_token_percent(SEXP private, char *bufptr, char *bufend);
int progress_token_rate(SEXP private, char *bufptr, char *bufend);
int progress_token_bytes(SEXP private, char *bufptr, char *bufend);
int progress_token_spin(SEXP private, char *bufptr, char *bufend);

/* ----------------------------------------------------------- */

SEXP progress_tick(SEXP self, SEXP private, SEXP len, SEXP tokens) {

  int first = asLogical(findVar(install("first"), private));
  double current = asReal(findVar(install("current"), private));
  double total = asReal(findVar(install("total"), private));
  int toupdate = asLogical(findVar(install("toupdate"), private));
  int complete = 0;

  if (first) {
    SEXP start;
    PROTECT(start = progress_now());
    setVar(install("first"), ScalarLogical(0), private);
    setVar(install("start"), start, private);
    UNPROTECT(1);
  }

  current += asInteger(len);
  setVar(install("current"), ScalarInteger(current), private);

  if (!toupdate) {
    double show_after = asReal(findVar(install("show_after"), private));
    SEXP start;
    double secs;
    PROTECT(start = findVar(install("start"), private));
    secs = progress_elapsed_since(start);
    if (secs > show_after) {
      toupdate = 1;
      setVar(install("toupdate"), ScalarLogical(1), private);
    }
    UNPROTECT(1);
  }

  if (current >= total) {
    complete = 1;
    setVar(install("complete"), ScalarLogical(1), private);
  }

  if (toupdate) progress_render(self, private, tokens);

  if (complete) {
    progress_terminate(self, private);
    SEXP callback = findVar(install("callback"), private);
    if (! isNull(callback)) {
      SEXP pkg;
      PROTECT(pkg = eval(lang2(install("getNamespace"),
			       ScalarString(mkChar("pkg"))),
			 R_GlobalEnv));
      eval(lang1(callback), pkg);
      UNPROTECT(1);
    }
  }

  return self;
}

SEXP progress_terminate(SEXP self, SEXP private) {

  /* TODO */

  SEXP pkg;

  PROTECT(pkg = eval(lang2(install("getNamespace"),
			   ScalarString(mkChar("pkg"))),
		     R_GlobalEnv));
  eval(lang3(install("pb_terminate"), self, private), pkg);

  UNPROTECT(1);

  return self;
}

#define PROGRESS_TOKEN_CURRENT 0
#define PROGRESS_TOKEN_TOTAL   1
#define PROGRESS_TOKEN_ELAPSED 2
#define PROGRESS_TOKEN_ETA     3
#define PROGRESS_TOKEN_PERCENT 4
#define PROGRESS_TOKEN_RATE    5
#define PROGRESS_TOKEN_BYTES   6
#define PROGRESS_TOKEN_BAR     7
#define PROGRESS_TOKEN_SPIN    8

SEXP progress_render(SEXP self, SEXP private, SEXP tokens) {

  int supported = asLogical(findVar(install("supported"), private));
  char buffer[500], *bufptr = buffer, *bufend = buffer + 500;
  int bar_pos = -1;
  const char *format;

  if (!supported) return self;

  format = CHAR(asChar(findVar(install("format"), private)));

  /* We put this here, so that we can print it in one go */
  *bufptr = '\r';
  bufptr++;

  /* TODO: custom tokens */

  while (bufptr != bufend && *format) {

    /* Copy over normal text */
    while (bufptr != bufend && *format && *format != ':') {
      *bufptr = *format;
      bufptr++; format++;
    }

    if (! strncmp(format, ":bar", 4)) {
      /* Do nothing for now, but write down position.
	 Bar will be inserted last, because we need to
	 know how much space there is for it. */
      bar_pos = bufptr - buffer;
      format += 4;

    } else if (!strncmp(format, ":current", 8)) {
      bufptr += progress_token_current(private, bufptr, bufend);
      format += 8;

    } else if (!strncmp(format, ":total", 6)) {
      bufptr += progress_token_total(private, bufptr, bufend);
      format += 6;

    } else if (!strncmp(format, ":elapsed", 8)) {
      bufptr += progress_token_elapsed(private, bufptr, bufend);
      format += 8;

    } else if (!strncmp(format, ":eta", 4)) {
      bufptr += progress_token_eta(private, bufptr, bufend);
      format += 4;

    } else if (!strncmp(format, ":percent", 8)) {
      bufptr += progress_token_percent(private, bufptr, bufend);
      format += 8;

    } else if (!strncmp(format, ":rate", 5)) {
      bufptr += progress_token_rate(private, bufptr, bufend);
      format += 5;

    } else if (!strncmp(format, ":bytes", 6)) {
      bufptr += progress_token_bytes(private, bufptr, bufend);
      format += 6;

    } else if (!strncmp(format, ":spin", 5)) {
      bufptr += progress_token_spin(private, bufptr, bufend);
      format += 5;

    }
  }

  *bufptr = 0;

  if (bar_pos != -1) {
    progress_token_bar(private, buffer, bufend, bar_pos);
  }

  progress_refresh_line(private, buffer);

  return self;
}

int progress_token_bar(SEXP private, char *buffer, char *bufend,
		       int bar_pos) {

  int width = asInteger(findVar(install("width"), private));
  int bar_width = width - strlen(buffer);
  double ratio = progress_ratio(private);
  int complete_len = (int) round(bar_width * ratio);
  int incomplete_len = bar_width - complete_len;
  SEXP chars = findVar(install("chars"), private);
  const char *complete_char = CHAR(asChar(VECTOR_ELT(chars, 0)));
  const char *incomplete_char = CHAR(asChar(VECTOR_ELT(chars, 1)));

  memcpy(buffer + bar_pos + bar_width, buffer + bar_pos, bar_width);
  for (buffer += bar_pos; complete_len > 0; complete_len--) {
    *buffer = *complete_char; buffer++;
  }
  for (; incomplete_len > 0; incomplete_len--) {
    *buffer = *incomplete_char; buffer++;
  }

  return bar_width;
}

/* TODO: same width as for 'total' */

int progress_token_current(SEXP private, char *bufptr, char *bufend) {
  double current = asReal(findVar(install("current"), private));
  return snprintf(bufptr, bufend - bufptr, "%i", (int) current);
}

int progress_token_total(SEXP private, char *bufptr, char *bufend) {
  double total = asReal(findVar(install("total"), private));
  return snprintf(bufptr, bufend - bufptr, "%i", (int) total);
}

/* TODO: smart time printing */

int progress_token_elapsed(SEXP private, char *bufptr, char *bufend) {
  SEXP start = findVar(install("start"), private);
  double secs = progress_elapsed_since(start);
  int ret = snprintf(bufptr, bufend - bufptr, "%is", (int) round(secs));
  return ret;
}

/* TODO: smart time printing */

int progress_token_eta(SEXP private, char *bufptr, char *bufend) {
  double percent = progress_ratio(private) * 100.0;
  double total = asReal(findVar(install("total"), private));
  double current = asReal(findVar(install("current"), private));
  SEXP start = findVar(install("start"), private);
  double elapsed_secs = progress_elapsed_since(start);
  double eta_secs = 0.0;
  int ret;
  if (percent < 100.0) {
    eta_secs = elapsed_secs * (total / current - 1.0);
  }

  if (! R_FINITE(eta_secs)) {
    ret = snprintf(bufptr, bufend - bufptr, "%ss", " ?");
  } else {
    ret = snprintf(bufptr, bufend - bufptr, "%is", (int) eta_secs);
  }

  return ret;
}

int progress_token_percent(SEXP private, char *bufptr, char *bufend) {
  double ratio = progress_ratio(private);
  int percent = (int) (ratio * 100);
  return snprintf(bufptr, bufend - bufptr, "%3i%%", percent);
}

int progress_token_rate(SEXP private, char *bufptr, char *bufend) {
  /* TODO */
  return 0;
}

int progress_token_bytes(SEXP private, char *bufptr, char *bufend) {
  /* TODO */
  return 0;
}

int progress_token_spin(SEXP private, char *bufptr, char *bufend) {
  int *spin = INTEGER(findVar(install("spin"), private));
  SEXP symbols = findVar(install("spin_symbols"), private);

  int ret = snprintf(bufptr, bufend - bufptr, "%s",
		     CHAR(STRING_ELT(symbols, *spin)));
  *spin += 1;

  if (*spin >= LENGTH(symbols)) *spin = 0;

  return ret;
}

void progress_refresh_line(SEXP private, ...) {

  int con_num = asInteger(findVar(install("stream"), private));
  Rconnection con = getConnection(con_num);

  va_list ap;
  va_start(ap, private);
  con->vfprintf(con, "%s", ap);
  va_end(ap);
}

double progress_ratio(SEXP private) {
  double current = asReal(findVar(install("current"), private));
  double total = asReal(findVar(install("total"), private));
  double ratio = current / total;
  if (ratio < 0) ratio = 0;
  if (ratio > 1) ratio = 1;
  return ratio;
}

SEXP progress_now() {
  struct timeval now;
  SEXP snow;
  PROTECT(snow = allocVector(REALSXP, 2));
  gettimeofday(&now, NULL);
  REAL(snow)[0] = now.tv_sec;
  REAL(snow)[1] = now.tv_usec;
  UNPROTECT(1);
  return snow;
}

double progress_elapsed_since(SEXP start) {
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec - REAL(start)[0] +
    (now.tv_usec - REAL(start)[1]) / 1000000.0;
}
