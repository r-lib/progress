
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Connections.h>

#include <sys/time.h>
#include <ctype.h>

SEXP progress_tick(SEXP self, SEXP private, SEXP len, SEXP tokens);
SEXP progress_update(SEXP self, SEXP private, SEXP ratio, SEXP tokens);
SEXP progress_terminate(SEXP self, SEXP private);
SEXP progress_render(SEXP self, SEXP private, SEXP tokens);

void progress_refresh_line(SEXP private, const char *str);
void progress_clear_line(SEXP private);
double progress_ratio(SEXP private);
int progress_add_custom_tokens(SEXP tokens, char * bufptr, char *bufend,
			       const char **format);
SEXP progress_list_elem(SEXP list, const char *str, int len);

SEXP progress_now();
double progress_elapsed_since(SEXP start);
int progress_pretty_time(double dt, char *buffer, int n);
int progress_pretty_bytes(double bytes, char *buffer, int n,
			  const char *suffix);

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

/* Predefined styles */

SEXP progress_tick_spinner(SEXP self, SEXP private, SEXP len, SEXP tokens);
