
#include "progress.h"

SEXP progress_tick_spinner(SEXP self, SEXP private, SEXP len, SEXP tokens) {

  SEXP now = PROTECT(progress_now());
  SEXP lastupdate = findVar(install("lastupdate"), private);
  double current = asReal(findVar(install("current"), private));
  double total = asReal(findVar(install("total"), private));
  double throttle = asReal(findVar(install("throttle"), private));  
  
  current += asInteger(len);
  setVar(install("current"), ScalarInteger(current), private);

  if (REAL(now)[0] - REAL(lastupdate)[0] +
      (REAL(now)[1] - REAL(lastupdate)[1]) / 1000000.0 >= throttle) {

    int *spin = INTEGER(findVar(install("spin"), private));
    SEXP symbols = findVar(install("spin_symbols"), private);
    int con_num = asInteger(findVar(install("stream"), private));

    if (con_num == 1) {
      Rprintf("%s%s", "\r", CHAR(STRING_ELT(symbols, *spin)));

    } else {
      REprintf("%s%s", "\r", CHAR(STRING_ELT(symbols, *spin)));
    }

    *spin += 1;
    if (*spin >= LENGTH(symbols)) *spin = 0;
    
    setVar(install("lastupdate"), now, private);
  }

  if (current >= total) {
    progress_terminate(self, private);
    SEXP callback = findVar(install("callback"), private);
    if (! isNull(callback)) {
      eval(lang2(callback, self), R_GlobalEnv);
    }
  }

  UNPROTECT(1);

  return self;
}
