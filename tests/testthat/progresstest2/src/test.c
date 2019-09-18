
#include <Rinternals.h>
#include <progress-api.h>

static progress_trigger_t progress__trigger;

SEXP test0() {
  int i;
  int res = 0;
  for (i = 0; i < 2000000000; i++) {
    res += i % 2;
  }
  return ScalarInteger(res);
}

SEXP test1() {
  int i;
  int res = 0;
  struct progress_bar *bar;
  progress_job_add(&bar, "name", "id", NULL, 100, "iterator", NULL, -1, 0);
  for (i = 0; i < 2000000000; i++) {
    TICK(bar);
    res += i % 2;
  }
  progress_job_destroy(&bar);
  return ScalarInteger(res);
}

SEXP test2() {
  int i = 0;
  int res = 0;
  struct progress_bar *bar;
  progress_job_add(&bar, "name", "id", NULL, 100, "iterator", NULL, -1, 0);
  int s, final, step = 2000000000 / 1000;
  for (s = 0; s < 1000; s++) {
    final = (s + 1) * step;
    for (i = s * step; i < final; i++) {
      res += i % 2;
    }
    TICK_BY(bar, step);
  }
  progress_job_destroy(&bar);
  return ScalarInteger(res);
}

static const R_CallMethodDef CallEntries[] = {
  { "test0", (DL_FUNC) test0, 0 },
  { "test1", (DL_FUNC) test1, 0 },
  { "test2", (DL_FUNC) test2, 0 },
  { NULL, NULL, 0 }
};

void R_init_progresstest2(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  progress_get_trigger(&progress__trigger);
}
