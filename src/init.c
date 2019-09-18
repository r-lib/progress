
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "progress.h"

#ifdef Win32
#include <windows.h>
#define CACHE_DLL_SYM 1
#else
typedef void *HINSTANCE;
#endif

static const R_CallMethodDef CallEntries[] = {
  {NULL, NULL, 0}
};

void R_init_progress(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  R_RegisterCCallable("progress", "progress_get_trigger",
                      (DL_FUNC) &progress_get_trigger);
  R_RegisterCCallable("progress", "progress__show",
                      (DL_FUNC) &progress__show);

  R_RegisterCCallable("progress", "progress_job_add",
                      (DL_FUNC) &progress_job_add);
  R_RegisterCCallable("progress", "progress_job_destroy",
                      (DL_FUNC) &progress_job_destroy);

  if (start_thread()) warning("Cannot initialize progress bars");
}

void R_unload_progress(DllInfo *dll) {
  stop_thread();
}
