
#ifndef R_PROGRESS2_H
#define R_PROGRESS2_H

#include <R_ext/Rdynload.h>

struct progress_bar {
  int current;
};

#define TICK(x) do { x->current++; SHOW(x); } while (0)
#define TICK_BY(x,i) do { x->current+=i; SHOW(x); } while (0)
#define SHOW(x) do { if (*progress__trigger) progress__show(x); } while (0)

/* --------------------------------------------------------------------- */

typedef int* progress_trigger_t;
typedef void (*progress_get_trigger_t)(progress_trigger_t *trigger);

static R_INLINE void progress_get_trigger(progress_trigger_t *trigger) {
  static progress_get_trigger_t ptr = NULL;
  if (ptr == NULL) {
    ptr = (progress_get_trigger_t)
      R_GetCCallable("progress", "progress_get_trigger");
  }
  ptr(trigger);
}

/* --------------------------------------------------------------------- */

typedef int (*progress__job_add_t)(struct progress_bar **bar,
                                   const char *name,
                                   const char *id,
                                   const char *status,
                                   int total,
                                   const char *type,
                                   const char *format,
                                   int estimate,
                                   int auto_estimate);

static R_INLINE int progress_job_add(struct progress_bar **bar,
                                     const char *name,
                                     const char *id,
                                     const char *status,
                                     int total,
                                     const char *type,
                                     const char *format,
                                     int estimate,
                                     int auto_estimate) {
  static progress__job_add_t ptr = NULL;
  if (ptr == NULL) {
    ptr = (progress__job_add_t)
      R_GetCCallable("progress", "progress_job_add");
  }
  return ptr(bar, name, id, status, total, type, format, estimate,
             auto_estimate);
}

/* --------------------------------------------------------------------- */

typedef int (*progress__show_t)(struct progress_bar *bar);

static R_INLINE int progress__show(struct progress_bar *bar) {
 static progress__show_t ptr = NULL;
  if (ptr == NULL) {
    ptr = (progress__show_t)
      R_GetCCallable("progress", "progress__show");
  }
  return ptr(bar);
}

#endif
