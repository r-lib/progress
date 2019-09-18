
#ifndef R_PROGRESS2_INTERNAL_H
#define R_PROGRESS2_INTERNAL_H

#include <Rinternals.h>

struct progress_bar {
  /* public parts */
  int current;
  /* The rest is private */
  char *name;
  char *id;
  char *status;
  char *type;
  char *format;
  int total;
  int estimate;
  int auto_estimate;
};

typedef int* progress_trigger_t;
extern int progress__trigger;

void progress_get_trigger(progress_trigger_t *trigger);

void progress__show(struct progress_bar *bar);

/* --------------------------------------------------------------------- */

int progress_job_add(struct progress_bar **bar,
                     const char *name,
                     const char *id,
                     const char *status,
                     int total,
                     const char *type,
                     const char *format,
                     int estimate,
                     int auto_estimate);

int start_thread();
int stop_thread();

#endif
