
#include "progress.h"

#include <string.h>
#include <time.h>
#include <stdlib.h>

int progress__trigger = 0;

void progress_get_trigger(progress_trigger_t *trigger) {
  *trigger = (progress_trigger_t) &progress__trigger;
}

void progress__show(struct progress_bar *bar) {
  Rprintf("%i\n", (int) time(NULL));
  progress__trigger = 0;
}

int progress_job_add(struct progress_bar **bar,
                     const char *name,
                     const char *id,
                     const char *status,
                     int total,
                     const char *type,
                     const char *format,
                     int estimate,
                     int auto_estimate) {

  *bar = calloc(1, sizeof(struct progress_bar));
  if (!*bar) {
    error("Cannot allocate memory for progress bar");
  }
  (*bar)->name = name ? strdup(name) : NULL;
  (*bar)->id = id ? strdup(id) : NULL; /* TODO: generate id */
  (*bar)->status = status ? strdup(status) : NULL;
  (*bar)->type = strdup(type);
  (*bar)->format = format ? strdup(format) : NULL;
  (*bar)->estimate = estimate;
  (*bar)->auto_estimate = auto_estimate;

  return 0;
}

void progress_job_destroy(struct progress_bar *bar) {
  if (bar) free(bar);
}
