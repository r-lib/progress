
#include "progress.h"

#include <string.h>
#include <time.h>

int progress__trigger = 0;

#define PROGRESS_BAR_STACK_SIZE 100
static struct progress_bar progress_bar_stack[PROGRESS_BAR_STACK_SIZE];
static int progress_bar_stack_ptr = 0;

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

  /* TODO: more progress bars, allocate the one by one */
  if (progress_bar_stack_ptr == PROGRESS_BAR_STACK_SIZE) {
    error("Cannot create more progress bars :(");
  }

  *bar = &progress_bar_stack[progress_bar_stack_ptr++];
  (*bar)->name = name ? strdup(name) : NULL;
  (*bar)->id = id ? strdup(id) : NULL; /* TODO: generate id */
  (*bar)->status = status ? strdup(status) : NULL;
  (*bar)->type = strdup(type);
  (*bar)->format = format ? strdup(format) : NULL;
  (*bar)->estimate = estimate;
  (*bar)->auto_estimate = auto_estimate;

  return 0;
}
