
#include "progress-api.h"

#include <pthread.h>
#include <time.h>

static pthread_t timer_thread = 0;

void *thread_fun(void *data) {
  struct timespec t = { 0, 1000 * 1000 * 200 };
  while (1) {
    progress__trigger = 1;
    nanosleep(&t, NULL);
  }
  pthread_exit(NULL);
}

int start_thread() {
  pthread_create(&timer_thread, NULL, thread_fun, NULL);
  return 0;
}

int stop_thread() {
  if (timer_thread) {
    pthread_cancel(timer_thread);
    pthread_join(timer_thread, NULL);
    timer_thread = 0;
  }
  return 0;
}
