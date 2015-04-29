
#include <Rcpp.h>
#include <cstring>
#include <cstdlib>

#include <sys/time.h>
#include <unistd.h>
#ifdef Win32
#  include <io.h>
#endif

#include <R_ext/Print.h>

namespace RProgress {

void clear_line(bool stderr, int width) {

  char spaces[width + 2];
  for (int i = 1; i <= width; i++) spaces[i] = ' ';
  spaces[0] = '\r';
  spaces[width + 1] = '\0';

  if (stderr) {
    REprintf(spaces);
  } else {
    Rprintf(spaces);
  }
}

void cursor_to_start(bool stderr) {

  if (stderr) {
    REprintf("\r");
  } else {
    Rprintf("\r");
  }
}

bool is_r_studio() {

  char *v = std::getenv("RSTUDIO");

  return v != 0 && v[0] == '1' && v[1] == '\0';
}

bool is_r_app() {

  char *v = std::getenv("R_GUI_APP_VERSION");

  return v != 0;
}

// In R Studio we should print to stdout, because priting a \r
// to stderr is buggy (reported)

bool default_stderr() {

  return !is_r_studio();
}

// If stdout is a terminal, or R Studio or OSX R.app
// On windows, stdout is a terminal, apparently

bool is_supported() {

  return isatty(1) || is_r_studio() || is_r_app();
}

// gettimeofday for windows, from
// http://stackoverflow.com/questions/10905892

#ifdef Win32

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <stdint.h> // portable: uint64_t   MSVC: __int64 

int gettimeofday(struct timeval * tp, struct timezone * tzp) {
  // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
  static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);
  
  SYSTEMTIME  system_time;
  FILETIME    file_time;
  uint64_t    time;
  
  GetSystemTime( &system_time );
  SystemTimeToFileTime( &system_time, &file_time );
  time =  ((uint64_t)file_time.dwLowDateTime )      ;
  time += ((uint64_t)file_time.dwHighDateTime) << 32;
  
  tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
  tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
  return 0;
}

#endif

double time_now() {
  struct timeval now;
  gettimeofday(&now, /* tzp = */ 0);
  return now.tv_sec + now.tv_usec / 1000000.0;
}

void replace_all(std::string& str, const std::string& from,
		 const std::string& to) {
    if (from.empty()) return;

    size_t start_pos = 0;

    while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
      str.replace(start_pos, from.length(), to);
      start_pos += to.length();
    }
}

// TODO
std::string vague_dt(double secs) {
  return "1s";
} 

// TODO
std::string pretty_bytes(long bytes) {
  return "1KB";
}

} // namespace RProgress
