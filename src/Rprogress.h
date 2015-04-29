/* -*- mode: c++ -*- */

#ifndef R_PROGRESS_H
#define R_PROGRESS_H

#include <string>

#include <Rinternals.h>

// For gettimeofday implementation on windows
#ifdef Win32

// MSVC defines this in winsock2.h!?
typedef struct timeval {
    long tv_sec;
    long tv_usec;
} timeval;
int gettimeofday(struct timeval * tp, struct timezone * tzp);

#endif

namespace RProgress {

class RProgress {

 public:
  
  RProgress(std::string format = "[:bar] :percent",
	    double total = 100,
	    int width = Rf_GetOptionWidth() - 2,
	    char complete_char = '=',
	    char incomplete_char = '-',
	    bool clear = true,
	    double show_after = 0.2);
  ~RProgress();

  void set_format(std::string format)    { this->format = format;         }
  void set_total(double total)           { this->total = total;           }
  void set_width(int width)              { this->width = width;           }
  void set_complete_char(char complete_char)       { this->complete_char = complete_char;     }
  void set_incomplete_char(char incomplete_char)   { this->incomplete_char = incomplete_char; }
  void set_clear(bool clear)             { this->clear = clear;           }
  void set_show_after(double show_after) { this->show_after = show_after; }

  void tick(double len = 1);
  void update(double ratio);
  
 private:
  
  bool first;			// Is the next one the first tick?
  bool supported;		// \r supported at all?
  std::string format;		// Format template
  double total;			// Total number of ticks
  double current;		// Current number of ticks
  int width;			// Width of progress bar
  bool stderr;			// Whether to print to stderr
  char complete_char;		// Character for completed ticks
  char incomplete_char;		// Character for incomplete ticks
  bool clear;			// Should we clear the line at the end?
  double show_after;		// Delay to show/increase the progress bar
  std::string last_draw;	// Last progress bar drawn

  double start;			// Start time
  bool toupdate;		// Are we updating? (After show_after.)
  bool complete;		// Are we complete?

  void render();
  void terminate();
  double ratio();
};

void clear_line(bool stderr, int width);
void cursor_to_start(bool stderr);
bool default_stderr();
bool is_supported();
double time_now();
void replace_all(std::string& str, const std::string& from,
		 const std::string& to);
std::string vague_dt(double secs);
std::string pretty_bytes(long bytes);

} // namespace RProgress

#endif
