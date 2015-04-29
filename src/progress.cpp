
#include <Rcpp.h>
#include <R_ext/Print.h>

#include <sstream>
#include <iomanip>
#include <cmath>

#include "RProgress.h"

namespace RProgress {

RProgress::RProgress(std::string format, double total, int width,
		     char complete_char, char incomplete_char, bool clear,
		     double show_after) :

  format(format), total(total), width(width), complete_char(complete_char),
  incomplete_char(incomplete_char), clear(clear), show_after(show_after),
  first(true), current(0), last_draw(""), start(0), toupdate(false),
  complete(false) {

  supported = is_supported();
  stderr = default_stderr();
}

void RProgress::tick(double len) {

  // Start the timer
  if (first) { start = time_now(); }

  current += len;

  // We only update after show_after secs
  toupdate = toupdate || time_now() - start > show_after;
  
  if (current >= total) complete = true;

  // Need to render at the beginning and at the end, always
  if (first || toupdate || complete) this->render();

  if (complete) this->terminate();

  first = false;
}

void RProgress::update(double ratio) {
  double goal = ratio * total;
  this->tick(goal - current);
}

void RProgress::render() {
  if (!supported) return;

  std::string str = format;
  
  std::stringstream buffer;

  double ratio_now = ratio();
  
  // percent
  buffer << std::setw(3) << ratio_now * 100 << "%";
  replace_all(str, ":percent", buffer.str());
  buffer.str(""); buffer.clear();

  // elapsed
  double elapsed_secs = time_now() - start;
  std::string elapsed = vague_dt(elapsed_secs);
  replace_all(str, ":elapsed", elapsed);

  // eta
  double percent = std::round(ratio_now * 100);
  double eta_secs = percent == 100 ? 0 :
    elapsed_secs * total / current - 1.0;
  std::string eta = std::isinf(eta_secs) ? "?s" : vague_dt(eta_secs);
  replace_all(str, ":eta", eta);

  // rate
  double rate_num = current / elapsed_secs;
  buffer << pretty_bytes(std::round(rate_num)) << "/s";
  replace_all(str, ":rate", buffer.str());
  buffer.str(""); buffer.clear();

  // current
  buffer << round(current);
  replace_all(str, ":current", buffer.str());
  buffer.str(""); buffer.clear();

  // total
  buffer << round(total);
  replace_all(str, ":total", buffer.str());
  buffer.str(""); buffer.clear();

  // bytes
  replace_all(str, ":bytes", pretty_bytes(round(current)));

  // bar
  std::string str_no_bar = str;
  replace_all(str_no_bar, ":bar", "");
  int bar_width = width - str_no_bar.length();
  if (bar_width < 0) bar_width = 0;

  double complete_len = round(bar_width * ratio_now);
  char bar[bar_width + 1];
  for (int i = 0; i < complete_len; i++) { bar[i] = complete_char; }
  for (int i = complete_len; i < bar_width; i++) {
    bar[i] = incomplete_char;
  }
  bar[bar_width] = '\0';
  replace_all(str_no_bar, ":bar", bar);

  if (last_draw != str) {
    if (last_draw.length() > str.length()) { clear_line(stderr, width); }
    cursor_to_start(stderr);
    if (stderr) {
      REprintf(str.c_str());
    } else {
      Rprintf(str.c_str());
    }
    last_draw = str;
  }
}

void RProgress::terminate() {
  if (! supported) return;
  if (clear) {
    clear_line(stderr, width);
    cursor_to_start(stderr);
  } else {
    if (stderr) {
      REprintf("\n");
    } else {
      Rprintf("\n");
    }
  }
}

double RProgress::ratio() {
  double ratio = current / total;
  if (ratio < 0) ratio = 0;
  if (ratio > 1) ratio = 1;
  return ratio;
}

} // namespace RProgress
