
#include <Rcpp.h>
#include <RProgress.h>
#include <unistd.h>

// [[Rcpp::export]]
Rcpp::CharacterVector test_progress(Rcpp::CharacterVector formatSEXP =
				    "[:bar] :percent ") {
  BEGIN_RCPP

  const char *format = formatSEXP[0];
  RProgress::RProgress pb(format);

  pb.tick(0);
  for (int i = 0; i < 100; i++) {
    usleep(2.0 / 100 * 1000000);
    pb.tick();
  }

  Rcpp::CharacterVector result(1);
  result[0] = "DONE";
  return result;

  END_RCPP
}

// [[Rcpp::export]]
Rcpp::CharacterVector test_units() {
  BEGIN_RCPP

  Rcpp::CharacterVector result(20);

  result[0] = RProgress::RProgress::vague_dt(0);
  result[1] = RProgress::RProgress::vague_dt(1);
  result[2] = RProgress::RProgress::vague_dt(1.1);
  result[3] = RProgress::RProgress::vague_dt(51);
  result[4] = RProgress::RProgress::vague_dt(2 * 60);
  result[5] = RProgress::RProgress::vague_dt(3 * 60 * 60);
  result[6] = RProgress::RProgress::vague_dt(3 * 60 * 60 * 24);
  result[7] = RProgress::RProgress::vague_dt(31 * 60 * 60 * 24);
  result[8] = RProgress::RProgress::vague_dt(400 * 60 * 60 * 24);
  result[9] = RProgress::RProgress::vague_dt(1500 * 60 * 60 * 24);

  result[10] = RProgress::RProgress::pretty_bytes(0);
  result[11] = RProgress::RProgress::pretty_bytes(133);
  result[12] = RProgress::RProgress::pretty_bytes(1337);
  result[13] = RProgress::RProgress::pretty_bytes(13337);
  result[14] = RProgress::RProgress::pretty_bytes(133337);
  result[15] = RProgress::RProgress::pretty_bytes(1333337);
  result[16] = RProgress::RProgress::pretty_bytes(13333337);
  result[17] = RProgress::RProgress::pretty_bytes(133333337);
  result[18] = RProgress::RProgress::pretty_bytes(1333333337);
  result[19] = RProgress::RProgress::pretty_bytes(13333333337);

  return result;

  END_RCPP
}
