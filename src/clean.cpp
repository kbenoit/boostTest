#include <Rcpp.h>
#include <string>
#include <boost/regex.hpp>

// [[Rcpp::depends(BH)]]

const boost::regex re_digits("[[:digit:]]");
const boost::regex re_punct("[[:punct:]]");
const std::string space0("");

std::string removeDigits(const std::string& s) {
   return boost::regex_replace(s, re_digits, space0, boost::match_default | boost::format_sed);
}

std::string removePunct(const std::string& s) {
   return boost::regex_replace(s, re_punct, space0, boost::match_default | boost::format_sed);
}


// [[Rcpp::export]]
Rcpp::DataFrame cleanCpp(std::vector<std::string> str) {
  
    int n = str.size();
    for (int i=0; i<n; i++) {
        str[i] = removeDigits(str[i]);
        str[i] = removePunct(str[i]);
    }

    return Rcpp::DataFrame::create (Rcpp::Named("text") = str);
}

