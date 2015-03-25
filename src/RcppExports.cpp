// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// tokenizecpp
Rcpp::CharacterVector tokenizecpp(SEXP x, SEXP sep, SEXP minLength, SEXP toLower, SEXP removeDigits, SEXP removePunct, SEXP removeTwitter, SEXP removeURL, SEXP removeAdditional);
RcppExport SEXP boostTest_tokenizecpp(SEXP xSEXP, SEXP sepSEXP, SEXP minLengthSEXP, SEXP toLowerSEXP, SEXP removeDigitsSEXP, SEXP removePunctSEXP, SEXP removeTwitterSEXP, SEXP removeURLSEXP, SEXP removeAdditionalSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< SEXP >::type minLength(minLengthSEXP);
    Rcpp::traits::input_parameter< SEXP >::type toLower(toLowerSEXP);
    Rcpp::traits::input_parameter< SEXP >::type removeDigits(removeDigitsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type removePunct(removePunctSEXP);
    Rcpp::traits::input_parameter< SEXP >::type removeTwitter(removeTwitterSEXP);
    Rcpp::traits::input_parameter< SEXP >::type removeURL(removeURLSEXP);
    Rcpp::traits::input_parameter< SEXP >::type removeAdditional(removeAdditionalSEXP);
    __result = Rcpp::wrap(tokenizecpp(x, sep, minLength, toLower, removeDigits, removePunct, removeTwitter, removeURL, removeAdditional));
    return __result;
END_RCPP
}
