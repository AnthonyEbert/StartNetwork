// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// graphExists
bool graphExists(std::vector<int> a);
RcppExport SEXP _StartNetwork_graphExists(SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(graphExists(a));
    return rcpp_result_gen;
END_RCPP
}
// number_of_graphs_cpp
double number_of_graphs_cpp(arma::ivec input, bool sorted);
RcppExport SEXP _StartNetwork_number_of_graphs_cpp(SEXP inputSEXP, SEXP sortedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::ivec >::type input(inputSEXP);
    Rcpp::traits::input_parameter< bool >::type sorted(sortedSEXP);
    rcpp_result_gen = Rcpp::wrap(number_of_graphs_cpp(input, sorted));
    return rcpp_result_gen;
END_RCPP
}
// sum_sim
double sum_sim(arma::ivec input);
RcppExport SEXP _StartNetwork_sum_sim(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::ivec >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_sim(input));
    return rcpp_result_gen;
END_RCPP
}
// proposal_sim
arma::ivec proposal_sim(arma::ivec input);
RcppExport SEXP _StartNetwork_proposal_sim(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::ivec >::type input(inputSEXP);
    rcpp_result_gen = Rcpp::wrap(proposal_sim(input));
    return rcpp_result_gen;
END_RCPP
}
// ergm_simulator_cpp
arma::ivec ergm_simulator_cpp(arma::ivec init, int loops, double theta);
RcppExport SEXP _StartNetwork_ergm_simulator_cpp(SEXP initSEXP, SEXP loopsSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::ivec >::type init(initSEXP);
    Rcpp::traits::input_parameter< int >::type loops(loopsSEXP);
    Rcpp::traits::input_parameter< double >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(ergm_simulator_cpp(init, loops, theta));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_StartNetwork_graphExists", (DL_FUNC) &_StartNetwork_graphExists, 1},
    {"_StartNetwork_number_of_graphs_cpp", (DL_FUNC) &_StartNetwork_number_of_graphs_cpp, 2},
    {"_StartNetwork_sum_sim", (DL_FUNC) &_StartNetwork_sum_sim, 1},
    {"_StartNetwork_proposal_sim", (DL_FUNC) &_StartNetwork_proposal_sim, 1},
    {"_StartNetwork_ergm_simulator_cpp", (DL_FUNC) &_StartNetwork_ergm_simulator_cpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_StartNetwork(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
