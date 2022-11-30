// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/ordinalForest.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rangerCpp
Rcpp::List rangerCpp(uint treetype, std::string dependent_variable_name, Rcpp::NumericMatrix input_data, std::vector<std::string> variable_names, uint mtry, uint num_trees, bool verbose, uint seed, uint num_threads, bool write_forest, uint importance_mode_r, uint min_node_size, std::vector<std::vector<double>>& split_select_weights, bool use_split_select_weights, std::vector<std::string>& always_split_variable_names, bool use_always_split_variable_names, std::string status_variable_name, bool prediction_mode, Rcpp::List loaded_forest, Rcpp::RawMatrix sparse_data, bool sample_with_replacement, bool probability, std::vector<std::string>& unordered_variable_names, bool use_unordered_variable_names, bool save_memory, uint splitrule_r, std::vector<std::vector<double>>& case_weights, bool use_case_weights, bool predict_all, bool keep_inbag, double sample_fraction, double alpha, double minprop, bool holdout, uint prediction_type_r, std::vector<double>& borders, bool userps);
RcppExport SEXP _ordinalForest_rangerCpp(SEXP treetypeSEXP, SEXP dependent_variable_nameSEXP, SEXP input_dataSEXP, SEXP variable_namesSEXP, SEXP mtrySEXP, SEXP num_treesSEXP, SEXP verboseSEXP, SEXP seedSEXP, SEXP num_threadsSEXP, SEXP write_forestSEXP, SEXP importance_mode_rSEXP, SEXP min_node_sizeSEXP, SEXP split_select_weightsSEXP, SEXP use_split_select_weightsSEXP, SEXP always_split_variable_namesSEXP, SEXP use_always_split_variable_namesSEXP, SEXP status_variable_nameSEXP, SEXP prediction_modeSEXP, SEXP loaded_forestSEXP, SEXP sparse_dataSEXP, SEXP sample_with_replacementSEXP, SEXP probabilitySEXP, SEXP unordered_variable_namesSEXP, SEXP use_unordered_variable_namesSEXP, SEXP save_memorySEXP, SEXP splitrule_rSEXP, SEXP case_weightsSEXP, SEXP use_case_weightsSEXP, SEXP predict_allSEXP, SEXP keep_inbagSEXP, SEXP sample_fractionSEXP, SEXP alphaSEXP, SEXP minpropSEXP, SEXP holdoutSEXP, SEXP prediction_type_rSEXP, SEXP bordersSEXP, SEXP userpsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< uint >::type treetype(treetypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type dependent_variable_name(dependent_variable_nameSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type variable_names(variable_namesSEXP);
    Rcpp::traits::input_parameter< uint >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< uint >::type num_trees(num_treesSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< uint >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< uint >::type num_threads(num_threadsSEXP);
    Rcpp::traits::input_parameter< bool >::type write_forest(write_forestSEXP);
    Rcpp::traits::input_parameter< uint >::type importance_mode_r(importance_mode_rSEXP);
    Rcpp::traits::input_parameter< uint >::type min_node_size(min_node_sizeSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<double>>& >::type split_select_weights(split_select_weightsSEXP);
    Rcpp::traits::input_parameter< bool >::type use_split_select_weights(use_split_select_weightsSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string>& >::type always_split_variable_names(always_split_variable_namesSEXP);
    Rcpp::traits::input_parameter< bool >::type use_always_split_variable_names(use_always_split_variable_namesSEXP);
    Rcpp::traits::input_parameter< std::string >::type status_variable_name(status_variable_nameSEXP);
    Rcpp::traits::input_parameter< bool >::type prediction_mode(prediction_modeSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type loaded_forest(loaded_forestSEXP);
    Rcpp::traits::input_parameter< Rcpp::RawMatrix >::type sparse_data(sparse_dataSEXP);
    Rcpp::traits::input_parameter< bool >::type sample_with_replacement(sample_with_replacementSEXP);
    Rcpp::traits::input_parameter< bool >::type probability(probabilitySEXP);
    Rcpp::traits::input_parameter< std::vector<std::string>& >::type unordered_variable_names(unordered_variable_namesSEXP);
    Rcpp::traits::input_parameter< bool >::type use_unordered_variable_names(use_unordered_variable_namesSEXP);
    Rcpp::traits::input_parameter< bool >::type save_memory(save_memorySEXP);
    Rcpp::traits::input_parameter< uint >::type splitrule_r(splitrule_rSEXP);
    Rcpp::traits::input_parameter< std::vector<std::vector<double>>& >::type case_weights(case_weightsSEXP);
    Rcpp::traits::input_parameter< bool >::type use_case_weights(use_case_weightsSEXP);
    Rcpp::traits::input_parameter< bool >::type predict_all(predict_allSEXP);
    Rcpp::traits::input_parameter< bool >::type keep_inbag(keep_inbagSEXP);
    Rcpp::traits::input_parameter< double >::type sample_fraction(sample_fractionSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type minprop(minpropSEXP);
    Rcpp::traits::input_parameter< bool >::type holdout(holdoutSEXP);
    Rcpp::traits::input_parameter< uint >::type prediction_type_r(prediction_type_rSEXP);
    Rcpp::traits::input_parameter< std::vector<double>& >::type borders(bordersSEXP);
    Rcpp::traits::input_parameter< bool >::type userps(userpsSEXP);
    rcpp_result_gen = Rcpp::wrap(rangerCpp(treetype, dependent_variable_name, input_data, variable_names, mtry, num_trees, verbose, seed, num_threads, write_forest, importance_mode_r, min_node_size, split_select_weights, use_split_select_weights, always_split_variable_names, use_always_split_variable_names, status_variable_name, prediction_mode, loaded_forest, sparse_data, sample_with_replacement, probability, unordered_variable_names, use_unordered_variable_names, save_memory, splitrule_r, case_weights, use_case_weights, predict_all, keep_inbag, sample_fraction, alpha, minprop, holdout, prediction_type_r, borders, userps));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ordinalForest_rangerCpp", (DL_FUNC) &_ordinalForest_rangerCpp, 37},
    {NULL, NULL, 0}
};

RcppExport void R_init_ordinalForest(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
