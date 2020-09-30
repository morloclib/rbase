#ifndef __PARSE_H__
#define __PARSE_H__

#include <nlohmann/json.hpp>
using json = nlohmann::json;

#include <string>
#include <Rcpp.h>
#include <sstream>

//' Serialize R data using a JSON type string
//'
//' @param x R data of (nearly) any type
//' @param schema JSON type string
// [[Rcpp::export]]
Rcpp::String serialize(SEXP x, std::string schema);

//' Deserialize JSON data to an R native type using a JSON type string
//'
//' @param data JSON data
//' @param schema JSON type string
// [[Rcpp::export]]
SEXP deserialize(std::string data, std::string schema);

#endif
