#include <RcppArmadillo.h>
#include <bits/stdc++.h>
using namespace std;

// [[Rcpp::export]]
bool graphExists(std::vector<int> a)
{
  int n = a.size();
  // Keep performing the operations until one
  // of the stopping condition is met
  while (1)
  {
    // Sort the list in non-decreasing order
    sort(a.begin(), a.end());
    reverse(a.begin(), a.end());

    // Check if all the elements are equal to 0
    if (a[0] == 0 and a[a.size() - 1] == 0)
      return true;

    // Store the first element in a variable
    // and delete it from the list
    int v = a[0];
    a.erase(a.begin() + 0);

    // Check if enough elements
    // are present in the list
    if (v > a.size())
      return false;

    // Subtract first element from next v elements
    for (int i = 0; i < v; i++)
    {
      a[i]--;

      // Check if negative element is
      // encountered after subtraction
      if (a[i] < 0)
        return false;
    }
  }
}


// [[Rcpp::export]]
double number_of_graphs_cpp(
    arma::ivec input,
    bool sorted
) {

  // typedef std::vector<int> stdvec;
  // stdvec z = arma::conv_to< stdvec >::from(input);
  // bool graphical = graphExists(z);
  //
  // if(!graphical){
  //   return -999999999999999999;
  // }


  arma::vec x = arma::conv_to<arma::vec>::from(input);

  Rcpp::List output_obj;

  int n = x.size();
  Rcpp::NumericVector y = Rcpp::wrap(x);
  double gamma2 = std::pow(arma::stddev(x),2) / (n - 1);
  double mud = arma::mean(x) / (n - 1);
  double output = 0.5 * log(2) + 0.25 -
    std::pow(gamma2,2) / (4 * std::pow(mud,2) * std::pow(1 - mud,2)) +
    (n * (n - 1) * 0.5) * (
        mud * log(mud) +
        (1 - mud)*log(1 - mud)
    );
  output += Rcpp::sum(Rcpp::lchoose(n - 1, y));

  Rcpp::IntegerVector table_x = Rcpp::table(y);
  double stable_x = Rcpp::sum(table_x);
  Rcpp::NumericVector stable_x2 = Rcpp::NumericVector::create(stable_x);
  stable_x2[0] = stable_x;
  output += Rcpp::sum(Rcpp::lfactorial(stable_x2));
  output += -Rcpp::sum(Rcpp::lfactorial(table_x));

  return output;
}

// [[Rcpp::export]]
double sum_sim(
    arma::ivec input
  ) {
  //arma::vec inputv = arma::conv_to<arma::vec>::from(input);
  double output = arma::sum(input);
  return output;
}

// [[Rcpp::export]]
arma::ivec proposal_sim(
    arma::ivec input
  ) {

  int flip = rand() % 2;
  int input_len = input.n_elem;
  arma::uvec positions = arma::randperm(input_len);
  int position0 = positions[0];
  int position1 = positions[1];
  arma::ivec output = input;

  if (flip == 0){
    output[position0] = output[position0] + 1;
    output[position1] = output[position1] + 1;
  } else {
    output[position0] = output[position0] - 1;
    output[position1] = output[position1] - 1;
  }

  return output;
}

// [[Rcpp::export]]
arma::ivec ergm_simulator_cpp(
    arma::ivec init,
    int loops,
    double theta
  ) {

  double (*sum_stat)(arma::ivec);
  sum_stat = &sum_sim;

  arma::ivec (*proposal)(arma::ivec);
  proposal = &proposal_sim;


  arma::ivec x;
  x = init;

  //arma::imat accepted(x.size(), loops);
  arma::ivec accepted(loops);

  double s_init;
  double s_star;
  arma::ivec x_star;

  Rcpp::NumericVector a0;
  float a1;

  double acceptance_prob;

  for(int i = 0; i < loops; ++i){
    s_init = sum_sim(x);
    x_star = proposal(x);
    s_star = sum_sim(x_star);

    acceptance_prob = number_of_graphs_cpp(x_star, false) -
      number_of_graphs_cpp(x, false) +
      s_star * theta - s_init * theta;

    //number_of_graphs_cpp(x, false) + s_star * theta - s_init * theta;

    a0 = Rcpp::rbinom(1, 1, std::min(1.0, std::exp(acceptance_prob)));
    a1 = a0[0];

    if(a1 == 1){
      x = x_star;
      s_init = s_star;
    }
    accepted(i) = s_init;
  }

  //Rcpp::List output_obj;
  //output_obj["accepted"] = accepted;

  return accepted;
}


// C++ implementation of the approach


// Function that returns true if
// a simple graph exists



