#include <RcppArmadillo.h>
#include <bits/stdc++.h>
using namespace std;

// cloteaux2018sufficient A sufficient condition for graphic sequences with given largest and smallest entries, length and sum

// [[Rcpp::export]]
bool cloteaux(arma::ivec a, bool sorted){
  if(any(a < 0)){
    return false;
  }

  if(!sorted){
    a = sort(a, "descend");
  }
  int n = a.size();
  int zero_loc = n;
  int i = 0;

  bool condition = true;

  while(i < n && condition){
    i = i + 1;
    if(a[i] == 0){
      zero_loc = i;
      condition = false;
    }
  }

  zero_loc += -1;
  arma::ivec b = a.subvec(0, zero_loc);

  n = b.size();

  int a1 = b[0];
  int an = b[n-1];
  int s = arma::sum(b);

  if(a1 > n - 1){
    return false;
  }

  if(s == 0){
    return true;
  }

  if(a1 == an){
    return true;
  }

  double summand = (a1 - an) * ((n - a1 - 1) / (n*a1 - s) + (an) / (s - n * an));

  // Rcpp::Rcout << "summand";

  if(summand >= 1){
    return true;
  }

  // Rcpp::Rcout << "summand_check";

  int lhs;
  int rhs;
  arma::ivec bsubvec;

  for(int r = 0; r < n; ++r){
    bsubvec = b.subvec(0,r);
    lhs = arma::sum(bsubvec);
    rhs = (r+1)*(r);
    for(int i = r; i < n; ++i){
      rhs += std::min(r+1, b[i]);
    }
    if(lhs > rhs){
      return false;
    }
  }

  return true;
}


// [[Rcpp::export]]
double number_of_graphs_cpp(
    arma::ivec input,
    bool sorted
) {

  input = sort(input, "descend");

  if(input.is_zero()){
    return 0;
  }
  int n = input.size();

  bool graphical = cloteaux(input, true);

  double negative_infinity = -arma::datum::inf;

  if(!graphical){
    return negative_infinity;
  }


  arma::vec x = arma::conv_to<arma::vec>::from(input);

  Rcpp::List output_obj;

  Rcpp::NumericVector y = Rcpp::wrap(x);
  double gamma2 = std::pow(arma::stddev(x),2) / (n - 1);
  double mud = arma::mean(x) / (n - 1);
  double output = 0.5 * log(2) + 0.25 -
    std::pow(gamma2,2) / (4 * std::pow(mud,2) * std::pow(1 - mud,2)) +
    (n * (n - 1) * 0.5) * (
        mud * log(mud) +
        (1 - mud)*log(1 - mud)
    );

  // output += Rcpp::sum(Rcpp::lchoose(n - 1, y));


  Rcpp::IntegerVector table_x = Rcpp::table(y);
  Rcpp::NumericVector table_x2 = Rcpp::as<Rcpp::NumericVector>(table_x);

  arma::vec values_x = arma::unique(x);
  arma::vec values_y = arma::conv_to<arma::vec>::from(values_x);
  Rcpp::NumericVector values_y2 = Rcpp::wrap(values_y);

  //output += Rcpp::sum(table_x2 * values_y2);

  arma::vec a0(Rcpp::as<arma::vec>(table_x));

  Rcpp::NumericVector a1_r = Rcpp::lchoose(n - 1, values_y2);
  arma::vec a1(Rcpp::as<arma::vec>(a1_r));

  double arma_input = arma::dot(a0,a1);
  output += arma_input;

  // Rcpp::NumericVector sum_input = table_x2 * Rcpp::lchoose(n - 1, values_y2);
  // output += Rcpp::sum(sum_input);

  // Rcpp::Rcout << repeats_2;
  // Rcpp::Rcout << values_y2;
  // Rcpp::Rcout << unique_2;


  // Rcpp::NumericVector stable_x2 = Rcpp::NumericVector::create(stable_x);
  // stable_x2[0] = stable_x;
  // output += Rcpp::sum(Rcpp::lfactorial(stable_x2));
  // output += -Rcpp::sum(Rcpp::lfactorial(table_x));

  return output;
}

// [[Rcpp::export]]
double sum_sim(
    arma::ivec input
  ) {
  //arma::vec inputv = arma::conv_to<arma::vec>::from(input);
  double output = arma::sum(input);
  return output / 2;
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


  if (flip == 0){
    input[position0] += 1;
    input[position1] += 1;
  } else {
    input[position0] += - 1;
    input[position1] += - 1;
  }

  return input;
}

// [[Rcpp::export]]
Rcpp::List ergm_simulator_cpp(
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

  arma::imat accepted_mat(x.size(), loops);
  arma::ivec accepted(loops);

  double s_init;
  double s_star;
  arma::ivec x_star;

  Rcpp::NumericVector a0;
  float a1;

  double acceptance_prob;

  for(int i = 0; i < loops; ++i){
    s_init = sum_stat(x);
    x_star = proposal(x);
    s_star = sum_stat(x_star);

    acceptance_prob = number_of_graphs_cpp(x_star, false) -
      number_of_graphs_cpp(x, false) +
      s_star * theta - s_init * theta;

    a0 = Rcpp::rbinom(1, 1, std::min(1.0, std::exp(acceptance_prob)));
    a1 = a0[0];

    if(a1 == 1){
      x = x_star;
      s_init = s_star;
    }

    accepted_mat.col(i) = x;
    accepted(i) = s_init;
  }

  Rcpp::List output_obj;
  output_obj["degree_sequences"] = accepted_mat;
  output_obj["stats"] = accepted;

  return output_obj;
}


