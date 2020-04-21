#include <Rcpp.h>
using namespace Rcpp;

int nChoosek( unsigned n, int k )
{
  if (k > n) return 0;
  if (k * 2 > n) k = n-k;
  if (k == 0) return 1;

  int result = n;
  for( int i = 2; i <= k; ++i ) {
    result *= n-i+1;
    result /= -i;
  }
  return result;
}
