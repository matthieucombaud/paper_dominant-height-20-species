#include <TMB.hpp>                                // Links in the TMB libraries
//#include <cmath>
//#include <algorithm>

template<class Type>
Type objective_function<Type>::operator() ()
{
  
  // using namespace Eigen;
  
  // help : https://kaskr.github.io/adcomp/Introduction.html

// this version takes into account heteroscedasticity by replacing sigma by sigma*height_simulated in the likelihood function
  
  // Data vector of observed values transmitted from R
  DATA_VECTOR(height_observed); // observed height at the observation year 
  DATA_SCALAR(initial_height); // same for all stands
  DATA_INTEGER(nb_stands); // number of stands
  DATA_INTEGER(nb_rows); // number of years*stands
  DATA_IVECTOR(c_age); // vector of age per stand

  // Data matrix of observed environmental variables
  DATA_MATRIX(m_alpha); 
  DATA_MATRIX(m_gamma);
  
  // Data string giving information on how to compute alpha, gamma and the objective function
  // DATA_STRING(heterosce);
  
  // Parameter value transmitted from R 

  PARAMETER(param_A0);
  PARAMETER(param_C0);
  PARAMETER(param_beta0);
  PARAMETER_VECTOR(param_alpha);
  PARAMETER_VECTOR(param_gamma);
  PARAMETER(param_sigma);
  PARAMETER(param_delta);
  
  // alpha
  vector<Type> alpha(nb_rows);
  //Type A0 = param_A0;
  
  if(m_alpha.cols()>0){
    
    // vector<Type> param_alpha = param_interest.segment(nb_intercept,m_alpha.cols()) ;
    // alpha = param_A0(0)/(exp(-m_alpha*param_alpha)+Type(1.0)); // 1/(exp(-m_alpha * param_alpha) + 1) = exp(m_alpha * param_alpha)/(1+exp(m_alpha * param_alpha));
    alpha = param_A0*invlogit(m_alpha*param_alpha);
    
  }else{

    alpha = param_A0*Type(0.5); 
    
  }
  
  // gamma
  vector<Type> gamma(nb_rows);
  //Type C0 = param_C0;
  
  if(m_gamma.cols()>0){
    
    // vector<Type> param_gamma = param_interest.segment(nb_intercept+m_alpha.cols(),m_gamma.cols());
    // gamma = param_C0(0)*(exp(-m_gamma*param_gamma)+Type(1.0));
    gamma = param_C0*invlogit(m_gamma*param_gamma);
    
  }else{
    
    gamma = param_C0*Type(0.5); 
    
  }
  
  // beta
  // Type beta0 = param_beta0;
  // vector<Type> beta(nb_rows);
  
    // for beta we always put an intercept, no we never have m_beta.cols() = 0 
  // vector<Type> param_beta = param_interest.segment(nb_intercept+m_alpha.cols()+m_gamma.cols(),m_beta.cols());
  // beta = param_beta0(0) ;
    
  // sigma
  // Type sigma = param_sigma;
  
  //delta
  // Type delta = param_delta;
  
  // simulated height
  vector<Type> height_simulated(nb_stands);
  
  // compute simulated height
  
  int idx = 0;
  for (int stand = 0; stand != nb_stands; ++stand) {
    Type height(initial_height);
    for (int year = 0; year != c_age(stand); ++year) {
      Type increment = alpha(idx)*pow(height, param_beta0) - gamma(idx)*height;

      height += (increment+pow(increment*increment,Type(0.5)))/Type(2.0) ; // not possible to use "if" and "max" on values derived from parameter because non differentiable, so we use thise trick to translate height=height+max(0,increment)
      
      ++idx;
    }
    
    height_simulated(stand) = height;
  }

  // Declare the "objective function" (neg. log. likelihood)
  Type nll = Type(0);                                         
  nll = -sum(dnorm(height_observed, height_simulated, param_sigma*pow(height_simulated,param_delta),true));               // Use R-style call to normal density
  
  // provide a cap over the objective function, to avoid returning -Inf (since some optimization algo do no support it, eg "L-BFGS-B" in "optim")
  // the cap is defined as the nll when all stands remain at their initial dominant height
  // vector<Type> initial_height_vector(nb_stands);
  
  // Type nll_limit = Type(0);
  // nll_limit = -sum(dnorm(height_observed, initial_height_vector, sigma*pow(initial_height_vector,delta),true));
  
  // Type nll_correction = nll - nll_limit ;
  // nll += - (nll_correction+pow(nll_correction*nll_correction,Type(0.5)))/Type(2.0) ;
  
  // Report for the last parameter values
  // REPORT(height_simulated); 
  // REPORT(height_observed);
  // REPORT(nll);
  // REPORT(nb_stands);

  return nll;
}

