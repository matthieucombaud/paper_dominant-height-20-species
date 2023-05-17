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
  DATA_INTEGER(nb_intercept); // number of intercepts in the model (to be able to select the good sub-vector for a given macroparameter)
  DATA_IVECTOR(c_age); // vector of age per stand
  DATA_VECTOR(unitary_vector); // vector containing 1 (but not integer vector, to be able to multiply), a number of times equal to the number of stand x year (may be useful depending on how you write the function, keep it)
  
  // Data matrix of observed environmental variables
  DATA_MATRIX(m_alpha); 
  DATA_MATRIX(m_gamma);
  DATA_MATRIX(m_beta);
  
  // Data string giving information on how to compute alpha, gamma and the objective function
  // DATA_STRING(heterosce);
  
  // Parameter value transmitted from R 
  PARAMETER_VECTOR(param_interest);
  PARAMETER_VECTOR(param_error);

  
  // alpha
  vector<Type> alpha(nb_rows);
  
  if(m_alpha.cols()>0){
    
    vector<Type> param_alpha = param_interest.segment(nb_intercept,m_alpha.cols()) ;
    alpha = param_interest(0)*invlogit(m_alpha * param_alpha); // exp(m_alpha * param_alpha)/(1+exp(m_alpha * param_alpha));
    
  }else{

  alpha = param_interest(0); 
    
  }
  
  // gamma
  vector<Type> gamma(nb_rows);
  
  if(m_gamma.cols()>0){
    
    vector<Type> param_gamma = param_interest.segment(nb_intercept+m_alpha.cols(),m_gamma.cols());
    gamma = param_interest(1)*invlogit(m_gamma * param_gamma);
    
  }else{
    
    gamma = param_interest(1); 
    
  }
  
  // beta
  vector<Type> beta(nb_rows);
  
    // for beta we always put an intercept, no we never have m_beta.cols() = 0 
  vector<Type> param_beta = param_interest.segment(nb_intercept+m_alpha.cols()+m_gamma.cols(),m_beta.cols());
  beta = m_beta * param_beta ;
    
  // sigma
  Type sigma = param_error(0);
  
  //delta
  Type delta = param_error(1);
  
  // simulated height
  vector<Type> height_simulated(nb_stands);
  
  // compute simulated height
  
  int idx = 0;
  for (int stand = 0; stand != nb_stands; ++stand) {
    Type height(initial_height);
    for (int year = 0; year != c_age(stand); ++year) {
      Type increment = alpha(idx)*pow(height, beta(idx)) - gamma(idx)*height;

      height += (increment+pow(increment*increment,Type(0.5)))/Type(2.0) ; // not possible to use "if" and "max" on values derived from parameter because non differentiable, so we use thise trick to translate height=height+max(0,increment)
      
      ++idx;
    }
    
    // two lines to be limit height at 100 m, to enable convergence of some algo
    // Type correction = height - Type(100.0) ;
    // height += - (correction+pow(correction*correction,Type(0.5)))/Type(2.0) ;
    
    height_simulated(stand) = height;
  }

  // Declare the "objective function" (neg. log. likelihood)
  Type nll = Type(0);                                         
  nll = -sum(dnorm(height_observed, height_simulated, sigma*pow(height_simulated,delta),true));               // Use R-style call to normal density
  
  // provide a cap over the objective function, to avoid returning -Inf (since some optimization algo do no support it, eg "L-BFGS-B" in "optim")
  // the cap is defined as the nll when all stands remain at their initial dominant height
  // vector<Type> initial_height_vector(nb_stands);
  
  // Type nll_limit = Type(0);
  // nll_limit = -sum(dnorm(height_observed, initial_height_vector, sigma*pow(initial_height_vector,delta),true));
  
  // Type nll_correction = nll - nll_limit ;
  // nll += - (nll_correction+pow(nll_correction*nll_correction,Type(0.5)))/Type(2.0) ;
  
  // Report for the last parameter values
  REPORT(height_simulated); 
  REPORT(height_observed);
  REPORT(nll);
  REPORT(nb_stands);

  return nll;
}

