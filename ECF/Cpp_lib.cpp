#include <Rcpp.h>
using namespace Rcpp;

Rcpp::CharacterVector add_chars_to_int1(Rcpp::NumericVector x){
  int n = x.size();
  Rcpp::CharacterVector BASEL_SEG(n);
  for(int i = 0; i < n; i++){
    BASEL_SEG[i] =  std::to_string(x[i]);
  }
  return BASEL_SEG;
}

// [[Rcpp::export]]
NumericMatrix cpp_initialize_na_matrix(int n, int c){
  NumericMatrix m(n,c) ;
  std::fill( m.begin(), m.end(), NumericVector::get_na() ) ;
  return m ;
}


//[[Rcpp::export]]
NumericMatrix cpp_num_matrix_with_num_matrix_add_by_col(const NumericMatrix m, const NumericMatrix n){
  
  int cl = m.cols();
  NumericMatrix out(m.rows(), cl);
  
  for(int i =0;i<cl;i++){
    out(_,i) = ((m(_,i) + n(_,i)));
  }
  
  return out;
}


//[[Rcpp::export]]
NumericMatrix cpp_num_matrix_mul_with_num_vec(const NumericMatrix m, const NumericVector v){
  
  int cl = v.size();
  NumericMatrix out(m.rows(), cl);
  
  for(int i =0;i<cl;i++){
    out(_,i) = m(_,i)*v[i];
  }
  
  return out;
}



// [[Rcpp::export]]
double get_x_before_y_long(NumericVector ts,double x, double y, int n=0, bool relative = true,double slip = 0.001) {
  if(n==0){
    n = ts.size();
  }
  if(n > ts.size()){
    n = ts.size();
  }
  NumericVector newts = clone(ts);
  if(relative){
    newts = newts/newts[0] - 1;
  }else{
    newts = newts - newts[0];
    newts = newts - slip;
  }
  
  for(int i = 0;i<n;i++){
    if(newts[i]>x){
      // std::cout << newts[i] << "  " << i <<"\n";
      return newts[i];
    }
    if(newts[i] < y){
      return newts[i];
    }
  }
  return newts[n-1];
}




// [[Rcpp::export]]
double get_x_trail(NumericVector ts,double x,int n=0,bool go_long = true) {
  if(n==0){
    n = ts.size();
  }
  if(n > ts.size()){
    n = ts.size();
  }
  NumericVector newts = clone(ts);
  newts = newts/newts[0] - 1;
  
  if(!go_long){
    newts = -newts;
  }
  
  double current_max = 0;
  
  for(int i = 0;i<n;i++){
    if(newts[i]<x){
      return newts[i];
    }
    
    if( newts[i] - current_max < x){
      return newts[i];
    }
    
    if(newts[i]>current_max){
      current_max = newts[i];
    }
    
  }
  return newts[n-1];
}




// [[Rcpp::export]]
double get_x_before_y_short(NumericVector ts,double x, double y, int n=0, bool relative = true,double slip = 0.001) {
  if(n==0){
    n = ts.size();
  }
  if(n > ts.size()){
    n = ts.size();
  }
  NumericVector newts = clone(ts);
  if(relative){
    newts = newts[0]/newts - 1;
    newts = newts - slip;
  }else{
    newts = newts[0] - newts;
  }
  
  for(int i = 0;i<n;i++){
    if(newts[i]>x){
      return newts[i];
    }
    if(newts[i] < y){
      return newts[i];
    }
  }
  return newts[n-1];
}




// [[Rcpp::export]]
NumericMatrix get_expectations_once(NumericVector ts, NumericVector thresh ,bool relative,bool go_long,double n=0){
  
  NumericMatrix out = cpp_initialize_na_matrix(thresh.size(),thresh.size());
  rownames(out) = add_chars_to_int1(thresh);
  colnames(out) = add_chars_to_int1(thresh);
  
  // thresh bo vedno ordered
  for(int j = 0;j <thresh.size();j++){
    // invert j
    for(int i = thresh.size()-1; i >=0;i--){
      if(R_IsNA(out(j,i))){
        double x = thresh[i];
        double y = -thresh[j];
        double tmp;
        if(go_long){
          tmp= get_x_before_y_long(ts,x,y,n,relative);
        }else{
          tmp = get_x_before_y_short(ts,x,y,n,relative);
        }
        out(j,i) = tmp;
      }
    }
  }
  
  return out;
}




// [[Rcpp::export]]
NumericMatrix get_expectations_once_trail(NumericVector ts, NumericVector thresh ,bool go_long,double n=0){
  
  NumericMatrix out = cpp_initialize_na_matrix(1,thresh.size());
  colnames(out) = add_chars_to_int1(thresh);
  
  // thresh bo vedno ordered
  for(int j = 0;j <thresh.size();j++){
    double tmp = get_x_trail(ts,-thresh[j],n,go_long);
    out(0,j) = tmp;
  }
  
  return out;
}



// [[Rcpp::export]]
double cpp_sd(NumericVector x) {
  int n = x.size();
  if (n <= 1) {
    return NA_REAL;
  }
  
  double mean = Rcpp::mean(x);
  double sum_sq_diff = 0.0;
  
  for(int i = 0; i < n; i++) {
    sum_sq_diff += pow(x[i] - mean, 2);
  }
  
  double variance = sum_sq_diff / (n - 1);
  return sqrt(variance);
}


// [[Rcpp::export]]
List get_expectations(IntegerVector inds,  NumericVector ts, NumericMatrix thresh ,bool relative,bool go_long,double n=10000,bool norm=true){
  // cpp_num_matrix_with_num_matrix_add_by_col()
  // main function
  
  List out(inds.size());
  NumericMatrix out1(thresh.cols());
  
  
  NumericVector tmp_ts;
  IntegerVector tmp_ind;
  NumericMatrix tmp;
  int ts_range = ts.size();
  int tmp_n;
  
  for(int i=0;i<inds.size();i++){
    if(inds[i]>= ts_range){
      continue;
    }
    tmp_n = inds[i]+n;
    if(tmp_n>=ts_range){
      tmp_n = ts_range-1;
    }
    tmp_ind = Range(inds[i],tmp_n);
    tmp_ts = ts[tmp_ind];
    NumericVector th_1;
    th_1 = thresh(i,_);
    tmp = get_expectations_once(tmp_ts,th_1,relative,go_long,n);
    out1 = cpp_num_matrix_with_num_matrix_add_by_col(out1,tmp);
    out[i] = tmp;
  }
  if(norm){
    out1 = out1/inds.size();
  }
  
  
  // rownames(out1) = add_chars_to_int1(thresh);
  // colnames(out1) = add_chars_to_int1(thresh);
  
  // out1 is mean
  
  // loop for sharp
  
  // std::cout << "here\n";
  
  NumericMatrix sharp(thresh.cols());
  
  for(int i=0;i<thresh.cols();i++){
    for(int j=0; j < thresh.cols();j++){
      // filling sharpe of i,j
      NumericVector tmp_v;
      for(int z=0;z<out.size();z++){
        NumericMatrix a = out[z];
        tmp_v.push_back(a(i,j));
      }
      double sh = mean(tmp_v)/cpp_sd(tmp_v);
      sharp(i,j) = sh;
    }
  }
  
  
  
  return List::create(
    _["sharpe"] = sharp,
    _["mean"] = out1,
    _["all"] = out
  );
  
  
}







// [[Rcpp::export]]
List get_expectations_trail(IntegerVector inds,  NumericVector ts, NumericMatrix thresh ,bool go_long,double n=10000){
  // cpp_num_matrix_with_num_matrix_add_by_col()
  // main function
  
  List out(inds.size());
  NumericMatrix out1(1,thresh.cols());
  
  
  NumericVector tmp_ts;
  IntegerVector tmp_ind;
  NumericMatrix tmp;
  int ts_range = ts.size();
  int tmp_n;
  
  for(int i=0;i<inds.size();i++){
    if(inds[i]>= ts_range){
      continue;
    }
    tmp_n = inds[i]+n;
    if(tmp_n>=ts_range){
      tmp_n = ts_range-1;
    }
    tmp_ind = Range(inds[i],tmp_n);
    tmp_ts = ts[tmp_ind];
    NumericVector th_1;
    th_1 = thresh(i,_);
    tmp = get_expectations_once_trail(tmp_ts,th_1,go_long,n);
    out1 = cpp_num_matrix_with_num_matrix_add_by_col(out1,tmp);
    out[i] = tmp;
  }
  out1 = out1/inds.size();
  
  
  NumericMatrix sharp(1,thresh.cols());
  
  
  for(int j=0; j < thresh.cols();j++){
    // filling sharpe of i,j
    NumericVector tmp_v;
    for(int z=0;z<out.size();z++){
      NumericMatrix a = out[z];
      tmp_v.push_back(a(0,j));
    }
    double sh = mean(tmp_v)/cpp_sd(tmp_v);
    sharp(0,j) = sh;
  }
  
  
  
  
  return List::create(
    _["sharpe"] = sharp,
    _["mean"] = out1,
    _["all"] = out
  );
  
  
}
