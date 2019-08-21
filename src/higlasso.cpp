#include <R.h>
#include <RcppArmadillo.h>

#define EPSILON 1e-10

//' @export
//' @useDynLib higlasso2
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
arma::field <arma::mat> generate_Xm(arma::field <arma::mat> &Xm)
{
    arma::mat Q;
    arma::mat R;
    for (arma::uword k = 0; k < Xm.n_elem; ++k) {
        if (!qr_econ(Q, R, Xm(k)))
            Rcpp::stop("Failed to perform QR decomposition");

        // Make Xm(k) have unit column variance
        Xm(k) = Q.each_col([](arma::vec &v){v /= stddev(v);});
    }
    return Xm;
}

// [[Rcpp::depends(RcppArmadillo)]]
//' @export
//' @useDynLib higlasso2
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
arma::field <arma::mat> generate_Xi(arma::field <arma::mat> Xm)
{
    auto s = Xm.n_elem;
    auto n = Xm(0).n_rows;
    auto Xi = arma::field <arma::mat> (s, s);
    for (arma::uword j$ = 0; j$ < s; ++j$) {
        for (arma::uword j = 0; j < j$; ++j) {
            auto Xmk1 = Xm(j);
            auto Xmk2 = Xm(j$);

            auto n_Xmk1_col = Xmk1.n_cols;
            auto n_Xmk2_col = Xmk2.n_cols;
            auto C = arma::cube(n, n_Xmk2_col, n_Xmk1_col);
            for (arma::uword k = 0; k < n_Xmk1_col; ++k)
                C.slice(k) = Xmk2.each_col() % Xmk1.col(k);

            // Collapse C into n x (pj * p$) matrix */
            C.reshape(C.n_rows, C.n_cols * C.n_slices, 1);
            // Make Xi(j,j$) have unit column variance
            Xi(j, j$) = C.slice(0).each_col([](arma::vec& v){v /= stddev(v);});
        }
    }
    return Xi;
}

//' @export
// [[Rcpp::export]]
arma::field <arma::vec> initalize_eta(arma::field <arma::vec> eta_init,
                                          arma::uword s)
{
    arma::uword i = 0;
    auto eta = arma::field <arma::vec> (s, s);
    for (arma::uword j$ = 0; j$ < s; ++j$) {
        for (arma::uword j = 0; j < j$; ++j)
        eta(j, j$) = eta_init(i++);
    }
    return eta;
}


double beta_penalized_likelihood(arma::vec residuals, arma::field <arma::vec>
                                     beta, double sigma, double l1)
{
    double beta_reg = 0.0;
    for (arma::uword k = 0; k < beta.n_elem; ++k)
        beta_reg += exp(-norm(beta(k), "inf") / sigma) * norm(beta(k));

    return 0.5 * dot(residuals, residuals) + l1 * beta_reg;
}

double penalized_likelihood(arma::vec residuals, arma::field <arma::vec> beta,
                                arma::field <arma::vec> eta, double sigma,
                                double l1, double l2)
{
    double beta_reg = 0.0;
    for (arma::uword k = 0; k < beta.n_elem; ++k)
        beta_reg += exp(-norm(beta(k), "inf") / sigma) * norm(beta(k));

    double eta_reg = 0.0;
    for (arma::uword j$ = 0; j$ < beta.n_elem; ++ j$)
        for (arma::uword j = 0; j < j$; ++j)
            eta_reg += exp(-norm(eta(j, j$), "inf") / sigma) * norm(eta(j, j$));

    return 0.5 * dot(residuals, residuals) + l1 * beta_reg + l2 * eta_reg;
}

arma::mat calculate_Xm_tilde_j(arma::mat Xm_j, arma::field <arma::mat> Xi,
                                   arma::field <arma::vec> beta, arma::field
                                   <arma::vec> eta, arma::uword j)
{
    auto Xm_tilde_j = Xm_j;
    auto I_pj = arma::eye <arma::mat>(beta(j).n_elem, beta(j).n_elem);

    for (arma::uword k = 0; k < j; ++k)
        Xm_tilde_j += Xi(k, j) * diagmat(eta(k, j)) * kron(beta(k), I_pj);

    for (auto l = j + 1; l < beta.n_elem; ++l)
        Xm_tilde_j += Xi(j, l) * diagmat(eta(j, l)) * kron(I_pj, beta(l));

    return Xm_tilde_j;
}

arma::vec calculate_Y_tilde_j(arma::vec residuals, arma::field <arma::mat> Xm,
                                  arma::field <arma::mat> Xi, arma::field
                                  <arma::vec> beta, arma::field <arma::vec> eta,
                                  arma::uword j)
{
    arma::vec Ytj = residuals;
    residuals += Xm(j) * beta(j);
    for (arma::uword j$ = 0; j$ < beta.n_elem; ++j$)
        for (arma::uword k = 0; k < j$; ++k)
            if (j$ == j || k == j)
                residuals += Xi(k, j$) * (eta(k, j$) % kron(beta(k), beta(j$)));

    return Ytj;
}

arma::vec calculate_D(arma::vec v, double sigma)
{
    arma::vec D = abs(v);

    double l2 = std::max(norm(D), EPSILON);
    if (l2 == EPSILON)
        D.fill(EPSILON);

    // calculate D from d_k
    double inf = norm(D, "inf");
    double w   = exp(-inf / sigma);
    for (arma::uword k = 0; k < D.n_elem; ++k) {
        if (std::abs(D(k) - inf) < 1e-12)
            D(k) =  w * (1.0 / l2 - l2 / (D(k) * sigma));
        else
            D(k) = w / l2;
    }

    return D;
}

arma::vec update_beta_j(arma::mat Xtj, arma::vec Ytj, arma::vec beta_j,
                            double l1, double sigma)
{

    int n = Ytj.n_elem;
    arma::vec D = calculate_D(beta_j, sigma);
    arma::vec C = (abs(D) - D) % beta_j;
    return inv(Xtj.t() * Xtj +  n * l1 * diagmat(D)) * (Xtj.t() * Ytj + l1 * C);
}

arma::vec update_eta_jj(arma::mat Xtjj$, arma::vec eta_jj$, arma::vec Y_tilde,
                            double l2, double sigma)
{

    int n = Y_tilde.n_elem;
    arma::vec D = calculate_D(eta_jj$, sigma);
    arma::vec C = (abs(D) - D) % eta_jj$;
    return inv(Xtjj$.t() * Xtjj$ + n * l2 * diagmat(D))
             * (Xtjj$.t() * Y_tilde + l2 * C);
}


void backwards_line_search_beta(arma::vec &new_beta, arma::vec &residuals,
                                    arma::field <arma::vec> beta, arma::field
                                    <arma::vec> eta, arma::field <arma::mat> Xm,
                                    arma::field <arma::mat> Xi, arma::uword j,
                                    int maxit, double l1, double sigma)
{


    auto adjust_residuals = [&](arma::vec new_beta)
    {
        // difference
        residuals -= Xm(j) * new_beta;
        for (arma::uword j$ = 0; j$ < beta.n_elem; ++j$) {
            for (arma::uword k = 0; k < j$; ++k) {
                if (j$ == j)
                    residuals -= Xi(k, j$) * (eta(k, j$) % kron(beta(k), new_beta));
                if (k == j)
                    residuals -= Xi(k, j$) * (eta(k, j$) % kron(new_beta, beta(j$)));
            }
        }
    };

    double pen = beta_penalized_likelihood(residuals, beta, sigma, l1);
    adjust_residuals(-beta(j));

    auto tmp = beta(j);
    beta(j) = new_beta;
    adjust_residuals(new_beta);
    double pen1 = beta_penalized_likelihood(residuals, beta, sigma, l1);
    beta(j) = tmp;
    int halfmax = maxit / 2;
    int i = 0;
    while (pen1 > pen && i++ < halfmax)
    {
        adjust_residuals(-new_beta);
        new_beta =  0.5 * (beta(j) + new_beta);
        adjust_residuals(new_beta);
        tmp = beta(j);
        beta(j) = new_beta;
        pen1 = beta_penalized_likelihood(residuals, beta, sigma, l1);
        beta(j) = tmp;
    }

}



//' @export
//' @useDynLib higlasso2
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
Rcpp::List higlasso(arma::vec Y, arma::field <arma::mat> Xm, arma::mat Z,
                    arma::field <arma::vec> beta, arma::field <arma::vec>
                    eta_init, double l1, double l2, double sigma, int maxit)
{
    Y = Y - mean(Y);

    auto eta = initalize_eta(eta_init, beta.n_elem);
    auto Xi  = generate_Xi(Xm);

    arma::vec residuals = Y;
    for (arma::uword k = 0; k < Xm.n_elem; ++k)
        residuals -= Xm(k) * beta(k);

    for (arma::uword j$ = 0; j$ < beta.n_elem; ++j$)
        for (arma::uword j = 0; j < j$; ++j) {
            residuals -= Xi(j, j$) * (eta(j, j$) % kron(beta(j), beta(j$)));
        }

    // psuedo inverse of Z
    arma::mat Zinv  = pinv(Z);
    arma::vec alpha = arma::vec(Z.n_cols, arma::fill::zeros);

    double pen_lik0 = 0.0;
    double pen_lik1 = penalized_likelihood(residuals, beta, eta, sigma, l1, l2);

    int it = 0;
    do {
        pen_lik0 = pen_lik1;

        // update non regularized coefficients
        arma::vec new_alpha = Zinv * (residuals + Z * alpha);
        residuals += Z * (alpha - new_alpha);
        alpha = new_alpha;

        //update beta
        for (arma::uword j = 0; j < beta.n_elem; ++j) {
            auto Xtj = calculate_Xm_tilde_j(Xm(j), Xi, beta, eta, j);
            auto Ytj = calculate_Y_tilde_j(residuals, Xm, Xi, beta, eta, j);
            auto new_beta = update_beta_j(Xtj, Ytj, beta(j), l1, sigma);

            // line search should go here
            backwards_line_search_beta(new_beta, residuals, beta, eta, Xm, Xi,
                                           j, maxit, l1, sigma);
            beta(j) = new_beta;
        }


        for (arma::uword j$ = 0; j$ < beta.n_elem; ++j$)
            for (arma::uword j = 0; j < j$; ++j)
                residuals += Xi(j, j$) * (eta(j, j$) % kron(beta(j), beta(j$)));
        // update eta
        for (arma::uword j$ = 0; j$ < beta.n_elem; ++j$) {
            for (arma::uword j = 0; j < j$; ++j) {
                arma::vec kp = kron(beta(j), beta(j$));
                auto Xtjj$ = Xi(j, j$) * diagmat(kp);
                eta(j, j$) = update_eta_jj(Xtjj$, eta(j, j$), residuals, l2, sigma);
            }
        }

        for (arma::uword j$ = 0; j$ < beta.n_elem; ++j$)
            for (arma::uword j = 0; j < j$; ++j)
                residuals -= Xi(j, j$) * (eta(j, j$) % kron(beta(j), beta(j$)));

        pen_lik1 = penalized_likelihood(residuals, beta, eta, sigma, l1, l2);
        Rcpp::Rcout << pen_lik1 << "\n";
        // check penalized likelihood
    } while (it++ < maxit && std::abs((pen_lik1 - pen_lik0)/ pen_lik0) >= .001);


    double mspe = sqrt(dot(residuals, residuals) / residuals.n_elem);
    return Rcpp::List::create(Rcpp::Named("alpha") = alpha,
        Rcpp::Named("beta") = beta, Rcpp::Named("eta") = eta,
        Rcpp::Named("mspe") = mspe);
}
