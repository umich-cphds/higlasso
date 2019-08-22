#include<RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
#define EPSILON 1e-10

//' @useDynLib higlasso
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
arma::field <arma::mat> generate_Xm(arma::field <arma::mat> Xm)
{
    mat Q;
    mat R;
    for (uword k = 0; k < Xm.n_elem; ++k) {
        if (!qr_econ(Q, R, Xm(k)))
            Rcpp::stop("Failed to perform QR decomposition");

        // Make Xm(k) have unit column variance
        Xm(k) = Q.each_col([](vec &v){v /= stddev(v);});
    }
    return Xm;
}

// [[Rcpp::export]]
arma::field <arma::mat> generate_Xi(arma::field <arma::mat> Xm)
{
    auto s = Xm.n_elem;
    auto n = Xm(0).n_rows;
    auto Xi = field <mat> (s, s);
    for (uword j$ = 0; j$ < s; ++j$) {
        for (uword j = 0; j < j$; ++j) {
            auto Xmk1 = Xm(j);
            auto Xmk2 = Xm(j$);

            auto n_Xmk1_col = Xmk1.n_cols;
            auto n_Xmk2_col = Xmk2.n_cols;
            auto C = cube(n, n_Xmk2_col, n_Xmk1_col);
            for (uword k = 0; k < n_Xmk1_col; ++k)
                C.slice(k) = Xmk2.each_col() % Xmk1.col(k);

            // Collapse C into n x (pj * p$) matrix */
            C.reshape(C.n_rows, C.n_cols * C.n_slices, 1);
            // Make Xi(j,j$) have unit column variance
            Xi(j, j$) = C.slice(0).each_col([](vec& v){v /= stddev(v);});
        }
    }
    return Xi;
}

double penalized_likelihood(vec residuals, field <vec> beta, field <vec> eta,
                                double sigma, double l1, double l2)
{
    double beta_reg = 0.0;
    for (uword j = 0; j < beta.n_elem; ++j)
        beta_reg += exp(-norm(beta(j), "inf") / sigma) * norm(beta(j));

    double eta_reg = 0.0;
    for (uword k = 0; k < beta.n_elem; ++ k)
        for (uword j = 0; j < k; ++j)
            eta_reg += exp(-norm(eta(j, k), "inf") / sigma) * norm(eta(j, k));

    return 0.5 * dot(residuals, residuals) + l1 * beta_reg + l2 * eta_reg;
}

mat calculate_Xtj(mat Xm_j, field <mat> Xi, field <vec> beta, field <vec> eta,
                      uword j)
{
    auto Xtj  = Xm_j;
    auto I_pj = eye <mat> (beta(j).n_elem, beta(j).n_elem);

    for (uword k = 0; k < j; ++k)
        Xtj += Xi(k, j) * diagmat(eta(k, j)) * kron(beta(k), I_pj);

    for (auto l = j + 1; l < beta.n_elem; ++l)
        Xtj += Xi(j, l) * diagmat(eta(j, l)) * kron(I_pj, beta(l));

    return Xtj;
}

vec calculate_Ytj(vec residuals, field <mat> Xm, field <mat> Xi,
                      field <vec> beta, field <vec> eta, uword j$)
{
    vec Ytj = residuals;
    Ytj += Xm(j$) * beta(j$);
    for (uword k = 0; k < beta.n_elem; ++k)
        for (uword j = 0; j < k; ++j)
            if (j$ == j || j$ == k)
                Ytj += Xi(j, k) * (eta(j, k) % kron(beta(j), beta(k)));

    return Ytj;
}

arma::vec calculate_Yt(arma::vec residuals, arma::field <arma::mat> Xi,
                                arma::field <arma::vec> beta, arma::field
                                <arma::vec> eta)
{
    arma::vec Yt = residuals;
    for (arma::uword j$ = 0; j$ < beta.n_elem; ++j$)
        for (arma::uword j = 0; j < j$; ++j)
                Yt += Xi(j, j$) * (eta(j, j$) % kron(beta(j), beta(j$)));

    return Yt;
}


vec calculate_D(vec v, double sigma)
{
    vec D = abs(v);

    double l2 = std::max(norm(D), EPSILON);
    if (l2 == EPSILON)
        D.fill(EPSILON);

    // calculate D from d_k
    double inf = norm(D, "inf");
    double w   = exp(-inf / sigma);
    for (uword k = 0; k < D.n_elem; ++k) {
        if (std::abs(D(k) - inf) < 1e-12)
            D(k) =  w * (1.0 / l2 - l2 / (D(k) * sigma));
        else
            D(k) = w / l2;
    }

    return D;
}

vec update_beta_j(mat Xtj, vec Ytj, vec beta_j, double l1, double sigma)
{
    int n = Ytj.n_elem;
    vec D = calculate_D(beta_j, sigma);
    vec C = (abs(D) - D) % beta_j;
    return inv(Xtj.t() * Xtj +  n * l1 * diagmat(D)) * (Xtj.t() * Ytj + l1 * C);
}

vec update_eta_jk(mat Xtjk, vec eta_jk, vec Yt, double l2, double sigma)
{

    int n = Yt.n_elem;
    vec D = calculate_D(eta_jk, sigma);
    vec C = (abs(D) - D) % eta_jk;
    return inv(Xtjk.t() * Xtjk + n * l2 * diagmat(D)) * (Xtjk.t() * Yt + l2 * C);
}

field <vec> initalize_eta(field <vec> eta_init, uword s)
{
    uword i = 0;
    auto eta = field <vec> (s, s);
    for (uword j$ = 0; j$ < s; ++j$)
        for (uword j = 0; j < j$; ++j)
            eta(j, j$) = eta_init(i++);

    return eta;
}

void backwards_line_search_beta(vec &new_beta, vec &residuals, field <vec> beta,
                                    field <vec> eta, field <mat> Xm, field
                                    <mat> Xi, uword j, int halfmax, double l1,
                                    double sigma)
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


    auto ppen_lik = [&](arma::vec nb)
    {
        double beta_reg = 0.0;
        for (arma::uword k = 0; k < beta.n_elem; ++k) {
            if (k != j)
                beta_reg += exp(-norm(beta(k), "inf") / sigma) * norm(beta(k));
            else
                beta_reg += exp(-norm(nb, "inf") / sigma) * norm(nb);
        }

        return 0.5 * dot(residuals, residuals) + l1 * beta_reg;
    };


    double pen = ppen_lik(beta(j));
    adjust_residuals(-beta(j));

    adjust_residuals(new_beta);
    double pen1 = ppen_lik(new_beta);

    int i = 0;
    while (pen1 > pen && i++ < halfmax)
    {
        adjust_residuals(-new_beta);
        new_beta =  0.5 * (beta(j) + new_beta);
        adjust_residuals(new_beta);
        pen1 = ppen_lik(new_beta);
    }
}


void backwards_line_search_eta(vec Yt, field <vec> new_eta, field <vec> eta,
                               field <vec> beta, field <mat> Xi, int halfmax,
                               double l2, double sigma)
{
    auto ppen_lik = [&](double omega)
    {
        vec residuals = Yt;
        double eta_reg = 0.0;
        for (uword k = 0; k < beta.n_elem; ++k) {
            for (uword j = 0; j < k; ++j) {
                vec e = omega * new_eta(j, k) + (1.0 - omega) * eta(j, k);
                residuals -= Xi(j, k) * (e % kron(beta(j), beta(k)));
                eta_reg += exp(-norm(e, "inf") / sigma) * norm(e);
            }
        }
        return 0.5 * dot(residuals, residuals) + l2 * eta_reg;
    };

    double pen0  = ppen_lik(0.0);
    double omega = 1.0;
    while (ppen_lik(omega) > pen0 && halfmax--)
        omega /= 2.0;

    for (uword k = 0; k < beta.n_elem; ++k)
        for (uword j = 0; j < k; ++j)
            eta(j, k) = omega * new_eta(j, k) + (1.0 - omega) * eta(j, k);
}


//' @export
//' @useDynLib higlasso2
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
Rcpp::List higlasso_internal(arma::vec Y, arma::field <arma::mat> Xm, arma::mat
                                 Z, arma::field <arma::vec> beta, arma::field
                                 <arma::vec> eta_init, double l1, double l2,
                                 double sigma, int maxit, int halfmax,
                                 double delta)
{
    field <vec> eta = initalize_eta(eta_init, beta.n_elem);
    field <mat> Xi  = generate_Xi(Xm);

    // initialize residuals
    vec residuals = Y;
    for (uword k = 0; k < Xm.n_elem; ++k)
        residuals -= Xm(k) * beta(k);

    for (uword j$ = 0; j$ < beta.n_elem; ++j$)
        for (uword j = 0; j < j$; ++j)
            residuals -= Xi(j, j$) * (eta(j, j$) % kron(beta(j), beta(j$)));

    // psuedo inverse of Z
    mat Zinv  = pinv(Z);
    vec alpha = vec(Z.n_cols, fill::zeros);

    double pen_lik0 = 0.0;
    double pen_lik1 = penalized_likelihood(residuals, beta, eta, sigma, l1, l2);

    int it = 0;
    do {
        pen_lik0 = pen_lik1;

        // update non regularized coefficients
        vec new_alpha = Zinv * (residuals + Z * alpha);
        residuals += Z * (alpha - new_alpha);
        alpha = new_alpha;

        //update beta
        for (uword j = 0; j < beta.n_elem; ++j) {
            mat Xtj = calculate_Xtj(Xm(j), Xi, beta, eta, j);
            vec Ytj = calculate_Ytj(residuals, Xm, Xi, beta, eta, j);
            vec new_beta = update_beta_j(Xtj, Ytj, beta(j), l1, sigma);

            // line search should go here
            backwards_line_search_beta(new_beta, residuals, beta, eta, Xm, Xi,
                                           j, halfmax, l1, sigma);
            beta(j) = new_beta;
        }


        // update eta
        vec Yt = calculate_Yt(residuals, Xi, beta, eta);
        field <vec> new_eta = eta;
        for (uword k = 0; k < beta.n_elem; ++k) {
            for (uword j = 0; j < k; ++j) {
                vec kp = kron(beta(j), beta(k));
                mat Xtjk = Xi(j, k) * diagmat(kp);
                new_eta(j, k) = update_eta_jk(Xtjk, eta(j, k), Yt, l2, sigma);
            }
        }

        backwards_line_search_eta(Yt, new_eta, eta, beta, Xi, halfmax, l2, sigma);
        residuals = Yt;
        for (uword j$ = 0; j$ < beta.n_elem; ++j$)
            for (uword j = 0; j < j$; ++j)
                residuals -= Xi(j, j$) * (eta(j, j$) % kron(beta(j), beta(j$)));


        pen_lik1 = penalized_likelihood(residuals, beta, eta, sigma, l1, l2);
        Rcpp::Rcout << pen_lik1 << "\n";
        // check penalized likelihood
    } while (it++ < maxit && (pen_lik0 - pen_lik1) / pen_lik0 >= delta);


    double mspe = dot(residuals, residuals) / (2.0 * residuals.n_elem);
    return Rcpp::List::create(Rcpp::Named("alpha") = alpha,
        Rcpp::Named("beta") = beta, Rcpp::Named("eta") = eta,
        Rcpp::Named("mspe") = mspe);
}
