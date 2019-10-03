#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;

#define EPSILON 1e-10
#define HALFMAX 100

//' @useDynLib higlasso
// [[Rcpp::export]]
arma::field <arma::mat> generate_Xi(arma::field <arma::mat> Xm)
{
    uword s = Xm.n_elem;
    uword n = Xm(0).n_rows;
    field <mat> Xi = field <mat> (s, s);
    for (uword j$ = 0; j$ < s; ++j$) {
        for (uword j = 0; j < j$; ++j) {
            mat Xmk1 = Xm(j);
            mat Xmk2 = Xm(j$);

            uword n_Xmk1_col = Xmk1.n_cols;
            uword n_Xmk2_col = Xmk2.n_cols;
            cube C = cube(n, n_Xmk2_col, n_Xmk1_col);
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

mat calculate_Xtj(mat Xmj, std::function <mat (uword, uword)> Xi, field <vec>
                      beta, field <vec> eta, uword j)
{
    mat Xtj  = Xmj;
    mat I_pj = eye <mat> (beta(j).n_elem, beta(j).n_elem);

    for (uword k = 0; k < j; ++k)
        Xtj += Xi(k, j) * diagmat(eta(k, j)) * kron(beta(k), I_pj);

    for (uword l = j + 1; l < beta.n_elem; ++l)
        Xtj += Xi(j, l) * diagmat(eta(j, l)) * kron(I_pj, beta(l));

    return Xtj;
}

mat calculate_Xt(std::function <mat (uword, uword)> Xi, field <vec> beta)
{
    uword p = 0;
    for (uword k = 0; k < beta.n_elem; ++k)
        for (uword j = 0; j < k; ++j)
            p += Xi(j, k).n_cols;

    mat Xt = mat(Xi(0,1).n_rows, p);

    p = 0;
    for (uword k = 0; k < beta.n_elem; ++k)
        for (uword j = 0; j < k; ++j) {
            uword p$ = p + Xi(j, k).n_cols;
            Xt.cols(p,  p$ - 1) = Xi(j, k) * diagmat(kron(beta(j), beta(k)));
            p = p$;
        }
    return Xt;
}

vec calculate_Ytj(vec residuals, field <mat> Xm, std::function
                      <mat (uword, uword)> Xi, field <vec> beta, field <vec>
                      eta, uword j$)
{
    vec Ytj = residuals;
    Ytj += Xm(j$) * beta(j$);
    for (uword k = 0; k < beta.n_elem; ++k)
        for (uword j = 0; j < k; ++j)
            if (j$ == j || j$ == k)
                Ytj += Xi(j, k) * (eta(j, k) % kron(beta(j), beta(k)));

    return Ytj;
}

vec calculate_Yt(vec residuals, std::function <mat (uword, uword)> Xi,
                     field <vec> beta, field <vec> eta)
{
    vec Yt = residuals;
    for (uword j$ = 0; j$ < beta.n_elem; ++j$)
        for (uword j = 0; j < j$; ++j)
                Yt += Xi(j, j$) * (eta(j, j$) % kron(beta(j), beta(j$)));

    return Yt;
}

vec calculate_D(vec v, double sigma)
{
    vec D = abs(v);

    if (norm(D, "inf") < EPSILON)
        D.fill(EPSILON);

    double l2_norm = norm(D);
    // calculate D from d_k
    double inf_norm = norm(D, "inf");
    double w        = exp(-inf_norm / sigma);
    for (uword k = 0; k < D.n_elem; ++k) {
        if (inf_norm - D(k) < 1e-12)
            D(k) =  w * (1.0 / l2_norm - l2_norm / (D(k) * sigma));
        else
            D(k) = w / l2_norm;
    }

    return D;
}

vec update_beta_j(mat Xtj, vec Ytj, vec beta_j, double l1, double sigma,
                      double tol, int fast)
{
    int n = Ytj.n_elem;
    vec D = calculate_D(beta_j, sigma);
    vec C = (abs(D) - D) % beta_j;

    mat M = Xtj.t() * Xtj +  n * l1 * diagmat(D);

    if (rcond(M) < tol)
        Rcpp::warning("large condition number!\n");

    if (fast)
        return solve(M, Xtj.t() * Ytj + l1 * C, solve_opts::likely_sympd +
                      solve_opts::fast);
    else
        return solve(M, Xtj.t() * Ytj + l1 * C);
}

field <vec> update_eta(mat Xt, vec Yt, field <vec> eta, double l2, double sigma,
                           double tol, int fast)
{
    int n = Yt.n_elem;

    vec e = vec(Xt.n_cols);
    vec D = vec(Xt.n_cols);
    uword p = 0;
    for (uword k = 0; k < eta.n_rows; ++k) {
        for (uword j = 0; j < k; ++j) {
            uword p$ = p + eta(j, k).n_elem;
            e.subvec(p, p$ - 1) = eta(j, k);
            D.subvec(p, p$ - 1) = calculate_D(eta(j, k), sigma);
            p = p$;
        }
    }
    vec C = (abs(D) - D) % e;

    //
    mat M = Xt.t() * Xt + n * l2 * diagmat(D);

    if (rcond(M) < tol)
        Rcpp::warning("large condition number!\n");

    if (fast)
        e = solve(M, Xt.t() * Yt + l2 * C, solve_opts::likely_sympd +
                      solve_opts::fast);
    else
        e = solve(M, Xt.t() * Yt + l2 * C);

    p = 0;
    field <vec> new_eta = eta;
    for (uword k = 0; k < eta.n_rows; ++k) {
        for (uword j = 0; j < k; ++j) {
            uword p$ = p + eta(j, k).n_elem;
            new_eta(j, k) = e.subvec(p, p$ - 1);
            p = p$;
        }
    }
    return new_eta;
}

field <vec> initalize_eta(field <vec> eta_init, uword s)
{
    uword i = 0;
    field <vec> eta = field <vec> (s, s);
    for (uword j$ = 0; j$ < s; ++j$)
        for (uword j = 0; j < j$; ++j)
            eta(j, j$) = eta_init(i++);

    return eta;
}

std::function <mat (uword, uword)> initalize_Xi(field <mat> &Xi_init, uword s)
{
    std::function <mat (uword, uword)> f = [s, &Xi_init] (uword j, uword k) ->
        mat {
            return Xi_init(j + s * k);
        };
    return f;
}

void bls_beta(vec &Ytj, vec new_beta, field <vec> &beta, field <vec> eta,
                  field <mat> Xm, std::function <mat (uword, uword)> Xi,
                  uword j$, double l1, double sigma)
{
    vec Ytj_org = Ytj;
    auto ppen_lik = [&](double omega)
    {
        vec b = omega * new_beta + (1.0 - omega) * beta(j$);

        Ytj = Ytj_org;
        Ytj -= Xm(j$) * b;
        double beta_reg = 0.0;
        for (uword k = 0; k < beta.n_elem; ++k) {
            if (j$ == k)
                beta_reg += exp(-norm(b, "inf") / sigma) * norm(b);
            else
                beta_reg += exp(-norm(beta(k), "inf") / sigma) * norm(beta(k));
            for (uword j = 0; j < k; ++j) {
                if (j$ == k)
                    Ytj -= Xi(j, k) * (eta(j, k) % kron(beta(j), b));
                if (j$ == j)
                    Ytj -= Xi(j, k) * (eta(j, k) % kron(b, beta(k)));
            }
        }
        return 0.5 * dot(Ytj, Ytj) + l1 * beta_reg;
    };


    int i = HALFMAX;
    double pen0  = ppen_lik(0.0);
    double omega = 1.0;
    while (ppen_lik(omega) > pen0 && i--)
        omega /= 2.0;


    beta(j$) = omega * new_beta + (1.0 - omega) * beta(j$);

}

// performs backtracking line search for eta. Yt and eta are modified.
void bls_eta(vec &Yt, field <vec> new_eta, field <vec> &eta, field <vec> beta,
                 std::function <mat (uword, uword)> Xi, double l2, double sigma)
{
    vec Yt_org = Yt;
    auto ppen_lik = [&](double omega)
    {
        Yt = Yt_org;
        double eta_reg = 0.0;
        for (uword k = 0; k < beta.n_elem; ++k) {
            for (uword j = 0; j < k; ++j) {
                vec e = omega * new_eta(j, k) + (1.0 - omega) * eta(j, k);
                Yt -= Xi(j, k) * (e % kron(beta(j), beta(k)));
                eta_reg += exp(-norm(e, "inf") / sigma) * norm(e);
            }
        }
        return 0.5 * dot(Yt, Yt) + l2 * eta_reg;
    };

    int i = HALFMAX;
    double pen0  = ppen_lik(0.0);
    double omega = 1.0;
    while (ppen_lik(omega) > pen0 && i--)
        omega /= 2.0;

    for (uword k = 0; k < beta.n_elem; ++k)
        for (uword j = 0; j < k; ++j)
            eta(j, k) = omega * new_eta(j, k) + (1.0 - omega) * eta(j, k);
}

//' @useDynLib higlasso
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
Rcpp::List higlasso_internal(arma::vec Y, arma::field <arma::mat> Xm,
                                 arma::field <arma::mat> Xi_init, arma::mat Z,
                                 arma::field <arma::vec> beta, arma::field
                                 <arma::vec> eta_init, double l1, double l2,
                                 double sigma, int maxit, double eps, double tol,
                                 int fast)
{
    field <vec> eta = initalize_eta(eta_init, beta.n_elem);
    auto Xi = initalize_Xi(Xi_init, beta.n_elem);

    field <vec> new_eta = eta;

    // initialize residuals
    vec residuals = Y;
    for (uword j = 0; j < Xm.n_elem; ++j)
        residuals -= Xm(j) * beta(j);

    for (uword k = 0; k < beta.n_elem; ++k)
        for (uword j = 0; j < k; ++j)
            residuals -= Xi(j, k) * (eta(j, k) % kron(beta(j), beta(k)));

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
            vec b   = update_beta_j(Xtj, Ytj, beta(j), l1, sigma, tol, fast);

            bls_beta(Ytj, b, beta, eta, Xm, Xi, j, l1, sigma);

            residuals = Ytj;
        }

        // update eta
        vec Yt = calculate_Yt(residuals, Xi, beta, eta);
        mat Xt = calculate_Xt(Xi, beta);
        new_eta = update_eta(Xt, Yt, eta, l2, sigma, tol, fast);

        bls_eta(Yt, new_eta, eta, beta, Xi, l2, sigma);
        residuals = Yt;

        pen_lik1 = penalized_likelihood(residuals, beta, eta, sigma, l1, l2);
        // check penalized likelihood
    } while (it++ < maxit && (pen_lik0 - pen_lik1) / pen_lik0 >= eps);

    if (it >= maxit && (pen_lik0 - pen_lik1) / pen_lik0 >= eps)
        Rcpp::warning("'maxit' reached without convergence.\n");
        
    double mspe = dot(residuals, residuals) / (2.0 * residuals.n_elem);
    return Rcpp::List::create(Rcpp::Named("alpha") = alpha,
        Rcpp::Named("beta") = beta, Rcpp::Named("eta") = eta,
        Rcpp::Named("mspe") = mspe);
}
