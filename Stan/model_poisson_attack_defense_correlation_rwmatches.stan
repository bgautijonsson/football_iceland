/**
 * Time-Varying Football Team Strength Model
 * 
 * This model estimates dynamic team strengths in football (soccer) using match results.
 * Key features:
 * - Separate offensive and defensive capabilities for each team
 * - Time-varying team strengths using random walks
 * - Hierarchical structure for team parameters
 * - Bivariate Poisson distribution for correlated goal scoring
 * - Accounts for varying time intervals between matches
 * - Home/away advantage effects
 * 
 * Model Structure:
 * 1. Team Strength Components:
 *    - Offense (attack strength)
 *    - Defense (defensive strength)
 *    Each follows a random walk process
 * 
 * 2. Time Evolution:
 *    For each team k and time t:
 *    Random walk: x_t = x_{t-1} + σ * √Δt * z_t
 *    where Δt is the time between matches
 * 
 * 3. Scoring Model:
 *    Goals follow a bivariate Poisson distribution with correlation
 *    Parameters depend on:
 *    - Team offensive and defensive strengths
 *    - Home advantage
 *    - Team-specific correlation effects
 * 
 * 4. Correlation Structure:
 *    Uses trivariate reduction method: Y₁ = X₁ + X₃, Y₂ = X₂ + X₃
 *    where X₁, X₂, X₃ are independent Poisson variables
 *    The shared component λ₃ is parameterized to ensure correlation
 *    is independent of scoring rates
 */

functions {

  /**
   * Bivariate Poisson log-PMF with log-scale parameters
   * 
   * Implements the trivariate reduction method for bivariate Poisson:
   * Y1 = X1 + X3, Y2 = X2 + X3
   * where X1, X2, X3 are independent Poisson variables
   * 
   * @param x Array of length 2 containing the observed counts [y1, y2]
   * @param log_lambda1 Log of the rate parameter for the first margin
   * @param log_lambda2 Log of the rate parameter for the second margin
   * @param log_lambda3 Log of the rate parameter for the shared component
   * @return The log probability mass function evaluated at x
   */
  real poisson_2d_log_lpmf(
    array[] int x,
    real log_lambda1,
    real log_lambda2,
    real log_lambda3
  ) {
    int x1 = x[1];
    int x2 = x[2];

    // If either count is negative, this probability is 0 
    if (x1 < 0 || x2 < 0)
      return negative_infinity();

    real lambda1 = exp(log_lambda1);
    real lambda2 = exp(log_lambda2);
    real lambda3 = exp(log_lambda3);

    // Summation index upper bound
    int K = min(x1, x2);

    vector[K + 1] log_terms;

    for (k in 0:K) {
      log_terms[k + 1] =
        k * log_lambda3
        + (x1 - k) * log_lambda1
        + (x2 - k) * log_lambda2
        - ( lgamma(k + 1)
          + lgamma(x1 - k + 1)
          + lgamma(x2 - k + 1) );
    }

    return
    - (lambda1 + lambda2 + lambda3)
    + log_sum_exp(log_terms);
  }

  /**
   * Random number generator for bivariate Poisson distribution
   * 
   * Generates correlated count data using the trivariate reduction method
   * 
   * @param lambda1 Rate parameter for first margin
   * @param lambda2 Rate parameter for second margin
   * @param lambda3 Rate parameter for shared component (controls correlation)
   * @return Array of length 2 containing the generated counts
   */
  array[] int poisson_2d_log_rng(real lambda1, real lambda2, real lambda3) {
    int y1 = poisson_log_rng(lambda1);
    int y2 = poisson_log_rng(lambda2);
    int y3 = poisson_log_rng(lambda3);

    array[2] int out;
    out[1] = y1 + y3;
    out[2] = y2 + y3;
    return out;
  }
}

/**
 * Input Data
 * 
 * The model requires match results and timing information:
 * - Match outcomes (goals scored by each team)
 * - Team identifiers
 * - Round/match timing information
 * - Season boundary indicators
 * - Prediction-related data
 */
data {
  int<lower=0> K;                     // Number of teams
  int<lower=0> N;                     // Number of games
  int<lower=0> N_rounds;              // Number of rounds
  array[N] int<lower=1, upper=K> team1;    // Team 1 ID for each game (home)
  array[N] int<lower=1, upper=K> team2;    // Team 2 ID for each game (away)
  array[N] int<lower=1, upper = N_rounds> round1;  // Round ID for each game
  array[N] int<lower=1, upper = N_rounds> round2;  // Round ID for each game
  matrix<lower = 0>[K, N_rounds] time_between_matches;  // Time difference between matches for each team and each round
  array[N] int<lower=0> goals1;       // Goals scored by   team 1 (home)
  array[N] int<lower=0> goals2;       // Goals scored by team 2 (away)
  
  // Prediction data
  int<lower = 0> N_top_teams;
  array[N_top_teams] int<lower=0> top_teams;
  vector[N_top_teams] time_to_next_games;

  int<lower=0> N_pred;                // Number of games to predict
  array[N_pred] int<lower=1, upper=K> team1_pred;  // Team 1 ID for each prediction game
  array[N_pred] int<lower=1, upper=K> team2_pred;  // Team 2 ID for each prediction game
  vector[N_pred] pred_timediff1;
  vector[N_pred] pred_timediff2;
}

transformed data {
  array[2, N] int goals1_2;
  for (n in 1:N) {
    goals1_2[1, n] = goals1[n];
    goals1_2[2, n] = goals2[n];
  }
  matrix[K, N_rounds] delta_t;
  for (k in 1:K) {
    for (n in 1:N_rounds) {
      delta_t[k, n] = sqrt(time_between_matches[k, n]);
    }
  }

  vector[N_top_teams] delta_t_top = sqrt(time_to_next_games);
  vector[N_pred] pred_delta_t1 = sqrt(pred_timediff1);
  vector[N_pred] pred_delta_t2 = sqrt(pred_timediff2);
}

/**
 * Parameters
 * 
 * Hierarchical structure for team-specific parameters:
 * - Each team parameter has a population mean and scale
 * - Team-specific values are drawn from these populations
 * - This sharing of information helps with teams having fewer matches
 */
parameters {
  // Offensive parameters with hierarchical structure
  sum_to_zero_vector[K] off0;              // Initial offensive strengths
  array[N_rounds] sum_to_zero_vector[K] z_off;  // Innovations for offense
  
  vector[K] z_sigma_off;                   // Scale of random walk
  real<lower = 0> scale_sigma_off;
  real mean_sigma_off;

  // Defensive parameters
  sum_to_zero_vector[K] def0;              // Initial defensive strengths
  array[N_rounds] sum_to_zero_vector[K] z_def;  // Innovations for defense
  
  vector[K] z_sigma_def;                   // Scale of random walk
  real<lower = 0> scale_sigma_def;
  real mean_sigma_def;

  // Home advantage parameters - non-centered parameterization
  vector<lower = 0>[K] home_advantage_off;
  vector<lower = 0>[K] home_advantage_def;
  // Correlation parameter
  real alpha_mu3;

  vector[N_pred] z_off_pred;
  vector[N_pred] z_def_pred;

  vector[N_top_teams] z_off_top;
  vector[N_top_teams] z_def_top;
}

/**
 * Transformed Parameters
 * 
 * Implements the core time series evolution:
 * 1. Transforms parameters to appropriate scales
 * 2. Evolves team strengths through time using random walks
 */
transformed parameters {
  // Offensive parameters over time
  array[N_rounds] vector[K] offense;        // Offensive strengths for each round
  vector<lower = 0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);

  // Defensive parameters over time
  array[N_rounds] vector[K] defense;        // Defensive strengths for each round
  vector<lower = 0>[K] sigma_def = exp(z_sigma_def);

  // Initialize first round
  offense[1, ] = off0;
  defense[1, ] = def0;

  // Remaining rounds follow random walk process
  for (i in 2:N_rounds) {
    offense[i, ] = offense[i - 1, ] + delta_t[ , i] .* sigma_off .* z_off[i, ];
    defense[i, ] = defense[i - 1, ] + delta_t[ , i] .* sigma_def .* z_def[i, ];
  }
}

/**
 * Model
 * 
 * Specifies:
 * 1. Prior distributions
 * 2. Hierarchical structure
 * 3. Time series evolution
 * 4. Likelihood of observed scores
 */
model {
  // Priors for offensive parameters
  off0 ~ std_normal();
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
  }
  
  // Priors for defensive parameters
  def0 ~ std_normal();  
  for (i in 1:N_rounds) {
    z_def[i] ~ std_normal();
  }

  z_off_top ~ std_normal();
  z_def_top ~ std_normal();
  z_off_pred ~ std_normal();
  z_def_pred ~ std_normal();
  
  // Priors for volatility parameters
  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(1);
  mean_sigma_off ~ normal(log(0.02), 2);

  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(1);
  mean_sigma_def ~ normal(log(0.02), 2);
  
  // Priors for home advantage
  home_advantage_off ~ std_normal();
  home_advantage_def ~ std_normal();

  // Priors for effect of goals scored and conceded in the league
  alpha_mu3 ~ std_normal();

  // Likelihood with correlation structure
  /**
   * The correlation between goals is modeled through the shared component λ₃
   * in the bivariate Poisson. To ensure the correlation is independent of the
   * scoring rates, we parameterize:
   * log(λ₃) = log(ρ) + 0.5 * (log(λ₁) + log(λ₂))
   * where ρ = inv_logit(α + α₁ + α₂) ∈ (0,1)
   * 
   * This gives λ₃ = ρ * √(λ₁λ₂), making the correlation structure
   * independent of the marginal rates λ₁, λ₂
   */
  for (n in 1:N) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;
    // Home team
    off[1] = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    def[1] = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];

    // Away team
    off[2] = offense[round2[n], team2[n]];
    def[2] = defense[round2[n], team2[n]];

    mu[1] = off[1] - def[2];
    mu[2] = off[2] - def[1];
    
    vector[2] lambda = exp(mu);

    real logit_rho = alpha_mu3;
    real mu3 = log_inv_logit(logit_rho) + 0.5 * (mu[1] + mu[2]);
    goals1_2[ , n] ~ poisson_2d_log(mu[1], mu[2], mu3);
    
  }
}

/**
 * Generated Quantities
 * 
 * Computes:
 * 1. Current team strengths
 * 2. Predictions for future matches
 * 3. Various team strength summaries
 * 4. Home/away performance metrics
 */
generated quantities {

  // Total home advantage
  vector[K] home_advantage_tot = home_advantage_off + home_advantage_def;

  // Current team strengths
  vector[N_top_teams] cur_offense_away = offense[N_rounds, top_teams] + 
    delta_t_top .* sigma_off[top_teams] .* z_off_top;

  vector[N_top_teams] cur_defense_away = defense[N_rounds, top_teams] + 
    delta_t_top .* sigma_def[top_teams] .* z_def_top;

  vector[N_top_teams] cur_strength_away = cur_offense_away + cur_defense_away;

  // Current team strengths on home field
  vector[N_top_teams] cur_offense_home = cur_offense_away + home_advantage_off[top_teams]; 
  vector[N_top_teams] cur_defense_home = cur_defense_away + home_advantage_def[top_teams];
  vector[N_top_teams] cur_strength_home = cur_offense_home + cur_defense_home;

  vector[N_top_teams] cur_offense = (cur_offense_away + cur_offense_home) / 2;
  vector[N_top_teams] cur_defense = (cur_defense_away + cur_defense_home) / 2;
  vector[N_top_teams] cur_strength = cur_offense + cur_defense;

  array[N_pred, 2] int<lower=0> goals_pred;          // Predicted goals for team 1
  array[N_pred] int<lower = 0> goals1_pred;
  array[N_pred] int<lower = 0> goals2_pred;
  array[N_pred] int goal_diff_pred;       // Predicted goal difference
  array[N_pred] int<lower=0> total_goals_pred;     // Predicted total goals

  // Generate predictions for future games
  for (n in 1:N_pred) {
    vector[2] off;
    vector[2] def;
    vector[2] mu;
    // Home team
    off[1] = offense[N_rounds, team1_pred[n]] + 
      pred_delta_t1[n] .* sigma_off[team1_pred[n]] .* z_off_pred[n] + 
      home_advantage_off[team1_pred[n]];

    def[1] = defense[N_rounds, team1_pred[n]] + 
      pred_delta_t1[n] .* sigma_def[team1_pred[n]] .* z_def_pred[n] +
      home_advantage_def[team1_pred[n]];

    // Away team
    off[2] = offense[N_rounds, team2_pred[n]] + 
      pred_delta_t2[n] .* sigma_off[team2_pred[n]] .* z_off_pred[n];

    def[2] = defense[N_rounds, team2_pred[n]] + 
      pred_delta_t2[n] .* sigma_def[team2_pred[n]] .* z_def_pred[n];
      
    mu[1] = off[1] - def[2];
    mu[2] = off[2] - def[1];
    
    vector[2] lambda = exp(mu);

    real logit_rho = alpha_mu3;
    real mu3 = log_inv_logit(logit_rho) + 0.5 * (mu[1] + mu[2]);
    
    goals_pred[n] = poisson_2d_log_rng(mu[1], mu[2], mu3);
    goals1_pred[n] = goals_pred[n, 1];
    goals2_pred[n] = goals_pred[n, 2];
    goal_diff_pred[n] = goals1_pred[n] - goals2_pred[n];
    total_goals_pred[n] = goals1_pred[n] + goals2_pred[n];
  }
}

