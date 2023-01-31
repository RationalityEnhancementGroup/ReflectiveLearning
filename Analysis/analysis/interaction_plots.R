source("00_format_data.R")
library(effects)

# 3.1.2 - p11 -----------------------------------------------------------------
# 'Accelerated learning'
performance_phase <- FALSE
if(performance_phase) N <- 7:21 else N <- 3:7
df <- subset(trial_frame, index %in% N)

interaction.plot(
  x.factor = df$index,
  trace.factor = df$reflection,
  response = df$strategyscore,
  fun = mean,
  ylab = "Expected Score",
  xlab = "Trial number",
  trace.label = "Reflection",
  col = c("#3977AF", "#EF8536"),
)

# 3.2.1 - p13 -----------------------------------------------------------------
performance_phase <- TRUE
if(performance_phase) N <- 7:21 else N <- 3:7
df <- subset(trial_frame, index %in% N)


# 'Reflection improves how people plan': Reflection x Baseline
interaction.plot(
  x.factor = df$type_fs_baseline,
  trace.factor = df$reflection,
  response = df$type_fs,
  fun = mean,
  ylab = "Use of far-sighted strategies",
  xlab = "Use of far-sighted strategies in Baseline trials",
  trace.label = "Reflection",
  col = c("#3977AF", "#EF8536"),
)

# 'Reflection improves how people plan': Reflection x Baseline
interaction.plot(
  x.factor = df$type_np_baseline,
  trace.factor = df$reflection,
  response = df$type_np,
  fun = mean,
  ylab = "Use of no-planning strategy",
  xlab = "Use of no-planning strategy in Baseline trials",
  trace.label = "Reflection",
  col = c("#3977AF", "#EF8536"),
)

# 'Reflection improves how people plan': Reflection x Baseline
interaction.plot(
  x.factor = df$type_np_baseline,
  trace.factor = df$reflection,
  response = df$type_fs,
  fun = mean,
  ylab = "Use of far-sighted strategies",
  xlab = "Use of no-planning strategy in Baseline trials",
  trace.label = "Reflection",
  col = c("#3977AF", "#EF8536"),
)


# 3.3.1 - p17 -----------------------------------------------------------------

# 'Reflection is especially helpful for low performers'
load("../results/models/03_trialdata_baselinevalue/performance_phase/21_strategyscore.RData")
interact_plot(model, pred = baseline, modx = reflection)

load("../results/models/03_trialdata_baselinevalue/learning_phase/7_strategyscore.RData")
interact_plot(model, pred = baseline, modx = reflection)

load("../results/models/03_trialdata_baselinevalue/performance_phase/21_score.RData")
interact_plot(model, pred = baseline, modx = reflection)

load("../results/models/03_trialdata_baselinevalue/learning_phase/7_score.RData")
interact_plot(model, pred = baseline, modx = reflection)


# 3.3.1 - p17 -----------------------------------------------------------------
# 'Reflection is especially helpful for bad decision-makers'
load("../results/models/03_trialdata_type/performance_phase/21_score.RData")
interact_plot(model, pred = type_np_b, modx = reflection)
interact_plot(model, pred = type_ns_b, modx = reflection)

load("../results/models/03_trialdata_type/performance_phase/21_strategyscore.RData")
interact_plot(model, pred = type_np_b, modx = reflection)

# 3.3.2 - p18 ------------------------------------------------------------------
# 'The quality of planning on the previous trial moderates the effect of reflection'
load("../results/models/04_transitiondata_previousvalue/21_strategy-magnitude.RData")
interact_plot(model, pred = prevscore, modx = reflection, mod2 = prompt)

# 'Effect of the previous score'
load("../results/models/04_transitiondata_previousscore/21_type-change.RData")
interact_plot(model, pred = prevscore, modx = reflection, mod2 = prompt)

load("../results/models/04_transitiondata_previousscore/21_strategy-magnitude.RData")
interact_plot(model, pred = prevscore, modx = reflection, mod2 = prompt)

# 'Reflecting on bad strategies'
load("../results/models/04_transitiondata_type/21_strategy-magnitude.RData")
cat_plot(model, pred = prev_type_ns, modx = reflection, mod2 = prompt)

load("../results/models/04_transitiondata_type/21_clicks-change.RData")
cat_plot(model, pred = prev_type_np, modx = reflection, mod2 = prompt)


# 3.4   - p19  ------------------------------------------------------------------
# 'The frequency of change appears to decrease over time'
load("../results/models/05_transitiondata_temporal/cluster_change_delta.RData")
interact_plot(model, pred = transition_scaled, modx = reflection)

# 'The amount of improvement decreases over time'
load("../results/models/05_transitiondata_temporal/strategyscore_delta.RData")
interact_plot(model, pred = transition_scaled, modx = reflection)


# 3.5   - p20  ------------------------------------------------------------------
# 'Need for cognition does not moderate the effect of systematic metacognitive reflection'
load("../results/models/06_transitiondata_engagement/21_strategy-magnitude.RData")
interact_plot(model, pred = prevscore, modx = HE)

