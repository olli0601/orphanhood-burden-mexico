# Understanding registration delay: proposed comparisons

Working proposal for discussion with Oliver. The mathematical model is still to be selected.

## What we want to learn

Which characteristics are associated with slower registration? How much geographical variation remains after accounting for those characteristics?

The main outputs would be adjusted registration curves, probabilities of registration by chosen deadlines, and maps of remaining geographical differences.

## The model choices

| Model family | Intuition | Main assumption in its simplest form |
|---|---|---|
| Hazard model | Characteristics change the chance of registering in the next interval, among those still waiting. | Each characteristic has the same effect across intervals on the chosen link scale. |
| Accelerated failure-time model | Characteristics stretch or compress the waiting time. | The delay distribution keeps the same shape on a rescaled time axis. |
| Multinomial delay model | Characteristics change the probabilities of different delay categories directly. | Effects are specified separately for categories; flexible but potentially parameter-heavy. |

**Starting preference:** a hazard model, with the other families available for comparison. A hazard model can use a logit or complementary log-log link; these give different interpretations of coefficients.

## Four versions of the selected model

| Geographical structure | Basic covariates | Extended covariates |
|---|---|---|
| iid unit effects | A | B |
| Spatial plus iid effects (BYM2) | C | D |

- **Basic:** deprivation plus common time and demographic adjustments.
- **Extended:** add a small set of justified predictors, potentially ruggedness, Indigenous population share, service access and programme coverage, subject to data availability.
- **iid:** places differ, but neighbours are not assumed to resemble one another.
- **BYM2:** some remaining differences follow a geographical pattern; others are specific to individual units.

Comparing basic and extended models shows what additional information contributes. Comparing iid and BYM2 shows how conclusions depend on allowing spatial dependence.

## Essential qualifications

All models must account for **right truncation**: registrations after the observation cutoff are absent. Prefer a model including the late-registration tail, with explicit extrapolation assumptions; compare it with an analysis conditional on registration within a fixed horizon. Registry data alone cannot identify never-registered events.

Compare covariate associations, registration curves and the amount of remaining geographical variation, with uncertainty. BYM2's spatial mixing parameter alone does not measure variation explained.

These are adjusted associations, not automatically causal effects. Spatial effects and geographically patterned covariates can compete to explain the same pattern.

## Next decisions

Choose the model family and time intervals, then specify the time trends, tail assumptions and covariates. Work through the mathematics before deciding on implementation.
