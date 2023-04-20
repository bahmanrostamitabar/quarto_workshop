## ---- fig-accuracy -----------
rmsse <- readr::read_rds(here::here("results/rmsse.rds")) |> 
  mutate(msse = rmsse^2)
mase <- readr::read_rds(here::here("results/mase.rds"))
crps <- readr::read_rds(here::here("results/crps.rds")) |>
  pivot_wider(names_from = model, values_from = crps) |>
  group_by(method, h, series) |>
  mutate(across(where(is.numeric), ~ .x / naiveecdf)) |>
  ungroup()

crps <- crps |>
  pivot_longer(ensemble:tscount, names_to = "model", values_to = "crps")

accuracy <- bind_rows(
  rmsse |> mutate(measure = "msse", accuracy = rmsse^2),
  mase |> mutate(measure = "mase", accuracy = mase),
  crps |> mutate(measure = "crps", accuracy = crps),
) |>
  select(-rmsse, -mase, -crps)

# Plot of average accuracy vs week for each method for Total
acc_summary <- accuracy |>
  filter(
    series %in% c("Total", "Control areas", "Health boards"),
    method == "mint", model != "qcomb"
  ) |>
  mutate(model = case_when(
    model == "naiveecdf" ~ "Naive",
    model == "ets" ~  "ETS",
    model == "tscount" ~ "TSGLM",
    model == "iglm" ~ "GLM",
    model == "ensemble" ~ "Ensemble"
  )) |> 
  mutate(
    measure = factor(measure, levels = c("mase","msse","crps"), labels = c("MASE","MSSE","CRPS")),
    series = factor(series, levels = c("Total", "Control areas", "Health boards")),
    model = factor(model, levels = c("Naive", "ETS", "TSGLM", "GLM", "Ensemble")),
    week = factor(trunc((h - 1) / 7) + 1)
  ) |>
  group_by(week, model, measure, series) |>
  summarise(accuracy = mean(accuracy), .groups = "drop") 


acc_summary |>
  ggplot(aes(x = week, y = accuracy, group = model, col = model)) +
  geom_line() +
  geom_point(size = .5) +
  facet_grid(measure ~ series, scales = "free_y") +
  labs(y = "Average accuracy", x = "Week ahead") +
  ggthemes::scale_color_colorblind() +
  ggthemes::theme_few()
