################################################################################
# RFLW.R - estimate length- weight parameters for rockfish 
#
# Translated from RFLW.sas by CWM on 10/03/23
#
# updated with data through 2023 on 10/03/24 by CWM
#
# Improved to better match original process CWM 02/13/25
################################################################################

library(tidyverse)
library(gt)
library(broom)
source("functions.R")


get_data("O:/DSF/GOAB/R data/RF/")

#####
rock2020$LENGTH <- rock2020$FORK_LENGTH

library(plyr)

rf <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                  rock2004, rock2005, rock2006, rock2007, rock2008, 
                                  rock2009, rock2010, rock2011, rock2012, rock2013, 
                                  rock2014, rock2015, rock2016, rock2017, rock2018, 
                                  rock2019, rock2020, rock2021, rock2022, rock2023))


detach(package:plyr)


lw <- rf %>% 
  # Fork length collected starting in 2020, converting these fork lengths to total lengths to be uniform across all years
  mutate(
    LENGTH = case_when(
      YEAR >= 2020 ~ (LENGTH + 1.421) / (0.983), 
      TRUE ~ LENGTH
    )
  ) %>% 
  filter(!is.na(WEIGHT), !is.na(LENGTH), !(SP == 152 & PORT == 'Whittier')) %>% 
  mutate(
    logl = log(LENGTH),
    logw = log(WEIGHT)
  )

## Estimate parameters of log-log model

Params <- lw %>%
  group_by(SP) %>%
  nest() %>%  # Nest data within each SP group
  mutate(model = map(data, ~ lm(logw ~ logl, data = .x)),  # Fit models
         tidy_model = map(model, tidy)) %>%  # Extract coefficients
  unnest(tidy_model)  # Expand the results into a flat table

print(Params)


# Merge results and observations into dataset
## First get MSE
Anova <- lw %>%
  group_by(SP) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(logw ~ logl, data = .x)),  # Fit models
    anova_table = map(model, anova)  # Extract ANOVA tables
  ) %>%
  unnest(anova_table) %>%
  select(SP, MSE = `Mean Sq`)  # Rename Mean Square column as MSE

MSE <- Anova %>%
  group_by(SP) %>%
  slice_tail(n = 1) %>%  # Select last row (Residuals/Error row)
  #mutate(MSE = `Mean Sq`) %>%  # Rename Mean Square to MSE
  select(SP, MSE)

## Next, get parameters
ParamsT <- Params %>%
  pivot_wider(names_from = term, values_from = estimate) %>%  # Equivalent to PROC TRANSPOSE
  rename(a = `(Intercept)`, b = logl) %>%  
  select(SP, a, b) %>% 
  group_by(SP) %>%
  summarise(
    a = first(na.omit(a)),  # Take the first non-NA value for 'a'
    b = first(na.omit(b))   # Take the first non-NA value for 'b'
  ) %>%
  ungroup()
## then, merge parameters and MSE
ParamsTMSE <- merge(MSE, ParamsT, by = 'SP')


merged <- merge(ParamsTMSE, lw, by = 'SP') %>% 
  mutate(
    predlogw = a + b * logl,
    UpperBoundLogW = predlogw + 2.5 * sqrt(MSE),
    LowerBoundLogW = predlogw - 2.5 * sqrt(MSE),
    predw = exp(a) * LENGTH^b,
    UBndW = exp(UpperBoundLogW),
    LBndW = exp(LowerBoundLogW)
  ) %>% 
  select(
    SP, a, b, MSE, YEAR, PAGE, LINE, SEX, LENGTH, WEIGHT, SPECIES, PORT, logl, logw, predw, LBndW, UBndW, predlogw
  )

# Length-Weight plots
ggplot(data = merged %>% filter(!(SP %in% c(144, 168, 169))), # exclude unspecified pel, dem, and slope rf
       ) +
  geom_point(aes(x = logl, y = logw), alpha = 0.2, color =  'blue') +
  geom_line(aes(x = logl, y = predlogw)) +
  facet_wrap(~SP)

# Plot length-weight in number space
ggplot(data = merged %>% filter(!(SP %in% c(144, 168, 169))), # exclude unspecified pel, dem, and slope rf
) +
  geom_point(aes(x = LENGTH, y = WEIGHT), alpha = 0.2, color =  'blue') +
  geom_line(aes(x = LENGTH, y = predw)) +
  # Add upper and lower 95% confidence interval
  geom_line(aes(x = LENGTH, y = UBndW), alpha = 0.4, color = 'red') +
  geom_line(aes(x = LENGTH, y = LBndW), alpha = 0.4, color = 'red') +
  facet_wrap(~SP)




###############
rmse_intercept <- Params %>%
  mutate(
    RMSE = sqrt(mean(residuals(model)^2)),
    Intercept = coef(model)[1],
    logl_coef = coef(model)['logl']
  ) %>% 
  select(-model)

gt(rmse_intercept)


##
lw_black <- lw %>%
  filter(SP == 142)


ggplot(data = lw_black, aes(x = LENGTH, y = WEIGHT)) +
  geom_point() +
  scale_x_continuous(
    name = "LENGTH",
    breaks = seq(0, max(lw_black$LENGTH), by = 1),
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_continuous(
    name = "WEIGHT",
    breaks = seq(0, max(lw_black$WEIGHT), by = 1),
    labels = scales::number_format(accuracy = 1)
  ) +
  geom_text(aes(label = as.character(YEAR)), hjust = 0.5, vjust = -0.5, size = 3) +
  theme_minimal()

