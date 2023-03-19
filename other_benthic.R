## Bare Substrate
### Generalized linear model

```{r}
rpn_bare <- read.csv('rpn_cover.csv') %>%
  as_tibble() %>%
  filter(group == "BARE") %>%
  mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/total_points,
    failures = total_points - total_count
  )

```

```{r}
rpn_bare
```

```{r}
bare.glm <- glm(cbind(total_count, failures) ~ location * depth, 
                family = binomial(link = "logit"), 
                data = rpn_bare)
```

```{r}
par(mfrow = c(2, 2))
plot(bare.glm)
```

```{r}
summary(bare.glm)
```

`Anova` function from the `car` package

```{r}
Anova(bare.glm, type = "III") # Type III because...
```

#### https://www.rpubs.com/daharo_calpoly/502695

```{r}
rpn_bare$groups <- interaction(rpn_bare$location, rpn_bare$depth)
```

## Create a post-hoc model

```{r}
model_bare <- with(rpn_bare, glm(cbind(total_count, failures) ~ groups, family = binomial))
```

## Determine the post-hoc comparisons of interest

```{r}
summary(glht(model_bare, 
             linfct = mcp(groups =
                            #Is the difference between these groups different from zero?
                            c("(north.sh) - (west.sh) = 0",
                              "(north.sh) - (west.dp) = 0",
                              "(north.sh) - (se.sh) = 0",
                              "(north.sh) - (se.dp) = 0",
                              "(north.dp) - (west.sh) = 0",
                              "(north.dp) - (west.dp) = 0",
                              "(north.dp) - (se.sh) = 0",
                              "(north.dp) - (se.dp) = 0",
                              "(se.sh) - (west.sh) = 0",
                              "(se.sh) - (west.dp) = 0",
                              "(se.dp) - (west.sh) = 0",
                              "(se.dp) - (west.dp) = 0"))),
        test = adjusted("holm"))
```

## CCA
### Generalized linear model

```{r}
rpn_cca <- read.csv('rpn_cover.csv') %>%
  as_tibble() %>%
  filter(group == "CCA") %>%
  mutate_at(vars(location, depth, transect, group), factor) %>%
  mutate(
    cover = total_count/total_points,
    failures = total_points - total_count
  )
```

```{r}
cca.glm <- glm(cbind(total_count, failures) ~ location * depth, 
               family = binomial(link = "logit"), 
               data = rpn_cca)
```

```{r}
par(mfrow = c(2, 2))
plot(cca.glm)
```

```{r}
summary(cca.glm)
```

`Anova` function from the `car` package

```{r}
Anova(cca.glm, type = "III") # Type III because...
```

#### https://www.rpubs.com/daharo_calpoly/502695

```{r}
rpn_cca$groups <- interaction(rpn_cca$location, rpn_cca$depth)
```

## Create a post-hoc model

```{r}
model_cca <- with(rpn_cca, glm(cbind(total_count, failures) ~ groups, family = binomial))
```

## Determine the post-hoc comparisons of interest

```{r}
summary(glht(model_cca, 
             linfct = mcp(groups =
                            #Is the difference between these groups different from zero?
                            c("(north.sh) - (west.sh) = 0",
                              "(north.sh) - (west.dp) = 0",
                              "(north.sh) - (se.sh) = 0",
                              "(north.sh) - (se.dp) = 0",
                              "(north.dp) - (west.sh) = 0",
                              "(north.dp) - (west.dp) = 0",
                              "(north.dp) - (se.sh) = 0",
                              "(north.dp) - (se.dp) = 0",
                              "(se.sh) - (west.sh) = 0",
                              "(se.sh) - (west.dp) = 0",
                              "(se.dp) - (west.sh) = 0",
                              "(se.dp) - (west.dp) = 0"))),
        test = adjusted("holm"))
```


















