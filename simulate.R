suppressWarnings(suppressMessages(library(languageserver)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(randomNames)))
suppressWarnings(suppressMessages(library(readxl)))
suppressWarnings(suppressMessages(library(openxlsx)))

args <- commandArgs(trailingOnly = TRUE)

challenges <- c(
    "Festival Zero",
    "StART-up",
    "This Is Contemporary",
    "You! Yes, You!"
)

skills <- c(
    "Community Leadership",
    "Cultural Operations",
    "Data Analysis",
    "Events and Tourism",
    "Small Business Management",
    "Social Media Marketing",
    "Strategic Growth",
    "Sustainability"
)

# set parameters

if (length(args) < 3) {
    message("Player numbers not (fully) specified, simulating 42 4 8")
    n <- c(players = 42, visionaries = 4, investors = 8)
} else {
    n <- c(players = as.integer(args[1]), visionaries = as.integer(args[2]), investors = as.integer(args[3]))
}

if (length(args) > 3) {
    paste("Warning: Argument(s)", paste(args[-c(1:3)], collapse = ", "), "ignored") %>% message()
}

# assign roles

set.seed(2025)

n <- c(n, employees = unname(n["players"] - n["visionaries"] - n["investors"]))
u <- c(n["employees"] / 4) %>% ceiling()
l <- c(n["employees"] / 4) %>% floor()

players <- randomNames(n["players"], name.order = "first.last", name.sep = " ") %>% sort()

visionaries <- sample(players, n["visionaries"]) %>% sort()
investors <- setdiff(players, visionaries) %>% sample(n["investors"]) %>% sort()
employees <- setdiff(players, visionaries) %>% setdiff(investors) %>% sample(n["employees"]) %>% sort()

roles <- bind_rows(
    tibble(Name = visionaries, Role = paste("Visionary", 1:n["visionaries"])),
    tibble(Name = investors, Role = paste("Investor", 1:n["investors"])),
    tibble(Name = employees, Role = paste("Employee", 1:n["employees"]))
) %>% arrange(Name)

# pick skillsets

skillsets <- foreach(i = employees, .combine = bind_rows) %do% {
    role <- filter(roles, Name == i) %>% pull(Role)
    pick <- sample(skills, 3)
    tibble_row(Role = role, Name = i, `Skill 1` = pick[1], `Skill 2` = pick[2], `Skill 3` = pick[3])
}

# pick challenges

challenges_left <- challenges

visionary_challenge <- foreach(i = visionaries, .combine = bind_rows) %do% {
    pick <- sample(challenges_left, 1)
    challenges_left <- setdiff(challenges_left, pick)
    tibble_row(Name = i, Challenge = pick)
}

investor_challenge <- foreach(i = investors, .combine = bind_rows) %do% {
    pick <- sample(challenges, 1)
    tokens <- runif(1)
    tibble_row(Name = i, Challenge = pick, Tokens = tokens)
}

challenges_left <- rep(challenges, each = u["employees"]) %>% paste(1:u["employees"])

employee_challenge <- foreach(i = employees, .combine = bind_rows) %do% {
    pick <- sample(challenges_left, 1)
    tokens <- runif(1)
    challenges_left <- setdiff(challenges_left, pick)
    tibble_row(Name = i, Challenge = pick, Tokens = tokens)
} %>% mutate(Challenge = str_remove(Challenge, " [:digit:]$"))

teams <- bind_rows(
    left_join(investor_challenge, roles, by = "Name"),
    left_join(employee_challenge, roles, by = "Name")
) %>% select(Role, Name, Challenge, Tokens) %>%
    group_by(Challenge) %>%
    mutate(Tokens = floor((Tokens / sum(Tokens)) * 100))

visionary_tokens <- foreach(i = visionaries, .combine = bind_rows) %do% {
    challenge <- filter(visionary_challenge, Name == i) %>% pull(Challenge)
    tokens <- filter(teams, Challenge == challenge) %>% pull(Tokens) %>% sum()
    tokens_left <- 100 - tokens
    tibble(Name = i, Tokens = tokens_left)
}

teams <- left_join(visionary_challenge, roles, by = "Name") %>%
    left_join(visionary_tokens, by = "Name") %>%
    select(Role, Name, Challenge, Tokens) %>%
    bind_rows(teams)

# export simulated data

if (!dir.exists("data/")) {
    dir.create("data/")
}

export <- createWorkbook()

addWorksheet(export, "roles")
writeData(export, "roles", roles)

addWorksheet(export, "skillsets")
writeData(export, "skillsets", skillsets)

addWorksheet(export, "teams")
writeData(export, "teams", teams)

saveWorkbook(export, "data/simulated.xlsx", overwrite = TRUE)

message("Saved to data/simulated.xlsx")

# check consistency with example data

if (file.exists("data/example.xlsx")) {
    check_roles <- read_excel("data/example.xlsx", sheet = "roles") %>% identical(roles)
    check_skillsets <- read_excel("data/example.xlsx", sheet = "skillsets") %>% identical(skillsets)
    check_teams <- read_excel("data/example.xlsx", sheet = "teams") %>% identical(teams)
    if (!all(check_roles, check_skillsets, check_teams)) {
        message("Warning: Inconsistencies with data/example.xlsx")
    }
}
