suppressWarnings(suppressMessages(library(languageserver)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(foreach)))
suppressWarnings(suppressMessages(library(readxl)))

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

# import data

if (length(args) < 1) {
    message("Data not specified, crunching data/example.xlsx")
    import <- "data/example.xlsx"
} else {
    import <- paste0("data/", args[1]) %>%
        str_replace("data/data/", "data/")
}

roles <- read_excel(import, "roles")
skillsets <- read_excel(import, "skillsets")
teams <- read_excel(import, "teams")

if (length(args) > 1) {
    paste("Warning: Argument(s)", paste(args[-1], collapse = ", "), "ignored") %>% message()
}

# retrieve parameters

n <- filter(roles, grepl("Employee", Role)) %>% nrow()
u <- c(n / 4) %>% ceiling()
l <- c(n / 4) %>% floor()

# retrieve roles

visionaries <- filter(roles, grepl("Visionary", Role)) %>% pull(Name)
investors <- filter(roles, grepl("Investor", Role)) %>% pull(Name)
employees <- filter(roles, grepl("Employee", Role)) %>% pull(Name)

# calculate team points

team_results <- foreach(i = challenges, .combine = bind_rows) %do% {
    team_investors <- filter(teams, grepl("Investor", Role), Challenge == i) %>% pull(Name)
    team_employees <- filter(teams, grepl("Employee", Role), Challenge == i) %>% pull(Name)
    team_skillsets <- filter(skillsets, Name %in% team_employees)
    best_skills <- select(team_skillsets, -Name, -Role) %>%
        pivot_longer(c(`Skill 1`, `Skill 2`, `Skill 3`), names_to = "Rank", values_to = "Skill") %>%
        mutate(Rank = as.integer(str_remove_all(Rank, "Skill "))) %>%
        group_by(Skill) %>%
        slice_min(Rank, with_ties = FALSE) %>%
        pull(Rank) %>%
        setNames(skills)
    if (length(team_employees) >= l && length(team_employees) <= u) {
        points <- sum(best_skills ^ -1) %>% round(2)
    } else {
        points <- 0
    }
    tibble(Challenge = i, Investors = length(team_investors), Employees = length(team_employees), Points = points)
} %>% arrange(-Points)

# assign scores

compute_scores <- function(x) {
    foreach(i = x, .combine = bind_rows) %do% {
        challenge <- filter(teams, Name == i) %>% pull(Challenge)
        team_points <- filter(team_results, Challenge == challenge) %>% pull(Points)
        team_investors <- filter(teams, grepl("Investor", Role), Challenge == challenge) %>% pull(Name)
        tokens <- filter(teams, Name == i) %>% pull(Tokens)
        player_points <- team_points * (1 + length(team_investors)) * tokens
        tibble_row(Name = i, Challenge = challenge, Tokens = tokens, Points = player_points)
    } %>% mutate(Score = round(Points / max(Points), 3) * 100) %>%
        select(-Points) %>%
        arrange(-Score) %>%
        head(10)
}

results <- list(
    Teams = team_results,
    Visionaries = compute_scores(visionaries),
    Investors = compute_scores(investors),
    Employees = compute_scores(employees)
)

# tabulate skills

skill_list <- pivot_longer(skillsets, c(`Skill 1`, `Skill 2`, `Skill 3`), names_to = "Rank", values_to = "Skill") %>%
    mutate(Rank = as.integer(str_remove_all(Rank, "Skill ")))

skill_results <- foreach(i = skills, .combine = bind_rows) %do% {
    total <- filter(skill_list, Skill == i) %>% nrow()
    first <- filter(skill_list, Skill == i, Rank == 1) %>% nrow()
    second <- filter(skill_list, Skill == i, Rank == 2) %>% nrow()
    third <- filter(skill_list, Skill == i, Rank == 3) %>% nrow()
    skill_employees <- filter(skill_list, Skill == i) %>% pull(Name)
    tokens <- filter(results[["Employees"]], Name %in% skill_employees) %>% pull(Tokens) %>% mean() %>% round(2)
    tibble_row(Skill = i, Total = total, First = first, Second = second, Third = third, `Avg. Tokens` = tokens)
} %>% arrange(-Total)

results[["Skills"]] <- skill_results

# save to markdown

if (!dir.exists("results/")) {
    dir.create("results/")
}

save_md <- function(x) {
    md <- rbind(
        rep("---", ncol(results[[x]])),
        apply(results[[x]], 2, as.character) %>%
            apply(2, str_trim)
    )
    write.table(
        md,
        file = paste0("results/", tolower(x), ".md"),
        quote = FALSE,
        sep = " | ",
        row.names = FALSE
    )
}

save_md("Teams")
message("Saved to results/teams.md")

save_md("Visionaries")
message("Saved to results/visionaries.md")

save_md("Investors")
message("Saved to results/investors.md")

save_md("Employees")
message("Saved to results/employees.md")

save_md("Skills")
message("Saved to results/skills.md")
