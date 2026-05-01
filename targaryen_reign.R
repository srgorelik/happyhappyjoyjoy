library(dplyr)
library(tidyr)
library(ggplot2)
library(gameofthrones)
library(showtext)
font_add('got', '~/.local/share/fonts/Game of Thrones.ttf')
showtext_auto()

df <- read.csv(
	text = "
y1,y2,name
1,37,Aegon I
37,42,Aenys I
42,48,Maegor I
48,103,Jaehaerys I
103,129,Viserys I
129,131,Aegon II
131,157,Aegon III
157,161,Daeron I
161,171,Baelor I
171,172,Viserys II
172,184,Aegon IV
184,209,Daeron II
209,221,Aerys I
221,233,Maekar I
233,259,Aegon V
259,262,Jaehaerys II
262,283,Aerys II
"
)

df <- df %>%
	mutate(
		n_years = y2 - y1,
		name = factor(name, levels = rev(unique(df$name))),
		firstname = stringr::str_split_i(name, ' ', 1),
		title = stringr::str_split_i(name, ' ', 2)
	)

avg.reign <- mean(df$n_years)

table(df$firstname)
# Aegon     Aenys     Aerys    Baelor    Daeron Jaehaerys    Maegor    Maekar   Viserys 
#     5         1         2         1         2         2         1         1         2

table(df$title)
# I  II III  IV   V 
# 9   5   1   1   1 

ggplot(df, aes(x = name, y = n_years, fill = name)) +
	geom_hline(yintercept = avg.reign, color = 'darkred', linetype = 'dotted', linewidth = 0.8) +
	annotate(
		geom = 'text',
		x = Inf, #'Baelor I',
		y = avg.reign,
		label = 'Average Reign',
		hjust = 0.5,
		vjust = -0.5,
		size = 3,
		color = 'darkred',
		family = 'serif'
	) +
	# annotate(
	# 	geom = 'text',
	# 	x = -Inf, 
	# 	y = -Inf,
	# 	label = 'Years'
	# ) +
	geom_col(width = 0.8) +
	scale_x_discrete(expand = c(0.05, 0.05)) +
	scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
	scale_fill_got(discrete = T, option = 'daenerys', direction = 1) +
	labs(x = NULL, y = 'Years', title = 'Length of Reign') +
	coord_flip(clip = 'off') +
	theme_minimal(base_size = 16, base_family = 'serif') +
	theme(
		plot.margin = margin(t = 3, r = 4, b = 1, l = 3, unit = 'lines'),
		plot.title = element_text(color = 'black', family = 'got', margin = margin(b = 2, unit = 'lines')),
		panel.grid.major.x = element_line(linewidth = 1),
		panel.grid.minor.x = element_line(linewidth = 0.5),
		panel.grid.major.y = element_blank(),
		axis.text = element_text(color = 'black'),
		axis.text.x = element_text(margin = margin(t = 0)),
		axis.title.x = element_text(color = 'black', margin = margin(t = 1, unit = 'lines')),
		legend.position = 'none'
	)

max_year <- max(df$y2)

df_years <- df %>%
	rowwise() %>%
	mutate(year = list(y1:y2)) %>% # inclusive ends (as provided)
	unnest(year) %>%
	ungroup() %>%
	group_by(year) %>%
	slice_max(order_by = y1, n = 1, with_ties = FALSE) %>%
	ungroup() %>%
	arrange(year) %>%
	filter(year >= 1, year <= max_year) %>%
	mutate(
		decade_col = ((year - 1) %/% 10) + 1, # 1 = years 1–10, 2 = 11–20, ...
		y_in_col = 10 - ((year - 1) %% 10) # year 1 at top (10), year 10 at bottom (1)
	)

decade_breaks <- seq(1, max(df_years$decade_col), by = 2) # every 2 decades (adjust)
decade_labels <- paste0((decade_breaks - 1) * 10 + 1) # label by decade start year

ggplot(df_years, aes(x = decade_col, y = y_in_col, fill = name)) +
	geom_tile(color = "white", linewidth = 0.2) +
	coord_equal() +
	scale_x_continuous(
		breaks = decade_breaks,
		labels = decade_labels,
		expand = c(0, 0)
	) +
	scale_y_continuous(breaks = 1:10, labels = 10:1, expand = c(0, 0)) +
	labs(
		x = "Decade (labeled by start year)",
		y = "Year within decade (top→bottom)",
		title = "Targaryen succession waffle timeline (1 tile = 1 year)"
	) +
	theme_minimal(base_size = 11) +
	theme(
		panel.grid = element_blank(),
		axis.ticks = element_blank()
	)


# Rolling successions per 20 years
library(slider)
max_year <- max(df$y2)

df_years <- df %>%
	rowwise() %>%
	mutate(year = list(y1:y2)) %>% # inclusive ends (as provided)
	unnest(year) %>%
	ungroup() %>%
	group_by(year) %>%
	slice_max(order_by = y1, n = 1, with_ties = FALSE) %>% # boundary year goes to new king
	ungroup() %>%
	arrange(year) %>%
	filter(year >= 1, year <= max_year)

# --- instability metric ---
win <- 20 # years in rolling window

instab <- df_years %>%
	transmute(
		year,
		# succession happens when the king changes from previous year
		succession = as.integer(
			name != dplyr::lag(name, default = dplyr::first(name))
		)
	) %>%
	mutate(
		# rolling count of successions in the trailing window
		succ_last_win = slide_int(
			succession,
			sum,
			.before = win - 1,
			.complete = TRUE
		),
		# convert to a rate (successions per 10 years, or per year, whatever you like)
		succ_per_10y = succ_last_win / win * 10
	)

ggplot(instab, aes(year, succ_per_10y)) +
	geom_line(linewidth = 1) +
	labs(
		x = "Year",
		y = sprintf("Successions per 10 years (rolling %d-year window)", win),
		title = "Targaryen dynasty instability over time"
	) +
	theme_minimal()
