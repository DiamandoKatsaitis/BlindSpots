suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(igraph)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
}))
set.seed(2026)
#rsconnect::writeManifest()

# ---------------------------------------------------------------------------
# THEME TOKENS (R-side, used inside ggplot/plotly; CSS variables mirror these)
# ---------------------------------------------------------------------------
col_bg        <- "#14161a"
col_panel     <- "#1c1f24"
col_signal    <- "#34d399"
col_void      <- "#5b6b7c"
col_canal     <- "#2fb8a6"
col_amber     <- "#f5b34e"
col_text      <- "#f4f1ea"
col_text_mute <- "#9aa3ad"
grad_pal      <- c(col_panel, col_canal, "#6ee7b7", col_signal)

# Diverging colorscale for correlation heatmaps specifically: grad_pal is a
# sequential green ramp (fine for density/accessibility, which are never
# negative), but a Spearman correlation matrix has a real, meaningful zero
# point and negative values -- a sequential ramp can't show that distinction.
# Coral/red for negative, dark neutral at zero, signal green for +1.
col_negative   <- "#e0575c"
diverging_pal  <- list(
  list(0,    col_negative),
  list(0.5,  "#20242b"),
  list(1,    col_signal)
)

plotly_dark <- function(p, legend_top = TRUE) {
  p %>% layout(
    paper_bgcolor = col_bg, plot_bgcolor = col_bg,
    font = list(color = col_text, family = "Space Grotesk"),
    xaxis = list(gridcolor = "#262a31", zerolinecolor = "#262a31"),
    yaxis = list(gridcolor = "#262a31", zerolinecolor = "#262a31"),
    legend = if (legend_top) list(orientation = "h", y = 1.08, font = list(color = col_text)) else list(font = list(color = col_text)),
    margin = list(t = 30)
  ) %>% config(displaylogo = FALSE)
}

# ---------------------------------------------------------------------------
# REAL THESIS RESULTS — Amsterdam network (not synthetic)
#    Hardcoded from the actual printed tables/messages in the two rendered
#    companion reports. "Full network" = all motorised classes; "No
#    residential roads" = residential/unclassified/living_street excluded.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# REAL THESIS RESULTS — Amsterdam network (not synthetic)
#    Loaded from app_data.rds, which is built by prepare_app_data.R from the
#    CSVs the two rendered companion reports themselves write out (see the
#    "app_*" export chunks near the end of Part H/J in each .Rmd) -- nothing
#    in this section is hand-typed. Re-run prepare_app_data.R and redeploy
#    any time the underlying analysis changes.
# ---------------------------------------------------------------------------
app_data <- readRDS("app_data.rds")

real_scale       <- app_data$real_scale
real_topten      <- app_data$real_topten
real_corr_matrix <- app_data$real_corr_matrix
real_kfun        <- app_data$real_kfun
real_bridges     <- app_data$real_bridges

# ---------------------------------------------------------------------------
# MODEL IMPACT OF RESIDENTIAL-ROAD REMOVAL — explicit side-by-side proportion
# and difference table, shown regardless of the scenario toggle above (the
# whole point of this table is to compare both at once, not one at a time).
# Every value here is pulled directly from real_scale/real_corr_matrix above
# (themselves loaded from the reports' own output) -- nothing here is a
# second, separately-typed copy of those numbers.
# ---------------------------------------------------------------------------
get_scale <- function(col, scenario) real_scale[[col]][real_scale$scenario == scenario]
get_cor   <- function(row, scenario) real_corr_matrix[[scenario]][row, "Baseline"]

real_model_diagnostics <- tibble(
  metric = c(
    "Eigen \u2194 power-iteration stationary-probability gap (largest component, top unit)",
    "PageRank \u2194 stationary-distribution correlation (largest component)",
    "P_time \u2194 Part F baseline correlation (Spearman \u03c1)",
    "P_speed \u2194 Part F baseline correlation (Spearman \u03c1)",
    "P_observed \u2194 Part F baseline correlation (Spearman \u03c1)",
    "Admin units with a computable Part F baseline score"
  ),
  full = c(
    get_scale("eigen_power_gap", "Full network"),
    get_scale("pagerank_cor", "Full network"),
    get_cor("P_time", "Full network"),
    get_cor("P_speed", "Full network"),
    get_cor("P_observed", "Full network"),
    get_scale("admin_units_baseline", "Full network")
  ),
  wr = c(
    get_scale("eigen_power_gap", "No residential roads"),
    get_scale("pagerank_cor", "No residential roads"),
    get_cor("P_time", "No residential roads"),
    get_cor("P_speed", "No residential roads"),
    get_cor("P_observed", "No residential roads"),
    get_scale("admin_units_baseline", "No residential roads")
  ),
  is_correlation = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
) %>%
  mutate(
    diff = wr - full,
    pct_change = 100 * diff / abs(full)
  )

# Difference in the 4-way accessibility-measure correlation matrix (largest
# component): "No residential roads" minus "Full network", cell by cell.
# Positive = that pair of measures agrees *more* once residential roads are
# removed; negative = they agree less.
real_corr_diff <- real_corr_matrix[["No residential roads"]] - real_corr_matrix[["Full network"]]

# How many of the top-10 most accessible neighbourhoods are the same list in
# both network versions (spoiler: computed dynamically here, not hardcoded).
real_topten_overlap <- length(intersect(
  real_topten$unit[real_topten$scenario == "Full network"],
  real_topten$unit[real_topten$scenario == "No residential roads"]
))

# ---------------------------------------------------------------------------
# SMALL SYNTHETIC "CANAL-RING" NETWORK — a teaching aid only, for the "See
# The Methods" tab. Clearly not Amsterdam's real geometry; used only to make
# the statistical machinery (coverage bias, KDE, K-function) tangible and
# clickable. Never presented as a result in its own right.
#
# Deliberately irregular rather than a perfect ring/grid: row spacing, column
# count, and column spacing are all randomised per row, plus a jitter on
# every node and a handful of random gaps/shortcuts, so it reads as an
# organic street layout rather than a geometric pattern. One continuous
# "highway" spine (motorway class) runs the length of the grid and is the
# natural thing to click closed in the Anomaly tab, though any segment can be.
# ---------------------------------------------------------------------------
n_rows_net <- 6

build_network <- function() {
  row_y <- cumsum(c(0, runif(n_rows_net - 1, 230, 430)))
  
  nodes <- data.frame(id = integer(), ring = integer(), spoke = integer(),
                      x = double(), y = double())
  id_ctr <- 0L
  row_node_ids <- vector("list", n_rows_net)
  
  for (r in seq_len(n_rows_net)) {
    n_cols <- sample(4:7, 1)
    col_x  <- cumsum(c(0, runif(n_cols - 1, 190, 360)))
    col_x  <- col_x - mean(col_x) + rnorm(1, 0, 70)
    jit_x  <- rnorm(n_cols, 0, 28)
    jit_y  <- rnorm(n_cols, 0, 28)
    ids <- (id_ctr + 1):(id_ctr + n_cols)
    nodes <- rbind(nodes, data.frame(
      id = ids, ring = r, spoke = seq_len(n_cols),
      x = col_x + jit_x, y = row_y[r] + jit_y
    ))
    row_node_ids[[r]] <- ids
    id_ctr <- id_ctr + n_cols
  }
  nodes$label <- paste0("Block ", nodes$ring, "-", nodes$spoke)
  
  edges <- data.frame(from = integer(), to = integer(), road_class = character(),
                      network_source = character(), speed_kmh = double(),
                      stringsAsFactors = FALSE)
  
  add_edge <- function(e, a, b, cls, src, spd) {
    rbind(e, data.frame(from = a, to = b, road_class = cls,
                        network_source = src, speed_kmh = spd,
                        stringsAsFactors = FALSE))
  }
  
  # local streets along each row, with occasional gaps for irregularity
  for (r in seq_len(n_rows_net)) {
    ids <- row_node_ids[[r]]
    if (length(ids) < 2) next
    for (i in seq_len(length(ids) - 1)) {
      if (runif(1) < 0.85) {
        edges <- add_edge(edges, ids[i], ids[i + 1], "residential", "Gemeente Amsterdam", 30)
      }
    }
  }
  
  # connectors between adjacent rows, each matched to its nearest neighbour
  # by x-position rather than a straight column, so cross-streets meander
  for (r in seq_len(n_rows_net - 1)) {
    ids_a <- row_node_ids[[r]]; ids_b <- row_node_ids[[r + 1]]
    xa <- nodes$x[match(ids_a, nodes$id)]; xb <- nodes$x[match(ids_b, nodes$id)]
    for (i in seq_along(ids_a)) {
      j <- which.min(abs(xb - xa[i]))
      if (runif(1) < 0.8) {
        edges <- add_edge(edges, ids_a[i], ids_b[j], "residential", "Gemeente Amsterdam", 30)
      }
    }
  }
  
  # a handful of longer secondary "avenues" cutting across the grid, for
  # road-like irregularity and a bit of route redundancy
  n_extra <- max(3, round(nrow(nodes) * 0.12))
  for (k in seq_len(n_extra)) {
    a  <- sample(nodes$id, 1)
    ax <- nodes$x[nodes$id == a]; ay <- nodes$y[nodes$id == a]
    d  <- sqrt((nodes$x - ax)^2 + (nodes$y - ay)^2)
    ord <- order(d)
    ord <- ord[nodes$id[ord] != a]
    pick <- ord[min(length(ord), sample(3:6, 1))]
    b <- nodes$id[pick]
    if (length(b) == 1 && a != b) {
      edges <- add_edge(edges, a, b, "secondary", "Rijkswaterstaat", 50)
    }
  }
  
  # the highway: one continuous arterial spine along one side of the grid,
  # linking the first node of every row in sequence
  highway_ids <- vapply(row_node_ids, function(ids) ids[1], integer(1))
  for (i in seq_len(length(highway_ids) - 1)) {
    edges <- add_edge(edges, highway_ids[i], highway_ids[i + 1], "motorway", "Rijkswaterstaat", 100)
  }
  
  # connectivity repair: the random gaps above can occasionally strand a
  # node or a small cluster of nodes; bridge every such fragment back to the
  # main component with one short local link, so every demo below can
  # always find a finite shortest path between any two nodes
  g_check <- graph_from_data_frame(edges[, c("from", "to")], directed = FALSE,
                                   vertices = nodes[, "id", drop = FALSE])
  comp <- components(g_check)
  if (comp$no > 1) {
    main_comp <- which.max(comp$csize)
    main_ids  <- nodes$id[comp$membership == main_comp]
    for (ci in setdiff(seq_len(comp$no), main_comp)) {
      frag_ids <- nodes$id[comp$membership == ci]
      fx <- nodes$x[match(frag_ids, nodes$id)]; fy <- nodes$y[match(frag_ids, nodes$id)]
      mx <- nodes$x[match(main_ids, nodes$id)]; my <- nodes$y[match(main_ids, nodes$id)]
      dmat <- outer(fx, mx, "-")^2 + outer(fy, my, "-")^2
      idx  <- which(dmat == min(dmat), arr.ind = TRUE)[1, ]
      edges <- add_edge(edges, frag_ids[idx[1]], main_ids[idx[2]], "residential", "Gemeente Amsterdam", 30)
    }
  }
  
  list(nodes = nodes, edges = edges)
}

net_raw  <- build_network()
nodes_df <- net_raw$nodes

edges_df <- net_raw$edges %>%
  left_join(nodes_df %>% select(id, x, y), by = c("from" = "id")) %>%
  rename(x1 = x, y1 = y) %>%
  left_join(nodes_df %>% select(id, x, y), by = c("to" = "id")) %>%
  rename(x2 = x, y2 = y) %>%
  mutate(
    edge_id  = row_number(),
    length_m = sqrt((x2 - x1)^2 + (y2 - y1)^2),
    mid_x    = (x1 + x2) / 2,
    mid_y    = (y1 + y2) / 2,
    travel_time_min = (length_m / 1000) / speed_kmh * 60,
    # sensor placement bias mirrors the report's real finding: Rijkswaterstaat's
    # network (motorway/primary/arterial) is monitored at ~90%+, while the
    # city network (residential streets) is sparsely covered.
    obs_prob = ifelse(network_source == "Rijkswaterstaat", 0.93, 0.16)
  )
edges_df$observed <- runif(nrow(edges_df)) < edges_df$obs_prob

g_struct <- graph_from_data_frame(edges_df %>% select(from, to), directed = FALSE,
                                  vertices = nodes_df %>% select(id, ring, x, y, label))
node_dist_mat <- distances(g_struct, weights = edges_df$length_m)

from_idx <- as.character(edges_df$from)
to_idx   <- as.character(edges_df$to)
half_len <- edges_df$length_m / 2

D_ff <- node_dist_mat[from_idx, from_idx]
D_ft <- node_dist_mat[from_idx, to_idx]
D_tf <- node_dist_mat[to_idx, from_idx]
D_tt <- node_dist_mat[to_idx, to_idx]
mid_dist_mat <- pmin(D_ff, D_ft, D_tf, D_tt) + outer(half_len, half_len, "+")
diag(mid_dist_mat) <- 0
rm(D_ff, D_ft, D_tf, D_tt)

total_length_m <- sum(edges_df$length_m)

# ---------------------------------------------------------------------------
# ANOMALY DEMO — baseline vs. Markov accessibility, and sensor coverage, on
# the same toy network with any segment(s) the person clicks on actually
# REMOVED from the graph (not just slowed down). A teaching aid for the
# "Anomaly" tab only, same caveat as the rest of the synthetic network: not
# Amsterdam, illustrative only.
# ---------------------------------------------------------------------------
compute_models <- function(e, remove_edge_ids = integer(0)) {
  e2 <- e %>% filter(!(edge_id %in% remove_edge_ids))
  g_t <- graph_from_data_frame(e2 %>% select(from, to), directed = FALSE,
                               vertices = nodes_df %>% select(id, ring, x, y, label))
  dmat <- distances(g_t, weights = e2$travel_time_min)
  n <- nrow(dmat)
  baseline <- vapply(seq_len(n), function(i) 1 / mean(dmat[i, -i]), numeric(1))
  names(baseline) <- rownames(dmat)
  
  idn <- as.character(nodes_df$id)
  P <- matrix(0, n, n, dimnames = list(idn, idn))
  for (k in seq_len(nrow(e2))) {
    a <- as.character(e2$from[k]); b <- as.character(e2$to[k])
    w <- 1 / e2$travel_time_min[k]
    P[a, b] <- P[a, b] + w
    P[b, a] <- P[b, a] + w
  }
  P <- P / rowSums(P)
  P_lazy <- 0.5 * diag(n) + 0.5 * P   # break periodicity; stationary dist. unaffected
  v <- rep(1 / n, n)
  for (it in 1:1500) v <- as.numeric(v %*% P_lazy)
  stat <- v / sum(v)
  names(stat) <- idn
  
  list(baseline = baseline, markov = stat)
}

# The untouched network's model, computed once and reused as the "before"
# reference every time the rank-shift table below needs to compare against it.
models_baseline <- compute_models(edges_df, remove_edge_ids = integer(0))

# Would removing this candidate set of edges isolate a node or split the
# network into more than one piece? Used to keep every click always land on
# a well-defined, still-connected network -- shortest-path distances and the
# Markov chain below both require that.
would_disconnect_network <- function(candidate_removed_ids) {
  e2 <- edges_df %>% filter(!(edge_id %in% candidate_removed_ids))
  if (length(union(e2$from, e2$to)) < nrow(nodes_df)) return(TRUE)
  g2 <- graph_from_data_frame(e2 %>% select(from, to), directed = FALSE,
                              vertices = nodes_df %>% select(id))
  igraph::components(g2)$no > 1
}

# ============================================================================
# UI
# ============================================================================
ui <- shinydashboard::dashboardPage(
  
  # Keep the shinydashboard shell for its tab machinery, but visually replace
  # it entirely with the custom "bs-" (Blind Spots) editorial interface below.
  shinydashboard::dashboardHeader(title = NULL),
  
  shinydashboard::dashboardSidebar(
    width = 235,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Overview", tabName = "home"),
      shinydashboard::menuItem("Real Findings", tabName = "realdata"),
      shinydashboard::menuItem("Station Square", tabName = "station"),
      shinydashboard::menuItem("See The Methods", tabName = "methods"),
      shinydashboard::menuItem("Anomaly", tabName = "anomaly")
    )
  ),
  
  shinydashboard::dashboardBody(
    
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,400;9..144,500;9..144,600&family=Space+Grotesk:wght@400;500;600;700&family=IBM+Plex+Mono:wght@400;500&display=swap",
        rel = "stylesheet"
      ),
      tags$style(HTML("
        :root {
          --bs-bg:     #14161a;
          --bs-card:   #1c1f24;
          --bs-border: #2a2e35;
          --bs-ink:    #f4f1ea;
          --bs-soft:   #d8dce0;
          --bs-faint:  #9aa3ad;
          --bs-accent: #34d399;
          --bs-teal:   #2fb8a6;
          --bs-warm:   #f5b34e;
          --bs-void:   #5b6b7c;
        }

        html, body, .content-wrapper, .right-side, .main-footer {
          background: var(--bs-bg) !important;
          font-family: 'Space Grotesk', sans-serif !important;
          color: var(--bs-ink) !important;
        }
        * { box-sizing: border-box; }

        /* Remove the stock dashboard chrome while retaining its tab system. */
        .main-header, .main-sidebar { display: none !important; }
        .content-wrapper, .right-side { margin-left: 0 !important; }
        .main-footer { display: none !important; }
        .content { padding: 0 2px 28px !important; }

        .bs-shell { max-width: 1240px; margin: 0 auto; padding: 28px 18px 8px; }

        /* ---- header ---- */
        .bs-header { margin-bottom: 22px; border-bottom: 1px solid var(--bs-border); padding-bottom: 16px; }
        .bs-header-top {
          display: flex; justify-content: space-between; gap: 16px; align-items: baseline;
          border-bottom: 1px solid var(--bs-border); padding-bottom: 8px; margin-bottom: 9px;
        }
        .bs-eyebrow { font-size: 9px; letter-spacing: .19em; text-transform: uppercase; color: var(--bs-faint); }
        .bs-title {
          font-family: 'Fraunces', Georgia, serif; font-size: 44px; font-weight: 600; line-height: 1;
          letter-spacing: -.03em; margin: 8px 0 6px; color: var(--bs-ink);
          text-shadow: 0 0 22px rgba(52,211,153,0.18);
        }
        .bs-title span { color: var(--bs-accent); }
        .bs-subtitle { margin: 0; color: var(--bs-faint); font-size: 11px; letter-spacing: .12em; text-transform: uppercase; }

        /* ---- top navigation ---- */
        .bs-nav {
          display: flex; gap: 3px; flex-wrap: wrap; margin-bottom: 22px; padding: 4px;
          background: var(--bs-card); border: 1px solid var(--bs-border); border-radius: 8px;
        }
        .bs-nav-link {
          display: inline-flex; align-items: center; gap: 7px; padding: 9px 13px; border-radius: 6px;
          color: var(--bs-faint) !important; background: transparent; text-decoration: none !important;
          font-size: 11px; font-weight: 600; letter-spacing: .02em; transition: all .18s ease;
        }
        .bs-nav-link:hover { color: var(--bs-ink) !important; background: #23262c; }
        .bs-nav-link.active { color: var(--bs-bg) !important; background: var(--bs-accent);
                               box-shadow: 0 0 14px rgba(52,211,153,0.35); }

        /* ---- signature dots ---- */
        .pulse-dot { display:inline-block; width:9px; height:9px; border-radius:50%;
                     background: var(--bs-accent); margin-right:6px; animation: pulseglow 2.2s infinite; }
        .blindspot-dot { display:inline-block; width:9px; height:9px; border-radius:50%;
                          border:1.5px dashed var(--bs-void); margin-right:6px; }
        .amber-dot { display:inline-block; width:9px; height:9px; border-radius:50%;
                     background: var(--bs-warm); margin-right:6px; animation: pulseglow-amber 2.2s infinite; }
        @keyframes pulseglow { 0%{box-shadow:0 0 0 0 rgba(52,211,153,.55);} 70%{box-shadow:0 0 0 9px rgba(52,211,153,0);} 100%{box-shadow:0 0 0 0 rgba(52,211,153,0);} }
        @keyframes pulseglow-amber { 0%{box-shadow:0 0 0 0 rgba(245,179,78,.55);} 70%{box-shadow:0 0 0 9px rgba(245,179,78,0);} 100%{box-shadow:0 0 0 0 rgba(245,179,78,0);} }

        /* ---- card system ---- */
        .bs-card {
          background: var(--bs-card); border: 1px solid var(--bs-border); border-radius: 8px; padding: 20px;
          transition: transform .15s ease, box-shadow .15s ease;
        }
        .bs-card:hover { transform: translateY(-2px); box-shadow: 0 10px 26px rgba(0,0,0,0.35); }
        .bs-card + .bs-card { margin-top: 16px; }
        .bs-card-title { font-family: 'Fraunces', Georgia, serif; font-size: 20px; line-height: 1.15;
                          font-weight: 500; color: var(--bs-ink); margin: 0 0 4px; }
        .bs-card-subtitle { font-size: 12px; line-height: 1.55; color: var(--bs-faint); margin: 0 0 16px; }
        .bs-section-kicker { font-size: 9px; text-transform: uppercase; letter-spacing: .16em;
                              color: var(--bs-faint); margin: 0 0 7px; font-weight: 600; }

        /* ---- hero ---- */
        .bs-hero { display: grid; grid-template-columns: minmax(0, 1.5fr) minmax(260px, .8fr); gap: 18px; margin-bottom: 18px; }
        .bs-hero-main { background: #0e3a2c; color: var(--bs-ink); border-radius: 8px; padding: 28px;
                        border: 1px solid var(--bs-border); }
        .bs-hero-main .bs-section-kicker { color: #9fe3c4; }
        .bs-hero-main h2 { font-family: 'Fraunces', Georgia, serif; font-size: 31px; line-height: 1.08;
                            font-weight: 500; margin: 0 0 10px; letter-spacing: -.02em; }
        .bs-hero-main p { color: #d7ddd8; max-width: 700px; font-size: 13px; line-height: 1.75; margin: 0; }
        .bs-hero-side { background: var(--bs-card); border: 1px solid var(--bs-border); border-radius: 8px; padding: 22px; }
        .bs-hero-side strong { display: block; font-family: 'Fraunces', Georgia, serif; font-size: 18px;
                                font-weight: 500; margin-bottom: 7px; color: var(--bs-ink); }
        .bs-hero-side p { font-size: 12px; color: var(--bs-soft); line-height: 1.65; margin: 0; }

        /* ---- roadmap stepper (Overview: how to read this app) ---- */
        .bs-stepper { position: relative; display: flex; justify-content: space-between; gap: 8px; padding: 6px 30px 2px; }
        .bs-stepper::before { content: ''; position: absolute; top: 23px; left: 46px; right: 46px; height: 2px;
                               background: var(--bs-border); z-index: 0; }
        .bs-step { position: relative; z-index: 1; display: flex; flex-direction: column; align-items: center;
                   text-align: center; flex: 1 1 0; min-width: 90px; }
        .bs-step-circle { width: 36px; height: 36px; border-radius: 50%; background: var(--bs-bg);
                           border: 2px solid var(--bs-accent); color: var(--bs-accent);
                           display: flex; align-items: center; justify-content: center;
                           font-family: 'IBM Plex Mono', monospace; font-weight: 600; font-size: 14px; margin-bottom: 9px; }
        .bs-step-label { font-size: 12px; font-weight: 600; color: var(--bs-ink); }
        .bs-step-sub { font-size: 10.5px; color: var(--bs-faint); margin-top: 3px; max-width: 150px; line-height: 1.4; }

        /* ---- KPI cards ---- */
        .bs-kpis { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 18px; }
        .bs-kpi { flex: 1 1 180px; min-width: 170px; background: var(--bs-card); border: 1px solid var(--bs-border);
                  border-radius: 8px; padding: 16px 19px; border-top: 3px solid var(--bs-accent); }
        .bs-kpi.teal { border-top-color: var(--bs-teal); }
        .bs-kpi.warm { border-top-color: var(--bs-warm); }
        .bs-kpi.void { border-top-color: var(--bs-void); }
        .bs-kpi-label { font-size: 10px; text-transform: uppercase; letter-spacing: .14em; color: var(--bs-faint); margin-bottom: 4px; }
        .bs-kpi-value { font-family: 'IBM Plex Mono', monospace; font-size: 23px; font-weight: 600;
                         letter-spacing: -.01em; color: var(--bs-ink); line-height: 1.1; }
        .bs-kpi-sub { font-size: 11px; color: var(--bs-faint); margin-top: 4px; }

        /* ---- grids ---- */
        .bs-grid-2 { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); gap: 16px; margin-bottom: 16px; }
        .bs-grid-3 { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 16px; margin-bottom: 16px; }
        .bs-grid-4 { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 16px; margin-bottom: 16px; }

        /* ---- callouts ---- */
        .bs-callout { padding: 13px 16px; border-left: 4px solid var(--bs-accent); background: rgba(52,211,153,.07);
                       border-radius: 0 7px 7px 0; color: var(--bs-soft); font-size: 12px; line-height: 1.65; margin-top: 14px; }
        .bs-callout.warm { border-left-color: var(--bs-warm); background: rgba(245,179,78,.08); }
        .bs-callout.teal { border-left-color: var(--bs-teal); background: rgba(47,184,166,.08); }
        .bs-callout.void { border-left-color: var(--bs-void); background: rgba(91,107,124,.12); }

        /* ---- controls: toggle switch, sliders, radio pills ---- */
        .bs-switch-row { display:flex; align-items:center; gap:0.7rem; flex-wrap:wrap; margin-bottom:0.4rem; }

        .bs-pills .shiny-options-group { display:flex; gap:4px; background:var(--bs-card); padding:4px;
                                          border-radius:8px; border:1px solid var(--bs-border); flex-wrap:wrap; margin:0; }
        .bs-pills input[type=radio], .bs-pills input[type=checkbox] { display:none; }
        .bs-pills label { display:inline-flex; align-items:center; padding:8px 14px; border-radius:6px; cursor:pointer;
                           color:var(--bs-faint) !important; font-size:11px !important; font-weight:600; letter-spacing:.02em;
                           margin:0 !important; transition:all .15s ease; white-space:nowrap; }
        .bs-pills label:has(input:checked) { background: var(--bs-accent); color: var(--bs-bg) !important;
                                              box-shadow: 0 0 10px rgba(52,211,153,0.3); }
        .bs-pills label:hover { color: var(--bs-ink) !important; }

        .bs-control .irs-bar, .bs-control .irs-single, .bs-control .irs-from, .bs-control .irs-to {
          background: var(--bs-accent) !important; border-color: var(--bs-accent) !important;
        }
        .bs-control .irs-line { background: #2a2e35 !important; border-color: #2a2e35 !important; }
        .bs-control label { color: var(--bs-faint); font-size: 11px; text-transform: uppercase; letter-spacing: .08em; }

        /* ---- badges (Station Square scenario indicator) ---- */
        .bs-badge { padding: 0.15rem 0.7rem; border-radius: 12px; font-size: 0.8rem; font-weight: 600; }

        /* ---- tables / DT ---- */
        table.dataTable { background: var(--bs-card) !important; color: var(--bs-ink) !important; }
        table.dataTable thead th { color: var(--bs-faint) !important; border-bottom: 1px solid var(--bs-border) !important; }
        table.dataTable tbody td { border-color: var(--bs-border) !important; }
        .dataTables_wrapper .dataTables_paginate .paginate_button { color: var(--bs-ink) !important; }

        /* ---- misc ---- */
        .bs-caption { text-align:center; margin-top:7px; color:#6b7480; font-size:9px; letter-spacing:.2em; text-transform:uppercase; }
        .cta-link { color: var(--bs-accent) !important; font-family:'Space Grotesk',sans-serif; font-size:0.88rem; text-decoration:none; }
        .cta-link:hover { text-decoration: underline; }

        /* ---- equations (MathJax) ---- */
        .bs-equation { padding: 16px 18px; background: var(--bs-bg); border: 1px solid var(--bs-border);
                        border-radius: 6px; margin-bottom: 12px; overflow-x: auto; color: var(--bs-ink);
                        font-size: 15px; }
        .bs-equation .MathJax { color: var(--bs-ink) !important; }

        /* ---- big reveal number (e.g. top-10 overlap stat) ---- */
        .flip-number { font-family: 'IBM Plex Mono', monospace; font-size: 2.4rem; font-weight: 600;
                        color: var(--bs-accent); text-shadow: 0 0 18px rgba(52,211,153,0.35); }
        .flip-caption { font-family: 'Space Grotesk', sans-serif; font-size: 0.85rem; color: var(--bs-faint); }

        /* Responsive */
        @media (max-width: 900px) {
          .bs-hero, .bs-grid-2, .bs-grid-3, .bs-grid-4 { grid-template-columns: 1fr; }
          .bs-title { font-size: 38px; }
        }
        @media (max-width: 640px) {
          .bs-shell { padding: 18px 8px; }
          .bs-header-top { flex-direction: column; gap: 4px; }
          .bs-title { font-size: 34px; }
          .bs-nav-link { padding: 8px 10px; }
          .bs-stepper { flex-direction: column; align-items: flex-start; gap: 18px; padding-left: 6px; }
          .bs-stepper::before { display: none; }
          .bs-step { flex-direction: row; text-align: left; gap: 12px; max-width: none; }
          .bs-step-circle { margin-bottom: 0; flex-shrink: 0; }
          .bs-step-sub { max-width: none; }
        }
      ")),
      
      # Keep the custom top-navigation highlight and Shiny tab state in sync.
      tags$script(HTML("
        $(document).on('shiny:connected', function(){
          function syncBsNav(tabName){
            if(!tabName){
              tabName = $('.sidebar-menu .active a').attr('data-value') ||
                        $('.tab-content .tab-pane.active').attr('id');
              if(tabName){ tabName = tabName.replace('shiny-tab-', ''); }
              tabName = tabName || 'home';
            }
            $('.bs-nav-link').removeClass('active');
            $('.bs-nav-link[data-tab=\"' + tabName + '\"]').addClass('active');
          }
          syncBsNav();
          $(document).on('shiny:inputchanged', function(event){
            if(event.name === 'tabs'){ syncBsNav(event.value); }
          });
          $(document).on('shown.bs.tab', function(){ setTimeout(syncBsNav, 10); });
        });
      "))
    ),
    
    withMathJax(),
    
    div(class = "bs-shell",
        
        # ---- header ----
        div(class = "bs-header",
            div(class = "bs-header-top"
            ),
            h1(class = "bs-title", HTML("Blind Spots<span>.</span>")),
            p(class = "bs-subtitle", "Traffic data availability & accessibility modelling")
        ),
        
        # ---- navigation ----
        tags$nav(class = "bs-nav", role = "navigation",
                 tags$a(class = "bs-nav-link active", href = "#", `data-tab` = "home",
                        onclick = "Shiny.setInputValue('bs_nav', 'home', {priority: 'event'}); return false;",
                        icon("house"), "Overview"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "realdata",
                        onclick = "Shiny.setInputValue('bs_nav', 'realdata', {priority: 'event'}); return false;",
                        icon("city"), "Real Findings"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "station",
                        onclick = "Shiny.setInputValue('bs_nav', 'station', {priority: 'event'}); return false;",
                        icon("subway"), "Station Square"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "methods",
                        onclick = "Shiny.setInputValue('bs_nav', 'methods', {priority: 'event'}); return false;",
                        icon("flask"), "See The Methods"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "anomaly",
                        onclick = "Shiny.setInputValue('bs_nav', 'anomaly', {priority: 'event'}); return false;",
                        icon("triangle-exclamation"), "Anomaly")
        ),
        
        shinydashboard::tabItems(
          
          # ======================================================
          # OVERVIEW
          # ======================================================
          shinydashboard::tabItem(
            tabName = "home",
            
            div(class = "bs-hero",
                div(class = "bs-hero-main",
                    p(class = "bs-section-kicker", "Research context"),
                    h2("Every sensor network has a blind spot."),
                    p("Traffic sensor coverage follows budget and geometry, not statistical design \u2014 arterials ",
                      "and motorways attract inductive loops and ANPR, while ordinary city streets go unmeasured. ",
                      "This app walks through the real numbers that coverage bias produces once it's run through ",
                      "network-constrained spatial statistics and two accessibility models."),
                    div(class = "bs-callout teal",
                        strong("Research focus: "), "traffic data availability, network K-functions and KDE, ",
                        "and homogeneous Markov chain accessibility modelling.")
                ),
                div(class = "bs-hero-side",
                    p(class = "bs-section-kicker", "On the data"),
                    strong("Real numbers, clearly labelled"),
                    p("Every figure on the Real Findings and Station Square tabs is read directly from the two ",
                      "rendered companion reports. See The Methods and Anomaly use a synthetic toy network, ",
                      "and both say so up front \u2014 nothing is mixed together or left ambiguous."),
                    div(style = "height:12px;"),
                    p(class = "bs-section-kicker", "Where to start"),
                    strong("New here?"),
                    p("The roadmap below walks through the four tabs in the order they're designed to be read.")
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "How to read this app"),
                div(class = "bs-stepper",
                    div(class = "bs-step",
                        div(class = "bs-step-circle", "1"),
                        div(class = "bs-step-label", "Real Findings"),
                        div(class = "bs-step-sub", "The headline numbers")),
                    div(class = "bs-step",
                        div(class = "bs-step-circle", "2"),
                        div(class = "bs-step-label", "Station Square"),
                        div(class = "bs-step-sub", "One concrete exception")),
                    div(class = "bs-step",
                        div(class = "bs-step-circle", "3"),
                        div(class = "bs-step-label", "See The Methods"),
                        div(class = "bs-step-sub", "How the statistics work")),
                    div(class = "bs-step",
                        div(class = "bs-step-circle", "4"),
                        div(class = "bs-step-label", "Anomaly"),
                        div(class = "bs-step-sub", "Break something on purpose"))
                )
            ),
            
            div(class = "bs-kpis",
                div(class = "bs-kpi",
                    div(class = "bs-kpi-label", "Sensors observed"),
                    div(class = "bs-kpi-value", "94.2%"),
                    div(class = "bs-kpi-sub", "653 of 693 monitoring locations")
                ),
                div(class = "bs-kpi teal",
                    div(class = "bs-kpi-label", "On Rijkswaterstaat roads"),
                    div(class = "bs-kpi-value", "93.9%"),
                    div(class = "bs-kpi-sub", "National highway network")
                ),
                div(class = "bs-kpi void",
                    div(class = "bs-kpi-label", "On Gemeente roads"),
                    div(class = "bs-kpi-value", "85.7%"),
                    div(class = "bs-kpi-sub", "City network (n = 7, small sample)")
                ),
                div(class = "bs-kpi warm",
                    div(class = "bs-kpi-label", "Real road segments"),
                    div(class = "bs-kpi-value", "21,605"),
                    div(class = "bs-kpi-sub", "Full Amsterdam network")
                )
            ),
            
            div(class = "bs-grid-4",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Stop 1"),
                    h2(class = "bs-card-title", "\U0001F4CA Real Findings"),
                    p(class = "bs-card-subtitle",
                      "Network scale, the accessibility ranking, and what happens when residential roads are ",
                      "stripped from the analysis. Flip the switch and watch a 130\u00d7 numerical-stability gap ",
                      "disappear."),
                    actionLink("goto_real", "See the real numbers \u2192", class = "cta-link")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Stop 2"),
                    h2(class = "bs-card-title", "\U0001F68F Station Square"),
                    p(class = "bs-card-subtitle",
                      "Five road segments hold the city's travel-time network together. Four are pure ",
                      "shortcuts. One is secretly also one of the most individually accessible places in ",
                      "Amsterdam \u2014 and it's not a coincidence why."),
                    actionLink("goto_station", "Meet the exception \u2192", class = "cta-link")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Stop 3"),
                    h2(class = "bs-card-title", "\U0001F52C See The Methods"),
                    p(class = "bs-card-subtitle",
                      "How a network K-function, KDE, or Markov chain actually works \u2014 a small interactive ",
                      "playground, clearly not Amsterdam's real geometry, to build intuition for the machinery."),
                    actionLink("goto_methods", "Play with the demo \u2192", class = "cta-link")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Stop 4"),
                    h2(class = "bs-card-title", "\u26A0\uFE0F Anomaly"),
                    p(class = "bs-card-subtitle",
                      "Click a road segment closed on the same toy network and watch the topology-free ",
                      "baseline miss what the Markov chain catches immediately."),
                    actionLink("goto_anomaly", "Break something \u2192", class = "cta-link")
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Acknowledgements"),
                p(style = "color:var(--bs-faint); font-size:12px; margin:0;",
                  em("With thanks to Renate Thiede, Inger Fabris-Rotelli \u2014 and, my cat." ))
            )
          ),
          
          # ======================================================
          # REAL FINDINGS
          # ======================================================
          shinydashboard::tabItem(
            tabName = "realdata",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "The real numbers behind the story"),
                h2(class = "bs-card-title", "Full network vs. no residential roads"),
                p(class = "bs-card-subtitle",
                  "Every figure below is read directly from the two rendered companion reports \u2014 the full ",
                  "motorised road network, and a second version with residential, unclassified, and ",
                  "living_street roads stripped out."),
                div(class = "bs-switch-row",
                    tags$span(style = "font-size:0.82rem; color:var(--bs-faint);", "Viewing:"),
                    div(class = "bs-pills",
                        radioButtons("scenario_real", NULL, inline = TRUE,
                                     choices = c("Full network", "No residential roads"),
                                     selected = "Full network"))),
                uiOutput("real_headline")
            ),
            
            div(class = "bs-kpis",
                div(class = "bs-kpi",
                    div(class = "bs-kpi-label", "Road segments"),
                    div(class = "bs-kpi-value", textOutput("real_segments", inline = TRUE))
                ),
                div(class = "bs-kpi teal",
                    div(class = "bs-kpi-label", "Network length"),
                    div(class = "bs-kpi-value", textOutput("real_length", inline = TRUE))
                ),
                div(class = "bs-kpi warm",
                    div(class = "bs-kpi-label", "Length at 30 km/h"),
                    div(class = "bs-kpi-value", textOutput("real_pct30", inline = TRUE))
                ),
                div(class = "bs-kpi void",
                    div(class = "bs-kpi-label", "Largest component"),
                    div(class = "bs-kpi-value", textOutput("real_largest_share", inline = TRUE))
                )
            ),
            
            div(class = "bs-grid-2",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Top 10"),
                    h2(class = "bs-card-title", "Most accessible neighbourhoods"),
                    p(class = "bs-card-subtitle",
                      "Markov chain stationary probability \u2014 how often a long-run random walker on the ",
                      "travel-time network ends up in each unit."),
                    plotlyOutput("real_topten_plot", height = "380px")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Agreement"),
                    h2(class = "bs-card-title", "How the four accessibility measures agree"),
                    p(class = "bs-card-subtitle",
                      "Spearman correlation between every pair of P_time, P_speed, the observed-speed variant, ",
                      "and the topology-free Part F baseline. Hover a cell for the exact \u03c1."),
                    plotlyOutput("real_corr_heat", height = "380px")
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Component by component"),
                h2(class = "bs-card-title", "\U0001F3AF Sensor K-function verdicts"),
                p(class = "bs-card-subtitle",
                  "Each row is a separate, disconnected cluster of sensor-carrying road, tested independently ",
                  "against complete spatial randomness. \U0001F7E2 clustered and stable under leave-one-out ",
                  "removal (the only verdicts worth reporting as genuine) \u00b7 \U0001F535 more regularly ",
                  "spaced than chance \u00b7 \u26AA indistinguishable from random placement."),
                DTOutput("real_kfun_table")
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Model impact of removing residential roads"),
                h2(class = "bs-card-title", "Full network vs. no residential roads, side by side"),
                p(class = "bs-card-subtitle",
                  "This table doesn't follow the switch above \u2014 both scenarios are shown at once so the ",
                  "difference is explicit rather than something you have to hold in your head while toggling. ",
                  "Correlation rows report the difference in Spearman \u03c1 as percentage points, since a ",
                  "\"% change\" on a value already near zero is misleading; the other rows report a genuine ",
                  "relative change."),
                DTOutput("real_model_diag_table")
            ),
            
            div(class = "bs-grid-2",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Where agreement shifts"),
                    h2(class = "bs-card-title", "Correlation-matrix difference"),
                    p(class = "bs-card-subtitle",
                      "(No residential roads) \u2212 (Full network), cell by cell. Green = that pair of ",
                      "accessibility measures agrees ", tags$em("more"), " once residential roads are removed; ",
                      "red = it agrees less. Hover a cell for the exact shift."),
                    plotlyOutput("real_corr_diff_heat", height = "340px")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Ranking overlap"),
                    h2(class = "bs-card-title", "Top-10 most accessible neighbourhoods"),
                    uiOutput("real_topten_overlap_stat")
                )
            ),
            
            p(style = "color:#6b7480; font-size:0.83rem;",
              em("Nothing on this tab is simulated \u2014 every number is read straight off the reports' ",
                 "rendered output."))
          ),
          
          # ======================================================
          # STATION SQUARE
          # ======================================================
          shinydashboard::tabItem(
            tabName = "station",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Case study"),
                h2(class = "bs-card-title", "The Station Square exception"),
                uiOutput("station_intro_callout"),
                br(),
                div(style = "display:flex; align-items:center; gap:0.7rem; flex-wrap:wrap;",
                    tags$span(style = "font-size:0.82rem; color:var(--bs-faint);", "Viewing:"),
                    div(class = "bs-pills",
                        radioButtons("scenario_station", NULL, inline = TRUE,
                                     choices = c("Full network", "No residential roads"),
                                     selected = "Full network")))
            ),
            
            div(class = "bs-grid-2",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Bridge units"),
                    h2(class = "bs-card-title", "Shortcut vs. destination"),
                    plotlyOutput("bridge_plot", height = "440px")
                ),
                div(class = "bs-card",
                    uiOutput("station_reveal_title"),
                    uiOutput("station_reveal_body")
                )
            ),
            
            div(class = "bs-callout",
                tags$strong("Why does this matter? \u2014"),
                "It's a concrete, checkable example of exactly the gap this research is about: two ",
                "structurally identical-looking things \u2014 five roads that all sit on many shortest paths ",
                "\u2014 can have completely different real-world importance, and only a network-topology-aware ",
                "model surfaces which one. Flip to the ", tags$strong("residential-excluded"), " network on ",
                "Real Findings first, then come back \u2014 the exception itself changes.")
          ),
          
          # ======================================================
          # SEE THE METHODS
          # ======================================================
          shinydashboard::tabItem(
            tabName = "methods",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Not Amsterdam \u2014 A toy network"),
                h2(class = "bs-card-title", "How do these statistical methods actually work?"),
                p(class = "bs-card-subtitle",
                  "This is a small synthetic street grid \u2014 a toy city, deliberately irregular rather than a ",
                  "neat geometric shape, built only so the machinery behind the real results is tangible and ",
                  "clickable. Every number on this tab is simulated live; nothing here is a research finding."),
                div(class = "bs-pills",
                    radioButtons("methods_view", NULL, inline = TRUE,
                                 choices = c("Coverage bias" = "coverage", "Network KDE" = "kde", "K-function" = "kfun"),
                                 selected = "coverage"))
            ),
            
            conditionalPanel(
              "input.methods_view == 'coverage'",
              p(class = "bs-caption", style = "text-align:left; letter-spacing:normal; text-transform:none; margin-bottom:10px;",
                span(class = "pulse-dot"), "Observed segment   ", span(class = "blindspot-dot"), "Blind spot"),
              div(class = "bs-grid-2",
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Interactive"),
                      h2(class = "bs-card-title", "Monitoring coverage across the toy network"),
                      radioButtons("source_filter", NULL, inline = TRUE,
                                   choices = c("All roads", "Rijkswaterstaat", "Gemeente Amsterdam"),
                                   selected = "All roads"),
                      plotlyOutput("coverage_map", height = "420px")
                  ),
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Resample"),
                      h2(class = "bs-card-title", "Redraw sensor placement"),
                      p(class = "bs-card-subtitle",
                        "Each segment's monitoring probability is fixed by its road authority \u2014 the same ",
                        "~94% vs. ~86% split found in the real report. Redraw which specific segments happen to ",
                        "be observed under that same policy to see how much the map's texture (not its overall ",
                        "bias) varies by chance."),
                      actionButton("resample", "\U0001F500 Resample sensor placement", class = "btn btn-outline-light"),
                      hr(style = "border-color: var(--bs-border);"),
                      DTOutput("coverage_table")
                  )
              )
            ),
            
            conditionalPanel(
              "input.methods_view == 'kde'",
              div(class = "bs-card",
                  p(class = "bs-section-kicker", "Network kernel density"),
                  div(class = "bs-equation",
                      helpText("$$\\hat{f}(s) \\;=\\; \\sum_{i} K\\!\\left(\\frac{d_L(s, x_i)}{h}\\right)$$")),
                  p(style = "font-size:0.85rem; color:var(--bs-soft); margin:0;",
                    "Density spreads only along connected road segments, not freely across the plane, where ",
                    tags$em("d_L"), " is shortest-path network distance and ", tags$em("h"),
                    " is the bandwidth \u03c3 below. Bright segments sit close to many sensors; dark segments ",
                    "are blind spots even if they carry real traffic.")
              ),
              br(),
              div(class = "bs-grid-2",
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Heatmap"),
                      h2(class = "bs-card-title", "Monitoring intensity"),
                      plotlyOutput("kde_map", height = "440px")
                  ),
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Bandwidth"),
                      h2(class = "bs-card-title", "Adjust \u03c3"),
                      div(style = "display:flex; gap:0.4rem; margin-bottom:0.8rem;",
                          actionButton("bw_tight", "Tight", class = "btn btn-sm btn-outline-light"),
                          actionButton("bw_balanced", "Balanced", class = "btn btn-sm btn-outline-light"),
                          actionButton("bw_wide", "Wide", class = "btn btn-sm btn-outline-light")),
                      div(class = "bs-control", sliderInput("bandwidth", "\u03c3 (metres)", min = 150, max = 2000, value = 600, step = 50)),
                      p(style = "font-size:0.85rem; color:var(--bs-faint);",
                        "Small \u03c3 shows individual sensor clusters; large \u03c3 smooths toward the coverage ",
                        "bias between the highway spine and the residential grid.")
                  )
              )
            ),
            
            conditionalPanel(
              "input.methods_view == 'kfun'",
              div(class = "bs-card",
                  p(class = "bs-section-kicker", "Linear network K-function"),
                  div(class = "bs-equation",
                      helpText("$$K(r) \\;=\\; \\frac{1}{\\lambda} \\times E[\\text{additional events within network distance } r]$$")),
                  p(style = "font-size:0.85rem; color:var(--bs-soft); margin:0;",
                    "Compared against a Monte Carlo envelope from placing the same number of points uniformly ",
                    "at random on the network. Above the envelope means clustering; below means more regular ",
                    "spacing than chance.")
              ),
              br(),
              div(class = "bs-grid-2",
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Envelope test"),
                      h2(class = "bs-card-title", "Observed K(r) vs. complete spatial randomness"),
                      plotlyOutput("kfun_plot", height = "380px"),
                      uiOutput("kfun_interpretation")
                  ),
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Settings"),
                      h2(class = "bs-card-title", "Simulation controls"),
                      actionButton("reshuffle", "\U0001F500 Reshuffle CSR envelope", class = "btn btn-outline-light", style = "margin-bottom:0.8rem;"),
                      div(class = "bs-control", selectInput("n_sim", "Monte Carlo simulations", choices = c(49, 99, 199, 399), selected = 199)),
                      div(class = "bs-control", sliderInput("r_max", "Maximum r (metres)", min = 800, max = 3500, value = 2200, step = 200))
                  )
              )
            )
          ),
          
          # ======================================================
          # ANOMALY
          # ======================================================
          shinydashboard::tabItem(
            tabName = "anomaly",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Not Amsterdam \u2014 A toy network"),
                h2(class = "bs-card-title", "What if a segment closes?"),
                p(class = "bs-card-subtitle",
                  "Click any road segment on the map below to remove it from the network entirely \u2014 not ",
                  "just slow it down. Click it again to restore it. Removed segments are marked in red ",
                  "everywhere below, so you can watch sensor coverage and both accessibility models react to ",
                  "the same closure. The thick diagonal spine is the highway; try it first."),
                actionButton("anomaly_reset", "\U0001F504 Reset all closures", class = "btn btn-outline-light")
            ),
            
            div(class = "bs-kpis",
                div(class = "bs-kpi warm",
                    div(class = "bs-kpi-label", "Segments removed"),
                    div(class = "bs-kpi-value", textOutput("anomaly_n_removed", inline = TRUE)),
                    div(class = "bs-kpi-sub", "Of any class, anywhere on the network")
                ),
                div(class = "bs-kpi void",
                    div(class = "bs-kpi-label", "Sensors lost"),
                    div(class = "bs-kpi-value", textOutput("anomaly_sensors_lost", inline = TRUE)),
                    div(class = "bs-kpi-sub", "Observed segments taken offline")
                ),
                div(class = "bs-kpi teal",
                    div(class = "bs-kpi-label", "Remaining highway coverage"),
                    div(class = "bs-kpi-value", textOutput("anomaly_coverage_pct", inline = TRUE)),
                    div(class = "bs-kpi-sub", "% of the original highway still open and observed")
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Network state \u2014 click a segment to toggle it"),
                h2(class = "bs-card-title", "The closure, marked in red"),
                p(class = "bs-card-subtitle",
                  "Green = observed, grey = blind spot, red = removed entirely. Everything below uses this ",
                  "same colour key. A click won't be allowed if it would cut a block off from the rest of the ",
                  "network completely."),
                plotlyOutput("anomaly_network_map", height = "440px")
            ),
            
            div(class = "bs-grid-2",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Topology-free"),
                    h2(class = "bs-card-title", "Baseline expected-travel-time accessibility"),
                    plotlyOutput("anomaly_baseline_map", height = "420px")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Network-topology-aware"),
                    h2(class = "bs-card-title", "Markov chain stationary accessibility"),
                    plotlyOutput("anomaly_markov_map", height = "420px")
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "The tell"),
                h2(class = "bs-card-title", "Units whose Markov rank moves most"),
                p(class = "bs-card-subtitle",
                  "Ranked by how far each unit's Markov stationary-probability rank shifts relative to the ",
                  "untouched network (0 segments removed). The baseline model, by construction, can only ever ",
                  "notice the units directly on the closed stretch \u2014 the Markov model also picks up ",
                  "second-order effects on units that connect to the rest of the network through it."),
                DTOutput("anomaly_rank_table")
            ),
            
            div(class = "bs-callout warm",
                tags$strong("Why this belongs next to Station Square: "),
                "same lesson, opposite direction. Station Square shows a road segment that's more important ",
                "than its traffic volume suggests; this shows what happens when a genuinely important segment ",
                "disappears \u2014 and why a topology-free baseline can miss it entirely.")
          )
        )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # Custom top navigation -> actual shinydashboard tab state.
  observeEvent(input$bs_nav, {
    req(input$bs_nav)
    shinydashboard::updateTabItems(session, "tabs", selected = input$bs_nav)
  }, ignoreInit = TRUE)
  
  # About-tab CTA links -> same mechanism
  observeEvent(input$goto_real,    shinydashboard::updateTabItems(session, "tabs", selected = "realdata"))
  observeEvent(input$goto_station, shinydashboard::updateTabItems(session, "tabs", selected = "station"))
  observeEvent(input$goto_methods, shinydashboard::updateTabItems(session, "tabs", selected = "methods"))
  observeEvent(input$goto_anomaly, shinydashboard::updateTabItems(session, "tabs", selected = "anomaly"))
  
  # ---------------------------------------------------- Real Amsterdam data
  # A single shared scenario value, kept in sync across the two pill
  # selectors (Real Findings and Station Square) so flipping either one
  # updates both immediately, without needing two DOM elements to share the
  # same input id (which Shiny doesn't support).
  scenario_rv <- reactiveVal("Full network")
  
  observeEvent(input$scenario_real, {
    req(input$scenario_real)
    if (!identical(input$scenario_real, scenario_rv())) scenario_rv(input$scenario_real)
  }, ignoreInit = TRUE)
  
  observeEvent(input$scenario_station, {
    req(input$scenario_station)
    if (!identical(input$scenario_station, scenario_rv())) scenario_rv(input$scenario_station)
  }, ignoreInit = TRUE)
  
  observeEvent(scenario_rv(), {
    updateRadioButtons(session, "scenario_real", selected = scenario_rv())
    updateRadioButtons(session, "scenario_station", selected = scenario_rv())
  }, ignoreInit = TRUE)
  
  real_scenario <- reactive(scenario_rv())
  real_row <- reactive(real_scale %>% filter(scenario == real_scenario()))
  
  output$real_segments <- renderText(format(real_row()$segments, big.mark = ","))
  output$real_length   <- renderText(paste0(real_row()$length_km, " km"))
  output$real_pct30    <- renderText(paste0(real_row()$pct_30kmh, "%"))
  output$real_largest_share <- renderText(paste0(real_row()$largest_share_pct, "% of nodes"))
  
  output$real_headline <- renderUI({
    r <- real_row()
    if (real_scenario() == "Full network") {
      div(class = "bs-callout",
          tags$strong("Full network: "), "the largest-component Markov chain is numerically delicate here \u2014 ",
          "the eigen and power-iteration methods disagree by up to ", tags$code(round(r$eigen_power_gap, 5)),
          " in stationary probability, and PageRank only correlates ", tags$code(r$pagerank_cor),
          " with the stationary ranking. Flip the switch to see what happens once residential/unclassified/",
          "living_street roads are removed.")
    } else {
      div(class = "bs-callout",
          tags$strong("No residential roads: "), "the same cross-check now agrees to within ",
          tags$code(round(r$eigen_power_gap, 5)), " \u2014 roughly ",
          round(0.05916 / r$eigen_power_gap), "\u00d7 tighter \u2014 and PageRank correlates ",
          tags$code(r$pagerank_cor), " with the stationary ranking. Stripping out the local street network ",
          "appears to remove the slow-mixing behaviour the full network's largest component was showing.")
    }
  })
  
  output$real_topten_plot <- renderPlotly({
    df <- real_topten %>% filter(scenario == real_scenario()) %>% arrange(rank)
    p <- suppressWarnings(
      ggplot(df, aes(x = stationary_prob, y = reorder(unit, stationary_prob),
                     text = paste0(unit, ": ", signif(stationary_prob, 3)))) +
        geom_col(fill = col_signal) +
        labs(x = "Stationary probability", y = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.background = element_rect(fill = col_bg, color = NA),
              plot.background  = element_rect(fill = col_bg, color = NA),
              panel.grid = element_line(color = "#262a31"),
              axis.text = element_text(color = col_text_mute),
              axis.title = element_text(color = col_text))
    )
    ggplotly(p, tooltip = "text") %>% plotly_dark(legend_top = FALSE)
  })
  
  output$real_corr_heat <- renderPlotly({
    m <- real_corr_matrix[[real_scenario()]]
    plot_ly(
      x = colnames(m), y = rownames(m), z = m, type = "heatmap",
      colorscale = diverging_pal, zmin = -1, zmax = 1,
      text = matrix(sprintf("%.2f", m), nrow(m)), hoverinfo = "text",
      showscale = TRUE
    ) %>% plotly_dark(legend_top = FALSE)
  })
  
  output$real_kfun_table <- renderDT({
    df <- real_kfun %>%
      filter(scenario == real_scenario()) %>%
      arrange(desc(n_sensors)) %>%
      transmute(
        Component   = component,
        Sensors     = n_sensors,
        `Max r (m)` = round(max_r),
        Verdict = case_when(
          classification == "Clustered" & reliability == "Stable" ~ "\U0001F7E2 Stable cluster",
          classification == "Clustered"                           ~ "\U0001F7E1 Unstable cluster",
          classification == "Regular/Dispersed"                   ~ "\U0001F535 Regular/dispersed",
          TRUE                                                     ~ "\u26AA Within CSR envelope"
        )
      )
    datatable(df, options = list(pageLength = 15, dom = "t"), rownames = FALSE)
  })
  
  # This table intentionally ignores scenario_rv()/real_scenario() -- both
  # networks are shown at once, since the point is the comparison itself.
  output$real_model_diag_table <- renderDT({
    df <- real_model_diagnostics %>%
      transmute(
        Metric = metric,
        `Full network` = ifelse(is_correlation, sprintf("%.2f", full), format(full, big.mark = ",")),
        `No residential roads` = ifelse(is_correlation, sprintf("%.2f", wr), format(wr, big.mark = ",")),
        Difference = ifelse(is_correlation,
                            paste0(ifelse(diff >= 0, "+", ""), sprintf("%.2f", diff), " pts"),
                            paste0(ifelse(diff >= 0, "+", ""), format(round(diff, 4), big.mark = ","))),
        `Relative change` = ifelse(is_correlation, "\u2014", paste0(ifelse(pct_change >= 0, "+", ""), round(pct_change, 1), "%"))
      )
    datatable(df, options = list(pageLength = 6, dom = "t"), rownames = FALSE)
  })
  
  output$real_corr_diff_heat <- renderPlotly({
    m <- real_corr_diff
    plot_ly(
      x = colnames(m), y = rownames(m), z = m, type = "heatmap",
      colorscale = diverging_pal, zmin = -max(abs(m)), zmax = max(abs(m)),
      text = matrix(sprintf("%+.2f", m), nrow(m)), hoverinfo = "text",
      showscale = TRUE
    ) %>% plotly_dark(legend_top = FALSE)
  })
  
  output$real_topten_overlap_stat <- renderUI({
    tagList(
      div(class = "flip-number", style = "text-align:center;", paste0(real_topten_overlap, " / 10")),
      p(class = "flip-caption", style = "text-align:center; margin-top:6px;",
        "neighbourhoods shared between the two top-10 lists"),
      div(class = "bs-callout", style = "margin-top:14px;",
          if (real_topten_overlap == 0) {
            tagList(tags$strong("Zero overlap: "), "removing residential roads doesn't just re-rank the same ",
                    "neighbourhoods \u2014 it produces an entirely different set of \"most accessible\" places. ",
                    "Whatever list you'd headline with depends materially on this one road-class decision.")
          } else {
            tagList(tags$strong(real_topten_overlap, " shared: "), "some neighbourhoods stay near the top ",
                    "regardless of whether residential roads are included, but most of the list still changes.")
          })
    )
  })
  
  # ------------------------------------------------------- Station Square
  # Was a static sentence hardcoding "Spearman rho = 0.039 across all 445
  # modelled units" -- 0.039 is actually the No-residential-roads value (444
  # units), paired with 445, the Full-network unit count. Now reads both the
  # correlation and the unit count from whichever scenario is selected, so
  # the two numbers always come from the same report.
  output$station_intro_callout <- renderUI({
    r <- real_row()
    div(class = "bs-callout warm",
        span(class = "amber-dot"),
        tags$strong("A road can be critical to a city without anyone wanting to go there."),
        " Five neighbourhoods act as the busiest “bridges” in Amsterdam's travel-time network ",
        "— remove any one and the rest of the city gets measurably harder to reach from itself. ",
        "Betweenness centrality identifies all five. But being a bridge and being an individually ",
        tags$em("accessible"), " place — somewhere a long-run random walker actually ends up ",
        "— turn out to be almost entirely unrelated (Spearman ρ = ", r$betweenness_accessibility_cor,
        " across all ", r$n_modelled_units, " modelled units). Almost.")
  })
  
  bridge_data <- reactive(real_bridges %>% filter(scenario == real_scenario()) %>% arrange(desc(betweenness)))
  
  output$bridge_plot <- renderPlotly({
    df <- bridge_data() %>%
      mutate(
        betweenness_norm = betweenness / max(betweenness),
        accessibility_norm = stationary_prob / max(stationary_prob),
        bar_color = if_else(is_exception, col_amber, col_canal)
      )
    long_df <- bind_rows(
      df %>% transmute(unit, value = betweenness_norm, raw = betweenness, bar_color,
                       metric = "Betweenness \u2014 how much of a shortcut"),
      df %>% transmute(unit, value = accessibility_norm, raw = stationary_prob, bar_color,
                       metric = "Accessibility \u2014 how popular a destination")
    )
    p <- suppressWarnings(
      ggplot(long_df, aes(x = value, y = reorder(unit, value), fill = bar_color,
                          text = paste0(unit, ": ", signif(raw, 3)))) +
        geom_col() +
        scale_fill_identity() +
        facet_wrap(~ metric, ncol = 1, scales = "free_y") +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.background = element_rect(fill = col_bg, color = NA),
              plot.background  = element_rect(fill = col_bg, color = NA),
              panel.grid = element_line(color = "#262a31"),
              strip.text = element_text(color = col_text, face = "bold", hjust = 0),
              strip.background = element_rect(fill = col_bg, color = NA),
              axis.text = element_text(color = col_text_mute),
              axis.title = element_text(color = col_text))
    )
    ggplotly(p, tooltip = "text") %>% plotly_dark(legend_top = FALSE)
  })
  
  output$station_reveal_title <- renderUI({
    if (any(bridge_data()$is_exception)) {
      tagList(p(class = "bs-section-kicker", "Reveal"),
              h2(class = "bs-card-title", HTML("<span class='amber-dot'></span> The exception, revealed")))
    } else {
      tagList(p(class = "bs-section-kicker", "Reveal"),
              h2(class = "bs-card-title", HTML("<span class='blindspot-dot'></span> No exception this time")))
    }
  })
  
  output$station_reveal_body <- renderUI({
    df <- bridge_data()
    if (any(df$is_exception)) {
      ex <- df %>% filter(is_exception)
      others_avg <- df %>% filter(!is_exception) %>% summarise(m = mean(stationary_prob)) %>% pull(m)
      div(class = "bs-callout warm",
          tags$strong(ex$unit), " is Amsterdam Centraal's station square. It has the single highest ",
          "betweenness centrality of any modelled unit (", format(ex$betweenness, big.mark = ","),
          ", roughly 30% higher than the next-highest bridge) ", tags$em("and"), " a stationary probability ",
          "of ", signif(ex$stationary_prob, 3), " \u2014 about ",
          round(ex$stationary_prob / ex$network_avg, 1), "\u00d7 the network average, and ",
          round(ex$stationary_prob / others_avg, 1), "\u00d7 higher than the other four bridges' own average.",
          br(), br(),
          "A major transit interchange is exactly the kind of place that earns both properties at once \u2014 ",
          "a structural chokepoint people are forced through, and somewhere people actually want to be. The ",
          "other four bridges are pure shortcuts, with no particular pull of their own.")
    } else {
      div(class = "bs-callout",
          "Once residential, unclassified, and living_street roads are removed, ", tags$strong("Stationsplein e.o."),
          " doesn't even appear among this network's top-5 bridges any more \u2014 the shortest paths that made ",
          "it a chokepoint evidently ran, in part, through local streets that no longer exist here. All five ",
          "bridge units in this version sit close together on accessibility, each near or only modestly above ",
          "the network average. This is the cleaner version of \u201cbridging \u2260 accessible\u201d, with no ",
          "single unit distorting the pattern.")
    }
  })
  
  # -------------------------------------------------- reactive sensor state
  edges_r <- reactiveVal(edges_df)
  observeEvent(input$resample, {
    e <- edges_r()
    e$observed <- runif(nrow(e)) < e$obs_prob
    edges_r(e)
  })
  
  filtered_edges <- reactive({
    e <- edges_r()
    if (!is.null(input$source_filter) && input$source_filter != "All roads") {
      e <- e[e$network_source == input$source_filter, ]
    }
    e
  })
  
  # ---------------------------------------------------------- Coverage demo
  output$coverage_map <- renderPlotly({
    e <- filtered_edges()
    p <- suppressWarnings(
      ggplot(e) +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = observed,
                         text = paste0(road_class, " \u00b7 ", network_source, "<br>",
                                       round(length_m), " m \u00b7 ", speed_kmh, " km/h")),
                     linewidth = 0.9) +
        scale_color_manual(values = c(`TRUE` = col_signal, `FALSE` = col_void),
                           labels = c(`TRUE` = "Observed", `FALSE` = "Blind spot")) +
        coord_equal() + theme_void(base_size = 13) +
        theme(legend.position = "none",
              plot.background  = element_rect(fill = col_bg, color = NA),
              panel.background = element_rect(fill = col_bg, color = NA))
    )
    ggplotly(p, tooltip = "text") %>% plotly_dark(legend_top = FALSE)
  })
  
  output$coverage_table <- renderDT({
    e <- filtered_edges()
    # NOTE: `observed` here is a logical column (one row per road segment).
    # The previous version wrote `observed = sum(observed), pct_observed =
    # round(100 * mean(observed), 1)` inside the same summarise() call --
    # dplyr evaluates a summarise()'s expressions in order and makes each
    # newly-created column available to the ones after it, so by the time
    # pct_observed was computed, "observed" no longer referred to the
    # original per-row logical column but to the just-created single-number
    # count (sum(observed)). mean() of that single number is just itself, so
    # pct_observed silently became sum(observed) * 100 (e.g. 6 observed segments
    # -> "600") instead of the actual percentage. Naming the count column
    # something else avoids the shadowing.
    tbl <- e %>% group_by(network_source) %>%
      summarise(segments = n(),
                n_observed = sum(observed),
                pct_observed = round(100 * n_observed / segments, 1),
                .groups = "drop") %>%
      arrange(desc(segments))
    datatable(tbl, options = list(dom = "t", pageLength = 5), rownames = FALSE,
              colnames = c("Network source", "Segments", "Observed", "% observed"))
  })
  
  # ---------------------------------------------------------------- KDE demo
  observeEvent(input$bw_tight,    updateSliderInput(session, "bandwidth", value = 250))
  observeEvent(input$bw_balanced, updateSliderInput(session, "bandwidth", value = 600))
  observeEvent(input$bw_wide,     updateSliderInput(session, "bandwidth", value = 1600))
  
  output$kde_map <- renderPlotly({
    e <- edges_r()
    h <- input$bandwidth
    obs_idx <- which(e$observed)
    kde_val <- if (length(obs_idx) == 0) rep(0, nrow(e)) else
      rowSums(matrix(dnorm(mid_dist_mat[, obs_idx], sd = h), ncol = length(obs_idx)))
    e$kde_value <- kde_val
    
    p <- suppressWarnings(
      ggplot(e) +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, color = kde_value,
                         text = paste0(road_class, "<br>KDE = ", round(kde_value, 4))),
                     linewidth = 1) +
        scale_color_gradientn(colours = grad_pal, name = "Density") +
        coord_equal() + theme_void(base_size = 13) +
        theme(plot.background  = element_rect(fill = col_bg, color = NA),
              panel.background = element_rect(fill = col_bg, color = NA),
              legend.text  = element_text(color = col_text))
    )
    p <- ggplotly(p, tooltip = "text") %>% plotly_dark(legend_top = FALSE)
    
    # ggplotly's colorbar for a continuous scale on geom_segment doesn't
    # reliably inherit the layout-level font colour the way ordinary legend
    # text does -- its tick/title font live on the trace itself
    # (marker.colorbar or line.colorbar), so force them explicitly here
    # rather than relying on plotly_dark()'s general font setting.
    for (i in seq_along(p$x$data)) {
      if (!is.null(p$x$data[[i]]$marker$colorbar)) {
        p$x$data[[i]]$marker$colorbar$tickfont  <- list(color = col_text)
        p$x$data[[i]]$marker$colorbar$titlefont <- list(color = col_text)
      }
      if (!is.null(p$x$data[[i]]$line$colorbar)) {
        p$x$data[[i]]$line$colorbar$tickfont  <- list(color = col_text)
        p$x$data[[i]]$line$colorbar$titlefont <- list(color = col_text)
      }
    }
    p
  })
  
  # ---------------------------------------------------------- K-function demo
  kfun_data <- reactive({
    input$reshuffle  # dependency only: click "reshuffle" to redraw the CSR envelope
    e <- edges_r()
    obs_idx <- which(e$observed)
    n_obs <- length(obs_idx)
    r_seq <- seq(200, input$r_max, length.out = 15)
    nsim  <- as.integer(input$n_sim)
    n_all <- nrow(e)
    
    K_at <- function(idx, r) {
      n <- length(idx)
      if (n < 2) return(0)
      sub <- mid_dist_mat[idx, idx]
      (total_length_m / n^2) * sum(sub <= r & sub > 0)
    }
    
    K_obs <- vapply(r_seq, function(r) K_at(obs_idx, r), numeric(1))
    
    sims <- replicate(nsim, {
      idx <- sample.int(n_all, n_obs)
      vapply(r_seq, function(r) K_at(idx, r), numeric(1))
    })
    lo <- apply(sims, 1, quantile, probs = 0.025)
    hi <- apply(sims, 1, quantile, probs = 0.975)
    
    list(r = r_seq, K_obs = K_obs, lo = lo, hi = hi,
         clustered = mean(K_obs > hi), dispersed = mean(K_obs < lo))
  })
  
  output$kfun_plot <- renderPlotly({
    res <- kfun_data()
    plot_ly() %>%
      add_ribbons(x = res$r, ymin = res$lo, ymax = res$hi, name = "CSR envelope (95%)",
                  fillcolor = "rgba(91,107,124,0.35)",
                  line = list(color = "rgba(91,107,124,0)")) %>%
      add_lines(x = res$r, y = res$K_obs, name = "Observed K(r)",
                line = list(color = col_signal, width = 3)) %>%
      layout(xaxis = list(title = "Network distance r (m)"), yaxis = list(title = "K(r)")) %>%
      plotly_dark()
  })
  
  output$kfun_interpretation <- renderUI({
    res <- kfun_data()
    verdict <- if (res$clustered > 0.3) {
      "Sensors sit clearly above the CSR envelope across most of the r-range: monitoring locations are spatially clustered, most plausibly along the arterial/motorway ring rather than spread evenly across the network."
    } else if (res$dispersed > 0.3) {
      "Sensors sit below the CSR envelope: monitoring locations are more evenly spaced than a random placement would produce."
    } else {
      "The observed curve stays mostly within the CSR envelope: no strong evidence of clustering or regularity at these distances, given the current sensor sample."
    }
    div(class = "bs-callout", style = "margin-top:0.8rem;", verdict)
  })
  
  # ---------------------------------------------------------- Anomaly demo
  # Shared colour grammar for the whole tab: every plot below marks removed
  # segments in the same red, so the network map, both accessibility maps,
  # and the KPI numbers are all reading off one consistent visual language.
  col_removed <- "#ef4444"
  
  removed_edges_rv <- reactiveVal(integer(0))
  
  observeEvent(input$anomaly_reset, removed_edges_rv(integer(0)))
  
  observeEvent(suppressWarnings(plotly::event_data("plotly_click", source = "anomaly_map")), {
    click <- suppressWarnings(plotly::event_data("plotly_click", source = "anomaly_map"))
    req(click, click$customdata)
    eid <- as.integer(click$customdata[1])
    current <- removed_edges_rv()
    if (eid %in% current) {
      # restoring a previously-removed edge is always safe
      removed_edges_rv(setdiff(current, eid))
    } else {
      candidate <- union(current, eid)
      if (would_disconnect_network(candidate)) {
        showNotification(
          "That would cut a block off from the rest of the network entirely \u2014 try a different segment.",
          type = "warning", duration = 4
        )
      } else {
        removed_edges_rv(candidate)
      }
    }
  })
  
  anomaly_models <- reactive(compute_models(edges_df, remove_edge_ids = removed_edges_rv()))
  
  output$anomaly_n_removed <- renderText(length(removed_edges_rv()))
  
  output$anomaly_sensors_lost <- renderText({
    sum(edges_df$observed[edges_df$edge_id %in% removed_edges_rv()])
  })
  
  output$anomaly_coverage_pct <- renderText({
    total_mw <- edges_df %>% filter(road_class == "motorway")
    if (nrow(total_mw) == 0) return("\u2014")
    n_ok <- sum(total_mw$observed & !(total_mw$edge_id %in% removed_edges_rv()))
    paste0(round(100 * n_ok / nrow(total_mw)), "%")
  })
  
  # The one plot that's actually clickable. Built directly with plot_ly
  # (rather than ggplot + ggplotly) so each edge's midpoint marker can carry
  # its edge_id as `customdata` -- a much more reliable way to identify which
  # segment was clicked than trying to back it out of curveNumber/pointNumber.
  output$anomaly_network_map <- renderPlotly({
    removed <- removed_edges_rv()
    e <- edges_df %>%
      mutate(is_removed = edge_id %in% removed,
             status = case_when(is_removed ~ "Removed", observed ~ "Observed", TRUE ~ "Blind spot"),
             is_motorway = road_class == "motorway")
    
    draw_group <- function(p, df, color, lw) {
      if (nrow(df) == 0) return(p)
      xs <- as.vector(rbind(df$x1, df$x2, NA))
      ys <- as.vector(rbind(df$y1, df$y2, NA))
      p %>% add_lines(x = xs, y = ys, line = list(color = color, width = lw),
                      hoverinfo = "skip", showlegend = FALSE)
    }
    
    p <- plot_ly(source = "anomaly_map") %>%
      draw_group(e %>% filter(status == "Blind spot", !is_motorway), col_void, 1.6) %>%
      draw_group(e %>% filter(status == "Blind spot", is_motorway), col_void, 3.2) %>%
      draw_group(e %>% filter(status == "Observed", !is_motorway), col_signal, 1.6) %>%
      draw_group(e %>% filter(status == "Observed", is_motorway), col_signal, 3.2) %>%
      draw_group(e %>% filter(status == "Removed"), col_removed, 4) %>%
      add_markers(
        x = e$mid_x, y = e$mid_y,
        marker = list(color = "rgba(244,241,234,0.45)", size = 9),
        customdata = e$edge_id,
        text = paste0(e$road_class, " \u00b7 ", e$network_source, "<br>", e$status, "<br>click to toggle"),
        hoverinfo = "text", showlegend = FALSE
      ) %>%
      layout(xaxis = list(visible = FALSE, zeroline = FALSE),
             yaxis = list(visible = FALSE, zeroline = FALSE, scaleanchor = "x"),
             showlegend = FALSE, margin = list(l = 0, r = 0, t = 10, b = 0),
             paper_bgcolor = col_bg, plot_bgcolor = col_bg,
             font = list(color = col_text, family = "Space Grotesk")) %>%
      config(displaylogo = FALSE) %>%
      plotly::event_register("plotly_click")
    p
  })
  
  render_anomaly_access_map <- function(values, metric_label) {
    nd <- nodes_df
    raw  <- values[as.character(nd$id)]
    val01 <- scales::rescale(raw)
    pal_fn <- scales::col_numeric(grad_pal, domain = c(0, 1))
    nd$col  <- pal_fn(val01)
    nd$size <- scales::rescale(val01, to = c(2, 7))
    nd$tip  <- paste0(nd$label, "<br>", metric_label, ": ", signif(raw, 4))
    
    e <- edges_df %>%
      mutate(is_removed = edge_id %in% removed_edges_rv(),
             col = ifelse(is_removed, col_removed, "#2a2e35"),
             lw  = ifelse(is_removed, 2.4, 0.5))
    
    p <- suppressWarnings(
      ggplot() +
        geom_segment(data = e, aes(x = x1, y = y1, xend = x2, yend = y2, color = col, linewidth = lw)) +
        geom_point(data = nd, aes(x = x, y = y, color = col, size = size, text = tip)) +
        scale_color_identity() +
        scale_linewidth_identity() +
        scale_size_identity() +
        coord_equal() + theme_void(base_size = 13) +
        theme(legend.position = "none",
              plot.background  = element_rect(fill = col_bg, color = NA),
              panel.background = element_rect(fill = col_bg, color = NA))
    )
    ggplotly(p, tooltip = "text") %>% plotly_dark(legend_top = FALSE)
  }
  
  output$anomaly_baseline_map <- renderPlotly(render_anomaly_access_map(anomaly_models()$baseline, "Baseline"))
  output$anomaly_markov_map   <- renderPlotly(render_anomaly_access_map(anomaly_models()$markov, "Stationary prob."))
  
  output$anomaly_rank_table <- renderDT({
    normal_markov <- models_baseline$markov
    current_markov <- anomaly_models()$markov
    tbl <- nodes_df %>%
      transmute(unit = label,
                rank_normal  = rank(-normal_markov[as.character(id)]),
                rank_current = rank(-current_markov[as.character(id)])) %>%
      mutate(rank_shift = rank_normal - rank_current) %>%
      arrange(desc(abs(rank_shift))) %>%
      head(12)
    datatable(tbl, options = list(dom = "t", pageLength = 12), rownames = FALSE,
              colnames = c("Unit", "Rank (untouched)", "Rank (current)", "Rank shift"))
  })
}


shinyApp(ui, server)