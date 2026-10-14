if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE) &&
    .Platform$OS.type == "unix") {
  for (loc in c("C.UTF-8", "en_US.UTF-8")) {
    if (grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) break
    suppressWarnings(try(Sys.setlocale("LC_CTYPE", loc), silent = TRUE))
  }
}

suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(igraph)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
  library(leaflet)
  library(jsonlite)
}))
set.seed(2026)

`%||%` <- function(a, b) if (is.null(a)) b else a

#rsconnect::writeManifest()

# Small helpers so a variable like "P_time" or "t_ij" reads as proper math
# (italic base, true subscript) everywhere it appears in prose, instead of
# a literal underscore -- used for anything outside the MathJax $$...$$
# equation blocks, which already render properly on their own. Two
# versions: one returns Shiny UI tags (for p()/div() body text), the other
# a raw HTML string (for contexts that can't take tag objects, like a DT
# table cell rendered with escape = FALSE, or a Plotly axis tick label,
# which Plotly's own text parser accepts a small pseudo-HTML subset for).
math_sub_tag <- function(base, sub, italic_base = TRUE) {
  tagList(if (italic_base) tags$em(base) else base, tags$sub(tags$em(sub)))
}
math_sub_html <- function(base, sub, italic_base = TRUE) {
  b <- if (italic_base) paste0("<i>", base, "</i>") else base
  paste0(b, "<sub><i>", sub, "</i></sub>")
}

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

# ---------------------------------------------------------------------------
# REAL-MAP-ONLY COLOURS -- built from the app's own five-colour palette
# above rather than an outside one (a viridis-style rainbow scale reads
# well for contrast but clashes with the rest of the app's teal/green/amber
# editorial look). Each layer still gets a colour nothing else on the map
# is using, so roads/sensors/choropleth stay visually distinct from each
# other -- just all drawn from the same family.
# ---------------------------------------------------------------------------
col_road_primary <- col_signal   # non-residential (arterial/motorway) roads -- the app's own signature green
col_road_local    <- "#f472b6"   # residential roads -- pink, clearly not grey/blue so it doesn't blend into the map
col_sensor_on     <- "#22d3ee"   # sensor: observed -- cyan, unlike anything else on the map
col_sensor_off    <- col_negative # sensor: unobserved -- reuses the app's existing "negative/missing" red
col_station_outline <- col_amber # Stationsplein e.o. outline -- amber, the same colour already used
# for "the exception" everywhere else (the KPI dot, the bridge-unit
# markers), so the map reinforces the same visual language
col_accessibility <- "#5b8cff"    # neighbourhood accessibility choropleth (high end) -- a distinct blue.
# This used to be col_signal, the exact same green as the
# non-residential roads layer, so a highly-accessible neighbourhood
# and an arterial road painted the same colour on top of each other --
# indistinguishable at a glance. Blue isn't used anywhere else on
# either map (green/pink/cyan/red/amber are all already spoken for),
# so the choropleth now reads as its own layer.
# Sequential choropleth ramp for neighbourhood accessibility -- a plain light
# grey (low) through to a distinct blue (high), interpolated into 4 quantile
# bands below. Deliberately just a two-colour ramp rather than a busier
# multi-hue scale.
map_choropleth_colors <- c("#e3e6ea", col_accessibility)  # light grey -> distinct blue

plotly_dark <- function(p, legend_top = TRUE) {
  p %>% layout(
    paper_bgcolor = col_bg, plot_bgcolor = col_bg,
    # Explicit size, not just color/family -- plotly.js's own default axis/
    # tick/legend text is ~12px, which reads as a blur from the back of a
    # room on a projector. 15px brings every plotly chart in the app up to
    # the same big-screen-legible baseline as the surrounding page text.
    font = list(color = col_text, family = "Space Grotesk", size = 15),
    xaxis = list(gridcolor = "#262a31", zerolinecolor = "#262a31"),
    yaxis = list(gridcolor = "#262a31", zerolinecolor = "#262a31"),
    legend = if (legend_top) list(orientation = "h", y = 1.08, font = list(color = col_text, size = 15)) else list(font = list(color = col_text, size = 15)),
    margin = list(t = 30)
  ) %>% config(displaylogo = FALSE)
}

# ---------------------------------------------------------------------------
# REAL THESIS RESULTS — Amsterdam network (not synthetic)
#    Hardcoded from the actual printed tables/messages in the two rendered
#    companion reports. "Full network" = all motorised classes; "No
#    residential roads" = residential/unclassified/living street excluded.
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

# Display-only relabelling for the two correlation heatmaps below -- the
# matrix's own dimnames ("P_time" etc.) stay as-is since get_cor() above
# indexes into them by that exact literal name; this is purely what's
# shown on the plot axes, via Plotly's own small pseudo-HTML text parser
# (it renders <sub>/<i> the same way it does in hover text or titles).
real_corr_axis_labels <- c(
  P_time     = math_sub_html("P", "time"),
  P_speed    = math_sub_html("P", "speed"),
  P_observed = math_sub_html("P", "observed"),
  Baseline   = "Baseline"
)

# ---------------------------------------------------------------------------
# REAL AMSTERDAM MAP LAYERS -- the actual road network (21,605 segments,
# same count as the Overview KPI) and administrative-unit boundaries, both
# exported straight from the two rendered reports' own sf objects (see
# export_map_data.R).
#
# Roads and the choropleth are loaded as plain GeoJSON *text* here, not
# parsed into R lists -- addGeoJSON() accepts either form, but for data this
# size the difference is enormous. Parsing into an R list and letting
# htmlwidgets serialise it back to JSON on every render means
# jsonlite::toJSON walking ~22,000 deeply-nested feature objects by hand,
# which is genuinely slow (independently measured at 6-9 seconds per road
# layer, ~8 seconds for the admin polygons -- and that cost was being paid
# freshly for *every* map render, twice over, once per tab). Handing Leaflet
# already-valid GeoJSON text instead skips that walk entirely; the browser's
# own JSON.parse() does the work in native code and is not the bottleneck.
#
# The road files below are the same network as app_roads.geojson, just
# pre-split by road_class once instead of on every app start. The
# choropleth files are further along the same idea: the "Neighbourhood
# accessibility" layer's per-feature fill colour never depends on anything
# the user can change at runtime (the colour tokens and the quantile
# binning are fixed), so both scenarios' fully-styled GeoJSON were
# precomputed once (see build_choropleth_cache.R) rather than being rebuilt
# from admin_units.geojson on every render and every scenario toggle.
# Re-run that script and redeploy its two output files if the colour
# tokens, the binning, or the underlying admin-units data ever change.
read_geojson_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

roads_nonresidential <- read_geojson_text("app_roads_nonresidential.json")
roads_residential    <- read_geojson_text("app_roads_residential.json")
admin_choropleth_json <- list(
  "Full network"         = read_geojson_text("app_admin_choropleth_full.json"),
  "No residential roads" = read_geojson_text("app_admin_choropleth_wr.json")
)

admin_geojson   <- jsonlite::fromJSON("app_admin_units.geojson", simplifyVector = FALSE)
sensors_geojson <- jsonlite::fromJSON("app_sensor_locations.geojson", simplifyVector = FALSE)

# One row per real admin unit (neighbourhood), pulled straight out of the
# GeoJSON properties -- used for markers/labels/lookups without needing sf
# at runtime. Only 517 small feature *properties* are touched here (the
# heavy nested coordinate arrays are never parsed into R for this), so this
# stays fast even though it does go through fromJSON.
admin_centroids <- do.call(rbind, lapply(admin_geojson$features, function(f) {
  p <- f$properties
  data.frame(
    unit_name             = p$unit_name %||% NA_character_,
    lon                    = p$centroid_lon %||% NA_real_,
    lat                    = p$centroid_lat %||% NA_real_,
    stationary_prob_full   = p$stationary_prob_full %||% NA_real_,
    stationary_prob_wr     = p$stationary_prob_wr %||% NA_real_,
    component              = p$component %||% NA_integer_,
    is_station_square      = isTRUE(p$is_station_square),
    stringsAsFactors = FALSE
  )
}))

# The 693 real monitoring locations (same count as the Overview KPI), as
# plain lon/lat -- an optional extra layer tying this map back to the whole
# "data availability" premise of the app, not just the road network itself.
sensor_points <- do.call(rbind, lapply(sensors_geojson$features, function(f) {
  p <- f$properties
  coords <- f$geometry$coordinates
  data.frame(
    location_name  = p$location_name %||% NA_character_,
    network_source = p$network_source %||% NA_character_,
    observed       = isTRUE(p$observed),
    lon            = coords[[1]] %||% NA_real_,
    lat            = coords[[2]] %||% NA_real_,
    stringsAsFactors = FALSE
  )
}))

# admin_pal_c1/admin_pal_c2 are kept (cheap -- only 517 numbers, not geometry)
# purely to drive addLegend()'s swatch; the actual per-feature fill colours
# now come from the precomputed admin_choropleth_json files above (built by
# build_choropleth_cache.R), not from calling either pal again at render time.
#
# Stationary probability is heavily right-skewed (a handful of very
# accessible units) -- a *linear* colour scale (col_numeric) crushes almost
# every neighbourhood into the same near-invisible shade near the low end.
# col_quantile bins by rank instead of raw value, so the map always has
# visible contrast no matter how skewed the underlying numbers are. Kept
# deliberately coarse (4 bands, not 7+) -- enough to read "low / mid / high
# / exceptional" without turning the map into visual noise.
#
# TWO separate palettes, not one: the road network splits into two
# disconnected components, so the Markov chain (and its stationary
# distribution) is run separately for each -- component 1, the main
# network (445 units), and component 2, a smaller landmass disconnected
# from it (70 units, south-east Amsterdam). Each chain's probabilities sum
# to 1 within that chain alone, so component 2's raw values run
# systematically higher purely because it has far fewer nodes sharing that
# same probability mass -- not because those units are more accessible in
# any way that's numerically comparable to component 1. Building each
# component's quantile palette from its own values (but the same colour
# ramp and bin count) lets both show up combined on one choropleth -- a
# colour always means "this unit's relative position within its own
# component" -- without implying the underlying numbers sit on one shared
# scale. See build_choropleth_cache.R for the full explanation and the
# per-feature tooltip that flags this for component-2 units.
admin_values_c1 <- c(admin_centroids$stationary_prob_full[admin_centroids$component == 1],
                     admin_centroids$stationary_prob_wr[admin_centroids$component == 1])
admin_values_c1 <- admin_values_c1[!is.na(admin_values_c1)]
admin_pal_c1 <- scales::col_quantile(
  map_choropleth_colors,
  domain = admin_values_c1,
  n = 4,
  na.color = col_negative
)

# ---------------------------------------------------------------------------
# BASEMAP TILES -- back on CartoDB's actual dark basemap, the look this app
# started with. CartoDB (CARTO) still gives this away for free -- it's just
# no longer key-less: as of their basemaps migration, raster tiles show an
# "API key required" watermark until a (still free) key is attached. Getting
# one takes under a minute, no CARTO account needed, no approval queue, 5
# million tile requests/month on the free tier:
#
#   1. Open https://dashboard.basemaps.carto.com/ (or the embed form at
#      https://dashboard.basemaps.carto.com/?embed=1) and generate a key.
#   2. Paste it into carto_api_key below, between the quotes.
#   3. Redeploy -- the watermark is gone and it's the real CartoDB Dark
#      Matter tiles, not a substitute.
#
# Left blank, the app quietly falls back to Esri's "World Dark Gray Base" --
# also genuinely free/no-key, also dark and minimal -- instead of ever
# showing CARTO's watermark.
carto_api_key <- ""   # <-- paste your free CARTO basemaps key here

basemap_tile_url <- if (nzchar(carto_api_key)) {
  paste0("https://basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png?key=", carto_api_key)
} else {
  "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}"
}
basemap_attribution <- if (nzchar(carto_api_key)) {
  "\u00a9 <a href='https://carto.com/attributions'>CARTO</a>, \u00a9 OpenStreetMap contributors"
} else {
  "Tiles \u00a9 Esri \u2014 Esri, DeLorme, NAVTEQ"
}

# A dark, real basemap consistent with the rest of the app's theme, with
# both real road-network layers plus the accessibility choropleth already
# on it. Shared by both the Real Findings and Station Square maps so they
# stay visually identical apart from their initial view and any extra
# markers -- built once per call, then adjusted with setView()/flyTo().
#
# preferCanvas = TRUE: with 21,605 individual road segments plus 517
# neighbourhood polygons plus 693 sensor points all drawn on the same map,
# Leaflet's default SVG renderer -- one DOM element per feature -- is the
# actual reason the map itself feels slow to paint in and pan/zoom
# afterwards. Canvas rendering draws all of it straight to pixels instead,
# which is dramatically faster at this feature count and looks identical.
base_road_leaflet <- function(scenario) {
  show_residential <- scenario != "No residential roads"
  leaflet(options = leafletOptions(minZoom = 10, maxZoom = 18, zoomControl = TRUE,
                                   preferCanvas = TRUE)) %>%
    addTiles(
      urlTemplate = basemap_tile_url,
      attribution = basemap_attribution,
      options = tileOptions(opacity = 1)
    ) %>%
    addGeoJSON(admin_choropleth_json[[scenario]], group = "Neighbourhood accessibility",
               stroke = TRUE, fill = TRUE) %>%
    addGeoJSON(roads_nonresidential, group = "Non-residential roads",
               color = col_road_primary, weight = 2, opacity = 0.95, fill = FALSE) %>%
    addGeoJSON(roads_residential, group = "Residential roads",
               color = col_road_local, weight = 0.8, opacity = 0.55, fill = FALSE) %>%
    addCircleMarkers(
      data = sensor_points, lng = ~lon, lat = ~lat,
      radius = 3.6, stroke = TRUE, color = "#0e1013", weight = 0.6, fillOpacity = 0.95,
      fillColor = ~ifelse(observed, col_sensor_on, col_sensor_off),
      label = ~paste0(network_source, ifelse(observed, " \u2014 observed", " \u2014 unobserved")),
      group = "Sensor coverage (693 real locations)"
    ) %>%
    # Stationsplein e.o. (Amsterdam Centraal) marked directly on the road
    # network itself, not only as the amber outline on its neighbourhood
    # polygon in the "Neighbourhood accessibility" choropleth -- that layer
    # is hidden by default, so until now the station square wasn't marked
    # anywhere a visitor would actually see without opening the layer picker
    # and switching it on. Off by default like the other extra overlays
    # below (a presenter switches it on deliberately, e.g. when walking
    # through Station Square specifically, rather than it always being on).
    addCircleMarkers(
      data = admin_centroids %>% filter(is_station_square), lng = ~lon, lat = ~lat,
      radius = 9, stroke = TRUE, color = col_station_outline, weight = 3, opacity = 1,
      fillColor = col_station_outline, fillOpacity = 0.25,
      label = ~paste0(unit_name, " \u2014 Amsterdam Centraal station square"),
      group = "Highlight: Station Square"
    ) %>%
    addLayersControl(
      overlayGroups = c("Non-residential roads", "Residential roads", "Neighbourhood accessibility",
                        "Sensor coverage (693 real locations)", "Highlight: Station Square"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup("Sensor coverage (693 real locations)") %>%
    hideGroup("Neighbourhood accessibility") %>%
    hideGroup("Highlight: Station Square") %>%
    { if (show_residential) . else hideGroup(., "Residential roads") } %>%
    # Swatch numbers reflect the main network's own scale (component 1) --
    # component 2's smaller, separately-run chain is coloured on the same
    # four-band ramp but its own scale (see admin_pal_c2 above), since one
    # legend can't show two numeric domains at once. The per-unit tooltip
    # flags this explicitly for every component-2 polygon.
    addLegend(
      position = "bottomright", pal = admin_pal_c1, values = admin_values_c1,
      title = HTML("Neighbourhood accessibility<br>stationary prob."), opacity = 0.95,
      className = "info legend legend-level"
    )
}

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
    paste0(math_sub_html("P", "time"), " \u2194 baseline correlation (Spearman \u03c1)"),
    paste0(math_sub_html("P", "speed"), " \u2194 baseline correlation (Spearman \u03c1)"),
    paste0(math_sub_html("P", "observed"), " \u2194 baseline correlation (Spearman \u03c1)"),
    "Neighbourhoods with a computable baseline score"
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
  # Topology-free baseline: each unit scored only from the roads touching it
  # (inverse of minutes-per-km over its own segments), with no shortest-path
  # or network reasoning -- the same local definition as the real analysis.
  ids_all <- as.character(nodes_df$id)
  baseline <- vapply(nodes_df$id, function(i) {
    inc <- e2 %>% filter(from == i | to == i)
    if (nrow(inc) == 0) return(NA_real_)
    sum(inc$length_m / 1000) / (sum(inc$travel_time_min) / 60)
  }, numeric(1))
  names(baseline) <- ids_all
  n <- length(ids_all)
  
  # Markov chain as in the paper: transitions only between units that are
  # adjacent in the ORIGINAL network (a closed road does not change which
  # units share a boundary), with weight 1 / (shortest-path travel time on
  # the CURRENT network). Closing a road therefore re-routes travel times
  # for every adjacent pair whose fastest route used it.
  g_t <- graph_from_data_frame(e2 %>% select(from, to), directed = FALSE,
                               vertices = nodes_df %>% select(id))
  dmat <- distances(g_t, weights = e2$travel_time_min)
  idn <- as.character(nodes_df$id)
  P <- matrix(0, n, n, dimnames = list(idn, idn))
  for (k in seq_len(nrow(e))) {
    a <- as.character(e$from[k]); b <- as.character(e$to[k])
    t_ab <- dmat[a, b]
    if (is.finite(t_ab) && t_ab > 0) {
      P[a, b] <- 1 / t_ab
      P[b, a] <- 1 / t_ab
    }
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
      shinydashboard::menuItem("How to read", tabName = "start"),
      shinydashboard::menuItem("Overview", tabName = "home", selected = TRUE),
      shinydashboard::menuItem("Accessibility", tabName = "access"),
      shinydashboard::menuItem("See The Methods", tabName = "methods"),
      shinydashboard::menuItem("Real Findings", tabName = "realdata"),
      shinydashboard::menuItem("Station Square", tabName = "station"),
      shinydashboard::menuItem("Anomaly", tabName = "anomaly"),
      shinydashboard::menuItem("Conclusion", tabName = "conclusion")
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

        /* Root font-size, not just cosmetic: every rem value in this
           stylesheet is relative to it, and it's also what any *unstyled*
           standard Bootstrap control (radio/select/slider labels, button
           text -- none of which get their own explicit font-size below)
           actually inherits. Bootstrap 3's own default is 14px; bumped
           here for a room of ~200 people reading off a projector, not a
           laptop screen up close. */
        html { font-size: 18px; }
        html, body, .content-wrapper, .right-side, .main-footer {
          background: var(--bs-bg) !important;
          font-family: 'Space Grotesk', sans-serif !important;
          color: var(--bs-ink) !important;
          font-size: 1rem !important;
        }
        * { box-sizing: border-box; }

        /* Remove the stock dashboard chrome while retaining its tab system. */
        .main-header, .main-sidebar { display: none !important; }
        .content-wrapper, .right-side { margin-left: 0 !important; }
        .main-footer { display: none !important; }
        .content { padding: 0 2px 28px !important; }

        /* Was a fixed 1240px, which left wide, wasted black margins on
           anything bigger than a small laptop -- including the projector
           this gets presented on. Scales with the viewport instead (94% of
           it) up to a cap that's still generous on very wide/4K displays,
           rather than stopping at a small-laptop width regardless of how
           much screen is actually available. */
        .bs-shell { max-width: min(97vw, 2600px); margin: 0 auto; padding: 28px 24px 8px; }
        /* ---- step list, gap stats, lessons, details ---- */
        .bs-steps { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 16px; margin-top: 14px; }
        .bs-steps > .bs-roadmap-step:last-child:nth-child(4n+3) { grid-column: span 2; }
        @media (max-width: 1100px) { .bs-steps { grid-template-columns: repeat(2, minmax(0, 1fr)); } .bs-steps > .bs-roadmap-step:last-child:nth-child(4n+3) { grid-column: auto; } }
        .bs-more { margin-top: 14px; color: var(--bs-soft); font-size: 18px; line-height: 1.65; }
        .bs-more summary { cursor: pointer; color: var(--bs-accent); font-weight: 600; font-size: 18px; }
        .bs-more p { margin: 10px 0 0; }
        .bs-gap-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 18px; margin: 20px 0 8px; }
        .bs-gap-stat { background: var(--bs-bg); border: 1px solid var(--bs-border); border-radius: 8px; padding: 22px 26px; }
        .bs-gap-label { font-size: 16px; letter-spacing: .14em; text-transform: uppercase; color: var(--bs-faint); font-weight: 700; }
        .bs-gap-num { font-family: 'Fraunces', Georgia, serif; font-size: 88px; line-height: 1.05; margin: 6px 0 12px; }
        .bs-gap-num.teal { color: var(--bs-accent); }
        .bs-gap-num.warm { color: var(--bs-warm); }
        .bs-gap-bar { height: 14px; background: #23262c; border-radius: 7px; overflow: hidden; }
        .bs-gap-fill { height: 100%; width: 0; border-radius: 7px; transition: width 1.6s cubic-bezier(.2,.7,.2,1); }
        .bs-gap-fill.teal { background: var(--bs-accent); }
        .bs-gap-fill.warm { background: var(--bs-warm); }
        .bs-gap-sub { margin-top: 12px; color: var(--bs-soft); font-size: 19px; line-height: 1.5; }
        .bs-lesson-num { font-family: 'Fraunces', Georgia, serif; font-size: 64px; line-height: 1; color: var(--bs-accent); }
        .bs-lesson h2 { font-size: 27px !important; }
        .bs-list { margin: 8px 0 0; padding-left: 22px; color: var(--bs-soft); font-size: 19px; line-height: 1.6; }
        .bs-list li { margin-bottom: 8px; }
        .bs-list li.main { color: var(--bs-ink); font-weight: 600; }
        .ring-dot { display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 2px solid var(--bs-warm);
                    margin-right: 6px; vertical-align: -1px; }
        .bs-fw h3 { font-family: 'Fraunces', Georgia, serif; font-size: 22px; color: var(--bs-accent); margin: 0; padding-top: 14px; }
        .bs-fw .bs-list { font-size: 18px; }
        .bs-fw .bs-list li { margin-bottom: 5px; }
        .bs-stack { grid-template-columns: 1fr !important; }
        .bs-stack .bs-fw { columns: 2; column-gap: 48px; }
        .bs-stack .bs-fw h3 { break-after: avoid; }
        .bs-stack .bs-fw ul { break-inside: avoid; }
        @media (max-width: 900px) { .bs-stack .bs-fw { columns: 1; } }
        .bs-present-btn { display: none !important; margin-left: auto; background: transparent; border: 1px solid var(--bs-border); color: var(--bs-faint);
                          border-radius: 6px; padding: 8px 14px; font-size: 16px; font-weight: 600; cursor: pointer; }
        .bs-present-btn:hover { color: var(--bs-ink); }
        body.bs-presenter .bs-present-btn { background: var(--bs-warm); color: #14171c; border-color: var(--bs-warm); }
        body.bs-presenter .bs-spoken, body.bs-presenter .bs-more,
        body.bs-presenter .bs-card-subtitle:not(.bs-keep) { display: none !important; }
        .bs-read p { margin: 0 0 8px; font-size: 19px; line-height: 1.5; color: var(--bs-soft); }
        .bs-read-dot { display: inline-block; width: 12px; height: 12px; border-radius: 50%; margin-right: 10px; }
        .bs-two-col { display: grid; grid-template-columns: 1fr 1fr; gap: 24px; margin-top: 8px; }
        .bs-two-col h3 { font-family: 'Fraunces', Georgia, serif; font-size: 24px; color: var(--bs-ink); margin: 0 0 6px; }
        .bs-two-col p { color: var(--bs-soft); font-size: 19px; line-height: 1.6; margin: 0; }
        @media (max-width: 900px) { .bs-gap-grid, .bs-two-col { grid-template-columns: 1fr; } .bs-steps { grid-template-columns: 1fr; } }
        .bs-walk-controls { display: flex; gap: 10px; flex-wrap: wrap; align-items: center; margin-bottom: 8px; }
        .bs-walk-count { color: var(--bs-soft); font-size: 18px; margin-left: 6px; }
        #bs-walk-svg { width: 100%; max-height: 360px; display: block; }
        @keyframes bsRise { from { opacity: 0; transform: translateY(12px); } to { opacity: 1; transform: none; } }
        .tab-content > .tab-pane.active .bs-card, .tab-content > .tab-pane.active .bs-kpi,
        .tab-content > .tab-pane.active .bs-callout { animation: bsRise .5s ease both; }
        .tab-content > .tab-pane.active .bs-grid-2 > :nth-child(2), .tab-content > .tab-pane.active .bs-grid-3 > :nth-child(2),
        .tab-content > .tab-pane.active .bs-kpis > :nth-child(2) { animation-delay: .08s; }
        .tab-content > .tab-pane.active .bs-grid-3 > :nth-child(3), .tab-content > .tab-pane.active .bs-kpis > :nth-child(3) { animation-delay: .16s; }
        .tab-content > .tab-pane.active .bs-kpis > :nth-child(4) { animation-delay: .24s; }
        @media (prefers-reduced-motion: reduce) { .bs-card, .bs-kpi, .bs-callout { animation: none !important; } }
        /* ---- University of Pretoria logo (top right of header) ---- */
        .bs-header-row { display: flex; align-items: center; justify-content: space-between; gap: 24px; }
        .bs-header-left { min-width: 0; }
        .bs-up-corner { flex: 0 0 auto; display: flex; align-items: center; gap: 16px; background: var(--bs-card);
                        border: 1px solid var(--bs-border); border-radius: 8px; padding: 12px 18px; }
        .bs-up-plate { background: #ffffff; border-radius: 6px; display: inline-flex; align-items: center; justify-content: center; padding: 8px 12px; }
        .bs-up-plate img { display: block; height: auto; width: 150px; }
        .bs-up-text { display: flex; flex-direction: column; gap: 2px; }
        .bs-up-dept { font-family: 'Fraunces', Georgia, serif; font-size: 20px; color: var(--bs-ink); line-height: 1.15; padding-bottom: 8px; margin-bottom: 2px; border-bottom: 1px solid var(--bs-border); }
        .bs-up-uni { font-size: 14px; color: var(--bs-faint); letter-spacing: .06em; text-transform: uppercase; }
        .bs-up-corner a.bs-paper-link { color: var(--bs-warm) !important; font-weight: 600; font-size: 16px;
                                        text-decoration: none; margin-top: 4px; }
        .bs-up-corner a.bs-paper-link:hover { text-decoration: underline; }

        /* ---- header ---- */
        .bs-header { margin-bottom: 22px; border-bottom: 1px solid var(--bs-border); padding-bottom: 16px; }
        .bs-header-top {
          display: flex; justify-content: space-between; gap: 16px; align-items: baseline;
          border-bottom: 1px solid var(--bs-border); padding-bottom: 8px; margin-bottom: 9px;
        }
        .bs-eyebrow { font-size: 15px; letter-spacing: .19em; text-transform: uppercase; color: var(--bs-faint); }
        .bs-title {
          font-family: 'Fraunces', Georgia, serif; font-size: 67.5px; font-weight: 600; line-height: 1;
          letter-spacing: -.03em; margin: 8px 0 6px; color: var(--bs-ink);
          text-shadow: 0 0 22px rgba(52,211,153,0.18);
        }
        .bs-title span { color: var(--bs-accent); }
        .bs-subtitle { margin: 0; color: var(--bs-faint); font-size: 17px; letter-spacing: .12em; text-transform: uppercase; }

        /* ---- top navigation ---- */
        .bs-nav {
          display: flex; gap: 3px; flex-wrap: wrap; margin-bottom: 22px; padding: 4px;
          background: var(--bs-card); border: 1px solid var(--bs-border); border-radius: 8px;
        }
        .bs-nav-link {
          display: inline-flex; align-items: center; gap: 7px; padding: 9px 13px; border-radius: 6px;
          color: var(--bs-faint) !important; background: transparent; text-decoration: none !important;
          font-size: 17px; font-weight: 600; letter-spacing: .02em; transition: all .18s ease;
        }
        .bs-nav-link:hover { color: var(--bs-ink) !important; background: #23262c; }
        .bs-nav-link.active { color: var(--bs-bg) !important; background: var(--bs-accent);
                               box-shadow: 0 0 14px rgba(52,211,153,0.35); }

        /* ---- signature dots ---- */
        .pulse-dot { display:inline-block; width:9px; height:9px; border-radius:50%;
                     background: var(--bs-accent); margin-right:6px; animation: pulseglow 2.2s infinite; }
        .blindspot-dot { display:inline-block; width:9px; height:9px; border-radius:50%;
                          background: var(--bs-void); border:1.5px dashed #8b97a3; margin-right:6px; }
        .amber-dot { display:inline-block; width:9px; height:9px; border-radius:50%;
                     background: var(--bs-warm); margin-right:6px; animation: pulseglow-amber 2.2s infinite; }
        .removed-dot { display:inline-block; width:9px; height:9px; border-radius:50%;
                        background: #e0575c; margin-right:6px; animation: pulseglow-red 2.2s infinite; }
        @keyframes pulseglow { 0%{box-shadow:0 0 0 0 rgba(52,211,153,.55);} 70%{box-shadow:0 0 0 9px rgba(52,211,153,0);} 100%{box-shadow:0 0 0 0 rgba(52,211,153,0);} }
        @keyframes pulseglow-red { 0%{box-shadow:0 0 0 0 rgba(224,87,92,.55);} 70%{box-shadow:0 0 0 9px rgba(224,87,92,0);} 100%{box-shadow:0 0 0 0 rgba(224,87,92,0);} }
        @keyframes pulseglow-amber { 0%{box-shadow:0 0 0 0 rgba(245,179,78,.55);} 70%{box-shadow:0 0 0 9px rgba(245,179,78,0);} 100%{box-shadow:0 0 0 0 rgba(245,179,78,0);} }

        /* ---- glowing map marker (Station Square exception) ---- */
        .glow-marker-wrap { background: transparent !important; border: none !important; }
        .glow-marker { width:10px; height:10px; margin-right:0; box-shadow: 0 0 8px 2px rgba(245,179,78,.6); }

        /* ---- card system ---- */
        .bs-card {
          background: var(--bs-card); border: 1px solid var(--bs-border); border-radius: 8px; padding: 20px;
          transition: transform .15s ease, box-shadow .15s ease;
        }
        .bs-card:hover { transform: translateY(-2px); box-shadow: 0 10px 26px rgba(0,0,0,0.35); }
        /* Every .bs-card gets a top margin unconditionally rather than via
           a '.bs-card + .bs-card' adjacent-sibling rule. The sibling-based
           version had two failure modes: (1) it fired between two cards
           that only *look* stacked but are actually side-by-side siblings
           inside a grid row (.bs-grid-2/3/4, .bs-roadmap) -- pushing every
           card after the first down by 16px within its own row, which with
           the grid still stretching each card's *bottom* to match is what
           read as 'these boxes are different sizes' throughout the app;
           and (2) it silently did nothing whenever the actual DOM sibling
           of a card wasn't another .bs-card at all -- e.g. the 'See The
           Methods' sub-tabs wrap each pill's content in its own
           shiny-panel-conditional div, so the formula card in one pill is
           a sibling of that wrapper div, never of the card before it, and
           the two rendered flush against each other with zero gap ('now
           theres no space'). An unconditional margin-top sidesteps both:
           it applies regardless of what the previous DOM sibling is, and
           is zeroed back out only for the specific cases that need it. */
        .bs-card { margin-top: 16px; }
        .tab-pane.active > .bs-card:first-child,
        .bs-grid-2 > .bs-card, .bs-grid-3 > .bs-card, .bs-grid-4 > .bs-card,
        .bs-roadmap > .bs-card { margin-top: 0 !important; }
        .bs-card-title { font-family: 'Fraunces', Georgia, serif; font-size: 30.5px; line-height: 1.15;
                          font-weight: 500; color: var(--bs-ink); margin: 0 0 4px; }
        .bs-card-subtitle { font-size: 18px; line-height: 1.55; color: var(--bs-faint); margin: 0 0 16px; }
        .bs-section-kicker { font-size: 15px; text-transform: uppercase; letter-spacing: .16em;
                              color: var(--bs-faint); margin: 0 0 7px; font-weight: 600; }

        /* ---- hero ---- */
        .bs-hero { display: grid; grid-template-columns: minmax(0, 1.5fr) minmax(260px, .8fr); gap: 18px; margin-bottom: 18px; }
        .bs-hero-main { background: #0e3a2c; color: var(--bs-ink); border-radius: 8px; padding: 28px;
                        border: 1px solid var(--bs-border); }
        .bs-hero-main .bs-section-kicker { color: #9fe3c4; }
        .bs-hero-main h2 { font-family: 'Fraunces', Georgia, serif; font-size: 46.5px; line-height: 1.08;
                            font-weight: 500; margin: 0 0 10px; letter-spacing: -.02em; }
        .bs-hero-main p { color: #d7ddd8; max-width: 700px; font-size: 19.5px; line-height: 1.75; margin: 0; }
        .bs-hero-side { background: var(--bs-card); border: 1px solid var(--bs-border); border-radius: 8px; padding: 22px; }
        .bs-hero-side strong { display: block; font-family: 'Fraunces', Georgia, serif; font-size: 27px;
                                font-weight: 500; margin-bottom: 7px; color: var(--bs-ink); }
        .bs-hero-side p { font-size: 18px; color: var(--bs-soft); line-height: 1.65; margin: 0; }

        /* ---- roadmap (Overview: how to read this app) ----
           One merged component: used to be a numbered-circle stepper teaser
           row followed by a separate row of four fuller Stop-N cards below
           the KPIs, both describing the same four tabs. Now a single row of
           four cards, each carrying the numbered circle, the icon+title, the
           fuller description, and the CTA link. */
        .bs-roadmap { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 16px; }
        .bs-roadmap-step {
          display: flex; flex-direction: column; align-items: flex-start;
          background: var(--bs-card); border: 1px solid var(--bs-border); border-radius: 8px;
          padding: 18px; height: 100%;
        }
        .bs-step-circle { width: 36px; height: 36px; border-radius: 50%; background: var(--bs-bg);
                           border: 2px solid var(--bs-accent); color: var(--bs-accent);
                           display: flex; align-items: center; justify-content: center;
                           font-family: 'IBM Plex Mono', monospace; font-weight: 600; font-size: 21px; margin-bottom: 12px; }
        .bs-roadmap-step .bs-card-title { font-size: 25.5px; margin-bottom: 6px; }
        .bs-roadmap-step .bs-card-subtitle { flex: 1 1 auto; margin-bottom: 14px; }
        .bs-roadmap-step .cta-link { margin-top: auto; }

        /* Icon badges (Overview roadmap + methodology tiles) -- Font Awesome
           glyphs in a tinted circle, cycling through the same four accent
           colours the KPI tiles already use, instead of plain emoji. */
        .bs-icon-badge { width: 36px; height: 36px; border-radius: 50%; margin-bottom: 10px;
                          display: flex; align-items: center; justify-content: center;
                          font-size: 21px; background: rgba(52,211,153,0.13); color: var(--bs-accent); }
        .bs-icon-badge.teal { background: rgba(47,184,166,0.14); color: var(--bs-teal); }
        .bs-icon-badge.void { background: rgba(91,107,124,0.20); color: var(--bs-void); }
        .bs-icon-badge.warm { background: rgba(245,179,78,0.14); color: var(--bs-warm); }

        /* ---- KPI cards ---- */
        .bs-kpis { display: flex; gap: 12px; flex-wrap: wrap; margin-top: 18px; margin-bottom: 18px; }
        .bs-kpi { flex: 1 1 180px; min-width: 170px; background: var(--bs-card); border: 1px solid var(--bs-border);
                  border-radius: 8px; padding: 16px 19px; border-top: 3px solid var(--bs-accent); }
        .bs-kpi.teal { border-top-color: var(--bs-teal); }
        .bs-kpi.warm { border-top-color: var(--bs-warm); }
        .bs-kpi.void { border-top-color: var(--bs-void); }
        .bs-kpi-label { font-size: 15px; text-transform: uppercase; letter-spacing: .14em; color: var(--bs-faint); margin-bottom: 4px; }
        .bs-kpi-value { font-family: 'IBM Plex Mono', monospace; font-size: 35px; font-weight: 600;
                         letter-spacing: -.01em; color: var(--bs-ink); line-height: 1.1; }
        .bs-kpi-sub { font-size: 17px; color: var(--bs-faint); margin-top: 4px; }

        /* ---- grids ---- */
        /* margin-top matters here: a standalone .bs-card is followed by one
           of these grids on several tabs (e.g. Anomaly's map card -> the
           Baseline/Markov chain grid), and since the grid container itself
           is not a .bs-card, the '.bs-card + .bs-card' rule above never
           fires between them -- without an explicit margin-top the two
           sections sit flush with zero gap. margin-top/-bottom collapse
           with neighbouring cards' own margins rather than stacking, so
           this does not create double spacing between two adjacent grids. */
        .bs-grid-2 { align-items: stretch; display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); gap: 16px; margin-top: 16px; margin-bottom: 16px; }
        .bs-grid-2.bs-stretch { align-items: stretch; }
        .bs-grid-2.bs-stretch > .bs-card { display: flex; flex-direction: column; }
        .bs-grid-2.bs-stretch > .bs-card > .bs-more { margin-top: auto; padding-top: 14px; }
        .bs-col-stack { display: flex; flex-direction: column; gap: 16px; }
        .bs-col-stack > .bs-card { margin-top: 0 !important; }
        .bs-col-stack > .bs-card:last-child { flex: 1 1 auto; display: flex; flex-direction: column; justify-content: center; }
        .bs-grid-3 { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 16px; margin-top: 16px; margin-bottom: 16px; }
        .bs-grid-4 { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 16px; margin-top: 16px; margin-bottom: 16px; }

        /* ---- callouts ---- */
        .bs-callout { padding: 13px 16px; border-left: 4px solid var(--bs-accent); background: rgba(52,211,153,.07);
                       border-radius: 0 7px 7px 0; color: var(--bs-soft); font-size: 18px; line-height: 1.65; margin-top: 14px; }
        .bs-callout.warm { border-left-color: var(--bs-warm); background: rgba(245,179,78,.08); }
        .bs-callout.teal { border-left-color: var(--bs-teal); background: rgba(47,184,166,.08); }
        .bs-callout.void { border-left-color: var(--bs-void); background: rgba(91,107,124,.12); }

        /* ---- controls: toggle switch, sliders, radio pills ---- */
        .bs-switch-row { display:flex; align-items:center; gap:0.7rem; flex-wrap:wrap; margin-bottom:0.4rem; }

        .bs-pills .shiny-options-group { display:flex; gap:4px; background:var(--bs-card); padding:4px;
                                          border-radius:8px; border:1px solid var(--bs-border); flex-wrap:wrap; margin:0; }
        .bs-pills input[type=radio], .bs-pills input[type=checkbox] { display:none; }
        .bs-pills label { display:inline-flex; align-items:center; padding:8px 14px; border-radius:6px; cursor:pointer;
                           color:var(--bs-faint) !important; font-size:17px !important; font-weight:600; letter-spacing:.02em;
                           margin:0 !important; transition:all .15s ease; white-space:nowrap; }
        .bs-pills label:has(input:checked) { background: var(--bs-accent); color: var(--bs-bg) !important;
                                              box-shadow: 0 0 10px rgba(52,211,153,0.3); }
        .bs-pills label:hover { color: var(--bs-ink) !important; }

        .bs-control .irs-bar, .bs-control .irs-single, .bs-control .irs-from, .bs-control .irs-to {
          background: var(--bs-accent) !important; border-color: var(--bs-accent) !important;
        }
        .bs-control .irs-line { background: #2a2e35 !important; border-color: #2a2e35 !important; }
        .bs-control label { color: var(--bs-faint); font-size: 17px; text-transform: uppercase; letter-spacing: .08em; }

        /* ---- badges (Station Square scenario indicator) ---- */
        .bs-badge { padding: 0.15rem 0.7rem; border-radius: 12px; font-size: 1.22rem; font-weight: 600; }

        /* ---- tables / DT ---- */
        /* DataTables ships its own ~13px default, unrelated to the rest of
           the page's type scale -- explicit here so tables read at the same
           big-screen-legible size as everything around them. */
        table.dataTable { background: var(--bs-card) !important; color: var(--bs-ink) !important; font-size: 17px !important; }
        table.dataTable thead th { color: var(--bs-faint) !important; border-bottom: 1px solid var(--bs-border) !important; }
        table.dataTable tbody td { border-color: var(--bs-border) !important; }
        /* DataTables' own stripe theme hard-codes #f9f9f9 (odd) / white (even)
           row backgrounds, which otherwise win over the dark theme above and
           leave near-white rows with barely-visible text -- override both
           explicitly rather than relying on the generic rule to cascade in. */
        table.dataTable tbody tr,
        table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd,
        table.dataTable.stripe tbody tr.even, table.dataTable.display tbody tr.even,
        table.dataTable.stripe tbody tr.odd td, table.dataTable.display tbody tr.odd td,
        table.dataTable.stripe tbody tr.even td, table.dataTable.display tbody tr.even td {
          background-color: var(--bs-card) !important; color: var(--bs-ink) !important;
        }
        table.dataTable tbody tr:hover,
        table.dataTable.stripe tbody tr.odd:hover, table.dataTable.display tbody tr.odd:hover,
        table.dataTable.stripe tbody tr.even:hover, table.dataTable.display tbody tr.even:hover {
          background-color: var(--bs-bg) !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button { color: var(--bs-ink) !important; }
        .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter { color: var(--bs-faint) !important; font-size: 15px !important; }

        /* ---- misc ---- */
        .bs-caption { text-align:center; margin-top:7px; color:#6b7480; font-size:15px; letter-spacing:.2em; text-transform:uppercase; }
        .cta-link { color: var(--bs-accent) !important; font-family:'Space Grotesk',sans-serif; font-size:1.35rem; text-decoration:none; }
        .cta-link:hover { text-decoration: underline; }

        /* ---- equations (MathJax) ---- */
        .bs-equation { padding: 16px 18px; background: var(--bs-bg); border: 1px solid var(--bs-border);
                        border-radius: 6px; margin-bottom: 12px; overflow-x: auto; color: var(--bs-ink);
                        font-size: 23px; }
        .bs-equation .MathJax { color: var(--bs-ink) !important; }

        /* ---- theory panel: Markov / baseline toggle ---- */
        .bs-theory-tabs { display: flex; gap: 10px; margin-bottom: 18px; flex-wrap: wrap; }
        .bs-theory-tab {
          font-family: 'Space Grotesk', sans-serif; font-size: 16px; font-weight: 600;
          letter-spacing: .04em; text-transform: uppercase; color: var(--bs-faint);
          background: var(--bs-bg); border: 1px solid var(--bs-border); border-radius: 20px;
          padding: 8px 18px; cursor: pointer; transition: color .15s, border-color .15s, background .15s;
        }
        .bs-theory-tab:hover { color: var(--bs-ink); }
        .bs-theory-tab-active {
          color: var(--bs-bg) !important; background: var(--bs-accent) !important;
          border-color: var(--bs-accent) !important;
        }

        /* ---- big reveal number (e.g. top-10 overlap stat) ---- */
        .flip-number { font-family: 'IBM Plex Mono', monospace; font-size: 3.65rem; font-weight: 600;
                        color: var(--bs-accent); text-shadow: 0 0 18px rgba(52,211,153,0.35); }
        .flip-caption { font-family: 'Space Grotesk', sans-serif; font-size: 1.28rem; color: var(--bs-faint); }

        /* ---- real Leaflet maps ----
           Basemap tiles (Esri World Dark Gray Base) are already dark and
           minimal, so no CSS filter is needed on them -- this just re-skins
           Leaflet's default (light, white-panelled) chrome to match the
           rest of the app. */
        .leaflet-container { background: var(--bs-bg) !important; border-radius: 6px; }
        .leaflet-control-zoom a, .leaflet-control-layers {
          background: var(--bs-card) !important; color: var(--bs-ink) !important; border-color: var(--bs-border) !important;
        }
        .leaflet-control-zoom a:hover { background: #23262c !important; }
        .leaflet-control-layers-expanded {
          background: var(--bs-card) !important; color: var(--bs-soft) !important;
          border: 1px solid var(--bs-border) !important; font-size: 17.5px !important;
        }
        .leaflet-popup-content-wrapper, .leaflet-popup-tip { background: var(--bs-card) !important; color: var(--bs-ink) !important; }
        .leaflet-bar a { border-color: var(--bs-border) !important; }
        .info.legend { background: var(--bs-card) !important; color: var(--bs-soft) !important;
                        border: 1px solid var(--bs-border) !important; border-radius: 6px !important;
                        padding: 8px 10px !important; font-size: 17px !important; line-height: 1.5 !important; }
        /* The level legend exists in the DOM from first render (Leaflet
           needs the addLegend() call regardless), but the layer itself is
           off by default -- hidden here so there's no legend showing
           before its layer checkbox is actually on; the script below
           shows it exactly when that checkbox is ticked. */
        .legend-level { display: none; }

        /* Responsive */
        @media (max-width: 900px) {
          .bs-hero, .bs-grid-2, .bs-grid-3, .bs-grid-4, .bs-roadmap { grid-template-columns: 1fr; }
          .bs-title { font-size: 57.5px; }
        }
        @media (max-width: 640px) {
          .bs-shell { padding: 18px 8px; }
          .bs-header-top { flex-direction: column; gap: 4px; }
          .bs-header-row { flex-direction: column; }
          .bs-up-corner { align-self: flex-start; }
          .bs-up-plate img { width: 120px; }
          .bs-title { font-size: 51.5px; }
          .bs-nav-link { padding: 8px 10px; }
          .bs-roadmap { grid-template-columns: 1fr; }
        }
      ")),
      
      # Keep the custom top-navigation highlight and Shiny tab state in sync,
      # and fix up the two real-map Leaflet widgets' tile size whenever their
      # container's actual pixel size changes. Three rounds of "wait N
      # milliseconds after the tab switches, then invalidateSize()" weren't
      # reliable -- a guessed delay can never be exactly right on every
      # machine. A ResizeObserver instead watches each map's own container
      # element directly and fires the instant its real width/height change
      # (from the hidden tab's 0x0 to its true shown size), which is the
      # actual event that needs to trigger the fix, not a timer guessing
      # around it -- correct regardless of machine speed or how AdminLTE's
      # tab-show animation behaves.
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
          function getLeafletMap(id){
            try {
              var widget = window.HTMLWidgets && HTMLWidgets.find('#' + id);
              return widget && widget.getMap && widget.getMap();
            } catch (e) { return null; }
          }
          function fixRealMapSizes(){
            ['real_road_map', 'station_road_map'].forEach(function(id){
              var map = getLeafletMap(id);
              if (map) map.invalidateSize();
            });
          }
          function watchMapContainer(id){
            var el = document.getElementById(id);
            if (!el) { setTimeout(function(){ watchMapContainer(id); }, 300); return; }
            if (!window.ResizeObserver) { return; }  // very old browser -- fallback timers below still run
            var lastW = 0, lastH = 0;
            new ResizeObserver(function(entries){
              var r = entries[0].contentRect;
              if (r.width > 0 && r.height > 0 && (r.width !== lastW || r.height !== lastH)) {
                lastW = r.width; lastH = r.height;
                var map = getLeafletMap(id);
                if (map) map.invalidateSize();
              }
            }).observe(el);
          }
          // The 'Neighbourhood accessibility' layer has its own
          // addLegend() box, present in the DOM at all times but only
          // relevant while that layer checkbox is actually on -- shown/
          // hidden via Leaflet's own overlayadd/overlayremove events.
          var LEVEL_GROUP = 'Neighbourhood accessibility';
          function wireLegendToggle(id){
            var map = getLeafletMap(id);
            if (!map) { setTimeout(function(){ wireLegendToggle(id); }, 300); return; }
            var container = document.getElementById(id);
            function setVisible(cls, visible){
              var el = container.querySelector('.' + cls);
              // 'block', not '' -- the CSS above defaults this to
              // display:none, so clearing the inline style would just
              // fall back to that instead of actually showing it.
              if (el) el.style.display = visible ? 'block' : 'none';
            }
            setVisible('legend-level', false);
            map.on('overlayadd', function(e){
              if (e.name === LEVEL_GROUP) setVisible('legend-level', true);
            });
            map.on('overlayremove', function(e){
              if (e.name === LEVEL_GROUP) setVisible('legend-level', false);
            });
          }
          syncBsNav();
          $(document).on('shiny:inputchanged', function(event){
            if(event.name === 'tabs'){ syncBsNav(event.value); }
          });
          $(document).on('shown.bs.tab', function(){ setTimeout(syncBsNav, 10); });
          watchMapContainer('real_road_map');
          watchMapContainer('station_road_map');
          wireLegendToggle('real_road_map');
          wireLegendToggle('station_road_map');
          // Belt-and-braces fallback (essentially never needed in 2026, but
          // costs nothing to keep alongside the ResizeObserver above).
          $(document).on('shown.bs.tab', function(){ setTimeout(fixRealMapSizes, 300); });
          $(window).on('resize', fixRealMapSizes);
        });
      ")),
      
      # Plain client-side show/hide for the Overview tab's theory panel --
      # deliberately NOT routed through a Shiny input/observer. It's pure
      # presentation state (is a div visible, which of two sub-panels is
      # showing), so a vanilla onclick is simpler and has zero server round
      # trip; nothing here needs to be reactive or persisted.
      tags$script(HTML("
        var BSW = {
          nodes: [[70,150,'A'],[190,60,'B'],[190,240,'C'],[340,60,'D'],[340,240,'E'],[470,150,'F']],
          edges: [[0,1,2],[0,2,3],[1,2,2],[1,3,3],[2,4,2],[3,4,4],[3,5,2],[4,5,3],[1,4,6]],
          cur: 0, total: 0, counts: [], timer: null, fast: false, show: false, pi: [], adj: [], els: []
        };
        function bsWalkRad(x){ return 12 + 90 * x; }
        function bsWalkInit(){
          var svg = document.getElementById('bs-walk-svg');
          if (!svg || svg.getAttribute('data-ready')) return;
          svg.setAttribute('data-ready', '1');
          var NS = 'http://www.w3.org/2000/svg', n = BSW.nodes.length, i;
          BSW.adj = []; for (i = 0; i < n; i++) BSW.adj.push([]);
          BSW.edges.forEach(function(e){
            var w = 1 / e[2]; BSW.adj[e[0]].push([e[1], w]); BSW.adj[e[1]].push([e[0], w]);
          });
          var s = [], tot = 0;
          for (i = 0; i < n; i++){ s[i] = BSW.adj[i].reduce(function(a, b){ return a + b[1]; }, 0); tot += s[i]; }
          BSW.pi = s.map(function(x){ return x / tot; });
          function mk(tag, attrs, parent){
            var el = document.createElementNS(NS, tag);
            for (var k in attrs) el.setAttribute(k, attrs[k]);
            (parent || svg).appendChild(el); return el;
          }
          BSW.edges.forEach(function(e){
            var a = BSW.nodes[e[0]], b = BSW.nodes[e[1]];
            mk('line', {x1: a[0], y1: a[1], x2: b[0], y2: b[1], stroke: '#3a3f47', 'stroke-width': 2});
            var t = mk('text', {x: (a[0] + b[0]) / 2, y: (a[1] + b[1]) / 2 - 6, fill: '#8d96a3', 'font-size': 18, 'text-anchor': 'middle'});
            t.textContent = e[2] + ' min';
          });
          BSW.els = BSW.nodes.map(function(nd, k){
            var ring = mk('circle', {cx: nd[0], cy: nd[1], r: bsWalkRad(BSW.pi[k]), fill: 'none', stroke: '#f5b34e',
                                     'stroke-width': 2.5, 'stroke-dasharray': '6 5', opacity: 0, 'class': 'bs-w-ring'});
            var fill = mk('circle', {cx: nd[0], cy: nd[1], r: 12, fill: 'rgba(52,211,153,0.35)', stroke: '#34d399', 'stroke-width': 2});
            fill.style.transition = 'r .35s ease';
            var lab = mk('text', {x: nd[0], y: nd[1] + 8, fill: '#f4f1ea', 'font-size': 22, 'font-weight': 700, 'text-anchor': 'middle'});
            lab.textContent = nd[2];
            var pct = mk('text', {x: nd[0], y: nd[1] + bsWalkRad(BSW.pi[k]) + 24, fill: '#c4cad3', 'font-size': 20, 'text-anchor': 'middle'});
            return {ring: ring, fill: fill, pct: pct};
          });
          BSW.token = mk('circle', {cx: 0, cy: 0, r: 9, fill: '#f5b34e', stroke: '#14171c', 'stroke-width': 2});
          BSW.token.style.transition = 'transform .4s ease';
          bsWalkReset();
        }
        function bsWalkRender(){
          BSW.nodes.forEach(function(nd, k){
            var share = BSW.total ? BSW.counts[k] / BSW.total : 0;
            BSW.els[k].fill.setAttribute('r', bsWalkRad(share));
            BSW.els[k].pct.textContent = Math.round(share * 100) + '%';
            BSW.els[k].ring.setAttribute('opacity', BSW.show ? 1 : 0);
          });
          var c = BSW.nodes[BSW.cur];
          BSW.token.style.transform = 'translate(' + c[0] + 'px,' + c[1] + 'px)';
          var h = document.getElementById('bs-walk-hops');
          if (h) h.textContent = BSW.total - 1;
        }
        function bsWalkStep(){
          var opts = BSW.adj[BSW.cur], sum = opts.reduce(function(a, b){ return a + b[1]; }, 0), r = Math.random() * sum, k = 0;
          while (k < opts.length - 1 && r > opts[k][1]){ r -= opts[k][1]; k++; }
          BSW.cur = opts[k][0]; BSW.counts[BSW.cur]++; BSW.total++;
          bsWalkRender();
        }
        function bsWalkTimer(){
          clearInterval(BSW.timer);
          BSW.timer = setInterval(bsWalkStep, BSW.fast ? 90 : 650);
        }
        function bsWalkToggle(){
          var b = document.getElementById('bs-walk-play');
          if (BSW.timer){ clearInterval(BSW.timer); BSW.timer = null; if (b) b.textContent = '\u25B6 Start the driver'; }
          else { bsWalkTimer(); if (b) b.textContent = '\u23F8 Pause'; }
        }
        function bsWalkFast(){
          BSW.fast = !BSW.fast;
          var b = document.getElementById('bs-walk-fast');
          if (b) b.textContent = BSW.fast ? 'Slow down' : 'Speed up';
          if (BSW.timer) bsWalkTimer();
        }
        function bsWalkReveal(){
          BSW.show = !BSW.show;
          var b = document.getElementById('bs-walk-reveal');
          if (b) b.textContent = BSW.show ? 'Hide the long-run answer' : 'Show the long-run answer';
          bsWalkRender();
        }
        function bsWalkReset(){
          clearInterval(BSW.timer); BSW.timer = null;
          var b = document.getElementById('bs-walk-play'); if (b) b.textContent = '\u25B6 Start the driver';
          BSW.cur = 0; BSW.counts = BSW.nodes.map(function(){ return 0; }); BSW.counts[0] = 1; BSW.total = 1;
          bsWalkRender();
        }
        $(function(){ bsWalkInit(); });
        function bsTogglePresenter(){
          var on = document.body.classList.toggle('bs-presenter');
          var b = document.getElementById('bs-present-btn');
          if (b) b.classList.toggle('on', on);
        }
        document.addEventListener('keydown', function(e){
          if ((e.key === 'p' || e.key === 'P') && !e.ctrlKey && !e.metaKey && !e.altKey &&
              !/INPUT|TEXTAREA|SELECT/.test((e.target.tagName || ''))) bsTogglePresenter();
        });
        function bsRunAnimations(){
          var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
          document.querySelectorAll('[data-count]').forEach(function(el){
            var target = parseFloat(el.getAttribute('data-count'));
            var dec = parseInt(el.getAttribute('data-dec') || '0', 10);
            if (reduce) { var tv = target.toFixed(dec); el.textContent = el.getAttribute('data-comma') ? Number(tv).toLocaleString('en-US', {minimumFractionDigits: dec, maximumFractionDigits: dec}) : tv; return; }
            var start = null, dur = 1500;
            function step(ts){
              if (start === null) start = ts;
              var p = Math.min((ts - start) / dur, 1), e = 1 - Math.pow(1 - p, 3);
              var v = (target * e).toFixed(dec); el.textContent = el.getAttribute('data-comma') ? Number(v).toLocaleString('en-US', {minimumFractionDigits: dec, maximumFractionDigits: dec}) : v;
              if (p < 1) requestAnimationFrame(step);
            }
            el.textContent = (0).toFixed(dec);
            requestAnimationFrame(step);
          });
          document.querySelectorAll('.bs-gap-fill').forEach(function(el){
            el.style.transition = 'none'; el.style.width = '0%'; void el.offsetWidth;
            el.style.transition = ''; el.style.width = el.getAttribute('data-w') + '%';
          });
        }
        $(document).on('shiny:connected', function(){
          setTimeout(bsRunAnimations, 700);
          $(document).on('shiny:inputchanged', function(e){
            if (e.name === 'tabs' && e.value === 'home') setTimeout(bsRunAnimations, 120);
          });
        });
        function bsShowTheoryTab(which){
          var markovBody = document.getElementById('theory-markov-body');
          var baselineBody = document.getElementById('theory-baseline-body');
          var markovTab = document.getElementById('theory-tab-markov');
          var baselineTab = document.getElementById('theory-tab-baseline');
          if (!markovBody || !baselineBody) return;
          var showMarkov = (which === 'markov');
          markovBody.style.display = showMarkov ? 'block' : 'none';
          baselineBody.style.display = showMarkov ? 'none' : 'block';
          if (markovTab) markovTab.classList.toggle('bs-theory-tab-active', showMarkov);
          if (baselineTab) baselineTab.classList.toggle('bs-theory-tab-active', !showMarkov);
        }
      "))
    ),
    
    # withMathJax() itself only needs to run once the two LaTeX formulas on
    # "See The Methods" are actually visible -- loaded eagerly at the top
    # here, it makes every session fetch MathJax's (non-trivial) CDN script
    # on the very first paint, even for someone who never opens that tab.
    # uiOutput + a one-way reactive flag defers that fetch until the user
    # first navigates there, and never re-fires it after.
    uiOutput("mathjax_loader"),
    
    div(class = "bs-shell",
        
        # ---- header ----
        div(class = "bs-header",
            div(class = "bs-header-top"
            ),
            div(class = "bs-header-row",
                div(class = "bs-header-left",
                    h1(class = "bs-title", HTML("Blind Spots<span>.</span>")),
                    p(class = "bs-subtitle", "Traffic data availability & accessibility modelling")
                ),
                div(class = "bs-up-corner",
                    div(class = "bs-up-plate",
                        tags$img(src = "up_logo.jpg", alt = "University of Pretoria")),
                    div(class = "bs-up-text",
                        span(class = "bs-up-dept", "Department of Statistics"),
                        tags$a(class = "bs-paper-link",
                               href = "https://drive.google.com/drive/folders/1_LQx5HxcvU3zwAKEJDKZCFavxpx1rnwy?usp=sharing",
                               target = "_blank", rel = "noopener noreferrer", "Read the research paper \u2197")
                    )
                )
            )
        ),
        
        # ---- navigation ----
        tags$nav(class = "bs-nav", role = "navigation",
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "start",
                        onclick = "Shiny.setInputValue('bs_nav', 'start', {priority: 'event'}); return false;",
                        icon("compass"), "How to read"),
                 tags$a(class = "bs-nav-link active", href = "#", `data-tab` = "home",
                        onclick = "Shiny.setInputValue('bs_nav', 'home', {priority: 'event'}); return false;",
                        icon("house"), "Overview"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "access",
                        onclick = "Shiny.setInputValue('bs_nav', 'access', {priority: 'event'}); return false;",
                        icon("route"), "Accessibility"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "methods",
                        onclick = "Shiny.setInputValue('bs_nav', 'methods', {priority: 'event'}); return false;",
                        icon("flask"), "See The Methods"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "realdata",
                        onclick = "Shiny.setInputValue('bs_nav', 'realdata', {priority: 'event'}); return false;",
                        icon("city"), "Real Findings"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "station",
                        onclick = "Shiny.setInputValue('bs_nav', 'station', {priority: 'event'}); return false;",
                        icon("subway"), "Station Square"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "anomaly",
                        onclick = "Shiny.setInputValue('bs_nav', 'anomaly', {priority: 'event'}); return false;",
                        icon("triangle-exclamation"), "Anomaly"),
                 tags$a(class = "bs-nav-link", href = "#", `data-tab` = "conclusion",
                        onclick = "Shiny.setInputValue('bs_nav', 'conclusion', {priority: 'event'}); return false;",
                        icon("flag-checkered"), "Conclusion"),
                 tags$button(id = "bs-present-btn", type = "button", class = "bs-present-btn",
                             onclick = "bsTogglePresenter()", icon("eye"), " Presenter view")
        ),
        
        shinydashboard::tabItems(
          
          # ======================================================
          # HOW TO READ THIS APP
          # ======================================================
          shinydashboard::tabItem(
            tabName = "start",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Start here"),
                h2(class = "bs-card-title", "How to read this app"),
                p(class = "bs-card-subtitle",
                  "Seven short stops, best read left to right. Real Findings and Station Square use the real ",
                  "Amsterdam data. See The Methods and Anomaly use a small made-up network, so you can play ",
                  "with the ideas without touching the real results."),
                div(class = "bs-steps",
                    div(class = "bs-roadmap-step",
                        div(class = "bs-step-circle", "1"),
                        h2(class = "bs-card-title", "Overview"),
                        p(class = "bs-card-subtitle", "The question: do the sensors cover the whole network?"),
                        actionLink("goto_home", "Go to Overview \u2192", class = "cta-link")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-step-circle", "2"),
                        h2(class = "bs-card-title", "Accessibility"),
                        p(class = "bs-card-subtitle", "The two models used throughout: a simple baseline and a Markov chain."),
                        actionLink("goto_access", "See the models \u2192", class = "cta-link")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-step-circle", "3"),
                        h2(class = "bs-card-title", "See The Methods"),
                        p(class = "bs-card-subtitle", "Kernel density and the K-function, explained on a toy network."),
                        actionLink("goto_methods", "Play with the demo \u2192", class = "cta-link")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-step-circle", "4"),
                        h2(class = "bs-card-title", "Real Findings"),
                        p(class = "bs-card-subtitle", "The Amsterdam results, with and without residential streets."),
                        actionLink("goto_real", "See the real numbers \u2192", class = "cta-link")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-step-circle", "5"),
                        h2(class = "bs-card-title", "Station Square"),
                        p(class = "bs-card-subtitle", "The one neighbourhood that is both a major bridge and highly accessible."),
                        actionLink("goto_station", "Meet the exception \u2192", class = "cta-link")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-step-circle", "6"),
                        h2(class = "bs-card-title", "Anomaly"),
                        p(class = "bs-card-subtitle", "Close a road on the toy network and watch both models react."),
                        actionLink("goto_anomaly", "Break something \u2192", class = "cta-link")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-step-circle", "7"),
                        h2(class = "bs-card-title", "Conclusion"),
                        p(class = "bs-card-subtitle", "Three lessons, the limitations, and future work."),
                        actionLink("goto_conclusion", "Wrap up \u2192", class = "cta-link"))
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Under the hood"),
                h2(class = "bs-card-title", "Four techniques, one job each"),
                div(class = "bs-roadmap",
                    div(class = "bs-roadmap-step",
                        div(class = "bs-icon-badge", icon("magnifying-glass")),
                        h2(class = "bs-card-title", "Availability audit"),
                        p(class = "bs-card-subtitle", "Which of the 693 monitoring locations actually report data.")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-icon-badge teal", icon("diagram-project")),
                        h2(class = "bs-card-title", "Network KDE"),
                        p(class = "bs-card-subtitle", "How densely the sensors cover each stretch of road, measured along the roads.")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-icon-badge void", icon("chart-line")),
                        h2(class = "bs-card-title", "K-function"),
                        p(class = "bs-card-subtitle", "Whether sensors are clustered, or spread as if placed at random.")),
                    div(class = "bs-roadmap-step",
                        div(class = "bs-icon-badge warm", icon("arrows-rotate")),
                        h2(class = "bs-card-title", "Markov accessibility"),
                        p(class = "bs-card-subtitle", "How easily each neighbourhood is reached, from travel times across the network."))
                )
            )
          ),
          
          # ======================================================
          # OVERVIEW
          # ======================================================
          shinydashboard::tabItem(
            tabName = "home",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "The question"),
                h2(class = "bs-card-title", style = "font-size: 38px;",
                   "Are traffic sensors a fair sample of the whole road network?"),
                p(class = "bs-card-subtitle bs-spoken",
                  "Sensors are placed by administrative priority, mostly on highways. If they are not a fair sample, ",
                  "does that change what our models say about getting around the city?"),
                div(class = "bs-gap-grid",
                    div(class = "bs-gap-stat",
                        div(class = "bs-gap-label", "Availability"),
                        div(class = "bs-gap-num teal", HTML("<span data-count=\"94.2\" data-dec=\"1\">94.2</span>%")),
                        div(class = "bs-gap-bar", div(class = "bs-gap-fill teal", `data-w` = "94.2")),
                        div(class = "bs-gap-sub", "of sensors are working and sending data (653 of 693).")
                    ),
                    div(class = "bs-gap-stat",
                        div(class = "bs-gap-label", "Coverage"),
                        div(class = "bs-gap-num warm", HTML("<span data-count=\"1.6\" data-dec=\"1\">1.6</span>%")),
                        div(class = "bs-gap-bar", div(class = "bs-gap-fill warm", `data-w` = "1.6")),
                        div(class = "bs-gap-sub", "of road segments have a sensor at all (5.2% of non-residential roads).")
                    )
                ),
                div(class = "bs-callout warm bs-spoken",
                    "Like a survey where almost everyone responds \u2014 but you only asked people on one street.")
            ),
            
            div(class = "bs-grid-2",
                div(class = "bs-card",
                    div(class = "bs-icon-badge teal", icon("circle-question")),
                    h2(class = "bs-card-title", "Is the sample fair?"),
                    p(class = "bs-card-subtitle",
                      "Where are the sensors, and is their placement random?"),
                    actionLink("ov_goto_methods", "See The Methods \u2192", class = "cta-link")),
                div(class = "bs-card",
                    div(class = "bs-icon-badge warm", icon("scale-balanced")),
                    h2(class = "bs-card-title", "Does it change the answer?"),
                    p(class = "bs-card-subtitle",
                      "Two accessibility models, compared on real data."),
                    actionLink("ov_goto_access", "See the models \u2192", class = "cta-link"))
            ),
            
            div(class = "bs-kpis",
                div(class = "bs-kpi",
                    div(class = "bs-kpi-label", "Monitoring locations"),
                    div(class = "bs-kpi-value", HTML("<span data-count=\"693\" data-dec=\"0\" data-comma=\"1\">693</span>")),
                    div(class = "bs-kpi-sub", "653 reporting data")),
                div(class = "bs-kpi warm",
                    div(class = "bs-kpi-label", "Road segments"),
                    div(class = "bs-kpi-value", HTML("<span data-count=\"21605\" data-dec=\"0\" data-comma=\"1\">21,605</span>")),
                    div(class = "bs-kpi-sub", "Full Amsterdam network")),
                div(class = "bs-kpi teal",
                    div(class = "bs-kpi-label", "By road authority"),
                    div(class = "bs-kpi-value", "93.9% / 85.7%"),
                    div(class = "bs-kpi-sub", "Rijkswaterstaat / Gemeente (n = 7)"))
            )
          ),
          
          # ======================================================
          # WHAT IS ACCESSIBILITY
          # ======================================================
          shinydashboard::tabItem(
            tabName = "access",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Before the findings"),
                h2(class = "bs-card-title", "What is accessibility?"),
                p(class = "bs-card-subtitle",
                  "How easy a place is to reach, scored two ways."),
                div(class = "bs-theory-tabs",
                    tags$button(id = "theory-tab-baseline", class = "bs-theory-tab bs-theory-tab-active",
                                onclick = "bsShowTheoryTab('baseline')", type = "button",
                                "1 \u00B7 Baseline model"),
                    tags$button(id = "theory-tab-markov", class = "bs-theory-tab",
                                onclick = "bsShowTheoryTab('markov')", type = "button",
                                "2 \u00B7 Markov chain model")
                )
            ),
            
            # ---- Baseline ----
            div(id = "theory-baseline-body", style = "display:block;",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Baseline"),
                    h2(class = "bs-card-title", "How fast is traffic inside the neighbourhood?"),
                    p(class = "bs-card-subtitle bs-spoken",
                      "Faster roads inside a neighbourhood, higher score."),
                    div(class = "bs-grid-2",
                        div(class = "bs-card",
                            p(class = "bs-section-kicker", "1. Travel time on each road"),
                            div(class = "bs-equation",
                                helpText("$$t_e \\;=\\; \\frac{\\ell_e}{s_e} \\times 60$$")),
                            p(class = "bs-spoken", style = "font-size:1.28rem; color:var(--bs-soft); margin:0;",
                              "Each road gets a free-flow travel time from its length ", math_sub_tag("\u2113", "e", italic_base = FALSE),
                              " and speed ", math_sub_tag("s", "e"), " (measured where a sensor exists).")
                        ),
                        div(class = "bs-card",
                            p(class = "bs-section-kicker", "2. One score per neighbourhood"),
                            div(class = "bs-equation",
                                helpText("$$\\text{baseline}_i \\;=\\; \\left(\\frac{\\sum_{e \\in i} t_e}{\\sum_{e \\in i} \\ell_e}\\right)^{-1}$$")),
                            p(class = "bs-spoken", style = "font-size:1.28rem; color:var(--bs-soft); margin:0;",
                              "Total time over total length inside neighbourhood ", tags$em("i"), ", inverted so faster is higher.")
                        )
                    ),
                    div(class = "bs-callout warm bs-spoken",
                        tags$strong("The catch: "), "it only looks inside the neighbourhood.")
                )
            ),
            
            # ---- Markov ----
            div(id = "theory-markov-body", style = "display:none;",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Markov chain"),
                    h2(class = "bs-card-title", "How easily is a neighbourhood reached from everywhere else?"),
                    p(class = "bs-card-subtitle bs-spoken",
                      "A driver hops at random between neighbourhoods. More time spent there, more accessible."),
                    div(class = "bs-card bs-walk",
                        p(class = "bs-section-kicker", "Watch it settle"),
                        div(class = "bs-walk-controls",
                            tags$button(id = "bs-walk-play", type = "button", class = "btn btn-outline-light",
                                        style = "color:#14171c;", onclick = "bsWalkToggle()", "\u25B6 Start the driver"),
                            tags$button(id = "bs-walk-fast", type = "button", class = "btn btn-outline-light",
                                        style = "color:#14171c;", onclick = "bsWalkFast()", "Speed up"),
                            tags$button(id = "bs-walk-reveal", type = "button", class = "btn btn-outline-light",
                                        style = "color:#14171c;", onclick = "bsWalkReveal()", "Show the long-run answer"),
                            tags$button(type = "button", class = "btn btn-outline-light",
                                        style = "color:#14171c;", onclick = "bsWalkReset()", "Reset"),
                            tags$span(class = "bs-walk-count", "Hops: ", tags$strong(id = "bs-walk-hops", "0"))),
                        HTML("<svg id=\"bs-walk-svg\" viewBox=\"0 0 540 300\" role=\"img\" aria-label=\"A driver hopping between six neighbourhoods\"></svg>"),
                        p(class = "bs-card-subtitle bs-keep", style = "margin:6px 0 0;",
                          "Circles grow with the share of time the driver spends there. The dashed ring is the long-run answer \u03c0.")
                    ),
                    div(class = "bs-grid-3",
                        div(class = "bs-card",
                            p(class = "bs-section-kicker", "1. Chance of each hop"),
                            div(class = "bs-equation",
                                helpText("$$P_{ij} \\;=\\; \\frac{w_{ij}}{\\sum_{k} w_{ik}}, \\qquad w_{ij} \\;=\\; \\frac{1}{t_{ij}}$$")),
                            p(class = "bs-spoken", style = "font-size:1.28rem; color:var(--bs-soft); margin:0;",
                              "Faster trip ", math_sub_tag("t", "ij"), ", more likely hop. Chances sum to 1.")
                        ),
                        div(class = "bs-card",
                            p(class = "bs-section-kicker", "2. Long-run share of time"),
                            div(class = "bs-equation",
                                helpText("$$\\pi P \\;=\\; \\pi, \\qquad \\sum_{i} \\pi_i \\;=\\; 1$$")),
                            p(class = "bs-spoken", style = "font-size:1.28rem; color:var(--bs-soft); margin:0;",
                              "The share of time stops changing. That stationary distribution \u03C0 is the accessibility score.")
                        ),
                        div(class = "bs-card",
                            p(class = "bs-section-kicker", "3. Finding \u03C0"),
                            div(class = "bs-equation",
                                helpText("$$v^{(t+1)} \\;=\\; v^{(t)} P, \\qquad \\pi \\;=\\; \\lim_{t \\to \\infty} v^{(t)}$$")),
                            p(class = "bs-spoken", style = "font-size:1.28rem; color:var(--bs-soft); margin:0;",
                              "Start anywhere, apply the hops again and again until the numbers settle.")
                        )
                    ),
                    div(class = "bs-callout teal",
                        tags$strong("This study's contribution: "),
                        "the original model (Thiede et al., 2023) built the hop probabilities from distance. Here they ",
                        "come from travel time, so real measured speeds can be fed into the model."),
                    tags$details(class = "bs-more",
                                 tags$summary("Technical notes"),
                                 p("The real Amsterdam network is not fully connected: a small area in the south-east has ",
                                   "no road link to the main network, so the chain is run on each connected ",
                                   "component. The report ranks only the main network (445 of 517 neighbourhoods); the small ",
                                   "south-eastern piece is shown for context and never compared with it."),
                                 p(math_sub_tag("P", "time"), ", ", math_sub_tag("P", "speed"), " and ",
                                   math_sub_tag("P", "observed"), " rebuild ", math_sub_tag("t", "ij"),
                                   " from three different travel-time estimates and run them through the same three steps, ",
                                   "as cross-checks of one model rather than three different models."))
                )
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
                  "The full motorised network, and a version without residential, unclassified and living-street roads."),
                div(class = "bs-switch-row",
                    tags$span(style = "font-size:1.24rem; color:var(--bs-faint);", "Viewing:"),
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
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "The actual streets, not a diagram"),
                h2(class = "bs-card-title", "The real Amsterdam road network"),
                p(class = "bs-card-subtitle",
                  "Every line is a real road segment (21,605). Use the layer picker to shade neighbourhoods by accessibility or overlay the 693 sensors."),
                leafletOutput("real_road_map", height = "540px"),
                tags$details(class = "bs-more",
                             tags$summary("More about this map"),
                             p("The \u201cNo residential roads\u201d switch also hides the residential layer here. The network splits into two ",
                               "disconnected pieces, a main network and a small landmass in the south-east, so the Markov chain is run on each ",
                               "separately. The report ranks only the main network (445 of 517 neighbourhoods). Colours show standing within ",
                               "each piece only, because the two sets of probabilities cannot be compared directly.")),
                p(class = "bs-caption", style = "text-align:left; letter-spacing:normal; text-transform:none; margin-top:10px;",
                  span(style = paste0("color:", col_road_primary, "; font-weight:600;"), "\u2014"), " Non-residential roads \u00b7 ",
                  span(style = paste0("color:", col_road_local, "; font-weight:600;"), "\u2014"), " Residential roads \u00b7 ",
                  span(style = paste0("color:", col_sensor_on, "; font-weight:600;"), "\u25cf"), " Observed sensor \u00b7 ",
                  span(style = paste0("color:", col_sensor_off, "; font-weight:600;"), "\u25cf"), " Unobserved sensor")
            ),
            
            div(class = "bs-grid-2",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Top 10"),
                    h2(class = "bs-card-title", "Most accessible neighbourhoods"),
                    p(class = "bs-card-subtitle",
                      "How often a long-run random walker ends up in each neighbourhood."),
                    plotlyOutput("real_topten_plot", height = "380px")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Agreement"),
                    h2(class = "bs-card-title", "How the four accessibility measures agree"),
                    p(class = "bs-card-subtitle",
                      "Spearman correlation between the accessibility measures. Hover a cell for \u03C1."),
                    plotlyOutput("real_corr_heat", height = "380px")
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Component by component"),
                h2(class = "bs-card-title", "\U0001F3AF Sensor K-function verdicts"),
                p(class = "bs-card-subtitle",
                  "Each row is a separate cluster of sensor roads, tested against random placement. ",
                  "\U0001F7E2 clustered and stable \u00B7 \U0001F535 more regular than chance \u00B7 \u26AA looks random."),
                DTOutput("real_kfun_table")
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Model impact of removing residential roads"),
                h2(class = "bs-card-title", "Full network vs. no residential roads, side by side"),
                p(class = "bs-card-subtitle",
                  "Both scenarios at once. Correlation rows show the change in \u03C1 (percentage points); other rows show relative change."),
                DTOutput("real_model_diag_table")
            ),
            
            div(class = "bs-grid-2",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Where agreement shifts"),
                    h2(class = "bs-card-title", "Correlation-matrix difference"),
                    p(class = "bs-card-subtitle",
                      "(No residential) \u2212 (Full). Green = the pair agrees more without residential roads; red = less."),
                    plotlyOutput("real_corr_diff_heat", height = "340px")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Ranking overlap"),
                    h2(class = "bs-card-title", "Top-10 most accessible neighbourhoods"),
                    uiOutput("real_topten_overlap_stat")
                )
            ),
            
            p(style = "color:#6b7480; font-size:1.26rem;",
              em("Nothing on this tab is simulated."))
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
                    tags$span(style = "font-size:1.24rem; color:var(--bs-faint);", "Viewing:"),
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
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "Where the exception actually is"),
                h2(class = "bs-card-title", "Amsterdam Centraal, on the real network"),
                p(class = "bs-card-subtitle",
                  HTML("Stationsplein e.o. is outlined in <strong>amber</strong>. Amber marks the exception, teal the four shortcuts. Zoom out to see how central it is.")),
                leafletOutput("station_road_map", height = "480px"),
                tags$details(class = "bs-more",
                             tags$summary("More about this map"),
                             p("Same real network as Real Findings. Tick \u201cNeighbourhood accessibility\u201d in the layer picker for the shading. ",
                               "The small southern component uses its own chain, so its colours are not comparable with the main network.")),
                p(class = "bs-caption", style = "text-align:left; letter-spacing:normal; text-transform:none; margin-top:10px;",
                  span(style = paste0("color:", col_road_primary, "; font-weight:600;"), "\u2014"), " Non-residential roads \u00b7 ",
                  span(style = paste0("color:", col_road_local, "; font-weight:600;"), "\u2014"), " Residential roads \u00b7 ",
                  span(style = paste0("color:", col_amber, "; font-weight:600;"), "\u25cf"), " Exception bridge")
            ),
            
            div(class = "bs-callout",
                tags$strong("Why it matters: "),
                "five roads that look alike can have very different real importance, and only a network-aware model shows which. ",
                "Switch to the residential-excluded network and the exception changes.")
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
                  "A small made-up street grid, so each method is easy to see. Nothing here is a research finding."),
                div(class = "bs-pills",
                    radioButtons("methods_view", NULL, inline = TRUE,
                                 choices = c("Network KDE" = "kde", "K-function" = "kfun",
                                             "Why it matters" = "relevance"),
                                 selected = "kde"))
            ),
            
            conditionalPanel(
              "input.methods_view == 'kde'",
              div(class = "bs-card",
                  p(class = "bs-section-kicker", "Network kernel density"),
                  div(class = "bs-equation",
                      helpText("$$\\hat{f}(s) \\;=\\; \\sum_{i} K\\!\\left(\\frac{d_L(s, x_i)}{h}\\right)$$")),
                  p(style = "font-size:1.28rem; color:var(--bs-soft); margin:0;",
                    "Density spreads along connected roads only. ", math_sub_tag("d", "L"), " is network distance and ",
                    tags$em("h"), " the bandwidth \u03C3. Bright = close to many sensors; dark = a blind spot.")
              ),
              br(),
              div(class = "bs-grid-2 bs-stretch",
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Heatmap"),
                      h2(class = "bs-card-title", "Monitoring intensity"),
                      plotlyOutput("kde_map", height = "440px")
                  ),
                  div(class = "bs-col-stack",
                      div(class = "bs-card",
                          p(class = "bs-section-kicker", "Bandwidth"),
                          h2(class = "bs-card-title", "Adjust \u03c3"),
                          div(style = "display:flex; gap:0.4rem; margin-bottom:0.8rem;",
                              actionButton("bw_tight", "Tight", class = "btn btn-sm btn-outline-light"),
                              actionButton("bw_balanced", "Balanced", class = "btn btn-sm btn-outline-light"),
                              actionButton("bw_wide", "Wide", class = "btn btn-sm btn-outline-light")),
                          div(class = "bs-control", sliderInput("bandwidth", "Bandwidth (metres)", min = 150, max = 2000, value = 600, step = 50)),
                          p(style = "font-size:1.28rem; color:var(--bs-faint);",
                            "Small \u03C3 shows single clusters. Large \u03C3 blurs them together.")
                      ),
                      div(class = "bs-card",
                          p(class = "bs-section-kicker", "How to read it"),
                          div(class = "bs-read",
                              tags$p(tags$span(class = "bs-read-dot", style = "background:#34d399;"),
                                     tags$strong("Bright roads: "), "close to many sensors."),
                              tags$p(tags$span(class = "bs-read-dot", style = "background:#3a4a52;"),
                                     tags$strong("Dark roads: "), "blind spots, even if they carry real traffic."))
                      )
                  )
              )
            ),
            
            conditionalPanel(
              "input.methods_view == 'kfun'",
              div(class = "bs-card",
                  p(class = "bs-section-kicker", "Linear network K-function"),
                  div(class = "bs-equation",
                      helpText("$$K(r) \\;=\\; \\frac{1}{\\lambda} \\times E[\\text{additional events within network distance } r]$$")),
                  p(style = "font-size:1.28rem; color:var(--bs-soft); margin:0;",
                    "Compared with a band of random placements. Above the band = clustered; below = more evenly spaced.")
              ),
              br(),
              div(class = "bs-grid-2 bs-stretch",
                  div(class = "bs-card",
                      p(class = "bs-section-kicker", "Envelope test"),
                      h2(class = "bs-card-title", "Observed K(r) vs. complete spatial randomness"),
                      plotlyOutput("kfun_plot", height = "380px"),
                      uiOutput("kfun_interpretation")
                  ),
                  div(class = "bs-col-stack",
                      div(class = "bs-card",
                          p(class = "bs-section-kicker", "Settings"),
                          h2(class = "bs-card-title", "Simulation controls"),
                          actionButton("reshuffle", "\U0001F500 Reshuffle CSR envelope", class = "btn btn-outline-light", style = "margin-bottom:0.8rem;"),
                          div(class = "bs-control", selectInput("n_sim", "Monte Carlo simulations", choices = c(49, 99, 199, 399), selected = 199)),
                          div(class = "bs-control", sliderInput("r_max", "Maximum r (metres)", min = 800, max = 3500, value = 2200, step = 200))
                      ),
                      div(class = "bs-card",
                          p(class = "bs-section-kicker", "How to read it"),
                          div(class = "bs-read",
                              tags$p(tags$span(class = "bs-read-dot", style = "background:#34d399;"),
                                     tags$strong("Above the band: "), "sensors are clustered."),
                              tags$p(tags$span(class = "bs-read-dot", style = "background:#8d96a3;"),
                                     tags$strong("Inside the band: "), "no different from random placement."),
                              tags$p(tags$span(class = "bs-read-dot", style = "background:#2fb8a6;"),
                                     tags$strong("Below the band: "), "more evenly spaced than chance."))
                      )
                  )
              )
            ),
            
            # ------------------------------------------------------------
            # WHY IT MATTERS -- a short bridge between the three spatial-
            # statistics mini-tabs above (coverage / KDE / K-function, all
            # about *where sensors are*) and the two accessibility models
            # used everywhere else in the app (about *how accessible a
            # neighbourhood is*). Wording draws directly on the research
            # report's own framing (abstract and methodology), not a
            # separate simplified story -- see the full report for the
            # complete numerical results and discussion.
            # ------------------------------------------------------------
            conditionalPanel(
              "input.methods_view == 'relevance'",
              div(class = "bs-card",
                  p(class = "bs-section-kicker", "The link back to accessibility"),
                  h2(class = "bs-card-title", "Why coverage bias matters"),
                  p(class = "bs-card-subtitle",
                    "KDE shows where monitoring is dense or sparse. The K-function tests whether the pattern is more than chance. ",
                    "Together they show where an accessibility score rests on measured data, and where it rests on assumed speeds."),
                  div(class = "bs-callout",
                      tags$strong("In Amsterdam: "),
                      "only 1.6% of road segments (5.2% of non-residential roads) carry a sensor."),
                  tags$details(class = "bs-more",
                               tags$summary("More detail"),
                               p("These coverage statistics are the diagnostic step that comes before any accessibility estimate. ",
                                 "They show exactly which segments the observed-speed Markov variant can use a measured value for, ",
                                 "and where new monitoring would improve the estimates most. See the research paper for the full results."))
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
                  "Click a road to close it, click again to restore it. Try a small local street first, then the thick diagonal spine (the highway)."),
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
            
            p(class = "bs-caption", style = "text-align:left; letter-spacing:normal; text-transform:none; margin:6px 0 10px;",
              span(class = "pulse-dot"), "Observed   ",
              span(class = "blindspot-dot"), "Blind spot   ",
              span(class = "removed-dot"), "Removed   ",
              span(class = "ring-dot"), "Score changed (bigger ring = bigger change)"),
            
            div(class = "bs-grid-3",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "1 \u00B7 Close a segment"),
                    h2(class = "bs-card-title", "The network"),
                    plotlyOutput("anomaly_network_map", height = "400px"),
                    p(class = "bs-card-subtitle", style = "margin-top:8px;",
                      "Click a road to close it, click again to restore. A closure that would cut a block off completely is blocked.")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "2 \u00B7 Topology-free"),
                    h2(class = "bs-card-title", "Baseline model"),
                    plotlyOutput("anomaly_baseline_map", height = "400px"),
                    uiOutput("anomaly_baseline_status")
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "3 \u00B7 Network-aware"),
                    h2(class = "bs-card-title", "Markov chain model"),
                    plotlyOutput("anomaly_markov_map", height = "400px"),
                    uiOutput("anomaly_markov_status")
                )
            ),
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "What's the difference?"),
                div(class = "bs-two-col",
                    div(tags$h3("Baseline: average speed"),
                        p("Only reads the speed of roads inside each neighbourhood, so a closure often barely registers.")),
                    div(tags$h3("Markov chain: connections"),
                        p("Also counts how connected a neighbourhood is, so losing a road moves the score much more."))
                )
            ),
            
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "The tell"),
                h2(class = "bs-card-title", "Units whose Markov rank moves most"),
                p(class = "bs-card-subtitle",
                  "Ranked by how far each unit's Markov rank moves compared with the untouched network. The ",
                  "baseline only reacts if a closure changes a neighbourhood's average road speed."),
                DTOutput("anomaly_rank_table")
            ),
            
            div(class = "bs-callout warm",
                tags$strong("The mirror image of Station Square: "),
                "there, an ordinary-looking street mattered far more than its traffic counts let on. Here, take away ",
                "a road and the baseline often sees nothing, because the neighbourhood's average speed has not ",
                "changed. The Markov chain feels it straight away, because the neighbourhood has lost a connection.")
          ),
          # ======================================================
          # CONCLUSION
          # ======================================================
          shinydashboard::tabItem(
            tabName = "conclusion",
            
            div(class = "bs-card",
                p(class = "bs-section-kicker", "To sum up"),
                h2(class = "bs-card-title", "Three lessons"),
                div(class = "bs-grid-3",
                    div(class = "bs-card bs-lesson",
                        div(class = "bs-lesson-num", "1"),
                        h2(class = "bs-card-title", "Having data is not the same as covering the network."),
                        p(class = "bs-card-subtitle bs-keep", "94.2% of sensors work, but sensors sit on only 1.6% of roads."),
                        actionLink("cc_goto_home", "Back to the Overview \u2192", class = "cta-link")),
                    div(class = "bs-card bs-lesson",
                        div(class = "bs-lesson-num", "2"),
                        h2(class = "bs-card-title", "The bias is in which roads have sensors, not how they are spaced."),
                        p(class = "bs-card-subtitle bs-keep", "Of 15 groups of monitored roads, only 2 were clustered."),
                        actionLink("cc_goto_real", "See the K-function results \u2192", class = "cta-link")),
                    div(class = "bs-card bs-lesson",
                        div(class = "bs-lesson-num", "3"),
                        h2(class = "bs-card-title", "How the network is defined matters more than the speed data."),
                        p(class = "bs-card-subtitle bs-keep",
                          "Assumed against observed speeds: \u03C1 = 0.92\u20130.95. Without residential streets, the two top-ten lists share no neighbourhoods."),
                        actionLink("cc_goto_agree", "See the agreement heatmap \u2192", class = "cta-link"))
                )
            ),
            
            div(class = "bs-grid-2 bs-stretch",
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Limitations"),
                    h2(class = "bs-card-title", "What this cannot say yet"),
                    tags$ul(class = "bs-list",
                            tags$li(class = "main", "Speeds away from sensors are assumed, not validated."),
                            tags$li("One year of weekday averages only."),
                            tags$li("Clipping the city boundary leaves 86% of units modelled.")),
                    tags$details(class = "bs-more",
                                 tags$summary("More limitations"),
                                 p("OpenStreetMap's residential streets were not independently verified. Summing unit travel times ",
                                   "over all node pairs may disadvantage large or irregular neighbourhoods. The KDE bandwidth sits at ",
                                   "the low end of the usable range, and the K-function envelope uses 199 simulations."))
                ),
                div(class = "bs-card",
                    p(class = "bs-section-kicker", "Future work"),
                    h2(class = "bs-card-title", "Where this goes next"),
                    tags$ul(class = "bs-list",
                            tags$li(class = "main", "Validate against independent travel times (probe or GPS data)."),
                            tags$li("Add sensors in simulation and compare placement strategies."),
                            tags$li("Test the direction of bias, and other travel-time aggregations."),
                            tags$li("Extend to more years and to other cities.")),
                    tags$details(class = "bs-more",
                                 tags$summary("Full list"),
                                 p(tags$strong("Sensor placement: "), "simulate sensors on unmonitored arterial links or at bridges like Stationsplein e.o.; ",
                                   "compare random, highest-betweenness and KDE gap-filling placement (Kaiser, 2026); find the saturation point ",
                                   "where observed speeds matter more than the choice of network."),
                                 p(tags$strong("Methods: "), "mean or minimum travel-time aggregation; sensitivity to community detection and ",
                                   "representative nodes; comparison with gravity-based or 2SFCA measures."),
                                 p(tags$strong("Data: "), "peak and off-peak hours, weekends and seasons; check OpenStreetMap against an ",
                                   "authoritative road dataset; buffer the municipal boundary so the south-east stays connected."))
                )
            )
            
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
  nav_targets <- c(goto_home = "home", goto_access = "access", goto_methods = "methods",
                   goto_real = "realdata", goto_station = "station", goto_anomaly = "anomaly",
                   goto_conclusion = "conclusion", ov_goto_methods = "methods", ov_goto_access = "access",
                   ov_goto_real = "realdata", cc_goto_home = "home", cc_goto_real = "realdata", cc_goto_agree = "realdata")
  lapply(names(nav_targets), function(id) {
    observeEvent(input[[id]], shinydashboard::updateTabItems(session, "tabs", selected = nav_targets[[id]]),
                 ignoreInit = TRUE)
  })
  
  # MathJax is needed for the LaTeX formulas on "See The Methods" and (now)
  # the Markov theory tile on Overview -- since Overview is the default
  # landing tab, this fires on essentially every session's first paint
  # regardless, but the deferred-load mechanism is kept as-is (rather than
  # calling withMathJax() unconditionally) so a session that somehow lands
  # straight on a different tab, or a future reordering of the sidebar,
  # still doesn't pay the CDN fetch until a formula is actually on screen.
  mathjax_loaded <- reactiveVal(FALSE)
  observeEvent(input$tabs, {
    if (input$tabs %in% c("access", "methods")) mathjax_loaded(TRUE)
  })
  output$mathjax_loader <- renderUI({
    req(mathjax_loaded())
    withMathJax()
  })
  
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
          HTML(paste0("<strong>Full network:</strong> the Markov chain is numerically delicate. Two solving methods differ by up to <code>",
                      round(r$eigen_power_gap, 5), "</code>. Remove residential roads to see it settle.")))
    } else {
      div(class = "bs-callout",
          HTML(paste0("<strong>No residential roads:</strong> the same two methods now agree to within <code>",
                      round(r$eigen_power_gap, 5), "</code>, roughly ", round(0.05916 / r$eigen_power_gap), "\u00D7 tighter.")))
    }
  })
  
  output$real_topten_plot <- renderPlotly({
    df <- real_topten %>% filter(scenario == real_scenario()) %>% arrange(rank)
    p <- suppressWarnings(
      ggplot(df, aes(x = stationary_prob, y = reorder(unit, stationary_prob),
                     text = paste0(unit, ": ", signif(stationary_prob, 3)))) +
        geom_col(fill = col_signal) +
        labs(x = "Stationary probability", y = NULL) +
        theme_minimal(base_size = 17) +
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
      x = unname(real_corr_axis_labels[colnames(m)]), y = unname(real_corr_axis_labels[rownames(m)]),
      z = m, type = "heatmap",
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
    fmt_diag <- function(x, kind) {
      ifelse(kind == "cor", sprintf("%.2f", x),
             ifelse(kind == "gap", sprintf("%.4f", x),
                    ifelse(kind == "pr",  sprintf("%.3f", x), format(round(x), big.mark = ","))))
    }
    df <- real_model_diagnostics %>%
      mutate(kind = dplyr::case_when(is_correlation ~ "cor",
                                     grepl("gap", metric, ignore.case = TRUE) ~ "gap",
                                     grepl("PageRank", metric) ~ "pr",
                                     TRUE ~ "n")) %>%
      transmute(
        Metric = metric,
        `Full network` = fmt_diag(full, kind),
        `No residential roads` = fmt_diag(wr, kind),
        Difference = ifelse(kind == "cor",
                            paste0(ifelse(diff >= 0, "+", "\u2212"), sprintf("%.2f", abs(diff)), " pts"),
                            paste0(ifelse(diff >= 0, "+", "\u2212"), fmt_diag(abs(diff), kind))),
        `Relative change` = ifelse(is_correlation, "\u2014", paste0(ifelse(pct_change >= 0, "+", ""), round(pct_change, 1), "%"))
      )
    # escape = FALSE: the Metric column carries the <i>/<sub> markup from
    # math_sub_html() above (e.g. P_time -> proper "P" with a true
    # subscript "time") rather than a literal underscore -- nothing else
    # in this data frame contains user input or untrusted text, so this is
    # safe to leave unescaped.
    datatable(df, escape = FALSE, options = list(pageLength = 6, dom = "t"), rownames = FALSE)
  })
  
  output$real_corr_diff_heat <- renderPlotly({
    m <- real_corr_diff
    plot_ly(
      x = unname(real_corr_axis_labels[colnames(m)]), y = unname(real_corr_axis_labels[rownames(m)]),
      z = m, type = "heatmap",
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
            tagList(tags$strong("Zero overlap: "), "removing residential roads gives an entirely different ",
                    "list of \u201cmost accessible\u201d places.")
          } else {
            tagList(tags$strong(real_topten_overlap, " shared: "), "a few neighbourhoods stay near the top, ",
                    "but most of the list changes.")
          })
    )
  })
  
  # A Leaflet map rendered inside a tab that starts hidden (Shiny hides every
  # tabItem but the first with display:none) captures the wrong container
  # size at render time -- Station Square isn't the first tab, so its tiles
  # come in patchy/misaligned until the map is told its real size. This is
  # fixed from the browser side (see the fixRealMapSizes() JS helper in the
  # tags$script block near the top of the UI), not from here -- the R
  # leaflet package's own invalidateSize() wrapper isn't exported by every
  # installed version of that package, and calling a function that doesn't
  # exist inside an observer crashes the whole app session, not just that
  # one feature.
  
  # ---------------------------------------------------- Real road-network map
  # The actual Amsterdam road geometry (see the GeoJSON loading block near
  # the top). Built once via renderLeaflet, then the neighbourhood
  # choropleth alone is refreshed in place with leafletProxy whenever the
  # scenario switch flips -- the tiles, road layers, and the viewer's own
  # pan/zoom position are left completely untouched.
  output$real_road_map <- renderLeaflet({
    base_road_leaflet(isolate(real_scenario())) %>%
      setView(lng = 4.895, lat = 52.372, zoom = 12)
  })
  
  observeEvent(real_scenario(), {
    proxy <- leafletProxy("real_road_map") %>%
      clearGroup("Neighbourhood accessibility") %>%
      addGeoJSON(admin_choropleth_json[[real_scenario()]], group = "Neighbourhood accessibility",
                 stroke = TRUE, fill = TRUE)
    if (real_scenario() == "No residential roads") {
      proxy %>% hideGroup("Residential roads")
    } else {
      proxy %>% showGroup("Residential roads")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$real_road_map_geojson_click, {
    tip <- input$real_road_map_geojson_click$properties$tip
    req(tip)
    showNotification(tip, type = "message", duration = 5)
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
        " The five neighbourhoods with the highest betweenness are Amsterdam's busiest bridges, yet being a bridge and being accessible are almost ",
        "unrelated (Spearman \u03C1 = ", r$betweenness_accessibility_cor, " across ", r$n_modelled_units,
        " units). Almost.")
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
        theme_minimal(base_size = 17) +
        theme(panel.background = element_rect(fill = col_bg, color = NA),
              plot.background  = element_rect(fill = col_bg, color = NA),
              panel.grid = element_line(color = "#262a31"),
              strip.text = element_text(color = col_text, face = "bold", hjust = 0),
              strip.background = element_rect(fill = col_bg, color = NA),
              axis.text = element_text(color = col_text_mute),
              axis.title = element_text(color = col_text))
    )
    ggplotly(p, tooltip = "text") %>% plotly_dark(legend_top = FALSE) %>% layout(showlegend = FALSE)
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
      div(class = "bs-callout warm",
          HTML(paste0("<strong>", ex$unit, "</strong> is Amsterdam Centraal's station square: the highest betweenness of any unit (",
                      format(ex$betweenness, big.mark = ","), ") and a stationary probability about ",
                      round(ex$stationary_prob / ex$network_avg, 1), "\u00D7 the network average.<br><br>",
                      "A major interchange is both a chokepoint people are forced through and somewhere they want to be. ",
                      "The other four bridges are pure shortcuts.")))
    } else {
      div(class = "bs-callout",
          tags$strong("Stationsplein e.o."), " is no longer a top-5 bridge without residential roads. ",
          "All five bridges sit close together on accessibility: bridging is not the same as accessible.")
    }
  })
  
  # -------------------------------------------------- Station Square real map
  # Same real base map as Real Findings, but centred on Stationsplein e.o.
  # (outlined in amber by build_admin_choropleth()) with the five current
  # bridge units marked -- amber for the dual-role exception, teal for the
  # four pure shortcuts. Both the choropleth and the markers stay in sync
  # with the shared scenario switch.
  output$station_road_map <- renderLeaflet({
    sq <- admin_centroids %>% filter(is_station_square)
    base_road_leaflet(isolate(real_scenario())) %>%
      showGroup("Highlight: Station Square") %>%
      setView(lng = sq$lon[1], lat = sq$lat[1], zoom = 14)
  })
  
  observeEvent(real_scenario(), {
    proxy <- leafletProxy("station_road_map") %>%
      clearGroup("Neighbourhood accessibility") %>%
      addGeoJSON(admin_choropleth_json[[real_scenario()]], group = "Neighbourhood accessibility",
                 stroke = TRUE, fill = TRUE)
    if (real_scenario() == "No residential roads") {
      proxy %>% hideGroup("Residential roads")
    } else {
      proxy %>% showGroup("Residential roads")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(bridge_data(), {
    df <- bridge_data() %>%
      left_join(admin_centroids %>% select(unit_name, lon, lat), by = c("unit" = "unit_name")) %>%
      filter(!is.na(lon))
    ex <- df %>% filter(is_exception)
    proxy <- leafletProxy("station_road_map") %>%
      clearGroup("Bridge units") %>%
      addCircleMarkers(
        data = df, lng = ~lon, lat = ~lat,
        radius = ~ifelse(is_exception, 12, 8),
        color = "#0e1013", weight = 2, opacity = 1,
        fillColor = ~ifelse(is_exception, col_amber, col_canal), fillOpacity = 0.9,
        label = ~unit,
        group = "Bridge units"
      ) %>%
      clearGroup("Bridge exception glow")
    # The reveal's whole point is "this one is different" -- give it an
    # actual pulsing glow (the same CSS animation the amber KPI dots use
    # elsewhere in the app) on top of its marker, instead of just a bigger
    # circle, so the exception is the thing your eye goes to first.
    # divIcon() is namespaced explicitly (leaflet::) rather than relying on
    # it being on the search path, and the whole thing is wrapped in
    # tryCatch -- this is a purely decorative extra, so if a particular R/
    # leaflet install can't build it for any reason, it's skipped quietly
    # instead of crashing the map (and the app) on every load.
    if (nrow(ex) > 0) {
      glow_icon <- tryCatch(
        leaflet::divIcon(className = "glow-marker-wrap",
                         html = "<div class='amber-dot glow-marker'></div>",
                         iconSize = c(10, 10), iconAnchor = c(5, 5)),
        error = function(e) NULL
      )
      if (!is.null(glow_icon)) {
        proxy %>% addMarkers(
          data = ex, lng = ~lon, lat = ~lat,
          icon = glow_icon,
          group = "Bridge exception glow"
        )
      }
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$station_road_map_geojson_click, {
    tip <- input$station_road_map_geojson_click$properties$tip
    req(tip)
    showNotification(tip, type = "message", duration = 5)
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
        coord_equal() + theme_void(base_size = 18) +
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
        coord_equal() + theme_void(base_size = 18) +
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
      "Above the band: sensors are clustered, most plausibly along the arterial ring."
    } else if (res$dispersed > 0.3) {
      "Below the band: sensors are more evenly spaced than chance."
    } else {
      "Inside the band: no strong sign of clustering or regularity."
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
  
  # Harmless-but-noisy console warning fix: plotly's event_data() warns
  # "source ID ... is not registered" if it's queried before the plot it's
  # watching has actually been drawn client-side -- which, at session
  # start, it hasn't (the Anomaly tab isn't the active tab, and the
  # observer below would otherwise evaluate event_data() immediately on
  # every session). Gating on a flag flipped once renderPlotly below has
  # run means event_data() is never called before the plot exists, so the
  # warning has nothing to fire on -- not just a suppressed print, an
  # actual fix.
  anomaly_map_drawn <- reactiveVal(FALSE)
  
  observe({
    req(anomaly_map_drawn())
    click <- suppressWarnings(plotly::event_data("plotly_click", source = "anomaly_map"))
    req(click, click$customdata)
    eid <- as.integer(click$customdata[1])
    # isolate() is load-bearing here, not cosmetic: this observer both reads
    # and writes removed_edges_rv(). Reading it reactively (the original
    # bug) makes this same observer re-fire every time it writes to
    # removed_edges_rv() -- and since the click's event_data() payload never
    # changes or clears on its own, each re-fire re-reads the *same* click,
    # sees the *new* current set, and flips the edge straight back --
    # forever. That infinite reactive loop pegs the R process at 100% CPU
    # and makes the whole app unresponsive on a single click (confirmed by
    # actually driving a headless browser against this app: CPU pegged,
    # port stopped responding entirely). isolate() breaks the dependency so
    # this observer only re-runs on an actual new click.
    current <- isolate(removed_edges_rv())
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
             font = list(color = col_text, family = "Space Grotesk", size = 15)) %>%
      config(displaylogo = FALSE) %>%
      plotly::event_register("plotly_click")
    anomaly_map_drawn(TRUE)
    p
  })
  
  anomaly_unit_ids <- as.character(nodes_df$id)
  # A unit counts as "changed" if its score moved by more than 0.5% relative to
  # the typical shift across the network (the Markov scores all renormalise
  # slightly after any closure, so the median shift is subtracted out).
  score_shift <- function(now, before) {
    r <- now[anomaly_unit_ids] / before[anomaly_unit_ids]
    abs(r / median(r) - 1)
  }
  anomaly_changed <- reactive({
    m <- anomaly_models()
    list(baseline = score_shift(m$baseline, models_baseline$baseline),
         markov   = score_shift(m$markov,   models_baseline$markov))
  })
  
  anomaly_status_ui <- function(shift) {
    changed <- shift > 0.005
    if (length(removed_edges_rv()) == 0) {
      return(p(class = "bs-card-subtitle", style = "margin-top:8px;", "Close a segment to see its effect."))
    }
    if (!any(changed)) {
      return(p(class = "bs-card-subtitle bs-keep", style = "margin-top:8px;",
               tags$strong(style = "color:var(--bs-warm);", "No change"), " \u2014 this model does not see the closure."))
    }
    p(class = "bs-card-subtitle bs-keep", style = "margin-top:8px;",
      tags$strong(style = "color:var(--bs-warm);", sum(changed)),
      sprintf(" of %d neighbourhoods changed score. Biggest change: %s%%.",
              length(changed), format(round(100 * max(shift)), big.mark = ",")))
  }
  output$anomaly_baseline_status <- renderUI(anomaly_status_ui(anomaly_changed()$baseline))
  output$anomaly_markov_status   <- renderUI(anomaly_status_ui(anomaly_changed()$markov))
  
  render_anomaly_access_map <- function(values, metric_label, shift = NULL) {
    changed <- if (is.null(shift)) NULL else shift > 0.005
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
    
    ring <- NULL
    if (!is.null(changed) && any(changed)) {
      ring <- nd[changed, , drop = FALSE]
      # ring size shows HOW MUCH the score moved (capped at a 100% change)
      ring$rsize <- scales::rescale(pmin(shift[changed], 1), to = c(10, 18), from = c(0, 1))
    }
    
    p <- ggplot() +
      geom_segment(data = e, aes(x = x1, y = y1, xend = x2, yend = y2, color = col, linewidth = lw))
    if (!is.null(ring)) {
      p <- p + geom_point(data = ring, aes(x = x, y = y, size = rsize), shape = 21, stroke = 1.6,
                          fill = NA, color = "#f5b34e")
    }
    p <- p +
      geom_point(data = nd, aes(x = x, y = y, color = col, size = size, text = tip)) +
      scale_color_identity() +
      scale_linewidth_identity() +
      scale_size_identity() +
      coord_equal() + theme_void(base_size = 18) +
      theme(legend.position = "none",
            plot.background  = element_rect(fill = col_bg, color = NA),
            panel.background = element_rect(fill = col_bg, color = NA))
    suppressWarnings(ggplotly(p, tooltip = "text")) %>% plotly_dark(legend_top = FALSE)
  }
  
  output$anomaly_baseline_map <- renderPlotly(
    render_anomaly_access_map(anomaly_models()$baseline, "Baseline", anomaly_changed()$baseline))
  output$anomaly_markov_map   <- renderPlotly(
    render_anomaly_access_map(anomaly_models()$markov, "Stationary prob.", anomaly_changed()$markov))
  
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