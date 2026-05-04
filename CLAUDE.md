# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Mountain Doodles is a Quarto-based blog at https://doodles.mountainmath.ca covering housing, demographics, and Canadian data topics. Posts are primarily written in R with Quarto (`.qmd`) or plain Markdown (`.markdown`).

## Build Commands

```bash
# Render the full site
quarto render

# Render a single post (from repo root)
quarto render posts/YYYY-MM-DD-slug/index.qmd

# Preview with live reload
quarto preview
```

After rendering, two post-render R scripts run automatically (`R/copy_images_html_data.R` and `R/fixup_citations.R`) — these copy static assets to `_site/` and normalize citation formatting in rendered HTML.

## Creating a New Post

From an R session at the project root:

```r
source("R/post_helpers.R")
new_post(
  title = "My Post Title",
  date = Sys.Date(),
  categories = c("Category1", "Category2")
)
```

This creates `posts/YYYY-MM-DD-my-post-title/index.qmd` with the standard front matter scaffold and opens it for editing.

## Post Structure

Posts live in `posts/YYYY-MM-DD-slug/` directories. The front matter template from `new_post()` is the canonical reference. Key front matter fields:

- `bibliography: ../../common_literature.bib` — all posts share the root-level bibliography
- `execute: cache: true` — R code output is cached; frozen outputs are committed to git
- `image:` — used for listing thumbnails and social cards

`posts/_metadata.yml` sets defaults for all posts: `freeze: true`, CC BY license, `keep-md: true`, PDF/EPUB downloads, and HTML TOC on the left with `sidebar-posts`.

## Architecture

- **`_quarto.yml`** — site-wide config: navbar, sidebars, themes (SCSS), Giscus comments, Google Analytics
- **`common_literature.bib`** — shared bibliography for all posts; add new references here
- **`css/mountainmath_light.scss` / `css/mountainmath_dark.scss`** — site themes
- **`R/`** — helper scripts; `post_helpers.R` is the main utility; other scripts are post-specific data processing
- **`data/`** — shared datasets accessible to all posts
- **`html/`**, **`widgets/`**, **`js/`** — static interactive content copied to `_site/` post-render
- **`_site/`** — build output, git-ignored; deployed to Netlify automatically on push

## Freeze and Reproducibility

All posts have `freeze: true` by default. This means Quarto will not re-execute R code unless you explicitly run `quarto render` with `--execute` or delete the `_freeze/` cache for a post. Frozen outputs (in `_freeze/`) are committed to git so the site can be rendered without R packages installed.
