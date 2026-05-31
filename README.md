# RPCRA Website

Official supplementary website for **Robust Portfolio Construction and Risk Analysis** (RPCRA) — the forthcoming Springer textbook by R. Douglas Martin, Thomas K. Philips, Bernd Scherer, and Kirk Li.

This repository contains the source for the public website: landing page, preface, vignettes, demo code, and online appendices (A–E). The site is built with [Quarto](https://quarto.org) and deployed automatically to GitHub Pages.

## Quick pointers

| Pointer | Link |
|---------|------|
| **Live website** | [robustport.github.io/RPCRAwebsite](https://robustport.github.io/RPCRAwebsite/) |
| **RPCRA R package** | [github.com/robustport/PCRA](https://github.com/robustport/PCRA) |

---

## Table of Contents

1. [About This Repository](#about-this-repository)
2. [Repository Pointers (Git Remotes)](#repository-pointers-git-remotes)
3. [Repository Structure](#repository-structure)
4. [How the Site Is Built](#how-the-site-is-built)
5. [Editing Content](#editing-content)
6. [Adding a New Page](#adding-a-new-page)
7. [Local Preview (Optional)](#local-preview-optional)
8. [Deployment to GitHub Pages](#deployment-to-github-pages)
9. [What Not to Commit](#what-not-to-commit)
10. [Support](#support)

---

## About This Repository

| Item | Description |
|------|-------------|
| **Organization** | [robustport](https://github.com/robustport) |
| **Repository** | [RPCRAwebsite](https://github.com/robustport/RPCRAwebsite) |
| **Default branch** | `main` |
| **Build output** | `docs/` (generated automatically; do not edit by hand) |
| **Deployment** | GitHub Actions → GitHub Pages |

Every push to `main` triggers a build. GitHub Actions renders all `.qmd` files to HTML and publishes the result. You do not need to build or upload HTML manually.

---

## Repository Pointers (Git Remotes)

This project uses **two Git remotes** — pointers to two related repositories on GitHub:

| Remote name | Repository | Purpose |
|-------------|------------|---------|
| **`origin`** | [robustport/RPCRAwebsite](https://github.com/robustport/RPCRAwebsite) | **Primary.** Edit here and push to deploy the live site. |
| **`legacy`** | [Aakarsh751/pcra-book-website](https://github.com/Aakarsh751/pcra-book-website) | **Archive.** Earlier site version; kept for reference only. Do not deploy from here. |

### View configured remotes

```bash
git remote -v
```

Expected output:

```
origin   https://github.com/robustport/RPCRAwebsite.git (fetch)
origin   https://github.com/robustport/RPCRAwebsite.git (push)
legacy   https://github.com/Aakarsh751/pcra-book-website.git (fetch)
legacy   https://github.com/Aakarsh751/pcra-book-website.git (push)
```

### Daily workflow (deploy to live site)

```bash
git add .
git commit -m "Describe your change"
git push origin main
```

### Add the legacy remote (if missing)

```bash
git remote add legacy https://github.com/Aakarsh751/pcra-book-website.git
```

To compare with the older site without switching folders:

```bash
git fetch legacy
git log legacy/main --oneline -5
```

---

## Repository Structure

```
RPCRAwebsite/
├── index.qmd              Landing page
├── preface.qmd            Preface (embedded PDF)
├── vignettes.qmd          Vignettes overview
├── vignette-1.qmd         Vignette 1
├── vignette-2.qmd         Vignette 2
├── vignette-3.qmd         Vignette 3
├── demo-code.qmd          Chapter demo scripts
├── appendices.qmd         Appendices overview
├── appendix-a.qmd … e     Appendices A–E (embedded PDFs)
├── reference-manual.qmd   Package reference manual
├── about.qmd              About the authors / book
│
├── _quarto.yml            Site configuration (navbar, footer, build settings)
├── styles.css             Global stylesheet (applies to every page)
├── _fonts.html            Web font imports
│
├── pdfs/                  PDFs served and embedded on pages
├── demos/                 R demo scripts for download
│
└── .github/workflows/
    └── publish.yml        Automated build and deploy workflow
```

**Key idea:** Each visible page on the website corresponds to one `.qmd` file at the repository root. Shared appearance (colors, fonts, navbar, cards, PDF viewers) is defined once in `styles.css` and `_quarto.yml`.

---

## How the Site Is Built

```
Edit .qmd files locally
        ↓
git commit && git push to main
        ↓
GitHub Actions runs Quarto
        ↓
HTML written to docs/
        ↓
GitHub Pages serves the live site
```

| Component | Role |
|-----------|------|
| **Quarto** | Converts Markdown (`.qmd`) into HTML |
| **styles.css** | Premium academic theme (Playfair Display, Inter, dark navbar, card grids) |
| **pdfs/** | Full documents embedded inline with download links |
| **GitHub Actions** | Builds and deploys on every push to `main` |

You never write HTML or CSS for routine content updates.

---

## Editing Content

### Minor text changes

1. Open the relevant `.qmd` file (see table below).
2. Edit the text and save.
3. Commit and push to `main`.

| Website page | Source file |
|--------------|-------------|
| Home | `index.qmd` |
| Preface | `preface.qmd` |
| Vignettes overview | `vignettes.qmd` |
| Vignette 1 / 2 / 3 | `vignette-1.qmd`, `vignette-2.qmd`, `vignette-3.qmd` |
| Demo Code | `demo-code.qmd` |
| Appendices overview | `appendices.qmd` |
| Appendix A–E | `appendix-a.qmd` … `appendix-e.qmd` |
| Reference Manual | `reference-manual.qmd` |
| About | `about.qmd` |
| Navbar, footer, site title | `_quarto.yml` |
| Global styling | `styles.css` |

### Replace a PDF

Replace the file in `pdfs/` keeping the **same filename**, then push. Example: updating Appendix A means replacing `pdfs/appendix-a.pdf`.

### Styled boxes (no custom CSS needed)

```markdown
::: {.copyright-notice}
Copyright text here.
:::

::: {.pdf-callout}
::: {.pdf-icon}
📄
:::
::: {.pdf-text}
**Download**
[Document title](pdfs/your-file.pdf){target="_blank"}
:::
:::
```

These classes are defined in `styles.css` and work on any page.

---

## Adding a New Page

1. **Create a file** — e.g. `news.qmd`:

   ```yaml
   ---
   title: "News"
   ---

   Your content here.
   ```

2. **Add to the navbar** in `_quarto.yml`:

   ```yaml
   navbar:
     left:
       - text: "News"
         href: news.qmd
   ```

3. **Commit and push** to `main`. The site rebuilds in about two minutes.

For a PDF page, copy the structure from `appendix-a.qmd` (download callout + embedded iframe).

---

## Local Preview (Optional)

Requires [Quarto](https://quarto.org/docs/get-started/) installed, or RStudio with bundled Quarto.

```bash
git clone https://github.com/robustport/RPCRAwebsite.git
cd RPCRAwebsite
quarto preview
```

This opens a local preview in your browser. Changes to `.qmd` files refresh automatically.

---

## Deployment to GitHub Pages

Deployment is fully automated. One-time setup on GitHub (if not already done):

1. Open **Settings → Pages**.
2. Under **Build and deployment → Source**, select **GitHub Actions**.
3. Push to `main`, or go to **Actions** and run **Deploy to GitHub Pages** manually.

After a successful run, the site is available at:

**https://robustport.github.io/RPCRAwebsite/**

### Troubleshooting

| Issue | Action |
|-------|--------|
| Workflow failed | Open **Actions**, click the failed run, read the build log |
| Site not updating | Confirm the push was to `main` and the workflow completed |
| Pages disabled | Re-enable under **Settings → Pages → Source: GitHub Actions** |

---

## What Not to Commit

| Do not commit | Reason |
|---------------|--------|
| `docs/` | Auto-generated build output |
| `.quarto/`, `_site/` | Local Quarto cache and preview output |
| LaTeX/LyX source trees | Not required to build or serve the website |
| Duplicate PDFs outside `pdfs/` | Single canonical copy lives in `pdfs/` |
| Local editor folders (`.Rproj.user`, etc.) | Machine-specific |

The `.gitignore` file excludes these automatically.

---

## Support

- **R package:** [github.com/robustport/PCRA](https://github.com/robustport/PCRA)
- **Quarto documentation:** [quarto.org/docs/websites](https://quarto.org/docs/websites/)
- **GitHub Pages:** [docs.github.com/pages](https://docs.github.com/en/pages)

---

© 2025 Martin, Philips, Scherer & Li. All rights reserved.
