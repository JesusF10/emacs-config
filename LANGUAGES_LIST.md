# Programming Languages List

**Emacs Configuration:** Data Science Edition  
**Date:** March 30, 2026  
**Total Languages:** 25

---

## Complete List (Alphabetical)

1. C++
2. CSS
3. CSV (NEW - Data Science)
4. Dockerfile
5. Emacs Lisp
6. HTML
7. Java
8. JavaScript
9. JSON
10. Julia (Data Science)
11. LaTeX
12. Lua
13. Makefile
14. Markdown (NEW - Documentation)
15. Org Mode
16. Python (PRIMARY - Full LSP Stack)
17. R (Data Science)
18. Racket/Scheme
19. Scala (Data Science)
20. SQL
21. TOML (NEW - Configuration)
22. TSX (React)
23. TypeScript
24. YAML (NEW - Configuration)
25. Dockerfile

---

## By Category

### Data Science (4)
- Python (Full LSP with Ty + Ruff)
- R (Statistical computing)
- Julia (High-performance numerical)
- Scala (Big Data, Spark)

### Web Development (6)
- JavaScript
- TypeScript
- TSX (TypeScript + JSX)
- HTML
- CSS
- JSON

### Data & Configuration Formats (5 - NEW)
- CSV (Column alignment)
- TOML (pyproject.toml)
- YAML (CI/CD, pipelines)
- Markdown (Documentation, MDX)
- Dockerfile (Containers)

### Systems Programming (3)
- C++
- Java
- Makefile

### Documentation (3)
- LaTeX (Academic papers)
- Org Mode (Research notebooks)
- Markdown (Technical docs)

### Scripting (3)
- Emacs Lisp (Native)
- Lua
- SQL

### Lisp Family (1)
- Racket/Scheme (REPL + paredit)

---

## Support Levels

### COMPLETE (Full LSP + Tree-sitter)
- Python

### EXCELLENT (Tree-sitter + Enhanced Features)
- JavaScript/TypeScript (optional LSP)
- CSV (column alignment)
- TOML, YAML, Markdown (NEW)
- R, Julia, Scala
- CSS (color preview)
- Emacs Lisp (native)
- Racket (REPL)

### GOOD (Tree-sitter + Syntax)
- C++, Java, HTML, JSON
- LaTeX, Org
- Lua, SQL, Makefile, Dockerfile

---

## Tree-sitter Auto-Enable (14 languages)

These automatically use Tree-sitter enhanced modes:

1. C++ → c++-ts-mode
2. CSS → css-ts-mode
3. CSV → csv-ts-mode (NEW)
4. Java → java-ts-mode
5. JavaScript → js-ts-mode
6. JSON → json-ts-mode
7. Julia → julia-ts-mode
8. Markdown → markdown-ts-mode (NEW)
9. Python → python-ts-mode
10. R → r-ts-mode
11. Scala → scala-ts-mode
12. TOML → toml-ts-mode (NEW)
13. TypeScript → typescript-ts-mode
14. YAML → yaml-ts-mode (NEW)

---

## LSP Support

### Fully Configured
1. **Python** → Ty (Rust-based, 10-100x faster than Pyright)
   - Type checking: Ty
   - Linting: Ruff (async)
   - Formatting: Ruff (auto on save)
   - Environment: uv-aware detection

2. **JavaScript/TypeScript** → typescript-language-server (optional, if installed)

### Easy to Add
- R → r-language-server
- Julia → LanguageServer.jl
- Scala → Metals
- C++ → clangd
- Java → eclipse.jdt.ls
- Rust → rust-analyzer (parser not in list)
- Go → gopls (parser not in list)

---

## Installation Commands

### Install All Tree-sitter Parsers
```elisp
M-x treesit-install-all-languages
```

### Check Parser Status
```elisp
M-x treesit-check-parsers
```

### Install Single Parser
```elisp
M-x treesit-install-language-grammar RET language-name RET
```

### Reinstall Problematic Parser
```elisp
M-x treesit-clean-and-reinstall RET language-name RET
```

---

## Summary

- **Total Languages:** 25
- **With Tree-sitter:** 24
- **Auto-remapped:** 14
- **With LSP:** 2 (Python + optional JS/TS)
- **NEW in Modernization:** 4 (CSV, TOML, YAML, Markdown)
- **Primary Language:** Python (complete stack)

---

## Quick Reference

### Most Used for Data Science
1. Python (primary)
2. R (statistics)
3. Julia (performance)
4. CSV (data files)
5. JSON (APIs)
6. YAML (configs)
7. Markdown (docs)

### Configuration Files
- **Python projects:** pyproject.toml (TOML)
- **DevOps:** docker-compose.yml (YAML)
- **Data:** *.csv (CSV)
- **APIs:** *.json (JSON)
- **Docs:** *.md (Markdown)

---

**Configuration File:** ~/.emacs.d/lisp/lsp.el  
**Documentation:** LANGUAGES.md (detailed), CONTEXT.md, README.md
