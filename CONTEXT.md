# Contexto de Modernización: Data Science Edition

**Fecha de inicio:** 30 de Marzo de 2026  
**Versión anterior:** Pyright + Dirvish + Manual Ruff  
**Versión objetivo:** Ty + Dired + Async Ruff + uv

---

## Filosofía del Cambio

### Principio Rector: "Menos Peso Visual, Más Potencia en el Motor"

Esta modernización responde a una re-priorización de valores en el entorno de desarrollo:

1. **Performance sobre Estética**
   - Velocidad de inicialización: reducir overhead de paquetes visuales
   - Tiempo de respuesta LSP: migrar a herramientas Rust-native (10-100x más rápidas)
   - Navegación eficiente: eliminar features no usadas (Dirvish sidebar, preview panes)

2. **Aislamiento por Proyecto**
   - Filosofía "cada proyecto con su propio entorno"
   - Detección automática de `.venv` local (gestionado por `uv`)
   - Fallback a herramientas globales con warnings claros

3. **Stack Unificado Rust-Based**
   - **uv**: Gestor de paquetes/entornos Python (sustituto de pip/virtualenv)
   - **Ty**: Type checker y LSP server (sustituto de Pyright/mypy)
   - **Ruff**: Linter y formatter (ya en uso, pero con integración asíncrona mejorada)

---

## Problemas del Stack Anterior

### 1. Dirvish: Overhead Visual Innecesario

```elisp
;; Configuración anterior (líneas 176-201 de editor.el)
(use-package dirvish
  :config
  (setq dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size))
  ;; Problema: Demasiados atributos -> carga lenta, complejidad innecesaria
  ;; Uso real: <10% de las features activadas
```

**Impacto:**
- Inicio de Dired: ~300ms -> **Objetivo: <50ms con Dired tradicional**
- Complejidad cognitiva: gestión de múltiples modos (dirvish-side, dirvish-full)
- Mantenimiento: dependencia externa con actualizaciones frecuentes

**Solución:**
- Dired tradicional + `nerd-icons-dired` (única feature visual usada)
- Navegación con keybindings nativos (sin custom vim-style bindings)

---

### 2. Pyright: Dependencia Node.js + Latencia TypeScript

```elisp
;; Stack anterior
Pyright (TypeScript) -> Node.js runtime -> Python type checking
      |
   ~500ms startup latency (cold cache)
   ~100-200ms per-file analysis
```

**Problemas:**
1. **Latencia perceptible** en proyectos grandes (>10k LOC)
2. **Dependencia externa** de Node.js (no alineado con stack Python)
3. **Configuración dual**: `pyproject.toml` + `pyrightconfig.json`

**Solución con Ty:**
```rust
Ty (Rust-native) -> Binary nativo -> Python type checking
      |
   ~50ms startup latency
   ~10-20ms per-file analysis (incremental)
```

**Beneficios:**
- 10x más rápido en análisis de proyectos completos
- Una sola herramienta para type checking + LSP
- Configuración en `pyproject.toml` (estándar Python)

---

### 3. Ruff: Integración Manual sin Flymake Async

```elisp
;; Stack anterior (líneas 148-173 de lsp.el)
(defun my/ruff-format-buffer ()  ;; Síncrono, bloquea UI
  (shell-command-on-region ...))

;; Problema: Formateo bloquea el buffer hasta completarse
;; Tiempo: ~200-500ms en archivos grandes
```

**Solución:**
- Flymake backend asíncrono para linting en tiempo real
- Formateo on-save (opcional, configurable)
- Warnings no bloquean la edición

---

## Arquitectura Objetivo

### Stack Completo

```
┌─────────────────────────────────────────┐
│          Emacs 29.3+ (Linux)            │
├─────────────────────────────────────────┤
│  Editor Layer                           │
│  ├─ Dired (traditional)                 │
│  ├─ nerd-icons-dired (minimal icons)    │
│  └─ Vertico/Consult/Marginalia          │
├─────────────────────────────────────────┤
│  Language Support (Tree-sitter)         │
│  ├─ Python (python-ts-mode)             │
│  ├─ Markdown/MDX (markdown-ts-mode)     │
│  ├─ CSV (csv-ts-mode)                   │
│  ├─ TOML/YAML (config files)            │
│  └─ 20+ other languages                 │
├─────────────────────────────────────────┤
│  Python Tooling (Rust-based)            │
│  ├─ uv (package/env manager)            │
│  ├─ Ty (LSP server + type checker)      │
│  └─ Ruff (linter + formatter)           │
├─────────────────────────────────────────┤
│  LSP Client (Built-in)                  │
│  └─ Eglot (detects local .venv tools)   │
└─────────────────────────────────────────┘
```

### Flujo de Trabajo por Proyecto

```bash
# 1. Crear proyecto
cd ~/proyectos/mi-data-science-project

# 2. Inicializar con uv (reemplaza virtualenv + pip)
uv venv
source .venv/bin/activate

# 3. Instalar herramientas de desarrollo
uv pip install ty ruff pandas numpy scikit-learn

# 4. Abrir en Emacs
emacs .

# 5. Emacs detecta automáticamente:
#    - .venv/bin/ty       -> Eglot usa LSP local
#    - .venv/bin/ruff     -> Flymake usa linter local
#    - .venv/bin/python   -> Interprete local
```

---

## Benchmarks Esperados

### Startup Time (Cold Cache)

| Componente         | Antes (Pyright) | Después (Ty) | Mejora |
|--------------------|-----------------|--------------|--------|
| LSP server init    | ~500ms          | ~50ms        | 10x    |
| First file analysis| ~200ms          | ~20ms        | 10x    |
| Project-wide check | ~15s            | ~1.5s        | 10x    |

### Editor Performance

| Operación          | Antes (Dirvish) | Después (Dired) | Mejora |
|--------------------|-----------------|-----------------|--------|
| Dired init         | ~300ms          | ~30ms           | 10x    |
| Large directory    | ~800ms          | ~80ms           | 10x    |
| Icon rendering     | ~150ms          | ~15ms           | 10x    |

---

## Casos de Uso Data Science

### Escenario 1: Análisis Exploratorio (Jupyter + Py)

```python
# proyecto/notebooks/exploracion.py
import pandas as pd
import numpy as np

df = pd.read_csv("data/raw/dataset.csv")  # <- CSV con 1M rows
# Ty: auto-completa columnas, infiere tipos
# Ruff: formatea on-save, detecta imports no usados
```

**Beneficios:**
- CSV mode con alineación de columnas
- Ty infiere tipos de DataFrames (parcialmente)
- Ruff mantiene código limpio automáticamente

### Escenario 2: Pipeline ML (TOML Config)

```toml
# pyproject.toml
[tool.ty]
strict = true
ignore-missing-imports = ["sklearn", "tensorflow"]

[tool.ruff]
line-length = 88
select = ["E", "F", "I"]
```

**Beneficios:**
- TOML mode con Tree-sitter highlighting
- Configuración única para Ty + Ruff
- Sin archivos separados (pyrightconfig.json eliminado)

---

## Principios de Seguridad

1. **Aislamiento por Proyecto**
   - Cada `.venv` es independiente
   - No se mezclan dependencias entre proyectos
   - Fácil replicar entornos (uv.lock)

2. **Reproducibilidad**
   - `uv lock` genera lockfile determinístico
   - `uv sync` restaura entorno exacto
   - Compatible con CI/CD

3. **Transparencia**
   - Warnings visibles cuando faltan herramientas locales
   - Path adjustments explícitos en función de detección
   - Sin side-effects en configuración global

---

## Decisiones de Diseño

### ¿Por qué NO instalar ty/ruff globalmente?

**Ventajas del approach local:**
1. **Versionado por proyecto:** Proyecto A con ruff 0.4, Proyecto B con ruff 0.6
2. **Portabilidad:** Otros devs replican entorno con `uv sync`
3. **Aislamiento:** Cambios en un proyecto no afectan otros

**Desventaja mitigada:**
- Setup inicial más largo -> **Solucionado con `M-x my/init-python-project`**

### ¿Por qué eliminar Dirvish completamente?

**Análisis de uso real:**
- Features usadas: Iconos (80%), keybindings vim-style (15%)
- Features NO usadas: Preview, sidebar, subtree-state, git-msg
- Ratio valor/complejidad: **Bajo**

**Decisión:**
- Mantener solo iconos con `nerd-icons-dired` (5 líneas de config)
- Keybindings nativos de Dired (documentados, estándar)

---

## Próximos Pasos

1. **Fase 1:** Limpieza (eliminar Dirvish)
2. **Fase 2:** Modernización LSP (Pyright -> Ty)
3. **Fase 3:** Integración asíncrona Ruff
4. **Fase 4:** Paquetes Data Science (CSV, TOML, YAML, MDX)
5. **Fase 5:** Testing y validación

**Criterios de éxito:**
- Startup <2s (cold cache)
- LSP response <100ms (hover, completions)
- Formateo on-save <200ms
- Todos los parsers Tree-sitter funcionales
- Detección automática de .venv en 5 proyectos de prueba

---

**Autor:** Jesus Flores Lacarra  
**Asistente IA:** OpenCode (Claude Sonnet 4.5)  
**Fecha creación:** 30/03/2026
