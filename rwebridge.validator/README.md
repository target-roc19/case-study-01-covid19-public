# rwebridge.validator

Reusable R validator code to validate metadata and codelists with a YAML config.

This repository is intentionally structured as plain R source (not an installable R package), so it can be included as a git subtree in other projects.

## Quick Start

1. Add validator as a subtree in the parent project:

```bash
# from the parent project root
git remote add rwebridge-validator https://github.com/UMC-Utrecht-RWE/rwebridge_validator.git
git fetch rwebridge-validator
git subtree add --prefix rwebridge.validator rwebridge-validator main --squash
```

2. Create starter files in the parent project:

- configuration/validation-config.yaml
- .github/workflows/rwe-bridge-validator.yaml
- run/run_rwe_bridge_validator.R

```r
# run from parent project root
source("rwebridge.validator/R/validate.R")
create_config_template()
```

3. Configure YAML:

- Edit configuration/validation-config.yaml with project file names and transforms.

4. Run validation locally:

```r
source("run/run_rwe_bridge_validator.R")
```

5. Run from VS Code Command Palette:

- Add task in `tasks.json`
```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Run RWE-BRIDGE Validator",
            "type": "shell",
            "command": "Rscript",
            "args": [
                "run/run_rwe_bridge_validator.R"
            ],
            "isBackground": false,
            "problemMatcher": [],
            "group": "test"
        }
    ]
}
```
- Press Cmd+Shift+P.
- Run Tasks: Run Test Task.
- Select Run RWE-BRIDGE Validator Test.

## GitHub Workflow

Workflow file: .github/workflows/rwe-bridge-validator.yaml

CI behavior:

- Triggers on push, pull request, and manual dispatch.
- Installs required R packages (data.table, yaml).
- Runs run/run_rwe_bridge_validator.R.
- Fails the job when validation status is FAILED.

## Subtree Update

Update subtree to latest main:

```bash
# from the parent project root
git fetch rwebridge-validator
git subtree pull --prefix rwebridge.validator rwebridge-validator main --squash
```

To report issues or suggest features / enhancement on rwebridge.validator,
please go to https://github.com/UMC-Utrecht-RWE/rwebridge_validator.
