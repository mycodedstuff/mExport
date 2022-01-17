### This is a tutorial for configuring smuggler2 for stack users

#### Note: Smuggler2 provides various options the one specified in the example works best for mExport
---
### Steps

1. Add smuggler2 dependency in your package.yaml
```
dependencies:
  - smuggler2
```
2. Add ghc-options
```
  ghc-options:
    - -fplugin=Smuggler2.Plugin
    - -fplugin-opt=Smuggler2.Plugin:MinimiseImports
```
3. (Optional) To enable smuggler via a runtime flag
```
  flags:
    smuggler2:
      description: Runs smuggler2 to automatically rewrite module imports to produce a minimal set.
      default: false
      manual: true

  when:
    - condition: flag(smuggler2)
      ghc-options:
      - -fplugin=Smuggler2.Plugin
      - -fplugin-opt=Smuggler2.Plugin:MinimiseImports
```

#### After the setup smuggler2 can be invoked via
```
  stack build --flags <package>:smuggler2
```
