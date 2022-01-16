### TODOs

#### Options
- Add help option
- Add option to warn or throw error if module isn't present
- Add option for styling exports
- Add option provide custom extensions
- Add option to remove existing exports

#### Refactor
- Create a DSL for MExport
- Use ReaderT to manage context
- Separate parser from file IO

#### Test
- Add test for symbols (Eg. (\$), (<$>))
- Return the exports per module from mExport to match expected outcome

#### Feature
- Consider existing exports
- Add ability to use yaml or json file for configuration
- Add ability to write project/modules to another directory
