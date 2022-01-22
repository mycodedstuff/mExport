### TODOs

#### Options
- Add help option
- Add option to warn or throw error if module isn't present
- Add option for styling exports
- Add option provide custom extensions
- Add option to remove existing exports
- Add option to exclude certain directories

#### Test
- Add unit tests

#### Feature
- Consider existing exports
- Add ability to use yaml or json file for configuration
- Add ability to write project/modules to another directory
- Add support for usage as plugin in projects
- Add option to check if exported types can e collapsed with (..), add a % value to when they should be

#### Issues
Test cases
Add case to fix newline added by prettyPrint
Bug in custom prettyPrint, split on ',' splits constructors in a type
Module header coords seems to be incorrect for FlowMonad


Add support in test to test project if there able to parse
Sort ExportSpec by name