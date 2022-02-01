### TODOs

#### Options
- Add option for styling exports
- Add option provide custom extensions
- Add option to exclude certain directories

#### Test
- Add unit tests

#### Feature
- Consider existing exports
- Add ability to use yaml or json file for configuration

#### Enhancements
- Add support for line length in export formatter
- Read available .gitignore to populate excludeDir config
- While writing exports preserve comments written in between exports list

#### Fixes
- Handle case when where keyword isn't present Setup.hs
- Fix case when two files with same module name are present in different dir
