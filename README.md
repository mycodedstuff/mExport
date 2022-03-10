# <image src="https://wiki.haskell.org/wikistatic/haskellwiki_logo.png?e89e3" width=33> MExport
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/mycodedstuff/mExport/blob/master/LICENSE)
![Version](https://img.shields.io/badge/version-v0.0.1-blue)
[![Haskell CI](https://github.com/mycodedstuff/mExport/actions/workflows/haskell.yaml/badge.svg)](https://github.com/mycodedstuff/mExport/actions/workflows/haskell.yaml)
![GHC Version](https://img.shields.io/badge/GHC-v8.10.7-brightgreen)
![Last Commit](https://img.shields.io/github/last-commit/mycodedstuff/mExport/main)

### This package helps in minimizing the export list of modules in a Haskell project

## Approach

The idea is simple, mExport parses all the imports of all modules in an Haskell project. Using the import details it can curate a list of functions, types, etc. required from a particular module.

## Prepare your project
To get the best results you can do the following:

You can add the below ghc-options to your project to dump the minimal imports which can be used by mExport.
> **In stack.yaml**
```yaml
ghc-options:
  - -ddump-minimal-imports
```
> **In .cabal**
```yaml
ghc-options: -ddump-minimal-imports
```
GHC will dump the minimal imports into a directory which is used by mExport. 

mExport automatically detects these dump directories by itself. Raise an issue if it's incorrect.

Note: This detection works by using stack/cabal tool. Hence it's required to be available in PATH

> Caution: mExport isn't tested with custom dump dir given using ```-dump-dir``` ghc option

### Note: After changing the ghc-options you should do a clean build
```shell
stack purge
stack build
-- or --
cabal v2-clean
cabal v2-build
```

## How to install from source
Commands:
```shell
git clone https://github.com/mycodedstuff/mExport
cd mExport
stack install #or cabal v2-install
mexport --help
```

Make sure you have ```~/.local/bin``` *(if installed via stack)* or ```~/.cabal/bin``` *(if installed via cabal)* in your PATH

Note: If you don't have ghc 8.10.7 installed it will install it which may take more time.

## How to use
Simply run `mexport` in your project directory

For cli options run `mexport --help`

CLI Options:
```shell
mExport 0.0.1 - minimize export list of haskell modules

Usage: mexport [--version | [--path DIR] [--analyze] [--extensions GHCEXT] 
                 [--dump-dir DIR] [--indent NUM] [--collapse NUM]]

Available options:
  --version                Print the version
  --path DIR               Path of Haskell project (default: ".")
  --analyze                Analyze the Haskell project, helps in verifying if
                           project can be parsed
  --extensions GHCEXT      Comma separated GHC Language extensions
  --indent NUM             Indentation for the exports
  --collapse NUM           Exports everything of a type if NUM or more
                           percentage is exported
  -h,--help                Show this help text
```

You can also provide some options using a yaml configuration file

Create a `.mexport.yaml` inside the project directory, `mExport` will detect the file itself
```yaml
indent: 2
collapse: 80
extensions:
  - BangPatterns
  - TypeApplications
```

## Code has compilation errors after running mexport
1. This could be due to ambiguity errors where an unqualified import of module may have same things as the module exporting it.
2. The import statement might be explicitly importing something which isn't used in the module
3. A module might be re-exporting an imported module, currently mexport doesn't support these.
3. Something else? Raise an issue.

## Code has compilation warnings after running mexport
1. These would mostly be due to unused code in your code base
2. Something else? Raise an issue to let me know.

## Problems
1. MExport doesn't handle reexported modules
2. MExport overwrites exports of modules hence any comments in between them gets overwritten
