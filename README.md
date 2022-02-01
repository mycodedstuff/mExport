# <image src="https://wiki.haskell.org/wikistatic/haskellwiki_logo.png?e89e3" width=33> MExport
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/mycodedstuff/mExport/blob/master/LICENSE)
![Version](https://img.shields.io/badge/version-v0.0.1-blue)
[![Haskell CI](https://github.com/mycodedstuff/mExport/actions/workflows/haskell.yaml/badge.svg)](https://github.com/mycodedstuff/mExport/actions/workflows/haskell.yaml)
![GHC Version](https://img.shields.io/badge/ghc-v8.10.7-brightgreen)
![Last Commit](https://img.shields.io/github/last-commit/mycodedstuff/mExport/main)

### This package helps in minimizing the export list of modules in a Haskell project

## Approach

The idea is simple, mExport parses all the imports of all modules in an Haskell project. Using the import details it can curate a list of functions, types, etc. required from a particular module.


## Assumption

The above approach works on the assumption that the modules of a Haskell project have correctly defined import including the functions, types, etc.

## Solution

In case your Haskell project doesn't have good import statement with proper declaration of required functions, types, etc.
This can be easily resolved by using libraries like [smuggler2](https://github.com/jrp2014/smuggler2). This package can help you improve the import statement for all your modules. Then on top of the output of smuggler2, mExport can be used to minimize the exports list.
Note: Smuggler2 has option to add exports explicitly but it will add everything to the export list.

To configure smuggler2 for your Haskell project follow the guide
- <a href="https://github.com/mycodedstuff/mExport/blob/main/wiki/smuggler2-stack.md" target="_blank">Stack users</a> - Smuggler2 github doesn't have any documentation for stack users, hence I've create one
- <a href="https://github.com/jrp2014/smuggler2/blob/master/README.md" target="_blank">Smuggler2 Doc</a>


## How to build
Steps:
- Clone the repository `git clone https://github.com/mycodedstuff/mExport`
- This project uses stack, hence you can run `stack build --copy-bins` inside the repo directory
- Now `mexport` command should be available

Note: If you don't have ghc 8.10.7 installed it will install it which may take more time.

If you don't have stack then check: [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)


## How to use
Simply run `mexport` in your project directory

For cli options run `mexport --help`

CLI Options:
```
mExport - minimize export list of haskell modules

Usage: mexport [--version | [--path DIR] [--analyze]]

Available options:
  --version                Print the version
  --path DIR               Path of Haskell project (default: ".")
  --analyze                Analyze the Haskell project
  -h,--help                Show this help text
```
