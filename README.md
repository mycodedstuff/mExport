# <image src="https://wiki.haskell.org/wikistatic/haskellwiki_logo.png?e89e3" width=33> MExport
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/mycodedstuff/mExport/blob/master/LICENSE)
![Version](https://img.shields.io/badge/version-v0.0.1-blue)
[![Haskell CI](https://github.com/mycodedstuff/mExport/actions/workflows/haskell.yaml/badge.svg)](https://github.com/mycodedstuff/mExport/actions/workflows/haskell.yaml)
![GHC Version](https://img.shields.io/badge/ghc-v8.10.7-brightgreen)
![Last Commit](https://img.shields.io/github/last-commit/mycodedstuff/mExport/main)

### This package helps in minimizing the export list of modules in a Haskell project

## Approach

The idea is simple, mExport parses all the imports of all modules in an Haskell project. Using the import details it can curate a list of functions, types, etc. required from a particular module.

## Prepare your project
To get the best results you can do the following:

You can add the below ghc-options to your project to dump the minimal imports which can be used by mExport.
```yaml
ghc-options:
  - -ddump-minimal-imports
```
GHC will dump the minimal imports into a directory which is required by mExport. 

Currently mExport doesn't detect the dump directory hence it requires this to be specified via `--dump-dir`.

Incase you don't know the default dump dir you can also change the dump directory by adding the following ghc-options.
```yaml
ghc-options:
  - -dumpdir /foo/bar
```
Then the dump directory path can be given to mExport
```shell
mexport --dump-dir /foo/bar
```
### Note: After changing the ghc-options you should do a clean build
```shell
stack purge
stack build
```

## How to install from source
Commands:
```shell
git clone https://github.com/mycodedstuff/mExport
cd mExport
stack install
mexport --help
```
Note: If you don't have ghc 8.10.7 installed it will install it which may take more time.

If you don't have stack then check: [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)


## How to use
Simply run `mexport` in your project directory

For cli options run `mexport --help`

CLI Options:
```shell
mExport - minimize export list of haskell modules

Usage: mexport [--version | [--path DIR] [--analyze] [--extensions GHCEXT] [--dump-dir DIR]]

Available options:
  --version                Print the version
  --path DIR               Path of Haskell project (default: ".")
  --analyze                Analyze the Haskell project
  --extensions GHCEXT      Comma separated GHC Language extensions
  --dump-dir DIR           GHC dump directory path
  -h,--help                Show this help text
```
