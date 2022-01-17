## MExport

### This package helps in minimizing the export list of modules in a Haskell project
---
### Approach

#### The idea is simple, mExport parses all the imports of all modules in an Haskell project. Using the import details it can curate a list of functions types, etc. required from a particular module.
---
### Assumption

#### The above approach works on the assumption that the modules of a Haskell project have correctly defined import including the functions, types, etc.
---
### Solution

#### In case your Haskell project doesn't have good import statement with proper declaration of required functions, types, etc.
#### This can be easily resolved by using libraries like [smuggler2](https://github.com/jrp2014/smuggler2). This package can help you improve the import statement for all your modules. Then on top of the output of smuggler2, mExport can be used to minimize the exports list.
#### Note: Smuggler2 has option to add exports explicitly but it will add everything to the export list.

#### To configure smuggler2 for your Haskell project follow the guide
- [Stack users](./wiki/smuggler2-stack.md) - Smuggler2 github doesn't have any documentation for stack users, hence I've create one
- [Smuggler2 Doc](https://github.com/jrp2014/smuggler2/blob/master/README.md)
