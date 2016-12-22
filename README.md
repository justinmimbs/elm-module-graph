# elm-module-graph

Visually explore package and module dependencies for an Elm project.


## Usage

1. Generate a `module-graph.json` file for your Elm project with the Python script in this repository.
2. Provide that file to this page: [elm-module-graph](https://justinmimbs.github.io/elm-module-graph).

The page displays two graph diagrams. Nodes are vertically sorted so that for each connection the higher node is a dependency of the lower node.

In the Packages graph, clicking a package name will toggle its modules in and out of the Modules graph. In the Modules graph, clicking a module name will highlight the subgraph it's connected to; the nodes are then colored based on distance from the selected module.


## module-graph.json

To generate this file, first save the script, [elm-module-graph.py](https://raw.githubusercontent.com/justinmimbs/elm-module-graph/master/elm-module-graph.py), locally. Execute it, passing it the file path to either an Elm module or an `elm-package.json`.

```sh
./elm-module-graph.py project/src/Main.elm
```

or

```sh
./elm-module-graph.py library/elm-package.json
```

By default it writes to a file in the current working directory named `module-graph.json`, but you can specify a different location with the `--output` option.
