# elm-module-graph

Visually explore package and module dependencies for an Elm project.


## Example

Explore dependency graphs for the Elm project at [knewter/time-tracker](https://github.com/knewter/time-tracker) in this [example](https://justinmimbs.github.io/elm-module-graph/example/).


## Usage

1.  __Generate__ a `module-graph.json` file for your Elm project with the Python script in this repository.

    To generate this file, first save the script, [elm-module-graph.py](https://raw.githubusercontent.com/justinmimbs/elm-module-graph/master/elm-module-graph.py), locally. Execute it, passing it the file path to either an Elm module or an `elm-package.json`.

    ```sh
    ./elm-module-graph.py project/src/Main.elm
    ```

    or

    ```sh
    ./elm-module-graph.py library/elm-package.json
    ```

    By default it writes to a file in the current working directory named `module-graph.json`, but you can specify a different output location with the `--output` option.


2.  __Provide__ that file to this page: [elm-module-graph](https://justinmimbs.github.io/elm-module-graph).


## About

The page displays two graph diagrams. Nodes are vertically sorted so that for each connection the lower node depends on the higher node.

In the Packages graph, clicking a package name will toggle its modules in and out of the Modules graph. In the Modules graph, clicking a module name will highlight the subgraph it's connected to; the nodes are then colored based on distance from the selected module.

Graph diagrams are drawn using [justinmimbs/elm-arc-diagram](http://package.elm-lang.org/packages/justinmimbs/elm-arc-diagram/latest).
