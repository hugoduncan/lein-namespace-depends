# namespace-depends

A [Leiningen](http://github.com/technomancy/leiningen) plugin to output the
namespace use/require dependency tree.

## Usage

    lein namespace-graph [pprint|graphviz-dot]

The default output is pprint, but this can be overriden in the `project.clj`,
using the `:graph-format` key.

    :graph-format :graphviz-dot


## Installation

Include `lein-namespace-dependency` in your `project.clj`'s `:dev-dependencies`.

    :dev-dependencies [[lein-namespace-depends "0.1.0-SNAPSHOT"]]

## License

Copyright (C) 2010 Hugo Duncan.

Distributed under the Eclipse Public License.
