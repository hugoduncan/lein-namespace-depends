(ns leiningen.namespace-graph
  "Display namespace dependencies."
  (:require
   clojure.contrib.find-namespaces
   clojure.contrib.pprint
   clojure.contrib.string
   leiningen.compile
   leiningen.classpath))


(def *dependencies*)
(def core-ns #'clojure.core/ns)

;; lifted from core so we can record the list of :use, :require in *dependencies*
(defmacro ns-with-recording
  {:arglists '([name docstring? attr-map? references*])
   :added "1.0"}
  [name & references]
  (let [process-reference
        (fn [[kname & args]]
          `(~(symbol "clojure.core" (clojure.core/name kname))
             ~@(map #(list 'quote %) args)))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        metadata   (when (map? (first references)) (first references))
        references (if metadata (next references) references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        gen-class-clause (first (filter #(= :gen-class (first %)) references))
        gen-class-call
          (when gen-class-clause
            (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (next gen-class-clause)))
        references (remove #(= :gen-class (first %)) references)
        ;ns-effect (clojure.core/in-ns name)
        ]
    (set! *dependencies* (vec references))
    `(do
       (clojure.core/in-ns '~name)
       (with-loading-context
        (binding [clojure.core/ns ~#'leiningen.namespace-graph/core-ns]
          ~@(when gen-class-call (list gen-class-call))
          ~@(when (and (not= name 'clojure.core)
                       (not-any? #(= :refer-clojure (first %)) references))
              `((clojure.core/refer '~'clojure.core)))
          ~@(map process-reference references))))))

(defmacro with-dependencies
  "Creates a thread binding for *dependencies*, and binds clojure.core/ns to
   our replacement."
  [& body]
  `(binding [*dependencies* nil
             clojure.core/ns #'ns-with-recording]
     ~@body))

(defn extract-dependencies
  "Process each references clause in ns, and extract namespace names."
  [dependencies]
  (letfn [(dependent-ns
           [expr]
           (if (symbol? expr)
             expr
             (if (keyword? (second expr))
               (first expr)
               (reduce
                (fn [s sym]
                  (symbol
                   (format "%s.%s"
                           (first expr)
                           (if (symbol? sym) sym (first sym)))))
                [] (rest expr)))))]
    (flatten
     (map
      (comp (partial map dependent-ns) rest)
      ;; we include 'import for find-namespaces
      (remove #(#{:import :refer-clojure 'import} (first %)) dependencies)))))


(defn find-graph
  "Find the namespace use/require graph. Requires each project namespace in turn
   extracting the list of namespaces it uses or requires.  Each namespace is
   recursively required to get the full dependency graph."
  [source-path]
  (loop [to-load (clojure.contrib.find-namespaces/find-namespaces-in-dir
                  (java.io.File. source-path))
         graph {}]
    (if (seq to-load)
      (let [dependencies (with-dependencies
                           (require (first to-load) :reload)
                           (extract-dependencies *dependencies*))]
        (recur
         (distinct (concat (rest to-load) dependencies))
         (assoc graph (first to-load) dependencies)))
      (into {} (filter #(seq (second %)) graph)))))



(defn dot-edge
  "Format an edge for dot."
  [from to]
  (format "\"%s\" -> \"%s\";" from to))


(defmulti output-graph
  "Output the dependency graph in a specific format"
  (fn [format graph] format))

(defmethod output-graph :pprint
  [_ graph]
  (clojure.contrib.pprint/pprint graph))

(defmethod output-graph :graphviz-dot
  [_ graph]
  (print
   (format
    "digraph { %s }"
    (clojure.contrib.string/join
     \newline
     (flatten
      (map (fn [[from to]]
             (map (partial dot-edge from) to))
           graph))))))

(defn namespace-graph*
  "Create the namespace graph"
  [source-path graph-format]
  (output-graph graph-format (find-graph source-path)))

(defn namespace-graph
  "Create the namespace graph.

   Output format can be defined using the :graph-format option in
   project.clj. Current formats are:
     :pprint       -- pprint dependency edges
     :graphviz-dot -- output a digraph for graphviz dot tool

  The format can also be specified (without the :) as a lein command line
  argument."
  ([project] (namespace-graph project (:graph-format project :pprint)))
  ([project format]
     (leiningen.compile/eval-in-project
      project
      `(do
         (require 'leiningen.namespace-graph)
         (leiningen.namespace-graph/namespace-graph*
          ~(:source-path project)
          ~(if (string? format) (keyword format) format)))
      (fn [java]
        (.setClasspath
         java
         (apply leiningen.classpath/make-path
                (concat
                 (leiningen.classpath/get-classpath project)
                 (.. (Thread/currentThread) getContextClassLoader getURLs)))))
      :skip-auto-compile)))
