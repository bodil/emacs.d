;;; cogre-loaddefs.el --- Auto-generated CEDET autoloads
;;
;;; Code:


;;;### (autoloads (cogre) "cogre" "cogre.el" (19575 43634))
;;; Generated autoloads from cogre.el

(eieio-defclass-autoload 'cogre-base-graph '(eieio-persistent) "cogre" "A Connected Graph.\na connected graph contains a series of nodes and links which are\nrendered in a buffer, or serialized to disk.")

(eieio-defclass-autoload 'cogre-graph-element '(eieio-named) "cogre" "A Graph Element.\nGraph elements are anything that is drawn into a `cogre-base-graph'.\nGraph elements have a method for marking themselves dirty.")

(eieio-defclass-autoload 'cogre-node '(cogre-graph-element) "cogre" "Connected Graph node.\nNodes are regions with a fill color, and some amount of text representing\na status, or values.")

(eieio-defclass-autoload 'cogre-link '(cogre-graph-element) "cogre" "Connected Graph link.\nLinks are lines drawn between two nodes, or possibly loose in space\nas an intermediate step.  Some links have text describing what they\ndo, and most links have special markers on one end or another, such as\narrows or circles.")

(eieio-defclass-autoload 'cogre-arrow '(cogre-link) "cogre" "This type of link is a simple arrow.")

(autoload 'cogre "cogre" "\
Create a new graph not associated with a buffer.
The new graph will be given NAME.  See `cogre-mode' for details.
Optional argument GRAPH-CLASS indicates the type of graph to create.

\(fn NAME &optional GRAPH-CLASS)" t nil)

;;;***

;;;### (autoloads (cogre-export-ascii) "cogre-ascii" "cogre-ascii.el"
;;;;;;  (19575 43634))
;;; Generated autoloads from cogre-ascii.el

(autoload 'cogre-export-ascii "cogre-ascii" "\
Export the current diagram into an ASCII buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-export-utest cogre-export-dot-method cogre-export-dot-postscript-print
;;;;;;  cogre-export-dot-png cogre-export-dot) "cogre-convert" "cogre-convert.el"
;;;;;;  (19575 43634))
;;; Generated autoloads from cogre-convert.el

(autoload 'cogre-export-dot "cogre-convert" "\
Export the current COGRE graph to DOT notation.
DOT is a part of GraphViz.

\(fn)" t nil)

(autoload 'cogre-export-dot-png "cogre-convert" "\
Export the current COGRE graph to DOT, then convert that to PNG.
The png file is then displayed in an Emacs buffer.
DOT is a part of GraphVis.

\(fn)" t nil)

(autoload 'cogre-export-dot-postscript-print "cogre-convert" "\
Print the current graph.
This is done by exporting the current COGRE graph to DOT, then
convert that to Postscript before printing.
DOT is a part of GraphVis.

\(fn)" t nil)

(autoload 'cogre-export-dot-method "cogre-convert" "\
Convert G into DOT syntax of semantic tags.

\(fn (G cogre-base-graph))" nil nil)

(autoload 'cogre-export-utest "cogre-convert" "\
Run all the COGRE structured export/convert test.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-dot-mode) "cogre-dot-mode" "cogre-dot-mode.el"
;;;;;;  (19575 43634))
;;; Generated autoloads from cogre-dot-mode.el

(autoload 'cogre-dot-mode "cogre-dot-mode" "\
Major mode for the dot language.
This is a mini-mode that will first attempt to load and install
`graphviz-dot-mode' in this buffer.  If that fails, it installs
the syntax table, and runs a hook needed to get Semantic working
as a parsing engine.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . cogre-dot-mode))

;;;***

;;;### (autoloads (cogre-layout) "cogre-layout" "cogre-layout.el"
;;;;;;  (19575 43634))
;;; Generated autoloads from cogre-layout.el

(autoload 'cogre-layout "cogre-layout" "\
Layout the current graph.
This function depends on graphviz `dot' program.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-mode) "cogre-mode" "cogre-mode.el" (19575
;;;;;;  43634))
;;; Generated autoloads from cogre-mode.el

(autoload 'cogre-mode "cogre-mode" "\
Connected Graph Editor Mode.
\\{cogre-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.cgr\\'" 'cogre-mode))

;;;***

;;;### (autoloads (cogre-periodic-utest cogre-periodic) "cogre-periodic"
;;;;;;  "cogre-periodic.el" (19575 43634))
;;; Generated autoloads from cogre-periodic.el

(autoload 'cogre-periodic "cogre-periodic" "\
Create a periodic table of COGRE objects.

\(fn)" t nil)

(autoload 'cogre-periodic-utest "cogre-periodic" "\
Run the cogre periodic table for unit testing.
Also test various output mechanisms from the periodic table.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-uml-quick-class cogre-export-code cogre-semantic-tag-to-node)
;;;;;;  "cogre-semantic" "cogre-semantic.el" (19575 43634))
;;; Generated autoloads from cogre-semantic.el

(autoload 'cogre-semantic-tag-to-node "cogre-semantic" "\
Convert the Semantic tag TAG into a COGRE node.
Only handles data types nodes.
To convert function/variables into methods or attributes in
an existing COGRE node, see @TODO - do that.

\(fn TAG)" nil nil)

(autoload 'cogre-export-code "cogre-semantic" "\
Export the current graph into source-code in FILE.
Uses `cogre-export-semantic' to convert into Semantic tags.
Uses `cogre-srecode-setup' to setup SRecode for code generation.

\(fn FILE)" t nil)

(autoload 'cogre-uml-quick-class "cogre-semantic" "\
Create a new UML diagram based on CLASS showing only immediate lineage.
The parent to CLASS, CLASS, and all of CLASSes children will be shown.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads (srecode-semantic-handle-:dot srecode-semantic-handle-:cogre
;;;;;;  cogre-srecode-setup) "cogre-srecode" "cogre-srecode.el" (19575
;;;;;;  43634))
;;; Generated autoloads from cogre-srecode.el

(autoload 'cogre-srecode-setup "cogre-srecode" "\
Update various paths to get SRecode to identify COGRE macros.

\(fn)" nil nil)

(autoload 'srecode-semantic-handle-:cogre "cogre-srecode" "\
Add macros to dictionary DICT based on COGRE data.

\(fn DICT)" nil nil)

(eval-after-load "srecode-map" '(cogre-srecode-setup))

(autoload 'srecode-semantic-handle-:dot "cogre-srecode" "\
Add macros to dictionary DICT based on the current DOT buffer.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads (cogre-uml-sort-for-lineage cogre-uml-enable-unicode)
;;;;;;  "cogre-uml" "cogre-uml.el" (19575 43634))
;;; Generated autoloads from cogre-uml.el

(eieio-defclass-autoload 'cogre-package '(cogre-node) "cogre-uml" "A Package node.\nPackages represent other class diagrams, and list the major nodes\nwithin them.  They can be linked by dependency links.")

(eieio-defclass-autoload 'cogre-note '(cogre-node) "cogre-uml" "An note node.\nNotes are used to add annotations inside a graph.\nNotes are generally linked to some node, and are supposed to look\nlike a little pieces of paper.")

(eieio-defclass-autoload 'cogre-scoped-node '(cogre-node) "cogre-uml" "A UML node that has a package specifier within which it is scoped.")

(eieio-defclass-autoload 'cogre-class '(cogre-scoped-node) "cogre-uml" "A Class node.\nClass nodes represent a class, and can list the attributes and methods\nwithin them.  Classes can have attribute links, and class hierarchy links.")

(eieio-defclass-autoload 'cogre-instance '(cogre-scoped-node) "cogre-uml" "An instance node.\nInstances are used in instance diagrams.\nInstances are linked together with plain links.")

(eieio-defclass-autoload 'cogre-inherit '(cogre-link) "cogre-uml" "This type of link indicates that the two nodes reference infer inheritance.\nThe `start' node is the child, and the `end' node is the parent.\nThis is supposed to infer that START inherits from END.")

(eieio-defclass-autoload 'cogre-aggregate '(cogre-link) "cogre-uml" "This type of link indicates aggregation.\nThe `start' node is the owner of the aggregation, the `end' node is\nthe item being aggregated.\nThis is supposed to infer that START contains END.")

(autoload 'cogre-uml-enable-unicode "cogre-uml" "\
Enable use of UNICODE symbols to create COGRE graphs.
Inheritance uses math triangle on page 25a0.
Aggregation uses math square on edge 25a0.
Line-drawing uses line-drawing codes on page 2500.
See http://unicode.org/charts/symbols.html.

The unicode symbols can be differing widths.  This will make the
cogre chart a little screwy somteims.  Your mileage may vary.

\(fn)" t nil)

(autoload 'cogre-uml-sort-for-lineage "cogre-uml" "\
Sort the current graph G for determining inheritance lineage.
Return it as a list of lists.  Each entry is of the form:
  ( NODE PARENT1 PARENT2 ... PARENTN)

\(fn G)" t nil)

;;;***

;;;### (autoloads (cogre-utest-quick-class cogre-utest) "cogre-utest"
;;;;;;  "cogre-utest.el" (19575 43634))
;;; Generated autoloads from cogre-utest.el

(autoload 'cogre-utest "cogre-utest" "\
Unit test Various aspects of COGRE.

\(fn)" t nil)

(autoload 'cogre-utest-quick-class "cogre-utest" "\
Test the quick-class function.

\(fn)" t nil)

;;;***

;;;### (autoloads (cogre-picture-insert-rectangle) "picture-hack"
;;;;;;  "picture-hack.el" (19575 43634))
;;; Generated autoloads from picture-hack.el

(autoload 'cogre-picture-insert-rectangle "picture-hack" "\
Overlay RECTANGLE with upper left corner at point.
Leaves the region surrounding the rectangle.

\(fn RECTANGLE)" nil nil)

;;;***

;;;### (autoloads (wisent-dot-setup-parser) "wisent-dot" "wisent-dot.el"
;;;;;;  (19575 43634))
;;; Generated autoloads from wisent-dot.el

(autoload 'wisent-dot-setup-parser "wisent-dot" "\
Setup buffer for parse.

\(fn)" nil nil)

(add-hook 'graphviz-dot-mode-hook 'wisent-dot-setup-parser)

(add-hook 'cogre-dot-mode-hook 'wisent-dot-setup-parser)

;;;***

;;;### (autoloads nil nil ("cogre-load.el" "wisent-dot-wy.el") (19884
;;;;;;  40438 898012))

;;;***

(provide 'cogre-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cogre-loaddefs.el ends here
