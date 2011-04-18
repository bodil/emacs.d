
;;;### (autoloads (bsh-script-help) "beanshell" "beanshell.el" (19289
;;;;;;  5860))
;;; Generated autoloads from beanshell.el

(autoload (quote bsh-script-help) "beanshell" "\
Display BeanShell User's Guide.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-ant-show-options jde-ant-projecthelp jde-ant-build)
;;;;;;  "jde-ant" "jde-ant.el" (19289 5860))
;;; Generated autoloads from jde-ant.el

(autoload (quote jde-ant-build) "jde-ant" "\
Build the current project using Ant.  If interactive, we try to prompt the
  user for certain variables..

\(fn BUILDFILE TARGET &optional INTERACTIVE-ARGS)" t nil)

(autoload (quote jde-ant-projecthelp) "jde-ant" "\
Display Ant project help for the current project.
This will execute the Ant program with the `-projecthelp' switch to output
available targets with their descriptions for the current buildfile. This
function uses the same rules as `jde-ant-build' for finding the buildfile.

\(fn BUILDFILE)" t nil)

(autoload (quote jde-ant-show-options) "jde-ant" "\
Show the JDE Ant Options panel.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-bookmark-list jde-bookmark-add jde-bookmark-visit)
;;;;;;  "jde-bookmark" "jde-bookmark.el" (19289 5860))
;;; Generated autoloads from jde-bookmark.el

(autoload (quote jde-bookmark-visit) "jde-bookmark" "\
Visit a class by bookmark name.

\(fn KEY)" t nil)

(autoload (quote jde-bookmark-add) "jde-bookmark" "\
Add the current visited class as a bookmark.

\(fn KEY &optional FQ-CLASS)" t nil)

(autoload (quote jde-bookmark-list) "jde-bookmark" "\
List bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-bsh-quote-expr jde-bsh-run jde-jeval) "jde-bsh"
;;;;;;  "jde-bsh.el" (19289 5860))
;;; Generated autoloads from jde-bsh.el

(autoload (quote jde-jeval) "jde-bsh" "\
Uses the JDEE's instance of the BeanShell
Java interpreter to evaluate the Java expression EXPR.  If the
BeanShell is not running, the JDEE starts an instance. This function
returns any text output by the Java interpreter's standard out or
standard error pipes.  If EVAL-RETURN is non-nil, this function
returns the result of evaluating the Java output as a Lisp
expression.

NO-PRINT-P, if non-nil, don't wrap JAVA-STATEMENT with a `print'
command yeilding the output.  This is going to need to be true
for most things since unless `show()' was invoked and output
prints out, Emacs has nothing to evaluate or report.

\(fn JAVA-STATEMENT &optional EVAL-RETURN NO-PRINT-P)" t nil)

(autoload (quote jde-bsh-run) "jde-bsh" "\
*Starts the JDEE version of the BeanShell.

\(fn)" t nil)

(autoload (quote jde-bsh-quote-expr) "jde-bsh" "\
Add necessary syntax for a beanshell string (parameter) in the current
buffer.  This is useful for quoting a whole buffer or strings with newlines,
etc.  This gives the outside double quotes as well.

NO-PARAM-P if non-nil, don't split string (surround double quotes) using string
concatentation.  The only way to get quotes in strings is to split them up and
concatenate the quote (') char using the plus (+) operator.  This doesn't do
this syntax change.

NO-QUOTE-WRAP-P, if non-nil, don't add double quotes around the whole statement.

\(fn &optional START END NO-PARAM-P NO-QUOTE-WRAP-P)" t nil)

;;;***

;;;### (autoloads (jde-bug-debug-app) "jde-bug" "jde-bug.el" (19289
;;;;;;  5860))
;;; Generated autoloads from jde-bug.el

(autoload (quote jde-bug-debug-app) "jde-bug" "\
Runs the debugger on the application in the current source buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-checkstyle jde-checkstyle-customize) "jde-checkstyle"
;;;;;;  "jde-checkstyle.el" (19289 5860))
;;; Generated autoloads from jde-checkstyle.el

(autoload (quote jde-checkstyle-customize) "jde-checkstyle" "\
Set Java style checking options.

\(fn)" t nil)

(autoload (quote jde-checkstyle) "jde-checkstyle" "\
Checks the Java program in the current buffer.
This command invokes the style checker specified by `jde-checkstyle-class'
with the options specif2ied by the JDEE customization variables
that begin with `jde-checkstyle'. If the variable
`jde-checkstyle-read-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jde-compat" "jde-compat.el" (19289 5860))
;;; Generated autoloads from jde-compat.el

(defconst jde-xemacsp (string-match "XEmacs" (emacs-version)) "\
Non-nil if we are running in the XEmacs environment.")

;;;***

;;;### (autoloads (jde-compile jde-set-compile-options) "jde-compile"
;;;;;;  "jde-compile.el" (19289 5860))
;;; Generated autoloads from jde-compile.el

(autoload (quote jde-set-compile-options) "jde-compile" "\
Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose.

\(fn OPTIONS)" t nil)

(autoload (quote jde-compile) "jde-compile" "\
Compile the Java program in the current buffer.
This command invokes the compiler specified by `jde-compiler'
with the options specified by the JDE customization variables
that begin with `jde-compile'. If the variable
`jde-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled. If `jde-compiler' specifies the JDE compile
server, this command uses the compile server. Otherwise, it
uses the compiler executable specified by
`jde-compiler' to compile.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-customize-option) "jde-custom" "jde-custom.el"
;;;;;;  (19289 5860))
;;; Generated autoloads from jde-custom.el

(defalias (quote jde-customize-variable) (quote jde-customize-option))

(autoload (quote jde-customize-option) "jde-custom" "\
Customize SYMBOL, which must be a JDEE option variable.

\(fn SYMBOL)" t nil)

;;;***

;;;### (autoloads (jde-debug-applet jde-debug jde-db-set-app-args
;;;;;;  jde-db-set-args jde-db-set-debugger) "jde-db" "jde-db.el"
;;;;;;  (19289 5860))
;;; Generated autoloads from jde-db.el

(autoload (quote jde-db-set-debugger) "jde-db" "\
Specify the pathname of the debugger, if an executable, or the
debugger's fully qualified class name, if a class.

\(fn NAME IS-EXECUTABLE)" t nil)

(autoload (quote jde-db-set-args) "jde-db" "\
Specify the arguments (except -classpath) to be passed to the debugger.

\(fn ARGS)" t nil)

(autoload (quote jde-db-set-app-args) "jde-db" "\
Specify the arguments to be passed to the Java application class.

\(fn ARGS)" t nil)

(autoload (quote jde-debug) "jde-db" "\
Run the debugger specified by `jde-debugger' on the Java application
whose source resides in the current buffer. This command determines
the main class of the application either from the variable
`jde-run-application-class' or from the source in the current
buffer. If `jde-run-application-class' does not specify a class, the
main class is assumed to be the class defined by the current source
buffer. This command creates a command buffer for the debug session.

\(fn)" t nil)

(autoload (quote jde-debug-applet) "jde-db" "\
Runs an applet in the jdb debugger. This function prompts you to enter
the path of an html document that displays the applet. If you
do not enter a path, this function next checks
whether `jde-run-applet-doc' specifies a document. If so, it displays
that specified document. Next, it checks whether the current directory
contains any html files. If so, it displays the first html file that
it finds. If if cannot find an html file, it signals an error.  This
function runs appletviewer in jdb to permit debugging. On startup, it
sets a breakpoint in the init method of the class specified by
`jde-run-application-class' or in the class corresponding to the Java
file in the current buffer.

\(fn &optional DOC)" t nil)

;;;***

;;;### (autoloads (jde-ejb-entity-bean-buffer jde-ejb-session-bean-buffer)
;;;;;;  "jde-ejb" "jde-ejb.el" (19289 5860))
;;; Generated autoloads from jde-ejb.el

(autoload (quote jde-ejb-session-bean-buffer) "jde-ejb" "\
Create a new Java buffer containing an EJB session bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jde-ejb templates.  This includes naming the files according
to the EJB naming convention.

\(fn EJB-NAME)" t nil)

(autoload (quote jde-ejb-entity-bean-buffer) "jde-ejb" "\
Create a new Java buffer containing an EJB entity bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jde-ejb templates.  This includes naming the files according
to the EJB naming convention.

\(fn EJB-NAME)" t nil)

;;;***

;;;### (autoloads (jde-gen-exception-buffer jde-gen-exception-buffer-template
;;;;;;  jde-gen-object-methods jde-gen-tostring-return jde-gen-tostring-method-template
;;;;;;  jde-gen-hashcode-body jde-gen-hashcode-method-template jde-gen-equals-return
;;;;;;  jde-gen-equals-method-template jde-gen-equals-parens-around-expression
;;;;;;  jde-gen-equals-trailing-and-operators jde-gen-buffer jde-gen-jfc-app-buffer
;;;;;;  jde-gen-bean-buffer jde-gen-console-buffer jde-gen-interface-buffer
;;;;;;  jde-gen-class-buffer) "jde-gen" "jde-gen.el" (19289 5860))
;;; Generated autoloads from jde-gen.el

(autoload (quote jde-gen-class-buffer) "jde-gen" "\
Create a new Java buffer containing a class of the same name.
This command inserts the class template generated by `jde-gen-class'.

\(fn FILE)" t nil)

(autoload (quote jde-gen-interface-buffer) "jde-gen" "\
Create a new Java buffer containing an interface of the same name.
This command inserts the interface template generated by `jde-gen-interface'.
It then moves the point to the location of the first method.

\(fn FILE)" t nil)

(autoload (quote jde-gen-console-buffer) "jde-gen" "\
Create a new Java buffer containing a console class of the same name.
This command inserts the class template generated by `jde-gen-class'.
It then moves the point to the location to the constructor.

\(fn FILE)" t nil)

(autoload (quote jde-gen-bean-buffer) "jde-gen" "\
Create a new Java buffer containing a Java bean of the same name.
This command inserts the class template generated by `jde-gen-bean'.
It then moves the point to the location of the constructor.

\(fn FILE)" t nil)

(autoload (quote jde-gen-jfc-app-buffer) "jde-gen" "\
Create a new Java buffer containing a JFC application class.
This command inserts the class template generated by `jde-gen-jfc-app'.
It then moves the point to the location to the constructor.

\(fn FILE)" t nil)

(autoload (quote jde-gen-buffer) "jde-gen" "\
Create a new Java buffer containing a code template.
This command inserts the specified template at the beginning
of the buffer.

\(fn TEMPLATE FILE)" t nil)

(defvar jde-gen-equals-trailing-and-operators nil "\
Specifies whether the '&&' operators in a generated equals
method are added at the end of the line or at the beginning.  If
this variable is t, the operator will be added at the end of the
line, else on the next line before the comparison.  With
`jde-gen-equals-trailing-and-operators' set to nil:

    return (a == o.a)
	&& (b == o.b)
	&& (s == null ? o.s == null : s.equals(o.s));

Or, with `jde-gen-equals-trailing-and-operators' set to t:

    return (a == o.a) &&
	(b == o.b) &&
	(s == null ? o.s == null : s.equals(o.s));
")

(custom-autoload (quote jde-gen-equals-trailing-and-operators) "jde-gen" t)

(defvar jde-gen-equals-parens-around-expression nil "\
Specifies whether the generated equals expression should be
surrounded by parentheses.
With `jde-gen-equals-trailing-and-operators' set to nil:

    return ((a == o.a)
	    && (b == o.b)
	    && (s == null ? o.s == null : s.equals(o.s)));

Or, with `jde-gen-equals-trailing-and-operators' set to t:

    return ((a == o.a) &&
	    (b == o.b) &&
	    (s == null ? o.s == null : s.equals(o.s)));
")

(custom-autoload (quote jde-gen-equals-parens-around-expression) "jde-gen" t)

(defvar jde-gen-equals-method-template (quote ("'>" "\"/**\" '> 'n" "\" * Check if this object is equal (equivalent) to another object.\" '> 'n" "\" */\" '> 'n" "(jde-gen-method-signature \"public\" \"boolean\" \"equals\" \"Object obj\")" "(jde-gen-electric-brace)" "\"if (obj == this) return true;\" '> 'n" "\"if ((obj == null) || !getClass().equals(obj.getClass())) return false;\" '> 'n" "'> 'n" "(jde-gen-equals-return \"obj\" \"o\") '> 'n" "\"}\" '> 'n '>))")) "\
*Template for creating an equals method.
Setting this variable defines a template instantiation command
`jde-gen-equals-method', as a side-effect.")

(custom-autoload (quote jde-gen-equals-method-template) "jde-gen" nil)

(autoload (quote jde-gen-equals-return) "jde-gen" "\
Generate a body of an appropriate override for the
java.lang.Object#equals(Object) function. This function gets the
list of member variables from`jde-parse-get-serializable-members'.

The first optional parameter `parm-name' is the parameter name of
the Object argument of the equals function.  Default is \"obj\".

The second optional parameter `var-name' denotes the variable
name used to cast the \"obj\" argument to. The default is \"o\".

The third optional parameter `class' can be a semantic tag, which
is then used instead of the result of `semantic-current-tag'.

Example:
    class Bean {
	int a;
	long b;
	String s;
    }

Result:
    Bean o = (Bean) obj;

    return (a == o.a)
	&& (b == o.b)
	&& (s == null ? o.s == null : s.equals(o.s));

Or, with `jde-gen-equals-trailing-and-operators' set to t:
    Bean o = (Bean) obj;

    return (a == o.a) &&
	(b == o.b) &&
	(s == null ? o.s == null : s.equals(o.s));

\(fn &optional PARM-NAME VAR-NAME CLASS)" t nil)

(defvar jde-gen-hashcode-method-template (quote ("'>" "\"/**\" '> 'n" "\" * Calculate the hash code for this object.\" '> 'n" "\" * \" '> 'n" "\" * <p>The rules laid out in J. Blosh's Effective Java are used\" '> 'n" "\" * for the hash code calculation.</p>\" '> 'n" "\" * \" '> 'n" "\" * @return the hash code.\" '> 'n" "\" * \" '> 'n" "\" * @see java.lang.Object#hashCode\" '> 'n" "\" */\" '> 'n" "(jde-gen-method-signature \"public\"\"int\" \"hashCode\" nil)" "(jde-gen-electric-brace)" "(jde-gen-hashcode-body) '> 'n" "\"}\" '> 'n '>")) "\
*Template for creating a hashCode method.
Setting this variable defines a template instantiation command
`jde-gen-hashcode-method', as a side-effect.")

(custom-autoload (quote jde-gen-hashcode-method-template) "jde-gen" nil)

(autoload (quote jde-gen-hashcode-body) "jde-gen" "\
Generate a body of a hashCode function.
This function gets the list of member variables of the current
class from `jde-parse-get-serializable-members'.

The first optional parameter `var-name' denotes the variable name used
to calculate the hash code, the default is \"code\".

The second optional parameter `class' can be a semantic tag, which is
then used instead of the result of `semantic-current-tag'.

\(fn &optional VAR-NAME CLASS)" t nil)

(defvar jde-gen-tostring-method-template (quote ("'>" "(when jde-gen-create-javadoc" "'(l " "\"/**\" '> 'n" "\" * Get a string representation of this object.\" '> 'n" "\" * \" '> 'n" "\" * @return a string representation of this object.\" '> 'n" "\" * \" '> 'n" "\" * @see java.lang.Object#toString\" '> 'n" "\" */\" '> 'n))" "(jde-gen-method-signature \"public\" \"String\" \"toString\" \"\")" "(jde-gen-electric-brace)" "(jde-gen-tostring-return) '> 'n" "\"}\" '>")) "\
*Template for creating an toString method.
Setting this variable defines a template instantiation
command `jde-gen-tostring-method', as a side-effect.")

(custom-autoload (quote jde-gen-tostring-method-template) "jde-gen" nil)

(autoload (quote jde-gen-tostring-return) "jde-gen" "\
Generate a body of an appropriate override for the
java.lang.Object#toString function. This gets the member variables
of the current class from semantic via `semantic-current-tag'.

\(fn &optional CLASS)" t nil)

(autoload (quote jde-gen-object-methods) "jde-gen" "\
Generates an equals(), a hashCode() and a toString method.

\(fn)" t nil)

(defvar jde-gen-exception-buffer-template (list "(open-line 1) (funcall jde-gen-boilerplate-function)" "(jde-gen-get-package-statement)" "(progn (require 'jde-javadoc) (jde-javadoc-insert-start-block))" "\" * Exception <code>\" (jde-parse-get-buffer-unqualified-class) \"</code>.\" '> 'n" "\" \" (jde-javadoc-insert-empty-line)" "\" * Created: \" (current-time-string) '> 'n" "\" \" (jde-javadoc-insert-empty-line)" "\" \" (jde-gen-save-excursion (jde-javadoc-insert 'tempo-template-jde-javadoc-author-tag))" "\" \" (jde-gen-save-excursion (jde-javadoc-insert 'tempo-template-jde-javadoc-version-tag))" "\" \" (jde-javadoc-insert-end-block)" "\"public class \"" "(jde-parse-get-buffer-unqualified-class)" "\" \" (jde-gen-get-extend-class)" "(jde-gen-electric-brace)" "'p'n" "'> (jde-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jde-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* <code>null</code> as its detail message.\" '>'n" "'> (jde-javadoc-insert-end-block)" "(jde-gen-method-signature \"public\" nil (jde-parse-get-buffer-unqualified-class) nil)" "(jde-gen-electric-brace)" "\"}\"'>'n" "'n" "'> (jde-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jde-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* the specified detail message.\" '>'n" "'> (jde-javadoc-insert-empty-line)" "\"* @param message the detail message string.\" '> 'n" "'> (jde-javadoc-insert-end-block)" "(jde-gen-method-signature \"public\" nil (jde-parse-get-buffer-unqualified-class) \"String message\")" "(jde-gen-electric-brace)" "\"super(message);\" '> 'n" "\"}\" '> 'n" "'n" "'> (jde-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jde-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* the specified cause and a detail message of\" '> 'n" "\"* <code>(cause == null ? null : cause.toString())</code>\" '> 'n" "\"* (which typically contains the class and detail message of cause).\" '> 'n" "'> (jde-javadoc-insert-empty-line)" "\"* @param cause the causing throwable. A null value is permitted\" '> 'n" "\"*     and indicates that the cause is nonexistent or unknown.\" '> 'n" "'> (jde-javadoc-insert-end-block)" "(jde-gen-method-signature \"public\" nil (jde-parse-get-buffer-unqualified-class) \"Throwable cause\")" "(jde-gen-electric-brace)" "\"super(cause == null ? (String) null : cause.toString());\" '> 'n" "\"initCause(cause);\" '> 'n" "\"}\" '> 'n" "'n" "'> (jde-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jde-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* the specified cause and message.\" '> 'n" "'> (jde-javadoc-insert-empty-line)" "\"* @param message the detail message string.\" '> 'n" "\"* @param cause the causing throwable. A null value is permitted\" '> 'n" "\"*     and indicates that the cause is nonexistent or unknown.\" '> 'n" "'> (jde-javadoc-insert-end-block)" "(jde-gen-method-signature \"public\" nil (jde-parse-get-buffer-unqualified-class) \"String message,Throwable cause\")" "(jde-gen-electric-brace)" "\"super(message);\" '> 'n" "\"initCause(cause);\" '> 'n" "\"}\" '> 'n" "\"}\" '>" "(if jde-gen-comments (concat \" // \" (jde-parse-get-buffer-unqualified-class)))" "'>'n") "\
*Template for a new exception class.
Setting this variable defines a template instantiation
command `jde-gen-exception', as a side-effect.")

(custom-autoload (quote jde-gen-exception-buffer-template) "jde-gen" nil)

(autoload (quote jde-gen-exception-buffer) "jde-gen" "\
Create a new Java buffer containing an exception class of the same name.
This command inserts the template generated by `jde-gen-exception'.
It then moves the point to the location of the first method.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads (jde-help-browse-jdk-doc jde-help-beanshell) "jde-help"
;;;;;;  "jde-help.el" (19289 5860))
;;; Generated autoloads from jde-help.el

(autoload (quote jde-help-beanshell) "jde-help" "\
Displays the BeanShell documentation.

\(fn)" t nil)

(autoload (quote jde-help-browse-jdk-doc) "jde-help" "\
Displays the JDK doc in a web browser. This function uses the URL
stored in the variable jde-jdk-doc-url to locate the JDK documentation.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-import-at-point jde-import-organize) "jde-import"
;;;;;;  "jde-import.el" (19289 5860))
;;; Generated autoloads from jde-import.el

(autoload (quote jde-import-organize) "jde-import" "\
Organize import statements of the current Java source buffer.
If optional FORCE is non-nil force reordering even if imports are
already organized.

Imports are organized into groups returned by the function specified
by `jde-import-group-function'.  Groups are inserted in the order they
are found unless `jde-import-sorted-groups' requires that they must be
alphabetically sorted.  In each group imports are sorted by name
alphabetically or in reverse order if `jde-import-reverse-sort-group'
is non-nil.  A blank line is inserted between groups.

Usage:
  \\[jde-import-organize] group and sort import statements.
  \\[universal-argument] \\[jde-import-organize] to force reordering.

The current buffer must be in `jde-mode'.  This command requires a
version of the JDE with the semantic parser.

\(fn &optional FORCE)" t nil)

(autoload (quote jde-import-at-point) "jde-import" "\
Import a class at the current point.
The fully qualified class is received from user input.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads (jde-java-font-lock-setup-keywords) "jde-java-font-lock"
;;;;;;  "jde-java-font-lock.el" (19289 5860))
;;; Generated autoloads from jde-java-font-lock.el

(autoload (quote jde-java-font-lock-setup-keywords) "jde-java-font-lock" "\
Setup font lock keywords in `java-font-lock-keywords-4'.
If optional REBUILD flag is non-nil create a new cache of regular
expressions.

\(fn &optional REBUILD)" t nil)

;;;***

;;;### (autoloads (jde-java-properties-validate jde-java-properties-mode)
;;;;;;  "jde-java-properties" "jde-java-properties.el" (19289 5860))
;;; Generated autoloads from jde-java-properties.el

(autoload (quote jde-java-properties-mode) "jde-java-properties" "\
Major mode for Java properties files buffer.

\(fn)" t nil)

(autoload (quote jde-java-properties-validate) "jde-java-properties" "\
Determine whether or not the properties file conforms to the Sun
java.util.Properties format.  Only the first invalid line is found.

GOTO indicates whether or not to put the point at the first invalid line
found.

\(fn &optional GOTO)" t nil)

;;;***

;;;### (autoloads (jde-javadoc-make-buffer jde-javadoc-make jde-javadoc-make-internal)
;;;;;;  "jde-javadoc-gen" "jde-javadoc-gen.el" (19289 5860))
;;; Generated autoloads from jde-javadoc-gen.el

(autoload (quote jde-javadoc-make-internal) "jde-javadoc-gen" "\
Generates javadoc for the current project if MAKE-PACKAGES-P
and `jde-javadoc-gen-packages' are nonnil; otherwise, make doc
for the current buffer. This command runs the
currently selected javadoc's program to generate the documentation.
It uses `jde-get-jdk-dir' to determine the location of
the currentlyh selected JDK. The variable `jde-global-classpath' specifies
the javadoc -classpath argument. The variable `jde-sourcepath'
specifies the javadoc  -sourcepath argument. You can specify all
other javadoc options via JDE customization variables. To specify the
options, select Project->Options->Javadoc from the JDE menu. Use
`jde-javadoc-gen-packages' to specify the packages, classes, or source
files for which you want to generate javadoc. If this variable is nil,
this command generates javadoc for the Java source file in the current
buffer. If `jde-javadoc-display-doc' is nonnil, this command displays
the generated documentation in a browser.

\(fn &optional MAKE-PACKAGES-P)" nil nil)

(autoload (quote jde-javadoc-make) "jde-javadoc-gen" "\
Generates javadoc for the current project. This command runs the
currently selected JDK's javadoc program to generate the documentation.
It uses `jde-get-jdk-dir' to determine the location of the currently
selected JDK. The variable `jde-global-classpath' specifies the javadoc
-classpath argument. The variable `jde-sourcepath'
specifies the javadoc  -sourcepath argument. You can specify all
other javadoc options via JDE customization variables. To specify the
options, select Project->Options->Javadoc from the JDE menu. Use
`jde-javadoc-gen-packages' to specify the packages, classes, or source
files for which you want to generate javadoc. If this variable is nil,
this command generates javadoc for the Java source file in the current
buffer. If `jde-javadoc-display-doc' is nonnil, this command displays
the generated documentation in a browser.

\(fn)" t nil)

(autoload (quote jde-javadoc-make-buffer) "jde-javadoc-gen" "\
Generates javadoc for the current buffer. This command runs the
currently selected JDK's javadoc program to generate the
documentation. It uses `jde-get-jdk-dir' to determine the location of the currently
selected JDK.  The variable `jde-global-classpath' specifies the
javadoc -classpath argument. The variable `jde-sourcepath' specifies
the javadoc -sourcepath argument. You can specify all other javadoc
options via JDE customization variables. To specify the options,
select Project->Options->Javadoc from the JDE menu. Use
`jde-javadoc-make' to generate doc for the files and packages
specified by `jde-javadoc-gen-packages'. If `jde-javadoc-display-doc'
is nonnil, this command displays the generated documentation in a
browser.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-javadoc-enable-menu-p jde-javadoc-remove jde-javadoc-checkdoc
;;;;;;  jde-javadoc-checkdoc-at-line jde-javadoc-remdoc-at-line jde-javadoc-autodoc-at-line
;;;;;;  jde-javadoc-customize jde-javadoc-checker-quit jde-javadoc-checker-fix
;;;;;;  jde-javadoc-checker-next jde-javadoc-checker-previous) "jde-javadoc"
;;;;;;  "jde-javadoc.el" (19289 5860))
;;; Generated autoloads from jde-javadoc.el

(autoload (quote jde-javadoc-checker-previous) "jde-javadoc" "\
Go to the previous tag with doc errors.

\(fn)" t nil)

(autoload (quote jde-javadoc-checker-next) "jde-javadoc" "\
Goto the next tag with doc errors.

\(fn)" t nil)

(autoload (quote jde-javadoc-checker-fix) "jde-javadoc" "\
Fix documentation of checked tag.
Used in `jde-javadoc-checker-report-mode'.

\(fn)" t nil)

(autoload (quote jde-javadoc-checker-quit) "jde-javadoc" "\
Quit the `jde-javadoc-checker' report buffer.
Used in `jde-javadoc-checker-report-mode'.

\(fn)" t nil)

(autoload (quote jde-javadoc-customize) "jde-javadoc" "\
Show the jde-javadoc options panel.

\(fn)" t nil)

(autoload (quote jde-javadoc-autodoc-at-line) "jde-javadoc" "\
Update javadoc comment block for declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jde-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN.

In the following examples, point is located at the beginning of the
line, before the word 'public' (but it could be anywhere on this
line):

1- Class example:
   -------------

-|-  public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe class <code>MyClass</code> here.
+     *
+     * @author \"David Ponce\" <david.ponce@wanadoo.fr>
+     * @version 1.0
+     * @since 1.0
+     * @see MySuperClass
+     * @see Runnable
+     * @see java.io.Serializable
+     */
     public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

2- Method example:
   --------------

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe <code>myMethod</code> method here.
+     *
+     * @param x an <code>int</code> value
+     * @param y a <code>long</code> value
+     * @exception Exception if an error occurs
+     */
     public
     void   myMethod( int  x,  long y )
       throws Exception
     {
       ...

3- Field example:
   --------------

-|-  private static final int SIZE = 10;

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe constant <code>SIZE</code> here.
+     */
     private static final int SIZE = 10;


`tempo' templates are used for each category of javadoc line.  The
following templates are currently defined and fully customizable (see
`tempo-define-template' for the different items that can be used in a
tempo template):

- - `jde-javadoc-author-tag-template'
- - `jde-javadoc-describe-class-template'
- - `jde-javadoc-describe-constructor-template'
- - `jde-javadoc-describe-interface-template'
- - `jde-javadoc-describe-method-template'
- - `jde-javadoc-describe-field-template'
- - `jde-javadoc-exception-tag-template'
- - `jde-javadoc-param-tag-template'
- - `jde-javadoc-return-tag-template'
- - `jde-javadoc-version-tag-template'

For example if you customize `jde-javadoc-describe-class-template'
with the following value:

'(\"* \" (P \"Class description: \"))

you will be asked to enter the class description in the minibuffer.
See also the `jde-javadoc-field-type', `jde-javadoc-a' and
`jde-javadoc-code' helper functions.

\(fn)" t nil)

(autoload (quote jde-javadoc-remdoc-at-line) "jde-javadoc" "\
Remove javadoc comment block for declaration at current line.
Require confirmation if optional NOCONFIRM is non-nil.
Return non-nil if done.
This uses `jde-javadoc-nonterminal-at-line' to find declarations.

\(fn &optional NOCONFIRM)" t nil)

(autoload (quote jde-javadoc-checkdoc-at-line) "jde-javadoc" "\
Check javadoc comment block of declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jde-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN.

\(fn)" t nil)

(autoload (quote jde-javadoc-checkdoc) "jde-javadoc" "\
Check doc comments of tags in the current buffer.
Report the next tag with documentation errors.

\(fn)" t nil)

(autoload (quote jde-javadoc-remove) "jde-javadoc" "\
Remove all Javadoc from the region (if one is active) or the buffer.
START, the start position in the buffer.
END, the end position in the buffer.

\(fn START END)" t nil)

(autoload (quote jde-javadoc-enable-menu-p) "jde-javadoc" "\
Return non-nil if corresponding doc menu item is enabled.
That is point is on the first line of a class, method, or field
definition.

\(fn)" nil nil)

;;;***

;;;### (autoloads (jde-junit-show-options jde-junit-run jde-junit-add-test-to-suite
;;;;;;  jde-junit4-test-class-buffer jde-junit-test-class-buffer
;;;;;;  jde-junit4-test-class jde-junit-test-class) "jde-junit" "jde-junit.el"
;;;;;;  (19289 5860))
;;; Generated autoloads from jde-junit.el

(autoload (quote jde-junit-test-class) "jde-junit" "\
Instantiate a test class template.

\(fn)" t nil)

(autoload (quote jde-junit4-test-class) "jde-junit" "\
Instantiate a test class template.

\(fn)" t nil)

(autoload (quote jde-junit-test-class-buffer) "jde-junit" "\
Create a buffer containing a skeleton unit test class having the same name as the
root name of the buffer. This command prompts you to enter the file name
of the test class. It assumes that the file name has the form CLASSTest.java
where CLASS is the name of the class to be tested, e.g., MyAppTest.java. Use
`jde-gen-junit-add-test-to-suite' to add tests to the test suite. Use of
tests generated with this template requires the JUnit test framework. For
more information, see http://www.junit.org.

\(fn)" t nil)

(autoload (quote jde-junit4-test-class-buffer) "jde-junit" "\
Create a buffer containing a skeleton unit test class having
the same name as the root name of the buffer. This command
prompts you to enter the file name of the test class. It assumes
that the file name has the form CLASSTest.java where CLASS is the
name of the class to be tested, e.g., MyAppTest.java. Use of
tests generated with this template requires the JUnit test
framework. For more information, see http://www.junit.org.

\(fn)" t nil)

(autoload (quote jde-junit-add-test-to-suite) "jde-junit" "\
Instantiate an addTest method.

\(fn)" t nil)

(autoload (quote jde-junit-run) "jde-junit" "\
Starts junit testrunner with buffer corresponding class name.

\(fn)" t nil)

(autoload (quote jde-junit-show-options) "jde-junit" "\
Show the JDE JUnit Options panel.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-make-show-options jde-make) "jde-make" "jde-make.el"
;;;;;;  (19289 5860))
;;; Generated autoloads from jde-make.el

(autoload (quote jde-make) "jde-make" "\
Run the make program specified by `jde-make-program' with the
command-line arguments specified by `jde-make-args'. If
`jde-read-make-args' is nonnil, this command also prompts you to enter
make arguments in the minibuffer and passes any arguments that you
enter to the make program along with the arguments specified by
`jde-make-args'.

\(fn)" t nil)

(autoload (quote jde-make-show-options) "jde-make" "\
Show the JDE Make Options panel.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-find-class-source jde-read-class) "jde-open-source"
;;;;;;  "jde-open-source.el" (19289 5860))
;;; Generated autoloads from jde-open-source.el

(autoload (quote jde-read-class) "jde-open-source" "\
Select a class interactively.  PROMPT is used to prompt the user for the
first class name, FQ-PROMPT is used only if the class name expands into more
than one fully qualified name.

PROMPT text used to prompt the user for the simple class name, or
\"Class\" as the default.  Don't add the colon/space at the end
of this prompt as a default will be added if it exists.

FQ-PROMPT text used to prompt the fully qualified class name, or
\"Select qualified class\" as the default.  Don't add the
colon/space at the end of this prompt as a default will be added
if it exists.

THIS-CLASS-P, if non-nil, use the current class name if no class name at point
and we are in a JDEE buffer.

CONFIRM-FQ-P, if non-nil, confirm the class name even when there
is only one unique fully qualified class found for the simple
class name (that is the class without the package part in the
name).

NO-CONFIRM-NFQ-P, if non-nil, don't confirm the class to check for fully
qualified classes if it is obtainable from either the point or this class (see
THIS-CLASS-P).  If obtained from the point, then the class name is parsed with
`jde-parse-class-name' for the input.

VALIDATE-FN is a function to validate the class.  This function takes an
argument the output from `jde-parse-class-name' given from the first user input
class name query.  The function should raise an error if there is anything
wrong with the class.  If this is `nil', then no validate is done.

When called interactively, select the class and copy it to the kill ring.

\(fn &optional PROMPT FQ-PROMPT THIS-CLASS-P CONFIRM-FQ-P NO-CONFIRM-NFQ-P VALIDATE-FN)" t nil)

(autoload (quote jde-find-class-source) "jde-open-source" "\
*Find the source file for a specified fully qualified class.
Calls `jde-find-class-source-file' to do the search.
If it finds the source file, it opens the file in a buffer.

\(fn CLASS &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads (jde-package-update) "jde-package" "jde-package.el"
;;;;;;  (19289 5860))
;;; Generated autoloads from jde-package.el

(autoload (quote jde-package-update) "jde-package" "\
Create or update the package statement in the current Java source
file buffer based on the file's location relative to the root of
the package directory as specified by one of the entries in
`jde-package-search-classpath-variables' or `jde-sourcepath'.
If these variables do not specify the root of the package directory,
this command does nothing. This command signals an error if the
 major mode of the current buffer is not `jde-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-member-to-attribute jde-camel-to-lisp jde-camel-to-c-const
;;;;;;  jde-camel-to-sql jde-hungarian-to-reverse-camel-notation)
;;;;;;  "jde-parse-expr" "jde-parse-expr.el" (19289 5860))
;;; Generated autoloads from jde-parse-expr.el

(autoload (quote jde-hungarian-to-reverse-camel-notation) "jde-parse-expr" "\
Converts Hungarian (i.e. m_sMyApp) to reverse camel notation (i.e. myApp).
Hungarian notation is used, for example, in most Microsoft visual C++ code
whereas reverse camel notation is used as the Sun Java standard style.

START the start of the buffer region
END is the end of the buffer region
LOCAL-REPLACEMENT-P, if non-nil, replace expecting a local variable
replacement.  This adds a `this.' to each replacment.

\(fn START END LOCAL-REPLACEMENT-P)" t nil)

(autoload (quote jde-camel-to-sql) "jde-parse-expr" "\
Convert from reverse camel notation to the SQL underscore convention.

\(fn START END)" t nil)

(autoload (quote jde-camel-to-c-const) "jde-parse-expr" "\
Convert from reverse camel notation to the C constant convention.

\(fn START END)" t nil)

(autoload (quote jde-camel-to-lisp) "jde-parse-expr" "\
Convert from reverse camel notation to the lisp symbol convention.

\(fn START END)" t nil)

(autoload (quote jde-member-to-attribute) "jde-parse-expr" "\
Convert Java member to a getter or setter syntax.

MEMBER-NAME is member to convret.  This is taken as the current word at point
when called interactively.

GETTERP, if non-nil, make it a getter, otherwise make it a setter.  If
\\[universal-argument] is used while calling interactively, then make it a
setter, otherwise, make a getter.

\(fn MEMBER-NAME GETTERP)" t nil)

;;;***

;;;### (autoloads (jde-parse-get-buffer-class) "jde-parse" "jde-parse.el"
;;;;;;  (19289 5860))
;;; Generated autoloads from jde-parse.el

(autoload (quote jde-parse-get-buffer-class) "jde-parse" "\
Get the fully qualified name of the class of this buffer.

NO-PACKAGE-P, if non-`nil', return only the class name (sans
package name), otherwise, include the package name.

If called interactively, add the name in the mini-buffer.

\(fn &optional NO-PACKAGE-P)" t nil)

;;;***

;;;### (autoloads (jde-create-new-project jde-save-project jde-open-project-file)
;;;;;;  "jde-project-file" "jde-project-file.el" (19289 5860))
;;; Generated autoloads from jde-project-file.el

(autoload (quote jde-open-project-file) "jde-project-file" "\
Opens the project file for the Java source file in the
current buffer.

\(fn)" t nil)

(autoload (quote jde-save-project) "jde-project-file" "\
Saves source file buffer options in one or more project files.
This command provides an easy way to create and update a project file
for a Java project. Simply open a source file, set the desired
options, using the JDE Options menu, then save the settings in the
project file, using this command.  Now, whenever you open a source
file from the same directory tree, the saved settings will be restored
for that file.

\(fn)" t nil)

(autoload (quote jde-create-new-project) "jde-project-file" "\
Creates a new JDE project file in directory NEW-DIR, saving any
current customized variables.  If a project file already exists in the
given directory, the project is simply re-saved.  This functions the
same as `jde-save-project' when no project files can be found for the
current source file.  But, if there already exist projects somewhere
along the path, this command unconditionally creates a project file in
the directory specified, thus allowing the user to create and maintain
hierarchical projects.

\(fn NEW-DIR)" t nil)

;;;***

;;;### (autoloads (jde-describe-path jde-project-create-project)
;;;;;;  "jde-project" "jde-project.el" (19289 5860))
;;; Generated autoloads from jde-project.el

(autoload (quote jde-project-create-project) "jde-project" "\
Creates a JDE project.

\(fn)" t nil)

(autoload (quote jde-describe-path) "jde-project" "\
Prints and gives file existance for each path.
PATH-TYPE is either `global classpath' for `jde-global-classpath' or
`source path' for `jde-sourcepath'.

\(fn PATH-TYPE)" t nil)

;;;***

;;;### (autoloads (jde-replace-fully-qualified-class-at-point jde-rename-class)
;;;;;;  "jde-refactor" "jde-refactor.el" (19289 5860))
;;; Generated autoloads from jde-refactor.el

(autoload (quote jde-rename-class) "jde-refactor" "\
Rename the current class to another class name.  This does a class name
string replace, changes the buffer name, and changes the file name.

\(fn NEW-CLASS-NAME)" t nil)

(autoload (quote jde-replace-fully-qualified-class-at-point) "jde-refactor" "\
Replace the unqualified class at the current point with the fully qualified
class.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads (jde-run-applet jde-run jde-run-set-applet-doc
;;;;;;  jde-run-set-applet-viewer jde-run-set-app-args jde-run-set-args
;;;;;;  jde-run-set-app) "jde-run" "jde-run.el" (19289 5860))
;;; Generated autoloads from jde-run.el

(autoload (quote jde-run-set-app) "jde-run" "\
Specify the name of the application class to run.

\(fn APP)" t nil)

(autoload (quote jde-run-set-args) "jde-run" "\
Specify arguments to be passed to the Java vm.
This command serves as an alternative to using the JDE Run Options
panel to specify command-line arguments for the Java interpreter.

\(fn ARGS)" t nil)

(autoload (quote jde-run-set-app-args) "jde-run" "\
Specify the arguments to be passed to the Java application class.
This command provides an alternative to using the JDE Run Options panel
to specify command-line arguments to pass to the application when starting
the application.

\(fn ARGS)" t nil)

(autoload (quote jde-run-set-applet-viewer) "jde-run" "\
Sets the viewer to be used to view an applet. The default is
appletviewer.

\(fn VIEWER)" t nil)

(autoload (quote jde-run-set-applet-doc) "jde-run" "\
Specify the doc to be used to view an applet.
This command provides an alternative to using the JDE Options
panel to specifying the applet document.

\(fn DOC)" t nil)

(autoload (quote jde-run) "jde-run" "\
Run the Java application specified by `jde-run-executable', if
not the null string. Otherwise run the class specified by
`jde-run-application-class', if non-null; otherwise the class in the
current buffer. Specifying a prefix argument, e.g., C-u C-c C-v C-r,
causes this command to prompt you to enter arguments to be passed to
the application's main method.  Specifying a minus prefix argument,
i.e., C-u - C-c C-v C-r, causes this command to prompt you to also
enter the name of the application's main class.  Note that you can use
`jde-run-read-app-args' to cause the command to prompt you for
application arguments by default (i.e., without having to specify a
prefix argument). This command creates a comint buffer to allow you to
interact with the program.

\(fn PREFIX)" t nil)

(autoload (quote jde-run-applet) "jde-run" "\
Runs an applet. This function prompts you to enter the path of an
html document that displays the applet. If you enter return without
specifying a document, this function next checks whether
`jde-run-applet-doc' specifies a document. If so, it displays that
specified document. Next, it checks whether the current directory
contains any html files. If the current directory contains an html
file with the same root name as the Java file in the current buffer,
it displays the file. If not, it displays the first html file that it
finds in the current directory. If if cannot find an html file, it
signals an error.  This function uses the viewer specified by
`jde-run-applet-viewer' to display the specified document. Note that
if you run two html applet files successively with the same name, you
must kill the buffer created to run the first file before running the
second file. Otherwise, this command will simply redisplay the first
file.

\(fn &optional DOC)" t nil)

;;;***

;;;### (autoloads (jde-stat-loc-report-directory jde-stat-loc-report-project
;;;;;;  jde-stat-loc-report) "jde-stat" "jde-stat.el" (19289 5860))
;;; Generated autoloads from jde-stat.el

(autoload (quote jde-stat-loc-report) "jde-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in the current Java source buffer. Optionally
a total count could be passed to be displayes, as well as the number of
processed files.

\(fn &optional COUNT &optional TOTAL-FILES)" t nil)

(autoload (quote jde-stat-loc-report-project) "jde-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory and subdirectories. This method will kill any
buffer containing a java file contained in dir.

\(fn DIR)" t nil)

(autoload (quote jde-stat-loc-report-directory) "jde-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory. This method will kill any buffer containing a java file
contained in dir.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (jde-htmlize-code jde-exception-goto jde-require)
;;;;;;  "jde-util" "jde-util.el" (19289 5860))
;;; Generated autoloads from jde-util.el

(autoload (quote jde-require) "jde-util" "\
Require FEATURE, either pre-installed or from the distribution.
 That is, first try to load the FEATURE library. Then try to load the
 jde-FEATURE library from the JDEE's distribution.
 Signal an error if FEATURE can't be found.

\(fn FEATURE)" nil nil)

(autoload (quote jde-exception-goto) "jde-util" "\
Go to the Java source file and line indicated by an exception stack trace.

\(fn)" t nil)

(autoload (quote jde-htmlize-code) "jde-util" "\
Write the current code region as an HTML document.
Line numbers are added as well.

See `jde-htmlize-code-destinations'.

\(fn START END &optional NO-LINE-NUMBERS-P)" t nil)

;;;***

;;;### (autoloads (jde-which-method-mode) "jde-which-method" "jde-which-method.el"
;;;;;;  (19289 5860))
;;; Generated autoloads from jde-which-method.el

(defvar jde-which-method-mode t "\
Enables the JDE's which method mode.
When which method mode is enabled, the current method name is
displayed in the mode line.")

(custom-autoload (quote jde-which-method-mode) "jde-which-method" t)

;;;***

;;;### (autoloads (jde-xref-customize jde-xref-update jde-xref-list-uncalled-functions
;;;;;;  jde-xref-display-call-tree jde-xref-next-caller jde-xref-first-caller
;;;;;;  jde-xref-make-xref-db) "jde-xref" "jde-xref.el" (19289 5860))
;;; Generated autoloads from jde-xref.el

(autoload (quote jde-xref-make-xref-db) "jde-xref" "\
Create a database of caller to callee (and the reverse) from the
classes in `jde-built-class-path' and store the data in the location
specified by `jde-xref-db-file'

\(fn)" t nil)

(autoload (quote jde-xref-first-caller) "jde-xref" "\
Put the list of who calls the current function on the stack and
display the first caller.  Subsequent callers are displayed through
`jde-xref-show-next-caller'.  STRICT should be true if the callers of
interfaces to a function, or calls to a superclass which may result in
a virtual function call to the subclass should not be considered.  In
other words, if STRICT is true, then only calls that are definitely to
the requested function are considered.

\(fn STRICT)" t nil)

(autoload (quote jde-xref-next-caller) "jde-xref" "\
If there are items still on the caller stack, pop the first one off
and show it

\(fn)" t nil)

(autoload (quote jde-xref-display-call-tree) "jde-xref" "\
Display an interactive call tree of which function call the current
  function, which can be expanded outward.  STRICT should be true if
  the callers of interfaces to a function, or calls to a superclass
  which may result in a virtual function call to the subclass should
  not be considered.  In other words, if STRICT is true, then only
  calls that are definitely to the requested function are considered. 

\(fn STRICT)" t nil)

(autoload (quote jde-xref-list-uncalled-functions) "jde-xref" "\
Displays a simple list of function that are never called, at least
not directly.  Do not assume that this means this code can never be
reached, since reflection could always call any method.  Use this list
and your best judgement to figure out what are good candidates for
code cleanup.  STRICT should be true if the callers of interfaces to a
function, or calls to a superclass which may result in a virtual
function call to the subclass should not be considered.  In other
words, if STRICT is true, then only calls that are definitely to the
requested function are considered.  This function could take a
while. If it does, you might want to consider increasing
`jde-xref-cache-size'.

\(fn STRICT)" t nil)

(autoload (quote jde-xref-update) "jde-xref" "\
Update the caller table after a recompile.  This can be called by
the user when they recompile outside of emacs.  It will update the
call list of all files modified in emacs

\(fn &rest IGNORED)" t nil)

(autoload (quote jde-xref-customize) "jde-xref" "\
Display the customization buffer for the xref package.

\(fn)" t nil)

;;;***

;;;### (autoloads (jde-compile-jde jde-show-help jde-mode jde-build
;;;;;;  jde-set-global-classpath jde-version) "jde" "jde.el" (19289
;;;;;;  5861))
;;; Generated autoloads from jde.el

(defconst jde-version "2.4.0.1" "\
JDE version number.")

(autoload (quote jde-version) "jde" "\
Get the version of JDEE.

\(fn)" t nil)

(autoload (quote jde-set-global-classpath) "jde" "\
Set the value of `jde-global-classpath'.
It specifies the -classpath argument for the Java compiler and
interpreter.

\(fn CLASSPATH)" t nil)

(autoload (quote jde-build) "jde" "\
Rebuild the entire project.
This command invokes the function defined by `jde-build-function'.

\(fn)" t nil)

(autoload (quote jde-mode) "jde" "\
Major mode for developing Java applications and applets.
\\{jde-mode-map}

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.java\\'" . jde-mode)))

(autoload (quote jde-show-help) "jde" "\
Displays the JDE User's Guide in a browser.

\(fn)" t nil)

(autoload (quote jde-compile-jde) "jde" "\
Byte-compile all uncompiled files of jde.

\(fn)" t nil)

;;;***

(provide 'jde-autoload)
