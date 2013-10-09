set PORT_FILE=%1
set BOOTCLASSPATH="%~dp0\..\scala-library.jar;%~dp0\..\scala-reflect.jar;%~dp0\..\scala-compiler.jar;%~dp0\..\scala-actors.jar"
set CLASSPATH="%~dp0\..\lib\asm-3.3.jar;%~dp0\..\lib\asm-commons-3.3.jar;%~dp0\..\lib\asm-tree-3.3.jar;%~dp0\..\lib\asm-util-3.3.jar;%~dp0\..\lib\critbit-0.0.4.jar;%~dp0\..\lib\ensime_2.10-0.9.8.9.jar;%~dp0\..\lib\json-simple-1.1.jar;%~dp0\..\lib\lucene-core-3.5.0.jar;%~dp0\..\lib\org.eclipse.jdt.core-3.6.0.v_A58.jar;%~dp0\..\lib\org.scala-refactoring_2.10.0-SNAPSHOT-0.6.1-20130201.063851-55.jar;%~dp0\..\lib\scala-actors-2.10.0.jar;%~dp0\..\lib\scala-compiler.jar;%~dp0\..\lib\scala-library.jar;%~dp0\..\lib\scala-reflect-2.10.0.jar;%~dp0\..\lib\scalariform_2.10-0.1.4-SNAPSHOT.jar;%JAVA_HOME%\lib\tools.jar"
if not defined ENSIME_JVM_ARGS (set ENSIME_JVM_ARGS=-Xms256M -Xmx1512M -XX:PermSize=128m -Xss1M -Dfile.encoding=UTF-8)
java -Xbootclasspath/a:%BOOTCLASSPATH% -classpath %CLASSPATH% %ENSIME_JVM_ARGS% org.ensime.server.Server %PORT_FILE%
