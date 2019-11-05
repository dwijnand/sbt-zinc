/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbti;

import xsbti.api.DependencyContext;

import java.io.File;
import java.nio.file.Path;
import java.util.EnumSet;
import java.util.Map;
import java.util.Optional;

public interface AnalysisCallback {
    /**
     * Set the source file mapped to a concrete {@link AnalysisCallback}.
     * @param source Source file mapped to this instance of {@link AnalysisCallback}.
     */
    void startSource(File source);

    /**
     * Indicate that the class <code>sourceClassName</code> depends on the
     * class <code>onClassName</code>.
     *
     * Note that only classes defined in source files included in the current
     * compilation will passed to this method. Dependencies on classes generated
     * by sources not in the current compilation will be passed as binary
     * dependencies to the `binaryDependency` method.
     *
     * @param onClassName Class name being depended on.
     * @param sourceClassName Dependent class name.
     * @param context The kind of dependency established between
     *                <code>onClassName</code> and <code>sourceClassName</code>.
     *
     * @see xsbti.api.DependencyContext
     */
    void classDependency(String onClassName,
                         String sourceClassName,
                         DependencyContext context);

    /**
     * Indicate that the class <code>fromClassName</code> depends on a class
     * named <code>onBinaryClassName</code> coming from class file or jar
     * <code>onBinaryEntry</code>.
     *
     * @param onBinaryEntry A binary entry represents either the jar or the
     *                      concrete class file from which the Scala compiler
     *                      knows that <code>onBinaryClassName</code> comes from.
     * @param onBinaryClassName Dependent binary name.
     *                 Binary name with JVM-like representation. Inner classes
     *                 are represented with '$'. For more information on the
     *                 binary name format, check section 13.1 of the Java
     *                 Language Specification.
     * @param fromClassName Represent the class file name where
     *                 <code>onBinaryClassName</code> is defined.
     *                 Binary name with JVM-like representation. Inner classes
     *                 are represented with '$'. For more information on the
     *                 binary name format, check section 13.1 of the Java
     *                 Language Specification.
     * @param fromSourceFile Source file where <code>onBinaryClassName</code>
     *                       is defined.
     * @param context The kind of dependency established between
     *                <code>onClassName</code> and <code>sourceClassName</code>.
     *
     * @see xsbti.api.DependencyContext for more information on the context.
     */
    void binaryDependency(File onBinaryEntry,
                          String onBinaryClassName,
                          String fromClassName,
                          File fromSourceFile,
                          DependencyContext context);

    /**
     * Map the source class name (<code>srcClassName</code>) of a top-level
     * Scala class coming from a given source file to a binary class name
     * (<code>binaryClassName</code>) coming from a given class file.
     *
     * This relation indicates that <code>classFile</code> is the product of
     * compilation from <code>source</code>.
     *
     * @param source File where <code>srcClassName</code> is defined.
     * @param classFile File where <code>binaryClassName</code> is defined. This
     *                  class file is the product of <code>source</code>.
     * @param binaryClassName Binary name with JVM-like representation. Inner
     *                        classes are represented with '$'. For more
     *                        information on the binary name format, check
     *                        section 13.1 of the Java Language Specification.
     * @param srcClassName Class name as defined in <code>source</code>.
     */
    void generatedNonLocalClass(File source,
                                File classFile,
                                String binaryClassName,
                                String srcClassName);

    /**
     * Map the product relation between <code>classFile</code> and
     * <code>source</code> to indicate that <code>classFile</code> is the
     * product of compilation from <code>source</code>.
     *
     * @param source File that produced <code>classFile</code>.
     * @param classFile File product of compilation of <code>source</code>.
     */
    void generatedLocalClass(File source, File classFile);

    /**
     * Register a public API entry coming from a given source file.
     *
     * @param sourceFile Source file where <code>classApi</code> comes from.
     * @param classApi The extracted public class API.
     */
    void api(File sourceFile, xsbti.api.ClassLike classApi);

    /**
     * Is this source file in the current compilation (even if not in the current run)?
     * @param file
     * @return
     */
    boolean inCompilation(File file);

    /**
     * Register a class containing an entry point coming from a given source file.
     *
     * A class is an entry point if its bytecode contains a method with the
     * following signature:
     * <pre>
     * public static void main(String[] args);
     * </pre>
     *
     * @param sourceFile Source file where <code>className</code> is defined.
     * @param className A class containing an entry point.
     */
    void mainClass(File sourceFile, String className);

    /**
     * Register the use of a <code>name</code> from a given source class name.
     *
     * @param className The source class name that uses <code>name</code>.
     * @param name The source name used in <code>className</code>.
     * @param useScopes Scopes(e.g. patmat, implicit) where name is used <code>className</code>.
     */
    void usedName(String className, String name, EnumSet<UseScope> useScopes);

    /**
     * Register a compilation problem.
     *
     * This error may have already been logged or not. Unreported problems may
     * happen because the reporting option was not enabled via command-line.
     *
     * @param what The headline of the error.
     * @param pos At a given source position.
     * @param msg The in-depth description of the error.
     * @param severity The severity of the error reported.
     * @param reported Flag that confirms whether this error was reported or not.
     */
    void problem(String what,
                 Position pos,
                 String msg,
                 Severity severity,
                 boolean reported);

    /**
     * Communicate to the callback that the dependency phase has finished.
     *
     * For instance, you can use this method it to wait on asynchronous tasks.
     */
    void dependencyPhaseCompleted();

    /**
     * Communicate to the callback that the API phase has finished.
     * For instance, you can use this method it to wait on asynchronous tasks.
     */
    void apiPhaseCompleted();

    // Essentially s.r.i.PickleBuffer, but without referring to scala classes.
    public static class PickleData {
        Object _orig;
        String _fqcn;
        byte[] _bytes;
        int _writeIndex;
        Path _path;
        public PickleData(Object orig, String fqcn, byte[] bytes, int writeIndex, Path path) {
            this._orig = orig;
            this._fqcn = fqcn;
            this._bytes = bytes;
            this._writeIndex = writeIndex;
            this._path = path;
        }
        public String fqcn() {return _fqcn;}
        public Object orig() {return _orig;}
        public byte[] bytes() { return _bytes;}
        public int writeIndex() { return _writeIndex;}
        public Path path() { return _path; }
    }

    /**
     * Pass new pickle data and write analysis as of this point.
     */
    void processPickleData(PickleData[] data);

                           /**
     * Return whether incremental compilation is enabled or not.
     *
     * This method is useful to know whether the incremental compilation
     * phase defined by <code>xsbt-analyzer</code> should be added.
     */
    boolean enabled();

    /**
     * Return class files in output jar at a given point in time.
     *
     * When straight-to-jar compilation is enabled, the following entrypoint
     * in the analysis callback tells the compiler which classes can be found
     * in the jar used as a compilation target (where all class files will be
     * store). The entrypoint will return all the paths to class files in Zinc
     * format, an example would be `xsbti/AnalysisCallback.class`.
     *
     * This entrypoint serves two main purposes:
     *
     * 1. Before the dependency phase is run, it returns the class files found
     *    in the jar previous to the current compilation.
     * 2. After dependency has run, when called again, it returns the class
     *    files written by the compiler in genbcode.
     *
     * The second purpose is required because the compiler cannot communicate
     * us via an internal programmatic API which files has written in genbcode
     * and therefore we need to pay the price of opening the jar again to figure
     * it out. If the compiler is to expose an entry point for this data, we
     * can repurpose `classesInOutputJar` to only do 1).
     */
    java.util.Set<String> classesInOutputJar();


    /**
     * Alternate mechanism for canceling compilation, since the actual s.t.n.reporters.Reporter
     * is not accessible to anyone who might want to cancel.
     */
    boolean isCanceled();

    /**
     * Invoked within ZincRun#advancePhase, typically for timing diagnostics.
     */
    void advancePhase(String prev, String next);

}
