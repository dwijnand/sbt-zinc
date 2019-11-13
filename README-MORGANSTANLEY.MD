The PR containing this file comprises Morgan Stanley's modifications to zinc.
All files in the PR, including this one, are subject to the this disclaimer:

THIS SOFTWARE IS CONTRIBUTED SUBJECT TO THE TERMS OF THE ORGANIZATION CONTRIBUTION LICENSE AGREEMENT V2.0, DATED JULY 13, 2012.

THIS SOFTWARE IS LICENSED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE AND ANY WARRANTY OF NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. THIS SOFTWARE MAY BE REDISTRIBUTED TO OTHERS ONLY BY EFFECTIVELY USING THIS OR ANOTHER EQUIVALENT DISCLAIMER IN ADDITION TO ANY OTHER REQUIRED LICENSE TERMS.

Within those constraints, the contents of this PR may be used, modified or adapted freely, and note that some modfication will certainly be
necessary, as we have been working with a somewhat dated branch of zinc and have not tested (or even built) a recent `dewvelop`
branch with our changes.


Background
===

The aspects of the Morgan Stanley build tool that are relevant to zinc are as follows:

1. We perform a pipelined build, starting a compilation as soon as the pickled type signatures of upstream
   dependencies are ready.  We compile java code separately and use zinc and scalac only to generate pickled signatures.
2. Our builds are referentially transparent, in that all artifacts are either in canonical and immutable locations and identified by
   a distinct version id, or have names containing a hash of all relevant sources and dependencies.
3. Irrespective of its actual storage location, source code is held in memory as strings.

Changes related to pipelining
===

For pickled signatures to be useful to downstream incremental compilations, they must be accompanied by some form
of "early analysis" that does not contain all the information necessary to make invalidation decisions for the module
itself, but does contain everything that downstream modules might need.  For the most part, this comes down to
publishing analysis as accrued through the `API` together with pickles.

The `storeEarlyAnalysis` method added to
`ExternalHooks` provides the direct means of storing the analysis, but there is one interesting complication (in
addition to some uninteresting ones): before publishing any early artifacts, we need to know that there are no further
source invalidations that will necessitate further `IncrementalCommon#cycle` recursions.  This is accomplished by
providing a `mergeAndInvalidate` callback that closes over analysis and invalidation information known within `IncrementalCommon`
making it available within `AnalysisCallback` (yes, it's a callback for the callback).  Note that, even though we have all the
information necessary for the early analysis as of `API`, we can't actually call `mergeAndInvalidate` to find out
whether to write it until after the `Dependency` phase.

The `API` and `Dependency` phases were also both modified to handle java compilation units - the strategy in
`s.i.i.classfile.Analyze` of reading completely compiled class files would obviously not be compatible with pipelining,
since information would only be available at the end of the cycle.  Note though that, because we don't use zinc to
generate java classes, we aren't just *able* to to compile
java sources through `Dependency`, we *must* compile them at least through `typer` - otherwise references to java
symbols won't be resolved at all.  Moreover, we must compile *all* java sources on *every* compilation cycle where there
are *any* invalidations, whether of java or scala sources.  This is not a generally optimal strategy, but it works for
us, because our codebase is dominantly scala (and because java compiles quickly).  The correct approach will be to have
java pickled signatures on the classpath and manage them with a generalization of `ClassFileManager`.

Changes related to RT build
===
Zinc by default expects upstream dependency jars and directories to have the same names from build to build, with any
modifications to those dependencies reflected in their corresponding analysis files.  Since our intra-project artifact
names contain a unique hash of their own dependencies, we have to play some `ReadMapper`/`WriteMapper` so that the
artifacts read from analyses match those on the compilation classpath (and various scalac arguments).  These mappers
are defined in the standard fashion in our proprietary build tool code - they're conceptually important but mostly
tedious boilerplate.

Of greater interest is that RT artifacts allow some significant optimizations.  The new `provenance` field of
`AnalyzedClass` is essentially the hash that was embedded in the name of the jar where the class was found.  Now,
when we encounter a class, we can check if a jar with *exactly* that hash is on the current classpath.  If it is, we
know that it isn't even necessary to crack open the corresponding upstream analysis - we know the api hasn't
changed.  (We're willing to give up incremental monkey-patching.)
This logic is implemented in the `quickAPI` method, defined in the `extraHooks` and invoked from
`IncrementalCompile#getExternalApi`.  A skeletal implementation is shown in the comments near that call.  It relies
on an analysis cache and on some data extracted during read mapping.

Another benefit of RT builds is that we no longer need to use filesystem time stamp for anything, so we override
`ExternalHooks#hashClassPath` to compute `FileHash` directly from the file names.

Changes related to in-memory source
===
While it remains a noble goal, we didn't feel like ripping out `File` throughout the zinc codebase, so we allow
source files to be specified by essentially arbitrary `File` instances, and provide a `SourceSource` hook that
gets tunneled all the way down to `CachedCompiler0#run`, which uses it to create `VirtualFile`s for scalac.
We use the same hook to calculate source file hashes directly from their string contents.


Additional changes
===
1. Because we can have simultaneous compilations in one VM relying on common upstream `Analysis` instances, we hit the concurreny issue
   that the `SafeLazy` comments warned about.  By storing the `thunk` and the `result` as `final` fields of a contained class,
   we ensure that they will will always be in sync with each other when used, without actually locking anything.
2. We use an optimized `UsedName` that stores the set of use scopes as bits in a single `Int`.
3. We found it helpful to make some changes to the `InvalidationProfiler` (and in our own code, to further override it
   so that `RunProfiler`s are accrued rather than discarded.
4. Because the `scala.tools.nsc.reporters.Reporter` ultimately passed to `Global` is created fairly deep within the
   zinc machinery in `CompilerInterface`, we felt that the most expedient way to transmit cancellation requests
   through it would be to check a callback provided through `ExternalHooks`.
5. While we did make multiple modifications to `xsbti` interface classes, as changes piled up, we started adding
   new hooks via a single `Map<String, Object extraHooks()`, along with static `Hooks` methods to set and retrieve
   specific hooks and callbacks, without further polluting the public interface.  Much of the instrumentation we added
   for progress reporting and testing was added in this manner.