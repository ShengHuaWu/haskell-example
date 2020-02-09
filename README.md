## bitcoin
https://functional.christmas/2019/23

### TODO
- Figure out `Data.Aeson.Lens`. (https://www.snoyman.com/blog/2017/05/playing-with-lens-aeson)
- Read more about `lenses`

### Stack
Stack is a build tool for Haskell and it has a strong focus on reproducible build plans, multi-package projects, and a consistent, easy-to-learn interface, while providing the customizability and power experienced developers need. As a build tool, Stack does not stand alone. It is built on the great work provided by:
1. The Glasgow Haskell Compiler (GHC), the premier Haskell compiler. Stack will manage your GHC installations and automatically select the appropriate compiler version for your project.
2. The Cabal build system, a specification for defining Haskell packages, together with a library for performing builds.
3. The Hackage package repository, providing more than ten thousand open source libraries and applications to help you get your work done.
4. The Stackage package collection, a curated set of packages from Hackage which are regularly tested for compatibility. Stack defaults to using Stackage package sets to avoid dependency problems.

The primary stack design point is reproducible builds. If you run stack build today, you should get the same result running stack build tomorrow. There are some cases that can break that rule (changes in your operating system configuration, for example), but, overall, stack follows this design philosophy closely. To make this a simple process, stack uses curated package sets called snapshots.

To build your project, stack uses a stack.yaml file in the root directory of your project as a sort of blueprint. That file contains a reference, called a resolver, to the snapshot which your package will be built against.

Finally, stack is isolated: it will not make changes outside of specific stack directories. stack-built files generally go in either the stack root directory (default ~/.stack or, on Windows, %LOCALAPPDATA%\Programs\stack) or ./.stack-work directories local to each project. The stack root directory holds packages belonging to snapshots and any stack-installed versions of GHC. Stack will not tamper with any system version of GHC or interfere with packages installed by cabal or any other build tools.

The `Setup.hs` file is a component of the Cabal build system which stack uses. It's technically not needed by stack, but it is still considered good practice in the Haskell world to include it.

Next, let's look at our `stack.yaml` file, which gives our project-level settings. `packages` tells stack which local packages to build. In our simple example, we have only a single package in our project, located in the same directory, so '.' suffices. However, stack has powerful support for multi-package projects. The other field is `resolver`. This tells stack how to build your package: which GHC version to use, versions of package dependencies, and so on.

Another file important to the build is `package.yaml`. Since Stack 1.6.1, the `package.yaml` is the preferred package format that is provided built-in by stack through the `hpack` tool. The default behaviour is to generate the `.cabal` file from this `package.yaml`, and accordingly you should not modify the `.cabal` file. It is also important to remember that stack is built on top of the Cabal build system. Therefore, an understanding of the moving parts in Cabal are necessary. In Cabal, we have individual packages, each of which contains a single `.cabal` file. The `.cabal` file can define 1 or more components: a library, executables, test suites, and benchmarks. It also specifies additional information such as library dependencies, default language pragmas, and so on.

#### Reference
- https://docs.haskellstack.org/en/stable/README/
- https://docs.haskellstack.org/en/stable/GUIDE/

### hpack
Hpack is a format for Haskell packages. It is a modern alternative to the Cabal package format and follows different design principles.

#### Reference
https://github.com/sol/hpack#readme

### Language Options
As with all known Haskell systems, GHC implements some extensions to the standard Haskell language. They can all be enabled or disabled by command line flags or language pragmas. By default GHC understands the most recent Haskell version it supports, plus a handful of extensions.

#### Reference
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html

### Imports
```Haskell
Import command	                    What is brought into scope	                    Notes
--
import Mod	                        x, y, z, (+++), Mod.x, Mod.y, Mod.z, (Mod.+++)	(By default, qualified and unqualified names.)
import Mod ()	                    (Nothing!)	                                    (Useful for only importing instances of typeclasses and nothing else)
import Mod (x,y, (+++))	            x, y, (+++), Mod.x, Mod.y, (Mod.+++)	        (Only x, y, and (+++), no z.)
import qualified Mod	            Mod.x, Mod.y, Mod.z, (Mod.+++)	                (Only qualified versions; no unqualified versions.)
import qualified Mod (x,y)	        Mod.x, Mod.y	                                (Only x and y, only qualified.)
import Mod hiding (x,y,(+++))	    z, Mod.z	                                    (x and y are hidden.)
import qualified Mod hiding (x,y)	Mod.z, (Mod.+++)	                            (x and y are hidden.)
import Mod as Foo	                x, y, z, (+++), Foo.x, Foo.y, Foo.z, (Foo.+++)	(Unqualified names as before. Qualified names use Foo instead of Mod.)
import Mod as Foo (x,y)	            x, y, Foo.x, Foo.y	                            (Only import x and y.)
import qualified Mod as Foo	        Foo.x, Foo.y, Foo.z, (Foo.+++)	                (Only qualified names, using new qualifier.)
import qualified Mod as Foo (x,y)	Foo.x, Foo.y	                                (Only qualified versions of x and y, using new qualifier)
```

### Reference
https://wiki.haskell.org/Import
