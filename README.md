## bitcoin
https://functional.christmas/2019/23

### TODO
- Explore more language options like `{-# LANGUAGE OverloadedStrings #-}`
- What are the differences between the followings
```
import Network.HTTP.Simple (httpBS, getResponseBody)
import qualified Data.ByteString.Char8 as BS
```
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