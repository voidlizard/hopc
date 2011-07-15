#!/usr/bin/env runhaskell
> import System.Cmd(system) 
> import System.Exit
> import System.Directory
> import Distribution.Simple
> import Distribution.Simple.Program
> makeBNFC args flags = do
>    putStrLn " *** Generate BNFC stuffs *** "
>    rootdir <- getCurrentDirectory 
>    setCurrentDirectory "./src/"
>    system "make -f Makefile.bnfc"
>    setCurrentDirectory rootdir 
>    (preBuild simpleUserHooks) args flags
>
> main = defaultMainWithHooks ( simpleUserHooks { hookedPrograms = [simpleProgram "make"], preBuild = makeBNFC } )

