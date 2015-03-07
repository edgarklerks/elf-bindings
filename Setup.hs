import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.Program.Run
main :: IO ()
main = print "wello" >> defaultMainWithHooks (simpleUserHooks {
                                                 preConf = \a b -> runSeq >> preConf simpleUserHooks a b
                                                                                                                                                         })

runSeq :: IO ()
runSeq = do
  runProgramInvocation normal $ simpleProgramInvocation "/usr/bin/autoconf" []
  runProgramInvocation normal $ simpleProgramInvocation "./configure" []
