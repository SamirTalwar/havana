module Havana.Serializer where

serializeToFile :: a -> String -> IO ()
serializeToFile ast outputPath = writeFile outputPath "Nope.\n"
