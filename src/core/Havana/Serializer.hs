module Havana.Serializer where

serializeToFile :: a -> FilePath -> IO ()
serializeToFile ast outputPath = writeFile outputPath "Nope.\n"
