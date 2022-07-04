module Endpoint ( 
    endpoint ) where

import Modules.ModelManager.Yamler ( 
    catchConfig )

endpoint :: IO ()
endpoint = print 3030
