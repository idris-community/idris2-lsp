-- @generated
module Server.Version

import Server.Generated
import Libraries.Data.Version

export
version : Version
version with (idrisLSPVersion)
 version | (s,"") = MkVersion s Nothing
 version | (s,t) = MkVersion s (Just t)
