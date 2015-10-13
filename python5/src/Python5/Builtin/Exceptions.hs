{-
    Python5 — a hypothetic language
    Copyright (C) 2015 - Yuriy Syrovetskiy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Python5.Builtin.Exceptions where

import Control.Exception  ( Exception, catch, throwIO )
import Data.Typeable      ( Typeable )

data ValueError = forall a. Show a => ValueError a
    deriving Typeable

deriving instance Show ValueError

instance Exception ValueError

raise :: Exception e => e -> IO a
raise = throwIO

except :: Exception e => IO a -> (e -> IO a) -> IO a
except = catch

safe_except :: IO (Either e a) -> (e -> IO a) -> IO a
safe_except action handler = action >>= either handler return
