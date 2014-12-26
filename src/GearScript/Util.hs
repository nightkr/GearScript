module GearScript.Util where

import Control.Applicative
import Text.Parsec.Combinator
import Text.Parsec.Prim

tryChoice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
tryChoice choices = choice (try <$> choices)
