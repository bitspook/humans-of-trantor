{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib
  ( run
  ) where
import           Magicbane
import           RIO
import qualified Session   as S

type Api = S.Spi

api = Proxy :: Proxy Api

run = do
  ctx <- newBasicContext
  defWaiMain $ magicbaneApp api EmptyContext ctx $ S.server
