-- |
-- Module      : Benchmarks.Vector
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.VectorUnboxed where

import Benchmarks.DefaultMain (defaultMain)
#define VECTOR_UNBOXED
#include "VectorCommon.hs"

main :: P.IO ()
main = $(defaultMain "VectorUnboxed")
