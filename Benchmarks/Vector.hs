-- |
-- Module      : Benchmarks.Vector
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.Vector where

import Benchmarks.DefaultMain (defaultMain)
#define VECTOR_BOXED
#include "VectorCommon.hs"

main :: P.IO ()
main = $(defaultMain "Vector")
