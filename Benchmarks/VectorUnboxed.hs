-- |
-- Module      : Benchmarks.Vector
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Benchmarks.VectorUnboxed where

#define VECTOR_UNBOXED
#include "VectorCommon.hs"
