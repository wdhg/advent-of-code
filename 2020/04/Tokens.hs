{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "Tokens.x" #-}

module Tokens where

import Passport
import Data.Char (isDigit)

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.





import Data.Word (Word8)
















import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type
















































































-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.














-- -----------------------------------------------------------------------------
-- Monad (default and with ByteString input)

























































































































































-- -----------------------------------------------------------------------------
-- Basic wrapper


type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode' c of
                             (b, bs) -> Just (b, (c, bs, s))



-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version
































-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.













-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version














-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.














alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 90)
  [ -8
  , -103
  , -110
  , -102
  , -90
  , -23
  , -99
  , 120
  , -62
  , 248
  , -24
  , -88
  , -45
  , 2
  , -94
  , 12
  , -43
  , 22
  , 57
  , 456
  , -100
  , -97
  , -96
  , -93
  , 466
  , -86
  , 476
  , -38
  , -35
  , -21
  , -20
  , 486
  , -19
  , 511
  , 531
  , 541
  , 566
  , 576
  , 0
  , 491
  , 699
  , 709
  , 719
  , 729
  , 739
  , 764
  , 0
  , 631
  , 839
  , -76
  , -79
  , -70
  , -28
  , 849
  , 859
  , 869
  , 879
  , 952
  , 888
  , 0
  , 1096
  , -59
  , -61
  , -64
  , -50
  , 1106
  , -29
  , 1116
  , 1126
  , 1136
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 537
  , -5
  , -37
  , -36
  , -16
  , -17
  , 0
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 1391)
  [ 0
  , 84
  , 84
  , 84
  , 84
  , 84
  , 73
  , 77
  , 80
  , 1
  , 64
  , 19
  , 12
  , 13
  , 16
  , 10
  , 27
  , 28
  , 29
  , 79
  , 34
  , 30
  , 32
  , 35
  , 84
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 4
  , 6
  , 52
  , 36
  , 37
  , 8
  , 61
  , 51
  , 50
  , 62
  , 3
  , 82
  , 78
  , 81
  , 76
  , 49
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 15
  , 17
  , 17
  , 17
  , 17
  , 17
  , 17
  , 17
  , 17
  , 17
  , 17
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 2
  , 74
  , 66
  , 14
  , 23
  , 22
  , 63
  , 20
  , 11
  , 0
  , 86
  , 0
  , 0
  , 85
  , 25
  , 0
  , 88
  , 87
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 89
  , 24
  , 24
  , 24
  , 24
  , 24
  , 24
  , 24
  , 24
  , 24
  , 24
  , 0
  , 21
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 57
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 58
  , 7
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 47
  , 9
  , 38
  , 38
  , 38
  , 39
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 57
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 59
  , 7
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 46
  , 26
  , 26
  , 26
  , 26
  , 26
  , 26
  , 26
  , 26
  , 26
  , 26
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 33
  , 33
  , 33
  , 33
  , 33
  , 33
  , 33
  , 33
  , 33
  , 33
  , 40
  , 40
  , 40
  , 40
  , 40
  , 40
  , 40
  , 40
  , 40
  , 40
  , 0
  , 0
  , 84
  , 84
  , 84
  , 84
  , 84
  , 0
  , 0
  , 26
  , 26
  , 26
  , 26
  , 26
  , 26
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 84
  , 0
  , 0
  , 0
  , 33
  , 33
  , 33
  , 33
  , 33
  , 33
  , 42
  , 42
  , 42
  , 42
  , 42
  , 42
  , 42
  , 42
  , 42
  , 42
  , 43
  , 43
  , 43
  , 43
  , 43
  , 43
  , 43
  , 43
  , 43
  , 43
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 41
  , 41
  , 41
  , 41
  , 41
  , 41
  , 44
  , 44
  , 44
  , 44
  , 44
  , 44
  , 44
  , 44
  , 44
  , 44
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 45
  , 47
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 53
  , 53
  , 53
  , 53
  , 53
  , 53
  , 53
  , 53
  , 53
  , 53
  , 5
  , 5
  , 5
  , 5
  , 5
  , 5
  , 5
  , 5
  , 5
  , 5
  , 54
  , 54
  , 54
  , 54
  , 54
  , 54
  , 54
  , 54
  , 54
  , 54
  , 55
  , 55
  , 55
  , 55
  , 55
  , 55
  , 55
  , 55
  , 55
  , 55
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 53
  , 53
  , 53
  , 53
  , 53
  , 53
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 58
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 60
  , 60
  , 60
  , 60
  , 60
  , 60
  , 60
  , 60
  , 60
  , 60
  , 65
  , 65
  , 65
  , 65
  , 65
  , 65
  , 65
  , 65
  , 65
  , 65
  , 67
  , 67
  , 67
  , 67
  , 67
  , 67
  , 67
  , 67
  , 67
  , 67
  , 68
  , 68
  , 68
  , 68
  , 68
  , 68
  , 68
  , 68
  , 68
  , 68
  , 69
  , 69
  , 69
  , 69
  , 69
  , 69
  , 69
  , 69
  , 69
  , 69
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 65
  , 65
  , 65
  , 65
  , 65
  , 65
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 83
  , 83
  , 83
  , 83
  , 83
  , 83
  , 83
  , 83
  , 83
  , 83
  , 75
  , 75
  , 75
  , 75
  , 75
  , 75
  , 75
  , 75
  , 75
  , 75
  , 72
  , 72
  , 72
  , 72
  , 72
  , 72
  , 72
  , 72
  , 72
  , 72
  , 71
  , 71
  , 71
  , 71
  , 71
  , 71
  , 71
  , 71
  , 71
  , 71
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 70
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 75
  , 75
  , 75
  , 75
  , 75
  , 75
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 1391)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 109
  , 117
  , 110
  , 99
  , 109
  , 35
  , 100
  , 58
  , 108
  , 58
  , 116
  , 114
  , 114
  , 121
  , 58
  , 114
  , 108
  , 58
  , 32
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 97
  , 98
  , 58
  , 58
  , 58
  , 116
  , 103
  , 104
  , 122
  , 114
  , 104
  , 110
  , 108
  , 98
  , 111
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 108
  , 110
  , 105
  , 99
  , 121
  , 121
  , 114
  , 103
  , 105
  , -1
  , 98
  , -1
  , -1
  , 101
  , 99
  , -1
  , 104
  , 105
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 112
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , 121
  , -1
  , -1
  , -1
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , -1
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 32
  , -1
  , -1
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , 39
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 63
  , 64
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , 91
  , 92
  , 93
  , 94
  , 95
  , 96
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 124
  , 125
  , 126
  , 127
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 90)
  [ 90
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 46
  , 46
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 59
  , 59
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 90
  , 90
  , 90
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 90)
  [ AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 13
  , AlexAcc 12
  , AlexAcc 11
  , AlexAcc 10
  , AlexAcc 9
  , AlexAcc 8
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  , AlexAccSkip
  , AlexAccSkip
  , AlexAccSkip
  , AlexAccSkip
  , AlexAccSkip
  , AlexAccSkip
  , AlexAccSkip
  ]

alex_actions = array (0 :: Int, 14)
  [ (13,alex_action_0)
  , (12,alex_action_1)
  , (11,alex_action_2)
  , (10,alex_action_3)
  , (9,alex_action_4)
  , (8,alex_action_5)
  , (7,alex_action_6)
  , (6,alex_action_7)
  , (5,alex_action_8)
  , (4,alex_action_9)
  , (3,alex_action_10)
  , (2,alex_action_11)
  , (1,alex_action_12)
  , (0,alex_action_13)
  ]

{-# LINE 32 "Tokens.x" #-}


data Token
  = TokenBYR Year
  | TokenIYR Year
  | TokenEYR Year
  | TokenHGT Length
  | TokenHCL Hex
  | TokenECL Color
  | TokenPID Id
  | TokenWhite
    deriving Show

tokenize :: String -> [Token]
tokenize
  = alexScanTokens

fromTokens :: [Token] -> Passport
fromTokens []
  = empty
fromTokens (t:ts)
  = case t of
      TokenBYR x -> passport {byr = Just x}
      TokenIYR x -> passport {iyr = Just x}
      TokenEYR x -> passport {eyr = Just x}
      TokenHGT x -> passport {hgt = Just x}
      TokenHCL x -> passport {hcl = Just x}
      TokenECL x -> passport {ecl = Just x}
      TokenPID x -> passport {pid = Just x}
    where
      passport = fromTokens ts

alex_action_0 = \s -> TokenBYR $ Year $ read $ drop 4 s
alex_action_1 = \s -> TokenIYR $ Year $ read $ drop 4 s
alex_action_2 = \s -> TokenEYR $ Year $ read $ drop 4 s
alex_action_3 = \s -> TokenHGT $ CM $ read $ take 3 $ drop 4 s
alex_action_4 = \s -> TokenHGT $ IN $ read $ take 2 $ drop 4 s
alex_action_5 = \s -> TokenHCL $ Hex $ drop 5 s
alex_action_6 = \_ -> TokenECL AMB
alex_action_7 = \_ -> TokenECL BLU
alex_action_8 = \_ -> TokenECL BRN
alex_action_9 = \_ -> TokenECL GRY
alex_action_10 = \_ -> TokenECL GRN
alex_action_11 = \_ -> TokenECL HZL
alex_action_12 = \_ -> TokenECL OTH
alex_action_13 = \s -> TokenPID $ Id $ read $ drop 4 s
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine































































alexIndexInt16OffAddr arr off = arr ! off




















alexIndexInt32OffAddr arr off = arr ! off











quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->



    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->



    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.

