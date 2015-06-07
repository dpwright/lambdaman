{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Z80
import ZXSpectrum

main = defaultMain "lambdaman" "lambdaprog" . org 0x6000 $ mdo
  end
