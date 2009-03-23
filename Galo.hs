module Galo (show3Dvec, show3Dvec') where
{-  Copyright (c) 2009, Rafael Cunha de Almeida <almeidaraf@gmail.com>
 -
 - Permission to use, copy, modify, and/or distribute this software for any
 - purpose with or without fee is hereby granted, provided that the above
 - copyright notice and this permission notice appear in all copies.
 -
 - THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 - WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 - MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 - ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 - WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 - ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 - OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 -}

import Graphics.UI.GLUT

import MainIO

show3Dvec :: (Double -> (Double, Double, Double)) -> [Double] -> IO ()
show3Dvec = show3Dvec' (return ())

show3Dvec' :: IO () -> (Double -> (Double, Double, Double)) -> [Double] -> IO ()
show3Dvec' userConfig f range = do
    (progname,_) <- getArgsAndInitialize

    initialDisplayMode $= [DoubleBuffered]

    createWindow "Galo - 3D vec"

    windowSize $= Size 800 600

    clearColor $= Color4 0.5 0.6 (1.0::GLfloat) 0

    lineWidth $= 2

    (display, keyboardMouse) <- initIO
    displayCallback $= display (map f range)
    keyboardMouseCallback $= Just keyboardMouse

    actionOnWindowClose $= MainLoopReturns

    userConfig

    mainLoop

--show3Dvec (\t -> (t, t, 0)) [-1.0,-0.99 .. 1.0]
--show3Dvec (\t -> (t**3, log (3 - t), sqrt t)) [-1.0,-0.99 .. 1.0]
