module Main (show3Dvec, main) where
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

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Display
import UserInput
import Control.Concurrent


show3Dvec :: (Double -> (Double, Double, Double)) -> [Double] -> IO ()
show3Dvec f range = do
    (progname,_) <- getArgsAndInitialize

    initialDisplayMode $= [DoubleBuffered]

    createWindow "Galo - 3D vec"

    windowSize $= Size 800 600

    rotX <- newIORef (0.0::GLfloat)
    rotY <- newIORef (0.0::GLfloat)
    pos <- newIORef (0.0::GLfloat, 0.0, 0.0)

    displayCallback $= display (map f range) rotX rotY pos

    idleCallback $= Just idle

    keyboardMouseCallback $= Just (keyboardMouse rotX rotY pos)

    mainLoop

main = show3Dvec (\t -> (t, t, 0)) [-1.0,-0.99 .. 1.0]
--show3Dvec (\t -> (t**3, log (3 - t), sqrt t)) [-1.0,-0.99 .. 1.0]

