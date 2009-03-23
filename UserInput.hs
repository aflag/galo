module UserInput (keyboardMouse) where
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


rotationConst :: GLfloat
rotationConst = 2.5
moveConst :: GLfloat
moveConst = 0.02
zoomConst :: GLfloat
zoomConst = 0.01

-- x-axis rotation
keyboard rotX _ _ _ (Char 'z') Down = do
    x <- get rotX
    rotX $= x + rotationConst
keyboard rotX _ _ _ (Char 'a') Down = do
    x <- get rotX
    rotX $= x - rotationConst
-- y-axis rotation
keyboard _ rotY _ _ (Char 'q') Down = do
    y <- get rotY
    rotY $= y + rotationConst
keyboard _ rotY _ _ (Char 'w') Down = do
    y <- get rotY
    rotY $= y - rotationConst
-- x-axis to the right
keyboard _ _ pos _ (Char 'l') Down = do
    (x, y, z) <- get pos
    pos $= (x-moveConst, y, z)
-- x-axis to the left
keyboard _ _ pos _ (Char 'h') Down = do
    (x, y, z) <- get pos
    pos $= (x+moveConst, y, z)
-- y-axis to the up
keyboard _ _ pos _ (Char 'k') Down = do
    (x, y, z) <- get pos
    pos $= (x, y-moveConst, z)
-- y-axis to the down
keyboard _ _ pos _ (Char 'j') Down = do
    (x, y, z) <- get pos
    pos $= (x, y+moveConst, z)
-- z-axis to the front
keyboard _ _ pos _ (Char 'f') Down = do
    (x, y, z) <- get pos
    pos $= (x, y, z-moveConst)
-- z-axis to the back
keyboard _ _ pos _ (Char 'd') Down = do
    (x, y, z) <- get pos
    pos $= (x, y, z+moveConst)
-- zoom in
keyboard _ _ _ zoom (Char 'v') Down = do
    z <- get zoom
    zoom $= z + zoomConst
-- zoom out
keyboard _ _ _ zoom (Char 'c') Down = do
    z <- get zoom
    let z' = z - zoomConst in
        if z' <= 0
            then zoom $= 0
            else zoom $= z'
keyboard _ _ _ _ _ _ = return ()

{-                  mine                  opengl          -
 -              ______|______     ___________|__________  -
 -             |             |   |                      | -}
keyboardMouse rotX rotY pos zoom key state modifiers position = do
    keyboard rotX rotY pos zoom key state
    postRedisplay Nothing
