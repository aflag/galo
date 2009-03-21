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

{-          mine               opengl 
 -          __|__      __________|__________
 -         |     |    |                     |
 - params: rotX rotY key state modifiers position
 -}
-- x-axis rotation
keyboardMouse rotX _ _ (Char 'z') Down _ _ = do
    x <- get rotX
    rotX $= x - rotationConst
keyboardMouse rotX _ _ (Char 'a') Down _ _ = do
    x <- get rotX
    rotX $= x + rotationConst
-- y-axis rotation
keyboardMouse _ rotY _ (Char 'q') Down _ _ = do
    y <- get rotY
    rotY $= y - rotationConst
keyboardMouse _ rotY _ (Char 'w') Down _ _ = do
    y <- get rotY
    rotY $= y + rotationConst
-- x-axis to the right
keyboardMouse _ _ pos (Char 'l') Down _ _ = do
    (x, y, z) <- get pos
    pos $= (x-moveConst, y, z)
-- x-axis to the left
keyboardMouse _ _ pos (Char 'h') Down _ _ = do
    (x, y, z) <- get pos
    pos $= (x+moveConst, y, z)
-- y-axis to the up
keyboardMouse _ _ pos (Char 'k') Down _ _ = do
    (x, y, z) <- get pos
    pos $= (x, y-moveConst, z)
-- y-axis to the down
keyboardMouse _ _ pos (Char 'j') Down _ _ = do
    (x, y, z) <- get pos
    pos $= (x, y+moveConst, z)
-- z-axis to the front
keyboardMouse _ _ pos (Char 'f') Down _ _ = do
    (x, y, z) <- get pos
    pos $= (x, y, z-moveConst)
-- z-axis to the back
keyboardMouse _ _ pos (Char 'd') Down _ _ = do
    (x, y, z) <- get pos
    pos $= (x, y, z+moveConst)
keyboardMouse _ _ _ _ _ _ _ = return ()
