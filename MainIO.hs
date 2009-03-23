module MainIO (initIO) where
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
import Data.IORef

{- This is the state closure which serves as the base of communication between
 - display and keyboardMouse functions.
 -}
data KBMState = KBMState {  rotX :: GLfloat
                          , rotY :: GLfloat
                          , pos  :: (GLfloat, GLfloat, GLfloat) 
                          , zoom :: GLfloat
                         }

initIO = do
    kbm <- newIORef $ KBMState {  rotX = 0
                                , rotY = 0
                                , pos  = (0, 0, 0)
                                , zoom = 1 }
    return (display kbm, keyboardMouse kbm)

{---------------------------
 -        Display          -
 ---------------------------}
-- XXX: do a proper implementation of an ``infinite'' line.
drawAxis = do
    renderPrimitive Lines $ do
        color $ Color3 1 0 (0::GLfloat)
        vertex $ Vertex3 (-99999.0) 0 (0::GLfloat)
        vertex $ Vertex3 (99999.0) 0 (0::GLfloat)
        color $ Color3 0 1 (0::GLfloat)
        vertex $ Vertex3 0 (-99999.0) (0::GLfloat)
        vertex $ Vertex3 0 (99999.0) (0::GLfloat)
        color $ Color3 0 0 (1::GLfloat)
        vertex $ Vertex3 (0::GLfloat) 0 (-99999.0)
        vertex $ Vertex3 (0::GLfloat) 0 (99999.0)
        color $ Color3 1 1 (1::GLfloat)

{- The display function is where everything comes together. It's called whenever
 - THE SYSTEM wants to display something. The swapBuffers at the end means that
 - all that happened in this function was going on in the background, but
 - swapBuffers swap the current matrix with the one constructed in the
 - background. For that to work initialDisplayMode $= [DoubleBuffered] must be
 - set.
 -}
display :: IORef KBMState -> [(Double, Double, Double)] -> IO ()
display kbm' points = do
    clear [ColorBuffer]

    loadIdentity

    kbm <- get kbm'
    -- rotate on X-axis.
    rotate (rotX kbm) $ Vector3 1 0 (0::GLfloat)

    -- rotate on Y-axis.
    rotate (rotY kbm) $ Vector3 0 1 (0::GLfloat)

    -- properly set the position seen on the graph
    let (x, y, z) = pos kbm
    translate $ Vector3 x y z

    let zm = zoom kbm
    scale zm zm zm

    renderPrimitive LineStrip $ mapM_ point points

    drawAxis

    swapBuffers

    where
        point (x, y, z) = vertex $ Vertex3 x y z


{---------------------------
 -     Keyboard/Mouse      -
 ---------------------------}
rotationConst :: GLfloat
rotationConst = 2.5

moveConst :: GLfloat
moveConst = 0.02

zoomConst :: GLfloat
zoomConst = 0.01

keyboard :: IORef KBMState -> Key -> KeyState -> IO ()
-- x-axis rotation
keyboard kbm' (Char 'z') Down = do
    kbm <- get kbm'
    kbm' $= kbm { rotX = rotX kbm + rotationConst }
keyboard kbm' (Char 'a') Down = do
    kbm <- get kbm'
    kbm' $= kbm { rotX = rotX kbm - rotationConst }

-- y-axis rotation
keyboard kbm' (Char 'q') Down = do
    kbm <- get kbm'
    kbm' $= kbm { rotY = rotY kbm + rotationConst }
keyboard kbm' (Char 'w') Down = do
    kbm <- get kbm'
    kbm' $= kbm { rotY = rotY kbm - rotationConst }

-- x-axis to the right
keyboard kbm' (Char 'l') Down = do
    kbm <- get kbm'
    let (x, y, z) = pos kbm
    kbm' $= kbm { pos = (x-moveConst, y, z) }

-- x-axis to the left
keyboard kbm' (Char 'h') Down = do
    kbm <- get kbm'
    let (x, y, z) = pos kbm
    kbm' $= kbm { pos = (x+moveConst, y, z) }

-- y-axis to the up
keyboard kbm' (Char 'k') Down = do
    kbm <- get kbm'
    let (x, y, z) = pos kbm
    kbm' $= kbm { pos = (x, y-moveConst, z) }

-- y-axis to the down
keyboard kbm' (Char 'j') Down = do
    kbm <- get kbm'
    let (x, y, z) = pos kbm
    kbm' $= kbm { pos = (x, y+moveConst, z) }

-- z-axis to the front
keyboard kbm' (Char 'f') Down = do
    kbm <- get kbm'
    let (x, y, z) = pos kbm
    kbm' $= kbm { pos = (x, y, z-moveConst) }

-- z-axis to the back
keyboard kbm' (Char 'd') Down = do
    kbm <- get kbm'
    let (x, y, z) = pos kbm
    kbm' $= kbm { pos = (x, y, z+moveConst) }

-- zoom in
keyboard kbm' (Char 'v') Down = do
    kbm <- get kbm'
    kbm' $= kbm { zoom = zoom kbm + zoomConst }

-- zoom out
keyboard kbm' (Char 'c') Down = do
    kbm <- get kbm'
    let z' = zoom kbm - zoomConst in
        if z' <= 0
            then kbm' $= kbm { zoom = 0 }
            else kbm' $= kbm { zoom = z' }

keyboard _ _ _ = return ()

keyboardMouse :: IORef KBMState -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse kbm key state modifiers position = do
    keyboard kbm key state
    postRedisplay Nothing
