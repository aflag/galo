module Display (display, idle) where
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


idle = postRedisplay Nothing

drawAxis = do
    renderPrimitive Lines $ do
        preservingClientAttrib [AllClientAttributes] $ do
            color $ Color3 1 0 (0::GLfloat)
            vertex $ Vertex3 (-99999.0) 0 (0::GLfloat)
            vertex $ Vertex3 (99999.0) 0 (0::GLfloat)
            color $ Color3 0 1 (0::GLfloat)
            vertex $ Vertex3 0 (-99999.0) (0::GLfloat)
            vertex $ Vertex3 0 (99999.0) (0::GLfloat)
            color $ Color3 0 0 (1::GLfloat)
            vertex $ Vertex3 (0::GLfloat) 0 (-99999.0)
            vertex $ Vertex3 (0::GLfloat) 0 (99999.0)

{- This function is responsible for the creation of the display matrix. As I
 - understand OpenGL, it's all about displaying a matrix. There's several
 - oprations you can do with it. The operations happen sequencially following
 - the IO monad.
 -
 - The display function is where everything comes together. It's called whenever
 - THE SYSTEM wants to display something. The swapBuffers at the end means that
 - all that happened in this function was going on in the background, but
 - swapBuffers swap the current matrix with the one constructed in the
 - background. For that to work initialDisplayMode $= [DoubleBuffered] must be
 - set.
 -}
display points rotX rotY pos = do
    clear [ColorBuffer]

    loadIdentity

    -- rotate on X-axis.
    x <- get rotX
    rotate x $ Vector3 1 0 (0::GLfloat)

    -- rotate on Y-axis.
    y <- get rotY
    rotate y $ Vector3 0 1 (0::GLfloat)

    -- properly set the position seen on the graph
    (x, y, z) <- get pos
    translate $ Vector3 x y z

    mapM_ point points

    drawAxis

    swapBuffers

    where
        point (x, y, z) = renderPrimitive Points $ vertex $ Vertex3 x y z
