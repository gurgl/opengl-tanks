package se.bupp.lek.slask

/*
 * Copyright (c) 2002-2008 LWJGL Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name of 'LWJGL' nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.IntBuffer
import org.lwjgl.LWJGLException
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.Display
import org.lwjgl.opengl.DisplayMode
import org.lwjgl.opengl.Pbuffer
import org.lwjgl.opengl.PixelFormat
import org.lwjgl.util.vector.Vector2f
import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu.GLU._

/**
 *
 * Tests Pbuffers
 *
 * @author elias_naur <elias_naur@users.sourceforge.net>
 * @version $Revision$
 * $Id$
 */
object PbufferTest {
  /**
   * Test entry point
   */
  def main(args: Array[String]): Unit = {
    System.out.println("Change between fullscreen and windowed mode, by pressing F and W respectively")
    System.out.println("Move quad using arrowkeys, and change rotation using +/-")
    var fswTest: PbufferTest = new PbufferTest
    fswTest.execute
    System.exit(0)
  }

  /**Max speed of all changable attributes */
  private final val MAX_SPEED: Float = 20.0f
  /**Pbuffer instance */
  private var pbuffer: Pbuffer = null
  /**The shared texture */
  private var tex_handle: Int = 0
}

class PbufferTest {

  import PbufferTest._

  /**
   * Executes the test
   */
  def execute: Unit = {
    initialize
    mainLoop
    cleanup
  }

  /**
   * Initializes the test
   */
  private def initialize: Unit = {
    try {
      pbuffer = new Pbuffer(512, 512, new PixelFormat, null, null)
      mode = findDisplayMode(1440, 900, 16)
      Display.setDisplayMode(mode)
      Display.create
      if ((Pbuffer.getCapabilities & Pbuffer.PBUFFER_SUPPORTED) == 0) {
        System.out.println("No Pbuffer support!")
        System.exit(1)
      }
      System.out.println("Pbuffer support detected")
      glInit
      initPbuffer
      Keyboard.create
      quadPosition = new Vector2f(100f, 100f)
      quadVelocity = new Vector2f(1.0f, 1.0f)
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
  }

  /**
   * Runs the main loop of the "test"
   */
  private def mainLoop: Unit = {
    while (!Keyboard.isKeyDown(Keyboard.KEY_ESCAPE) && !Display.isCloseRequested) {
      if (Display.isVisible) {
        processKeyboard
        logic
        render
      }
      else {
        if (Display.isDirty) {
          render
        }
        try {
          Thread.sleep(100)
        }
        catch {
          case inte: Exception => {
          }
        }
      }
      Display.update
    }
  }

  /**
   * Performs the logic
   */
  private def logic: Unit = {
    angle += angleRotation
    if (angle > 90.0f) {
      angle = 0.0f
    }
    quadPosition.x += quadVelocity.x
    quadPosition.y += quadVelocity.y
    if (quadPosition.x + 50 >= mode.getWidth || quadPosition.x - 50 <= 0) {
      quadVelocity.x *= -1
    }
    if (quadPosition.y + 50 >= mode.getHeight || quadPosition.y - 50 <= 0) {
      quadVelocity.y *= -1
    }
  }

  private def render: Unit = {
    if (pbuffer.isBufferLost) {
      System.out.println("Buffer contents lost - will recreate the buffer")
      pbuffer.destroy
      try {
        pbuffer = new Pbuffer(512, 512, new PixelFormat, null, null)
        initPbuffer
      }
      catch {
        case e: LWJGLException => {
          e.printStackTrace
        }
      }
    }
    try {
      pbuffer.makeCurrent
    }
    catch {
      case e: LWJGLException => {
        throw new RuntimeException(e)
      }
    }
    glClear(GL_COLOR_BUFFER_BIT)
    glPushMatrix;
    {
      glTranslatef(quadPosition.x, quadPosition.y, 0)
      glRotatef(angle, 0.0f, 0.0f, 1.0f)
      glColor3f(1.0f, 1.0f, 1.0f)
      glBegin(GL_QUADS);
      {
        glVertex2i(-50, -50)
        glVertex2i(50, -50)
        glVertex2i(50, 50)
        glVertex2i(-50, 50)
      }
      glEnd
    }
    glPopMatrix
    glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, 512, 512, 0)
    try {
      Display.makeCurrent
    }
    catch {
      case e: LWJGLException => {
        throw new RuntimeException(e)
      }
    }
    glClear(GL_COLOR_BUFFER_BIT)
    glPushMatrix;
    {
      glTranslatef(quadPosition.x, quadPosition.y, 0)
      glRotatef(angle, 0.0f, 0.0f, 1.0f)
      glColor3f(1.0f, 1.0f, 0.0f)
      glBegin(GL_QUADS);
      {
        glTexCoord2f(0f, 0f)
        glVertex2i(-50, -50)
        glTexCoord2f(1f, 0f)
        glVertex2i(50, -50)
        glTexCoord2f(1f, 1f)
        glVertex2i(50, 50)
        glTexCoord2f(0f, 1f)
        glVertex2i(-50, 50)
      }
      glEnd
    }
    glPopMatrix
  }

  private def initPbuffer: Unit = {
    try {
      pbuffer.makeCurrent
      initGLState(256, 256, 0.5f)
      glBindTexture(GL_TEXTURE_2D, tex_handle)
      Display.makeCurrent
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
  }

  /**
   * Processes keyboard input
   */
  private def processKeyboard: Unit = {
    Keyboard.poll
    if (Keyboard.isKeyDown(Keyboard.KEY_F)) {
      try {
        Display.setDisplayMode(mode)
        Display.setFullscreen(true)
      }
      catch {
        case e: Exception => {
          e.printStackTrace
        }
      }
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_W)) {
      try {
        Display.setFullscreen(false)
      }
      catch {
        case e: Exception => {
          e.printStackTrace
        }
      }
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_UP)) {
      quadVelocity.y += 0.1f
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_DOWN)) {
      quadVelocity.y -= 0.1f
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) {
      quadVelocity.x += 0.1f
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_LEFT)) {
      quadVelocity.x -= 0.1f
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_ADD)) {
      angleRotation += 0.1f
    }
    if (Keyboard.isKeyDown(Keyboard.KEY_SUBTRACT)) {
      angleRotation -= 0.1f
    }
    if (quadVelocity.x < -MAX_SPEED) {
      quadVelocity.x = -MAX_SPEED
    }
    if (quadVelocity.x > MAX_SPEED) {
      quadVelocity.x = MAX_SPEED
    }
    if (quadVelocity.y < -MAX_SPEED) {
      quadVelocity.y = -MAX_SPEED
    }
    if (quadVelocity.y > MAX_SPEED) {
      quadVelocity.y = MAX_SPEED
    }
    if (angleRotation < 0.0f) {
      angleRotation = 0.0f
    }
    if (angleRotation > MAX_SPEED) {
      angleRotation = MAX_SPEED
    }
  }

  private def destroyTexture: Unit = {
    var buffer: IntBuffer = ByteBuffer.allocateDirect(4).order(ByteOrder.nativeOrder).asIntBuffer
    buffer.put(0, tex_handle)
    glDeleteTextures(buffer)
  }

  /**
   *  Cleans up the test
   */
  private def cleanup: Unit = {
    destroyTexture
    pbuffer.destroy
    Display.destroy
  }

  /**
   * Retrieves a displaymode, if one such is available
   *
   * @param width Required width
   * @param height Required height
   * @param bpp Minimum required bits per pixel
   * @return
   */
  private def findDisplayMode(width: Int, height: Int, bpp: Int): DisplayMode = {
    var modes: Array[DisplayMode] = Display.getAvailableDisplayModes
    for (mode <- modes) {
      if (mode.getWidth == width && mode.getHeight == height && mode.getBitsPerPixel >= bpp) return mode
    }
    return null
  }

  private def initGLState(width: Int, height: Int, color: Float): Unit = {
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity
    gluOrtho2D(0, mode.getWidth, 0, mode.getHeight)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity
    glViewport(0, 0, width, height)
    glClearColor(color, color, color, 0.0f)
  }

  /**
   * Initializes OGL
   */
  private def glInit: Unit = {
    Display.setVSyncEnabled(true)
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE)
    glEnable(GL_TEXTURE_2D)
    var buffer: IntBuffer = ByteBuffer.allocateDirect(4).order(ByteOrder.nativeOrder).asIntBuffer
    glGenTextures(buffer)
    tex_handle = buffer.get(0)
    glBindTexture(GL_TEXTURE_2D, tex_handle)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    initGLState(mode.getWidth, mode.getHeight, 0f)
  }

  /**Intended deiplay mode */
  private var mode: DisplayMode = null
  /**our quad moving around */
  private var quadPosition: Vector2f = null
  /**our quadVelocity */
  private var quadVelocity: Vector2f = null
  /**angle of quad */
  private var angle: Float = 0.0f
  /**degrees to rotate per frame */
  private var angleRotation: Float = 1.0f
}