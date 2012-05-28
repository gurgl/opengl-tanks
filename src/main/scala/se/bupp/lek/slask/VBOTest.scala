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
/**
 * $Id$
 *
 * Simple java test program.
 *
 * @author elias_naur <elias_naur@users.sourceforge.net>
 * @version $Revision$
 */

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.FloatBuffer
import java.lang.Exception
import org.lwjgl.Sys
import org.lwjgl.input.Keyboard
import org.lwjgl.input.Mouse
import org.lwjgl.opengl.Display
import org.lwjgl.opengl.DisplayMode
import org.lwjgl.opengl.GLContext
import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._
import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu.GLU._

final object VBOTest {

  try {

    var modes: Array[DisplayMode] = Display.getAvailableDisplayModes;

    var mode = modes.find {
      mode =>
        mode.getWidth == 640 && mode.getHeight == 480 && mode.getBitsPerPixel >= 16
    }

    mode.foreach {
      m =>
        System.out.println("Setting display mode to " + m)
        Display.setDisplayMode(m)
        System.out.println("Created display.")
    }
  }

  catch {
    case e: Exception => {
      System.err.println("Failed to create display due to " + e)
    }
  }
  try {
    Display.create
    System.out.println("Created OpenGL.")
  }
  catch {
    case e: Exception => {
      System.err.println("Failed to create OpenGL due to " + e)
      System.exit(1)
    }
  }

  def main(arguments: Array[String]): Unit = {
    try {
      init
      while (!finished) {
        Display.update
        if (!Display.isVisible) Thread.sleep(200)
        else if (Display.isCloseRequested) System.exit(0)
        mainLoop
        render
      }
    }
    catch {
      case t: Exception => {
        t.printStackTrace
      }
    }
    finally {
      cleanup
    }
    System.exit(0)
  }

  /**
   * All calculations are done in here
   */
  private def mainLoop: Unit = {
    angle += 1f
    if (angle > 360.0f) angle = 0.0f
    if (Mouse.getDX != 0 || Mouse.getDY != 0 || Mouse.getDWheel != 0) {
      System.out.println("Mouse moved " + Mouse.getDX + " " + Mouse.getDY + " " + Mouse.getDWheel)
    }
    var i: Int = 0
    while (i < Mouse.getButtonCount) {
      if (Mouse.isButtonDown(i)) {
        System.out.println("Button " + i + " down")
      }

      i += 1;
    }

    if (Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) {
      finished = true
    }
    i = 0
    while (i < Keyboard.getNumKeyboardEvents) {
      Keyboard.next
      if (Keyboard.getEventKey == Keyboard.KEY_ESCAPE && Keyboard.getEventKeyState) {
        finished = true
      }
      if (Keyboard.getEventKey == Keyboard.KEY_T && Keyboard.getEventKeyState) {
        System.out.println("Current time: " + Sys.getTime)
      }

      i += 1
    }
  }

  /**
   * All rendering is done in here
   */
  private def render: Unit = {
    glClear(GL_COLOR_BUFFER_BIT)
    glPushMatrix
    glTranslatef(Display.getDisplayMode.getWidth / 2, Display.getDisplayMode.getHeight / 2, 0.0f)
    glRotatef(angle, 0, 0, 1.0f)
    var new_mapped_buffer: ByteBuffer = glMapBufferARB(GL_ARRAY_BUFFER_ARB, GL_WRITE_ONLY_ARB, mapped_buffer)
    if (new_mapped_buffer ne mapped_buffer) mapped_float_buffer = new_mapped_buffer.order(ByteOrder.nativeOrder).asFloatBuffer
    mapped_buffer = new_mapped_buffer
    mapped_float_buffer.rewind
    vertices.rewind
    mapped_float_buffer.put(vertices)
    if (glUnmapBufferARB(GL_ARRAY_BUFFER_ARB)) glDrawArrays(GL_QUADS, 0, 4)
    glPopMatrix
  }

  /**
   * Initialize
   */
  private def init: Unit = {
    System.out.println("Timer resolution: " + Sys.getTimerResolution)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity
    gluOrtho2D(0, Display.getDisplayMode.getWidth, 0, Display.getDisplayMode.getHeight)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity
    glViewport(0, 0, Display.getDisplayMode.getWidth, Display.getDisplayMode.getHeight)
    if (!GLContext.getCapabilities.GL_ARB_vertex_buffer_object) {
      System.out.println("ARB VBO not supported!")
      System.exit(1)
    }
    buffer_id = glGenBuffersARB
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, buffer_id)
    vertices = ByteBuffer.allocateDirect(2 * 4 * 4).order(ByteOrder.nativeOrder).asFloatBuffer
    vertices.put(-50).put(-50).put(50).put(-50).put(50).put(50).put(-50).put(50)
    glBufferDataARB(GL_ARRAY_BUFFER_ARB, 2 * 4 * 4, GL_STREAM_DRAW_ARB)
    glEnableClientState(GL_VERTEX_ARRAY)
    glVertexPointer(2, GL_FLOAT, 0, 0)
  }

  /**
   * Cleanup
   */
  private def cleanup: Unit = {
    glDeleteBuffersARB(buffer_id)
    Display.destroy
  }

  /**
   * Is the game finished?
   */
  private var finished: Boolean = false
  /**
   * A rotating square!
   */
  private var angle: Float = 0.0f
  private var buffer_id: Int = _
  private var vertices: FloatBuffer = null
  private var mapped_buffer: ByteBuffer = null
  private var mapped_float_buffer: FloatBuffer = null
}