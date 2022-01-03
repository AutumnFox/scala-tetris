package me.afox.tetris
import me.afox.tetris.ui.gdx.MyGdxGame

import scala.language.postfixOps
import com.badlogic.gdx.backends.lwjgl.LwjglApplication
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration
import me.afox.tetris.domain.{Figures, Tetris}

object Bootstrap {
  private val pod = new Tetris()

  private val screenBlockSize = 50
  private val height = pod.height * screenBlockSize
  private val width = pod.width * screenBlockSize

  def main(arg: Array[String]): Unit = {
    val config = new LwjglApplicationConfiguration
    config.height = height
    config.width = width
    config.forceExit = false
    config.fullscreen = false
    config.samples = 4

    new LwjglApplication(new MyGdxGame(pod, screenBlockSize), config)
  }
}








