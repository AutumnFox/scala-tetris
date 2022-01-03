package me.afox.tetris.ui.gdx

import com.badlogic.gdx.ApplicationAdapter
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.graphics.{Pixmap, Texture}
import com.badlogic.gdx.utils.ScreenUtils
import me.afox.tetris.domain.{Figure, Tetris}

class MyGdxGame(val pod: Tetris, screenBlockSize: Int) extends ApplicationAdapter {
  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  private lazy val batch = new SpriteBatch
  private lazy val pixmap = new Pixmap(screenBlockSize - 1, screenBlockSize - 1, Pixmap.Format.RGBA8888)

  //lazy val  img: Texture = new Texture("badlogic.jpg")
  //lazy val box = new Rectangle(0,0,100,100);
  lazy val brickTaken: Texture = {
    pixmap.setColor(0.3F, 0, 0, 1)
    pixmap.fill()
    new Texture(pixmap)
  }

  lazy val brickFree: Texture = {
    pixmap.setColor(0, 0, 1, 1)
    pixmap.fill()
    new Texture(pixmap)
  }

  override def create(): Unit = {
    //batch = new SpriteBatch
    //img = new Texture("badlogic.jpg")
    //val t = img
    batch
    brickFree
    brickTaken
  }

  def renderPod(pod: Tetris): Unit = {
    pod.foreachCell(drawCell(pod.figure))
  }

  def drawCell(figure: Figure)(colNumber: Int, rowNumber: Int, cellContent: Option[java.awt.Color]): Unit = {
    val block = if (figure.brickAt(colNumber, rowNumber).isEmpty && cellContent.isEmpty)
      brickFree else brickTaken

    batch.draw(block, colNumber * screenBlockSize.toFloat, (pod.height - rowNumber - 1) * screenBlockSize.toFloat)
  }

  var mover: FigureMover = FigureMover(pod, (0, 0, 0, 0, 0), changed = true)

  override def render(): Unit = {
    ScreenUtils.clear(1, 0, 0, 1)

    batch.begin()
    renderPod(mover.pod)
    batch.end()

    mover = mover.processEvents
  }

  override def dispose(): Unit = {
    brickFree.dispose()
    brickTaken.dispose()
    pixmap.dispose()
    batch.dispose()
  }
}
