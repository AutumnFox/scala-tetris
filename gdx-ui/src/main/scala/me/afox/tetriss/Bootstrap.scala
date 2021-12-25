package me.afox.tetriss
import com.badlogic.gdx.graphics.Pixmap
import me.afox.tetriss.ui._

import scala.language.postfixOps
import com.badlogic.gdx.ApplicationAdapter
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g2d.SpriteBatch
import com.badlogic.gdx.utils.ScreenUtils
import com.badlogic.gdx.backends.lwjgl.LwjglApplication
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration
import com.badlogic.gdx.math.Rectangle
import me.afox.tetriss.data.Bricks.Brick
import me.afox.tetriss.data.{Figures, Pod}

object Bootstrap {
  var pod = new Pod()
    .inject(Figures.F_LL)

  val screenBlockSize = 50
  val height = pod.height * screenBlockSize
  val width = pod.width * screenBlockSize

  def main(arg: Array[String]): Unit = {
    val config = new LwjglApplicationConfiguration
    config.height = height
    config.width = width

    new LwjglApplication(new MyGdxGame(pod, screenBlockSize), config)
  }
}


class MyGdxGame(val pod: Pod, screenBlockSize: Int) extends ApplicationAdapter {
  lazy val batch: SpriteBatch = new SpriteBatch
  //lazy val  img: Texture = new Texture("badlogic.jpg")
  //lazy val box = new Rectangle(0,0,100,100);
  lazy val pixmap = new Pixmap(screenBlockSize-1,screenBlockSize-1,Pixmap.Format.RGBA8888);
  lazy val brickTaken = {
    pixmap.setColor(0.3F,0,0,1)
    pixmap.fill()
    new Texture(pixmap);
  }
  lazy val brickFree = {
    pixmap.setColor(0,0,1,1)
    pixmap.fill()
    new Texture(pixmap);
  }

  override def create(): Unit = {
    //batch = new SpriteBatch
    //img = new Texture("badlogic.jpg")
    //val t = img
  }

  def renderPod(pod: Pod): Unit = {
    pod.aggregatePod(
      cellExtractor = (colNumber: Int, rowNumber: Int, rowHead: Option[Brick]) => {
        val isFree = pod.figure.get.brickAt(colNumber, rowNumber).isEmpty && rowHead.isEmpty
        drawBrick(isFree,colNumber, rowNumber)
      },
      cellAggregator = (rowHead: Unit, aggregatedRow: Unit ) => {},
      rowAggregator  = (podHead: Unit, aggregatedPod: Unit) => {}
    )((),())
  }

  def drawBrick(isFree: Boolean, x: Int, y: Int) = {
    val block = if(isFree) brickFree else brickTaken
    batch.draw(block, x * screenBlockSize, (pod.height - y-1) * screenBlockSize);
  }

  var mover = FigureMover(pod, (0,0,0,0,0))
  override def render(): Unit = {
    ScreenUtils.clear(1, 0, 0, 1)
    batch.begin()
    renderPod(mover.pod)
    batch.end()

    mover = mover.processEvents
  }

  override def dispose(): Unit = {
    pixmap.dispose()
    brickFree.dispose()
    brickTaken.dispose()
    batch.dispose()
  }
}

case class FigureMover (
                  val pod: Pod,
                  val data: (Float, Float, Float, Float, Float)) {

  val (upKeyTime, leftKeyTime, rightKeyTime, downKeyTime, fallInterval) = data;

  def makeMove(pod: Pod, action: (Pod)=>Pod, altAction: (Pod)=>Pod) = {
    val nextStep = action(pod)
    if(pod.testCommit && nextStep.testCommit) nextStep else altAction(pod)
  }

  def processMovements(pod: Pod, data: (Float, Float, Float, Float, Float)): (Pod, (Float, Float, Float, Float, Float)) = {
    data match {
      case (up, left, right, down, fall) => {
        val (pod1, up1) = if (up > 0.07) (makeMove(pod, p => p.rotate, p => p), 0.0F) else (pod, up)
        val (pod2, left1) = if (left > 0.07) (makeMove(pod1, p => p.stepLeft, p => p), 0.0F) else (pod1, left)
        val (pod3, right1) = if (right > 0.07) (makeMove(pod2, p => p.stepRight, p => p), 0.0F) else (pod2, right)
        val (pod4, down1) = if (down > 0.07) (makeMove(pod3, p => p.stepDown, p => p), 0.0F) else (pod3, down)
        val (pod5, fall1) = if (fall > 0.5) (makeMove(pod4,  p => p.stepDown, p => p.commit.inject(Figures.F_LR)), 0.0F) else (pod4, fall)

        (pod5, (up1, left1, right1, down1, fall1))
      }
      case _ => (pod, data)
    }
  }

  def processEvents = {
    import com.badlogic.gdx.Gdx
    import com.badlogic.gdx.Input

    val deltaTime = Gdx.graphics.getDeltaTime

    import Gdx.input
    val up = if (input.isKeyJustPressed(Input.Keys.UP)) 1
      else if(input.isKeyPressed(Input.Keys.UP)) upKeyTime + deltaTime else 0

    val left = if (input.isKeyJustPressed(Input.Keys.LEFT)) 1
      else if(input.isKeyPressed(Input.Keys.LEFT)) leftKeyTime + deltaTime else 0

    val right = if (input.isKeyJustPressed(Input.Keys.RIGHT)) 1
      else if(input.isKeyPressed(Input.Keys.RIGHT)) rightKeyTime + deltaTime else 0

    val down = if (input.isKeyJustPressed(Input.Keys.DOWN)) 1
      else if(input.isKeyPressed(Input.Keys.DOWN)) downKeyTime + deltaTime else 0

    val fall = fallInterval + deltaTime

    val(newPod, newData) = processMovements(pod, (up, left, right, down, fall))

    FigureMover(newPod, newData);
  }
}



