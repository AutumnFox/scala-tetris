package me.afox.tetris.ui.gdx

import me.afox.tetris.domain.Tetris

case class FigureMover(
    pod: Tetris,
    data: (Float, Float, Float, Float, Float),
    changed: Boolean) {

  val (upKeyTime, leftKeyTime, rightKeyTime, downKeyTime, fallInterval) = data

  def makeMove(pod: Tetris, action: Tetris => Tetris, altAction: Tetris => Tetris): Tetris = {
    val nextStep = action(pod)
    if (pod.testCommit && nextStep.testCommit) nextStep else altAction(pod)
  }

  def processMovements(pod: Tetris, data: (Float, Float, Float, Float, Float)): (Tetris, (Float, Float, Float, Float, Float)) = {
    data match {
      case (up, left, right, down, fall) =>
        val (pod1, up1) = if (up > 0.07) (makeMove(pod, p => p.rotate, p => p), 0.0F) else (pod, up)
        val (pod2, left1) = if (left > 0.07) (makeMove(pod1, p => p.stepLeft, p => p), 0.0F) else (pod1, left)
        val (pod3, right1) = if (right > 0.07) (makeMove(pod2, p => p.stepRight, p => p), 0.0F) else (pod2, right)
        val (pod4, down1) = if (down > 0.07) (makeMove(pod3, p => p.stepDown, p => p), 0.0F) else (pod3, down)

        val (pod5, fall1) = if (fall > 0.5) (makeMove(pod4, p => p.stepDown, p => p.commit), 0.0F) else (pod4, fall)

        (pod5, (up1, left1, right1, down1, fall1))

      case _ => (pod, data)
    }
  }

  def processEvents: FigureMover = {
    import com.badlogic.gdx.{Gdx, Input}

    val deltaTime = Gdx.graphics.getDeltaTime

    import Gdx.input
    val up = if (input.isKeyJustPressed(Input.Keys.UP)) 1
    else if (input.isKeyPressed(Input.Keys.UP)) upKeyTime + deltaTime else 0

    val left = if (input.isKeyJustPressed(Input.Keys.LEFT)) 1
    else if (input.isKeyPressed(Input.Keys.LEFT)) leftKeyTime + deltaTime else 0

    val right = if (input.isKeyJustPressed(Input.Keys.RIGHT)) 1
    else if (input.isKeyPressed(Input.Keys.RIGHT)) rightKeyTime + deltaTime else 0

    val down = if (input.isKeyJustPressed(Input.Keys.DOWN)) 1
    else if (input.isKeyPressed(Input.Keys.DOWN)) downKeyTime + deltaTime else 0

    val fall = fallInterval + deltaTime

    val (newPod, newData) = processMovements(pod, (up, left, right, down, fall))

    FigureMover(newPod, newData, pod != newPod)
  }
}
